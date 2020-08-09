module Language.JSON.Decode

import Data.List

import Language.JSON
import Language.JSON.Data

||| Describes how to turn a `JSON` into a `v`.
||| Use with `decodeJSON` or `decodeString`.
public export
Decoder : Type -> Type
Decoder = \v => JSON -> Either String v


private
error : String -> Decoder a
error expected actual =
    Left $ "Expected " ++ expected ++ ", got: " ++ format 0 actual


||| Run a `Decoder` on some `JSON`.
public export
decodeJSON : Decoder a -> JSON -> Either String a
decodeJSON decoder json =
    decoder json

||| Parse a `String` to `JSON` and run a `Decoder` on it
public export
decodeString : Decoder a -> String -> Either String a
decodeString decoder str =
    case parse str of
        Just json => decoder json
        Nothing => Left "parse error"


||| Given a function, transform a decoder's result
public export
map : (a -> b) -> Decoder a -> Decoder b
map f decoder =
    \json =>
    case decoder json of
        Right x => Right (f x)
        Left x => Left x


||| Always decode the given value. Alias of `pure`.
public export
succeed : a -> Decoder a
succeed x _ =
    Right x


||| Always fail with the given error string.
public export
fail : String -> Decoder a
fail err _ =
    Left err


||| Apply a function to the result of a decoder to get another decoder, then run that
public export
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f decoder json =
    case decoder json of
        Right x => (f x) json
        Left x => Left x


||| Always decode the given value
public export
pure : a -> Decoder a
pure x json =
    Right x


||| Given a decoder of function and one argument, return a decoder of result
public export
ap : Decoder (a -> b) -> Decoder a -> Decoder b
ap df dx json =
    case (df json, dx json) of
        (Right f, Right x) => Right $ f x
        (Right f, Left err) => Left err
        (Left err, _ ) => Left err


||| Alias of apply
public export
(<*>) : Decoder (a -> b) -> Decoder a -> Decoder b
(<*>) = ap

||| Alias of map
public export
(<$>) : (a -> b) -> Decoder a -> Decoder b
(<$>) = map



||| Avoid infinite loops in recursive decoders by wrapping the recursive call with lazy
public export
lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
  andThen thunk (succeed ())


||| Decode the given value when encountering JNull
public export
null : a -> Decoder a
null x JNull = Right x
null _ json = error "null" json


||| Decode a JString
public export
string : Decoder String
string (JString str) = Right str
string json = error "string" json


||| Decode JBoolean
||| Renamed from "bool" because causing "INTERNAL ERROR: $bool is not a function application"
bol : Decoder Bool
bol (JBoolean bool) = Right bool
bol json = error "bol" json


||| Decode a `JNumber` by casting it to `Int`
public export
int : Decoder Int
int (JNumber x) =
    if floor x == x
    then Right (cast x)
    else error {a = Int } "int" (JNumber x)
int json = error "int" json


||| Decode a `JNumber` as `Double`
public export
float : Decoder Double
float (JNumber x) = Right x
float json = error "string" json


||| Given a list of decoders, use the first one that succeeds
public export
oneOf : List (Decoder a) -> Decoder a
oneOf [] json = error "oneOf" json
oneOf (d :: ds) json =
    case d json of
        Right v => Right v
        Left err => oneOf ds json


||| Decode a list of elements
-- list : Decoder a -> Decoder (List a)
-- list decoder json@(JArray lst) =
--     map reverse $ foldr f (Right []) $ map decoder $ lst
--     where
--        f : Either String JSON -> Either String (List JSON) -> Either String (List JSON)  -- FIXME attempt at a type but compiler not happy
--        f (Right v) (Right l) = Right (v :: l)
--        f (Left err) _ = Left err
--        f (Right v) (Left err) = Left err
-- list _ json = error "list" json


||| Decode a JObject as key-value pairs
-- keyValuePairs : Decoder a -> Decoder (List (String, a))
-- keyValuePairs decoder (JObject o) =
--     map reverse $ foldr f (Right []) $ map (\( k, v ) => ( k, decoder v)) $ o
--     where
--         f : ( String, Either String a ) -> Either String (List ( String, a )) -> Either String (List ( String, a ))
--         f ( k, Right v ) (Right lst) = Right (( k, v ) :: lst)
--         f ( k, Right v) (Left err) = Left err
--         f ( k, Left err ) _ = Left err
-- keyValuePairs _ json = error "keyValuePairs" json


||| Decode the given field in a `JObject`
public export
field : String -> Decoder a -> Decoder a
field key decoder json@(JObject fields) =
    case filter ((== key) . fst) fields of
        ( _, v ) :: _ => decoder v
        _ => error ("object with field \"" ++ key ++ "\"") json
field key _ json = error ("object with field \"" ++ key ++ "\"") json


||| Decode a nested `JObject` field
public export
at : List String -> Decoder a -> Decoder a
at fields decoder =
    foldr field decoder fields

||| Temporarily borrow from Idris1 Prelude:
||| Attempt to find a particular element of a list.
|||
||| If the provided index is out of bounds, return Nothing.
index' : (n : Nat) -> (l : List a) -> Maybe a
index' Z     (x::xs) = Just x
index' (S n) (x::xs) = index' n xs
index' _     []      = Nothing

||| Decode the `n`th element of a `JArray`
public export
index : Int -> Decoder a -> Decoder a
index n decoder json@(JArray lst) =
    case index' (integerToNat (cast n)) lst of -- Int->Nat via Integer, no direct cast in Idris2.
        Just j => decoder j
        Nothing => error ("list of length > " ++ cast n ) json
index n decoder json =
    error "list" json


||| Decode `Just` a value if the given decoder succeeds, `Nothing` otherwise
public export
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder json =
    Right $ either (const Nothing) Just (decoder json)


||| Decode `Just` a value or `Nothing` in case of `JNull`
public export
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
  oneOf
    [ null Nothing
    , map Just decoder
    ]
