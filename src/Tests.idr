module Tests

import System
import Data.Strings             --isInfixOf
import Language.JSON
import Language.JSON.Data
import Language.JSON.Decode


test : String -> (() -> Either String Bool) -> IO ()
test name f = do
    putStrLn $ "Test: " ++ name
    case f () of
        Right _ => putStrLn "OK"
        Left err => do
            putStrLn $ "Failed: " ++ err
            exitFailure

--------------------------------------------------------------------------------
-- `int` tests
--------------------------------------------------------------------------------

testInt : Bool -> String -> Either String Bool
testInt val str =
    case (val, Language.JSON.Decode.decodeString int str) of
        (True, Right v) => Right True
        (True, Left err) => Left (show err)
        (False, Left err) => Right False
        (False, Right v) => Left $ "Expected to fail but succeeded: " ++ str

intTests : IO ()
intTests = do
    test "whole int" $ \() => testInt True "4"
    test "-whole int" $ \() => testInt True "-4"
    test "whole float" $ \() => testInt True "4.0"
    test "-whole float" $ \() => testInt True "-4.0"
    test "large int" $ \() => testInt True "1801439850948"
    test "-large int" $ \() => testInt True "-1801439850948"
    test "float" $ \() => testInt False "4.2"
    test "-float" $ \() => testInt False "-4.2"
    test "Infinity" $ \() => testInt False "Infinity"
    test "-Infinity" $ \() => testInt False "-Infinity"
    test "NaN" $ \() => testInt False "NaN"
    test "-NaN" $ \() => testInt False "-NaN"
    test "true" $ \() => testInt False "true"
    test "false" $ \() => testInt False "false"
    test "string" $ \() => testInt False "\"string\""
    test "object" $ \() => testInt False "{}"
    test "null" $ \() => testInt False "null"
    test "undefined" $ \() => testInt False "undefined"


--------------------------------------------------------------------------------
-- `field` tests
--------------------------------------------------------------------------------

||| A test JSON object
||| Note `parse (show t1json)` on the REPL is unusably slow (minutes...).  Use :exec putStrLn (show (<whatever>)) instead.
t1json : JSON
t1json = JObject [
                 ("foo", JNumber 42)
                 ,("bar", JArray [JNumber 2, JNumber 4, JNumber 6])
                 ,("baz", JObject [
                                 ("quux", JString "Hellow world!")
                                 ,("ary1", JArray [JString "alpha", JString "beta" ])
                                 ,("nobj", JObject [
                                                  ("ary2", JArray [JBoolean True, JBoolean False])
                                 ])
                 ])
                 ,("ary0", JArray [
                          JObject [
                                  ("aa", JNumber 22)
                                  ,("bb", JNumber 33)
                          ]
                 ])
       ]


||| Test that is expected to pass.
||| @expectedVal The value expected to be returned on success.
||| @actualVal The Either error message or actual value structure returned by the decoding function.
testPass : (Show a, Eq a) => (expectedVal : a) -> (actualVal : Either JSONError a) -> Either String Bool
testPass x (Left err) = Left $ "Failed: " ++ (show err)
testPass x (Right y) = if x == y
                         then Right True
                         else Left $ "expected " ++ (show x) ++ " got " ++ (show y)

||| Test that is expected to fail.
||| @expectedErr The error message expected to be triggered.
||| @actualVal The Either error message or actual value structure returned by the decoding function.
testFail : (Show a, Eq a) => (expectedErr : JSONError) -> (actualVal : Either JSONError a) -> Either String Bool
testFail x (Left err) = if x == err
                           then Right True -- expected error
                           else Left $ "wrong error, expected <" ++ (show x) ++ ">, got <" ++ (show err) ++ ">"
testFail _ (Right x) = Left $ "Expected to fail but succeeded: " ++ (show x)


||| Test related to field access.
fieldTests : IO ()
fieldTests = do
  -- test valid field access
  test "field exists, number, value same" $ \()
    => testPass 5 (field "foo" int (JObject [("foo", JNumber 5)]))
  test "field exists, string, value same" $ \()
    => testPass "hello" (field "foo" string (JObject [("foo", JString "hello")]))
  test "field exists, null, value same" $ \()
    => testPass True (field "foo" null (JObject [("foo", JNull)]))
  test "field exists, boolean, value same" $ \()
    => testPass False (field "foo" bol (JObject [("foo", JBoolean False)]))
  test "field exists, array, value same" $ \()
    => testPass [4,5,6]
      (field "foo" (list int) (JObject [("foo", JArray [JNumber 4, JNumber 5, JNumber 6])]))

  -- test invalid field access
  test "field exists, wrong decoder type" $ \()
    => testFail (JSONTypeErr JTNumber JTString) (field "foo" int (JObject [("foo", JString "hello")]))

  test "field not exists, number" $ \()
    => testFail (JSONNoSuchField "bar" "{\n\"foo\": 5.0\n}") (field "bar" int (JObject [("foo", JNumber 5)]))

  -- test error generation when trying to access things which are not fields.
  test "filed of non-object, null" $ \()
    => testFail (JSONTypeErr JTObject JTNull) (field "foo" int (JNull))
  test "field of non-object, boolean" $ \()
    => testFail (JSONTypeErr JTObject JTBoolean) (field "foo" int (JBoolean True))
  test "field of non-object, number" $ \()
    => testFail (JSONTypeErr JTObject JTNumber) (field "foo" int (JNumber 5))
  test "field of non-object, string" $ \()
    => testFail (JSONTypeErr JTObject JTString) (field "foo" int (JString "hello"))
  test "field of non-object, array" $ \()
    => testFail (JSONTypeErr JTObject JTArray) (field "foo" int (JArray []))



--------------------------------------------------------------------------------
-- Other testing
--------------------------------------------------------------------------------

-- customTests : IO ()
-- customTests =
--     test "customDecoder preserves user error messages" $ \() => assertion
--     where
--         jsonString : String
--         jsonString = "{ \"foo\": \"bar\" }"

--         customErrorMessage : String
--         customErrorMessage = "I want to see this message!"

--         myDecoder : Decoder Int
--         myDecoder = andThen (\foo => fail customErrorMessage) (field "foo" string)

--         assertion : Either String Bool
--         assertion =
--             case decodeString myDecoder jsonString of
--                 Right _ =>
--                     Left $ "expected `customDecoder` to produce Left, but got Right"

--                 Left message =>
--                     if isInfixOf customErrorMessage message then
--                         Right True
--                     else
--                         Left $
--                             "expected `customDecoder` to preserve user's error message '"
--                                 ++ customErrorMessage
--                                 ++ "', but instead got: "
--                                 ++ message


export
tests : IO ()
tests = do
    intTests
    fieldTests
    --customTests
