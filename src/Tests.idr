module Tests

import System
import Data.Strings             --isInfixOf
import Language.JSON.Decode


test : String -> (() -> Either String Bool) -> IO ()
test name f = do
    putStrLn $ "Test: " ++ name
    case f () of
        Right _ => putStrLn "OK"
        Left err => do
            putStrLn $ "Failed: " ++ err
            exitFailure


testInt : Bool -> String -> Either String Bool
testInt val str =
    case (val, Language.JSON.Decode.decodeString int str) of
        (True, Right v) => Right True
        (True, Left err) => Left err
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


customTests : IO ()
customTests =
    test "customDecoder preserves user error messages" $ \() => assertion
    where
        jsonString : String
        jsonString = "{ \"foo\": \"bar\" }"

        customErrorMessage : String
        customErrorMessage = "I want to see this message!"

        myDecoder : Decoder Int
        myDecoder = andThen (\foo => fail customErrorMessage) (field "foo" string)

        assertion : Either String Bool
        assertion =
            case decodeString myDecoder jsonString of
                Right _ =>
                    Left $ "expected `customDecoder` to produce Left, but got Right"

                Left message =>
                    if isInfixOf customErrorMessage message then
                        Right True
                    else
                        Left $
                            "expected `customDecoder` to preserve user's error message '"
                                ++ customErrorMessage
                                ++ "', but instead got: "
                                ++ message


export
tests : IO ()
tests = do
    intTests
    customTests
