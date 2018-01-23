{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Nest where

import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Nest
import           Nest.Prelude

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           System.IO (IO)

prop_variable :: Property
prop_variable = property $ do
  name <- forAll genName
  value <- forAll $ Gen.element ["apple", "orange", "banana", "pear"]
  result <- runWith (env [(name, value)]) $
    variable name
  result === Right value

prop_variable_not_set :: Property
prop_variable_not_set = property $ do
  name <- forAll genName
  result <- runWith (env []) $
    variable name
  result === Left (NestMissing name)

prop_variable_default :: Property
prop_variable_default = property $ do
  name <- forAll genName
  value <- forAll $ Gen.element ["apple", "orange", "banana", "pear"]
  result <- runWith (env []) $
    variable name `withDefault` value
  result === Right value

prop_optional_set :: Property
prop_optional_set = property $ do
  name <- forAll genName
  value <- forAll $ Gen.element ["apple", "orange", "banana", "pear"]
  result <- runWith (env [(name, value)]) $
    option $ variable name
  result === Right (Just value)

prop_optional_not_set :: Property
prop_optional_not_set = property $ do
  name <- forAll genName
  result <- runWith (env []) $
    option $ variable name
  result === Right Nothing

prop_string :: Property
prop_string = property $ do
  name <- forAll genName
  value <- forAll $ Gen.element ["apple", "orange", "banana", "pear"]
  result <- runWith (env [(name, Text.encodeUtf8 value)]) $
    string name
  result === Right value

prop_numeric :: Property
prop_numeric = property $ do
  name <- forAll genName
  value <- forAll $ Gen.int (Range.constant 0 999999)
  result <- runWith (env [(name, Text.encodeUtf8 . Text.pack . show $ value)]) $
    numeric name
  result === Right value

prop_flag :: Property
prop_flag = property $ do
  name <- forAll genName
  value <- forAll $ Gen.element [True, False]
  result <- runWith (env [(name, bool "false" "true" value)]) $
    flag name True False
  result === Right value

data Mode =
    Green
  | Red
  | Blue
    deriving (Eq, Ord, Show)

prop_setting :: Property
prop_setting = property $ do
  name <- forAll genName
  value <- forAll $ Gen.element [Green, Red, Blue]
  result <- runWith (env [(name, Text.encodeUtf8 . Text.pack . show $ value)]) $
    setting name . Map.fromList $ [
        (Text.pack . show $ value, value)
      ]
  result === Right value

env :: [(ByteString, ByteString)] -> Environment
env =
  Environment . Map.fromList

genName :: Gen ByteString
genName = do
  prefix <- Gen.element ["HOMER", "MARGE", "BART", "LISA", "MAGGIE"]
  suffix <- Gen.element ["PORT", "HOST", "DB", "THREADS", "CONTEXT", "MODE"]
  pure . mconcat $ [prefix, "_", suffix]

tests :: IO Bool
tests =
  checkParallel $$(discover)
