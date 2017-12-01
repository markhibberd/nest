{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Nest where

import           Nest
import           Nest.Prelude

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           System.IO (IO)


tests :: IO Bool
tests =
  checkParallel $$(discover)
