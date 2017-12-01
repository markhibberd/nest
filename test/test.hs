{-# LANGUAGE NoImplicitPrelude #-}

import           Nest.Prelude

import           System.Exit (exitFailure)
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Nest


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Test.Nest.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
