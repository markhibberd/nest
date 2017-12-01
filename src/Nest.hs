{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nest (
  -- * Errors
    NestError (..)
  , renderNestError

  -- * Types
  , Environment (..)
  , Parser (..)

  -- * Basic parsers
  , variable
  , string
  , numeric

  -- * Combinators for enriching parsers
  , option
  , withDefault
  , withContext

  -- * Run parsers with system environment
  , run
  , force
  ) where

import           Data.ByteString (ByteString)
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import           Data.Map (Map)
import qualified Data.Map as Map

import           Nest.Prelude

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO
import qualified System.Posix.Env.ByteString as Posix

data NestError =
    NestMissing ByteString
  | NestParseError ByteString Text
  | NestContextError Text NestError
    deriving (Eq, Ord, Show)

renderNestError :: NestError -> Text
renderNestError e =
  case e of
    NestMissing field ->
      mconcat ["[", Text.decodeUtf8 field, "]: required environment variable not found."]
    NestParseError field msg ->
      mconcat ["[", Text.decodeUtf8 field, "]: environment variable could not be parsed. ", msg]
    NestContextError context err ->
      mconcat [renderNestError err, " | ", context]

newtype Environment =
  Environment {
      getEnvironment :: Map ByteString ByteString
    } deriving (Eq, Ord, Show)

newtype Parser a =
  Parser {
       parse :: Environment -> Either NestError a
    }

instance Functor Parser where
  fmap f m =
    Parser $ \e -> f <$> parse m e

instance Applicative Parser where
  pure a =
    Parser $ \_ -> pure a
  f <*> a =
    Parser $ \e ->
      parse f e <*> parse a e

instance Monad Parser where
  return =
    pure
  m >>= f =
    Parser $ \e ->
      case parse m e of
         Left err ->
           Left err
         Right a ->
           parse (f a) e

option :: Parser a -> Parser (Maybe a)
option p =
  Parser $ \e ->
    case parse p e of
      Left (NestMissing _) ->
        Right Nothing
      Left err ->
        Left err
      Right x ->
        Right (Just x)

withDefault :: Parser a -> a -> Parser a
withDefault p dfault =
  Parser $ \e ->
    case parse p e of
      Left (NestMissing _) ->
        Right dfault
      x ->
        x

withContext :: Parser a -> Text -> Parser a
withContext p context =
  Parser $ \e ->
    case parse p e of
      Left err ->
        Left $ NestContextError context err
      x ->
        x

variable :: ByteString -> Parser ByteString
variable name =
  Parser $ \e ->
    fromMaybeM (Left $ NestMissing name) $
      Map.lookup name $
        getEnvironment e

string :: IsString s => ByteString -> Parser s
string name =
  with (variable name) $
    fromString . Text.unpack . Text.decodeUtf8

numeric :: (Read n, Num n) => ByteString -> Parser n
numeric name = do
  s <- string name
  Parser $ \_ ->
    fromMaybeM (Left . NestParseError name . mconcat $ ["Could not parse numeric value from '", s , "'"]) $
      readMaybe . Text.unpack $ s

run :: Parser a -> IO (Either NestError a)
run p =
  with Posix.getEnvironment $ \e ->
    parse p (Environment $ Map.fromList e)

force :: Parser a -> IO a
force p =
  run p >>= \e -> case e of
    Left err -> do
      Text.hPutStrLn IO.stderr $ renderNestError err
      Exit.exitFailure
    Right a ->
      pure a
