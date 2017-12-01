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


import           Control.Monad.IO.Class (MonadIO (..))

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

newtype Parser m a =
  Parser {
       parse :: Environment -> EitherT NestError m a
    }

instance Functor m => Functor (Parser m) where
  fmap f m =
    Parser $ \e -> f <$> parse m e

instance Monad m => Applicative (Parser m) where
  pure a =
    Parser $ \_ -> pure a
  f <*> a =
    Parser $ \e ->
      parse f e <*> parse a e

instance Monad m => Monad (Parser m) where
  return =
    pure
  m >>= f =
    Parser $ \e ->
      newEitherT $
        runEitherT (parse m e) >>= \x ->
          case x of
            Left err ->
             pure $ Left err
            Right a ->
             runEitherT $ parse (f a) e

variable :: Monad m => ByteString -> Parser m ByteString
variable name =
  Parser $ \e ->
    fromMaybeM (left $ NestMissing name) $
      Map.lookup name $
        getEnvironment e

string :: (Monad m, IsString s) => ByteString -> Parser m s
string name =
  with (variable name) $
    fromString . Text.unpack . Text.decodeUtf8

numeric :: (Monad m, Read n, Num n) => ByteString -> Parser m n
numeric name = do
  s <- string name
  Parser $ \_ ->
    fromMaybeM (left . NestParseError name . mconcat $ ["Could not parse numeric value from '", s ,"'"]) $
      readMaybe . Text.unpack $ s

option :: Monad m => Parser m a -> Parser m (Maybe a)
option p =
  Parser $ \e ->
    newEitherT $ runEitherT (parse p e) >>= \xx -> case xx of
      Left (NestMissing _) ->
        pure $ Right Nothing
      Left err ->
        pure $ Left err
      Right x ->
        pure $ Right (Just x)

withDefault :: Monad m => Parser m a -> a -> Parser m a
withDefault p dfault =
  Parser $ \e ->
    newEitherT $ runEitherT (parse p e) >>= \xx ->
      case xx of
        Left (NestMissing _) ->
          pure $ Right dfault
        x ->
          pure x

withContext :: Monad m => Parser m a -> Text -> Parser m a
withContext p context =
  Parser $ \e ->
    newEitherT $ runEitherT (parse p e) >>= \xx ->
      case xx of
        Left err ->
          pure . Left $ NestContextError context err
        x ->
          pure x

run :: MonadIO m => Parser m a -> m (Either NestError a)
run p =
  liftIO Posix.getEnvironment >>= \e ->
    runEitherT (parse p (Environment $ Map.fromList e))

force :: MonadIO m => Parser m a -> m a
force p =
  run p >>= \e -> case e of
    Left err -> liftIO $ do
      Text.hPutStrLn IO.stderr $ renderNestError err
      Exit.exitFailure
    Right a ->
      pure a
