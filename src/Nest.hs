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
  , flag
  , setting
  , failure

  -- * Combinators for enriching parsers
  , option
  , withDefault
  , withContext

  -- * Run parsers with environment
  , run
  , runT
  , runWith
  , runWithT
  , force
  ) where


import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))

import           Data.ByteString (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

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

instance MonadIO m => MonadIO (Parser m) where
  liftIO =
    lift . liftIO

instance MonadTrans Parser where
  lift =
    Parser . const . lift

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
  fromMaybeM (failure name . mconcat $ ["Could not parse numeric value from '", s ,"'"]) $
    readMaybe . Text.unpack $ s

flag :: Monad m => ByteString -> a -> a -> Parser m a
flag name true false = do
  s <- string name
  Parser $ \_ ->
    case s of
      "t" ->
        pure true
      "true" ->
        pure true
      "1" ->
        pure true
      "f" ->
        pure false
      "false" ->
        pure false
      "0" ->
        pure false
      _ ->
        left . NestParseError name . mconcat $ [
            "Invalid boolean flag value, expected true ['t', 'true', '1'] or false ['f', 'false', '0'], got: ", s
          ]

setting :: Monad m => ByteString -> Map Text a -> Parser m a
setting name settings = do
  s <- string name
  Parser $ \_ ->
    case Map.lookup s settings of
      Just x ->
        pure x
      Nothing ->
        left . NestParseError name $
          mconcat [
              "Unknown setting option ["
            , s
            , "]. Expected one of: ["
            , Text.intercalate ", " $ Map.keys settings
            , "]"
            ]

failure :: Monad m => ByteString -> Text -> Parser m a
failure name message =
  Parser $ \_ ->
    left $ NestParseError name message

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
  liftIO Posix.getEnvironment >>=
    flip runWith p . Environment . Map.fromList

runT :: MonadIO m => Parser m a -> EitherT NestError m a
runT =
  newEitherT . run

runWith :: MonadIO m => Environment -> Parser m a -> m (Either NestError a)
runWith e p =
  runEitherT (parse p e)

runWithT :: MonadIO m => Environment -> Parser m a -> EitherT NestError m a
runWithT e =
  newEitherT . runWith e

force :: MonadIO m => Parser m a -> m a
force p =
  run p >>= \e -> case e of
    Left err -> liftIO $ do
      Text.hPutStrLn IO.stderr $ renderNestError err
      Exit.exitFailure
    Right a ->
      pure a
