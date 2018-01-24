# nest

Parsing environment variables.

## usage

Standard usage within a main method, either:

Build a data type:

```
import Nest

data Verbose =
    Verbose
  | NotVerbose

data Config =
  Config {
      port :: Int
    , db :: ByteString
    , mode :: Maybe Text
    , verbose :: Verbose
    }

main :: IO ()
main = do
  config <- force $
    Config
      <$> numeric "PORT" `withDefault` 5555
      <*> variable "DB"
      <*> optional (string "MODE")
      <*> flag "VERBOSE" Verbose NotVerbose `withDefault` NotVerbose

  run config
```

Note: try `run` instead of `force` if you want to handle the errors
yourself, instead of the default usage and exit behaviour.


Or just go at it like you still live in a cave:

```
import Nest

main :: IO ()
main = do
  port <- force $ numeric "PORT" `withDefault` 5555
  db <- force $ variable "DB"
  run port db
```

Built in parsers...


```
# Raw ByteString from environment.
varbiable "VARIABLE_NAME"

# Anything that IsString.
string "VARIABLE_NAME"

# Any Num that can be parsed with Read.
numeric "VARIABLE_NAME"

# Boolean like flags, True = 't', 'true', '1'; False = 'f', 'false', '0'.
flag "VARIABLE_NAME" True False

# String settings
Nest.setting "AUTHENTICATION_MODE" (Map.fromList [
      ("oauth", OAuth)
    , ("db", DbAuth)
    , ("none", NoAuth)
    ])

# String settings, that lead to different dependent settings
join $ Nest.setting "AUTHENTICATION_MODE" (Map.fromList [
      ("oauth", OAuth <$> string "OAUTH_TOKEN")
    , ("db", DbAuth <$> string "DB")
    , ("none", pure NoAuth)
    ])

# defaults
numeric "VARIABLE_NAME" `withDefault` 10

# optional
optional $ numeric "VARIABLE_NAME"

# extra error message context
numeric "VARIABLE_NAME" `withContext` "VARIABLE_NAME is important...."
```
