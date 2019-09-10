A basic way to log to loggly from Haskell.

```
{-# LANGUAGE DeriveGeneric #-}
import Data.String
import Loggly
import Data.Aeson

data Foo = Foo { bar :: Text
               , zar :: Int
               } deriving(ToJSON)

main :: IO ()
main = do
    tok <- fromString <$> getEnv "LOGGLY_TOKEN"
    lgy <- newLoggly tok
    loggly lgy
```
