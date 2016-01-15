# simple-date-parsing

This is a small library for parsing relative dates like "-1d7h" meaning one day and 7 hours ago.

The package contains one function you need to know about `parseDate :: String -> IO (Either ParseError UTCTime)`

## Example

```haskell
import Data.Date.SimpleParse
import Data.Time.Format

main :: IO ()
main = do
    date <- parseDate "-1d5h"
    case date of
      Right d -> print $ formatTime defaultTimeLocale "%c" d
      Left e -> putStrLn "There was an error" >> print e
```
