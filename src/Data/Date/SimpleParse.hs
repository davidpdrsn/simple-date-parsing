-- | Module for parsing simple relative dates like "-1d4h" meaning 1 day and 4 hours ago
module Data.Date.SimpleParse
    ( parseDate
    )
  where

import Data.Date.SimpleParse.Internal
import Text.ParserCombinators.Parsec
import Data.Time.Clock

-- | Parse a relative date like "-1d4h" meaning 1 day and 4 hours ago
parseDate :: String -> IO (Either ParseError UTCTime)
parseDate str = case parseToDateDiff str of
                  Left e -> return $ Left e
                  Right date -> Right <$> compileDate date
