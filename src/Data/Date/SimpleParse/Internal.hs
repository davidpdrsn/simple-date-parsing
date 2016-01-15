-- | Internal functions, do not use these. They might change without any notice
module Data.Date.SimpleParse.Internal
    ( DateDiff(..)
    , Offset(..)
    , parseToDateDiff
    , compileDate
    )
  where

import Text.ParserCombinators.Parsec
import Data.Time.Clock

parseToDateDiff :: String -> Either ParseError DateDiff
parseToDateDiff "now" = return $ Ago [Seconds 0]
parseToDateDiff s = parse dateParser "" s

data DateDiff = Ago [Offset]
              | FromNow [Offset]
              deriving (Eq, Show)

data Offset = Years   Integer
            | Months  Integer
            | Weeks   Integer
            | Days    Integer
            | Hours   Integer
            | Minutes Integer
            | Seconds Integer
            deriving (Eq, Show)

dateDiffToSeconds :: DateDiff -> Integer
dateDiffToSeconds (Ago offsets) = sum (map toSeconds offsets) * (- 1)
dateDiffToSeconds (FromNow offsets) = sum (map toSeconds offsets)

toSeconds :: Offset -> Integer
toSeconds (Years n)   = n * secondsInYear
toSeconds (Months n)  = n * secondsInMonth
toSeconds (Weeks n)   = n * secondsInWeek
toSeconds (Days n)    = n * secondsInDay
toSeconds (Hours n)   = n * secondsInHour
toSeconds (Minutes n) = n * secondsInMinute
toSeconds (Seconds n) = n * secondsInSecond

compileDate :: DateDiff -> IO UTCTime
compileDate date = do
    now <- getCurrentTime
    let diff = realToFrac $ secondsToDiffTime $ dateDiffToSeconds date
    return $ addUTCTime diff now

secondsInYear :: Integer
secondsInYear = secondsInDay * 365

secondsInMonth :: Integer
secondsInMonth = secondsInDay * 30

secondsInWeek :: Integer
secondsInWeek = secondsInDay * 7

secondsInDay :: Integer
secondsInDay = secondsInHour * 24

secondsInHour :: Integer
secondsInHour = secondsInMinute * 60

secondsInMinute :: Integer
secondsInMinute = secondsInSecond * 60

secondsInSecond :: Integer
secondsInSecond = 1

dateParser :: Parser DateDiff
dateParser = do
    ago <- optionMaybe $ char '-'
    let diff = case ago of
                 Nothing -> FromNow
                 Just _ -> Ago
    offset <- many1 offsetParser
    return $ diff offset

offsetParser :: Parser Offset
offsetParser = do
    offset <- read <$> many1 digit
    range <- oneOf $ map fst constructors
    case lookup range constructors of
      Nothing -> fail "Unknown time specifier"
      Just constructor -> return $ constructor offset

constructors :: [(Char, Integer -> Offset)]
constructors = [ ('y', Years)
               , ('M', Months)
               , ('w', Weeks)
               , ('h', Hours)
               , ('d', Days)
               , ('m', Minutes)
               , ('s', Seconds)
               ]
