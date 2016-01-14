module Data.Date.SimpleParse
    ( DateDiff(..)
    , OffsetDirection(..)
    , parseToDateDiff
    , parseDate
    )
  where

import Text.ParserCombinators.Parsec
import Data.Time.Clock

parseDate :: String -> IO (Either ParseError UTCTime)
parseDate str = case parseToDateDiff str of
                  Left e -> return $ Left e
                  Right date -> Right <$> compileDate date

parseToDateDiff :: String -> Either ParseError DateDiff
parseToDateDiff = parse dateParser ""

data DateDiff = Years   OffsetDirection Integer
              | Months  OffsetDirection Integer
              | Weeks   OffsetDirection Integer
              | Days    OffsetDirection Integer
              | Hours   OffsetDirection Integer
              | Minutes OffsetDirection Integer
              | Seconds OffsetDirection Integer
              deriving (Eq, Show)

data OffsetDirection = Ago
                     | FromNow
                     deriving (Eq, Show)

--

dateDiffToSeconds :: DateDiff -> Integer
dateDiffToSeconds (Years direction n) = applyDirection direction $ n * secondsInYear
dateDiffToSeconds (Months direction n) = applyDirection direction $ n * secondsInMonth
dateDiffToSeconds (Weeks direction n) = applyDirection direction $ n * secondsInWeek
dateDiffToSeconds (Days direction n) = applyDirection direction $ n * secondsInDay
dateDiffToSeconds (Hours direction n) = applyDirection direction $ n * secondsInHour
dateDiffToSeconds (Minutes direction n) = applyDirection direction $ n * secondsInMinute
dateDiffToSeconds (Seconds direction n) = applyDirection direction $ n * secondsInSecond

compileDate :: DateDiff -> IO UTCTime
compileDate date = do
    now <- getCurrentTime
    let diff = realToFrac $ secondsToDiffTime $ dateDiffToSeconds date
    return $ addUTCTime diff now

applyDirection :: Num a => OffsetDirection -> a -> a
applyDirection Ago = (* (-1))
applyDirection FromNow = id

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
    let direction = case ago of
                      Nothing -> FromNow
                      Just _ -> Ago
    offset <- read <$> many1 digit
    range <- oneOf $ map fst constructors
    case lookup range constructors of
      Nothing -> fail "Unknown time specifier"
      Just constructor -> return $ constructor direction offset

constructors :: [(Char, OffsetDirection -> Integer -> DateDiff)]
constructors = [ ('y', Years)
               , ('M', Months)
               , ('w', Weeks)
               , ('h', Hours)
               , ('d', Days)
               , ('m', Minutes)
               , ('s', Seconds)
               ]
