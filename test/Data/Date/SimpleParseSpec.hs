module Data.Date.SimpleParseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Date.SimpleParse.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parsing dates" $ do
    describe "special cases" $
      it "parses now" $
        parseToDateDiff "now" `shouldBe` Right (Ago [Seconds 0])

    describe "days" $ do
      it "parses days in the past" $ do
        parseToDateDiff "-1d" `shouldBe` Right (Ago [Days 1])
        parseToDateDiff "-2d" `shouldBe` Right (Ago [Days 2])
        parseToDateDiff "-10d" `shouldBe` Right (Ago [Days 10])
      it "parses days in the future" $ do
        parseToDateDiff "1d" `shouldBe` Right (FromNow [Days 1])
        parseToDateDiff "2d" `shouldBe` Right (FromNow [Days 2])
        parseToDateDiff "10d" `shouldBe` Right (FromNow [Days 10])
      it "parse multiple dates" $ do
        parseToDateDiff "1d5h" `shouldBe` Right (FromNow [Days 1, Hours 5])
        parseToDateDiff "-15d5h2s" `shouldBe` Right (Ago [Days 15, Hours 5, Seconds 2])

    describe "hours" $ do
      it "parses hours in the past" $ do
        parseToDateDiff "-1h" `shouldBe` Right (Ago [Hours 1])
        parseToDateDiff "-2h" `shouldBe` Right (Ago [Hours 2])
        parseToDateDiff "-10h" `shouldBe` Right (Ago [Hours 10])
      it "parses hours in the future" $ do
        parseToDateDiff "1h" `shouldBe` Right (FromNow [Hours 1])
        parseToDateDiff "2h" `shouldBe` Right (FromNow [Hours 2])
        parseToDateDiff "10h" `shouldBe` Right (FromNow [Hours 10])

    describe "months" $ do
      it "parses months in the past" $ do
        parseToDateDiff "-1M" `shouldBe` Right (Ago [Months 1])
        parseToDateDiff "-2M" `shouldBe` Right (Ago [Months 2])
        parseToDateDiff "-10M" `shouldBe` Right (Ago [Months 10])
      it "parses months in the future" $ do
        parseToDateDiff "1M" `shouldBe` Right (FromNow [Months 1])
        parseToDateDiff "2M" `shouldBe` Right (FromNow [Months 2])
        parseToDateDiff "10M" `shouldBe` Right (FromNow [Months 10])

    describe "minutes" $ do
      it "parses minutes in the past" $ do
        parseToDateDiff "-1m" `shouldBe` Right (Ago [Minutes 1])
        parseToDateDiff "-2m" `shouldBe` Right (Ago [Minutes 2])
        parseToDateDiff "-10m" `shouldBe` Right (Ago [Minutes 10])
      it "parses minutes in the future" $ do
        parseToDateDiff "1m" `shouldBe` Right (FromNow [Minutes 1])
        parseToDateDiff "2m" `shouldBe` Right (FromNow [Minutes 2])
        parseToDateDiff "10m" `shouldBe` Right (FromNow [Minutes 10])

    describe "seconds" $ do
      it "parses seconds in the past" $ do
        parseToDateDiff "-1s" `shouldBe` Right (Ago [Seconds 1])
        parseToDateDiff "-2s" `shouldBe` Right (Ago [Seconds 2])
        parseToDateDiff "-10s" `shouldBe` Right (Ago [Seconds 10])
      it "parses seconds in the future" $ do
        parseToDateDiff "1s" `shouldBe` Right (FromNow [Seconds 1])
        parseToDateDiff "2s" `shouldBe` Right (FromNow [Seconds 2])

    describe "years" $ do
      it "parses years in the past" $ do
        parseToDateDiff "-1y" `shouldBe` Right (Ago [Years 1])
        parseToDateDiff "-2y" `shouldBe` Right (Ago [Years 2])
        parseToDateDiff "-10y" `shouldBe` Right (Ago [Years 10])
      it "parses years in the future" $ do
        parseToDateDiff "1y" `shouldBe` Right (FromNow [Years 1])
        parseToDateDiff "2y" `shouldBe` Right (FromNow [Years 2])
        parseToDateDiff "10y" `shouldBe` Right (FromNow [Years 10])

    describe "weeks" $ do
      it "parses weeks in the past" $ do
        parseToDateDiff "-1w" `shouldBe` Right (Ago [Weeks 1])
        parseToDateDiff "-2w" `shouldBe` Right (Ago [Weeks 2])
        parseToDateDiff "-10w" `shouldBe` Right (Ago [Weeks 10])
      it "parses weeks in the future" $ do
        parseToDateDiff "1w" `shouldBe` Right (FromNow [Weeks 1])
        parseToDateDiff "2w" `shouldBe` Right (FromNow [Weeks 2])
