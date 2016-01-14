module Data.Date.SimpleParseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Date.SimpleParse

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parsing dates" $ do
    describe "days" $ do
      it "parses days in the past" $ do
        parseToDateDiff "-1d" `shouldBe` Right (Days Ago 1)
        parseToDateDiff "-2d" `shouldBe` Right (Days Ago 2)
        parseToDateDiff "-10d" `shouldBe` Right (Days Ago 10)
      it "parses days in the future" $ do
        parseToDateDiff "1d" `shouldBe` Right (Days FromNow 1)
        parseToDateDiff "2d" `shouldBe` Right (Days FromNow 2)
        parseToDateDiff "10d" `shouldBe` Right (Days FromNow 10)

    describe "hours" $ do
      it "parses hours in the past" $ do
        parseToDateDiff "-1h" `shouldBe` Right (Hours Ago 1)
        parseToDateDiff "-2h" `shouldBe` Right (Hours Ago 2)
        parseToDateDiff "-10h" `shouldBe` Right (Hours Ago 10)
      it "parses hours in the future" $ do
        parseToDateDiff "1h" `shouldBe` Right (Hours FromNow 1)
        parseToDateDiff "2h" `shouldBe` Right (Hours FromNow 2)
        parseToDateDiff "10h" `shouldBe` Right (Hours FromNow 10)

    describe "months" $ do
      it "parses months in the past" $ do
        parseToDateDiff "-1M" `shouldBe` Right (Months Ago 1)
        parseToDateDiff "-2M" `shouldBe` Right (Months Ago 2)
        parseToDateDiff "-10M" `shouldBe` Right (Months Ago 10)
      it "parses months in the future" $ do
        parseToDateDiff "1M" `shouldBe` Right (Months FromNow 1)
        parseToDateDiff "2M" `shouldBe` Right (Months FromNow 2)
        parseToDateDiff "10M" `shouldBe` Right (Months FromNow 10)

    describe "minutes" $ do
      it "parses minutes in the past" $ do
        parseToDateDiff "-1m" `shouldBe` Right (Minutes Ago 1)
        parseToDateDiff "-2m" `shouldBe` Right (Minutes Ago 2)
        parseToDateDiff "-10m" `shouldBe` Right (Minutes Ago 10)
      it "parses minutes in the future" $ do
        parseToDateDiff "1m" `shouldBe` Right (Minutes FromNow 1)
        parseToDateDiff "2m" `shouldBe` Right (Minutes FromNow 2)
        parseToDateDiff "10m" `shouldBe` Right (Minutes FromNow 10)

    describe "seconds" $ do
      it "parses seconds in the past" $ do
        parseToDateDiff "-1s" `shouldBe` Right (Seconds Ago 1)
        parseToDateDiff "-2s" `shouldBe` Right (Seconds Ago 2)
        parseToDateDiff "-10s" `shouldBe` Right (Seconds Ago 10)
      it "parses seconds in the future" $ do
        parseToDateDiff "1s" `shouldBe` Right (Seconds FromNow 1)
        parseToDateDiff "2s" `shouldBe` Right (Seconds FromNow 2)

    describe "years" $ do
      it "parses years in the past" $ do
        parseToDateDiff "-1y" `shouldBe` Right (Years Ago 1)
        parseToDateDiff "-2y" `shouldBe` Right (Years Ago 2)
        parseToDateDiff "-10y" `shouldBe` Right (Years Ago 10)
      it "parses years in the future" $ do
        parseToDateDiff "1y" `shouldBe` Right (Years FromNow 1)
        parseToDateDiff "2y" `shouldBe` Right (Years FromNow 2)
        parseToDateDiff "10y" `shouldBe` Right (Years FromNow 10)

    describe "weeks" $ do
      it "parses weeks in the past" $ do
        parseToDateDiff "-1w" `shouldBe` Right (Weeks Ago 1)
        parseToDateDiff "-2w" `shouldBe` Right (Weeks Ago 2)
        parseToDateDiff "-10w" `shouldBe` Right (Weeks Ago 10)
      it "parses weeks in the future" $ do
        parseToDateDiff "1w" `shouldBe` Right (Weeks FromNow 1)
        parseToDateDiff "2w" `shouldBe` Right (Weeks FromNow 2)

