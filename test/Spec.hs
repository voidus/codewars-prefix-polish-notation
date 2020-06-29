module Main (main) where

import Lib (calculate)
import Test.Hspec

main :: IO ()
main = hspec do
  describe "example tests" $ do
    it "reads numbers" $ do
      calculate "0" `shouldBe` 0
      calculate "123" `shouldBe` 123
      calculate "12.456" `shouldBe` 12.456
    it "performs operations" $ do
      calculate "+ 3 5" `shouldBe` 8
      calculate "* + 2 2 3" `shouldBe` 12
      calculate "/ + 3 5 * 2 2" `shouldBe` 2
