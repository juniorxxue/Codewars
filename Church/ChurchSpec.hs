{-# Language RankNTypes #-}

module ChurchSpec (spec) where

import Prelude hiding (not,and,or)
import Church (Boolean,false,true, not,and,or,xor)
import Test.Hspec ( describe, it, shouldBe, Spec )

spec :: Spec
spec = do
  describe "example tests" $ do
    it "not" $ do
      unchurch (not false) `shouldBe` True
      unchurch (not true) `shouldBe` False
    it "and" $ do
      unchurch (false `and` false) `shouldBe` False
      unchurch (false `and` true) `shouldBe` False
      unchurch (true `and` false) `shouldBe` False
      unchurch (true `and` true) `shouldBe` True
    it "or" $ do
      unchurch (false `or` false) `shouldBe` False
      unchurch (false `or` true) `shouldBe` True
      unchurch (true `or` false) `shouldBe` True
      unchurch (true `or` true) `shouldBe` True
    it "xor" $ do
      unchurch (false `xor` false) `shouldBe` False
      unchurch (false `xor` true) `shouldBe` True
      unchurch (true `xor` false) `shouldBe` True
      unchurch (true `xor` true) `shouldBe` False

unchurch :: Boolean -> Bool
unchurch bool = bool True False