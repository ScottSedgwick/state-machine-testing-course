module Main where

import PropertyTests (propertyTests)
import LawPropertiesBonus (myBTreePrismLaws)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests = testGroup "Warm up - Level 00" [propertyTests, myBTreePrismLaws]

main :: IO ()
main = defaultMain tests
