{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports #-}
{-# LANGUAGE RankNTypes #-}
module PropertyTests (propertyTests, genTree, genMyBTreeVal) where

import           Control.Lens        (Prism', matching, preview, review)
import           Control.Lens.TH     (makePrisms)

import           Data.Function       (on)
import           Data.List           (delete, filter, insert, nub, sort)
import           Data.Monoid         (Endo (..))
import           Data.Maybe          (isJust, isNothing)

import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           MyBTree
import           Generators

-- [OPTIONAL]
import           LawPropertiesBonus  (myBTreePrismLaws, firstPrismLaw, secondPrismLaw, thirdPrismLaw, _Empty, _Node)

----------------------------------------------------------------------------------------------------
addTen :: Int -> Int
addTen = appEndo . foldMap Endo $ replicate 10 succ

-- A simple property for the 'addTen' function, ensure that:
--
-- \/ (x : Int) -> addTen x === (x + 10)
prop_addTen :: Property
prop_addTen = property $ do
  x <- forAll $ Gen.int (Range.linear 0 1000)
  addTen x === (x + 10)

----------------------------------------------------------------------------------------------------

-- Property based testing is useful for debugging. In this exercise we've given
-- you a broken function. Using the output of the test, fix up the function. If
-- you're keen you can expand the properties that it must satisfy to make the
-- test, and commensurately the function itself, more robust.
--
badReverse :: [a] -> [a]
badReverse = reverse 

prop_badReverse :: Property
prop_badReverse = property $ do
  xs <- forAll (Gen.list (Range.linear 0 1000) Gen.bool)
  ys <- forAll (Gen.list (Range.linear 0 1000) Gen.bool)
  badReverse (badReverse xs) === xs
  badReverse xs === badReverse xs
  if length xs > 1 && head xs /= last xs
    then badReverse xs /== xs
    else assert True
  badReverse (xs ++ ys) === badReverse ys ++ badReverse xs

  -- [BONUS]: Are there other properties for reversing a list that could make this test more robust?

----------------------------------------------------------------------------------------------------

-- In this set of exercises we will build properties to ensure that our
-- functions for a given data structure do what we expect.
--
-- These examples are lifted from a presentation by John Hughes: "Building on developer intuitions". 
-- Which may be viewed at: https://www.youtube.com/watch?v=NcJOiQlzlXQ

-- We will start with the following data structure.
newtype Coin = Coin Int deriving (Eq, Show)

instance Enum Coin where
  toEnum = Coin
  fromEnum (Coin x) = x

instance Bounded Coin where
  minBound = Coin 0
  maxBound = Coin maxCoinValue

-- That has a pre-determined maximum value.
maxCoinValue :: Int
maxCoinValue = 1000000

-- We're able to validate whether the Coin is valid.
validCoin :: Coin -> Bool
validCoin (Coin c) = c >= 0 && c < maxCoinValue

-- This is the function we're going to write some tests for.
addCoins :: Coin -> Coin -> Maybe Coin
addCoins (Coin a) (Coin b) =
  if (a + b < maxCoinValue) && (a < maxCoinValue) && (b < maxCoinValue)
    then Just (Coin $ a + b)
    else Nothing

-- Write our generator for Coin
genCoin :: MonadGen m => m Coin
genCoin = Gen.enumBounded

genCoinB :: MonadGen m => Int -> Int -> m Coin
genCoinB lower upper = Coin <$> Gen.int (Range.linear lower upper)

-- Test our 'normal' case, aka the happy path
prop_addCoins_Normal :: Property
prop_addCoins_Normal = property $ do
  x <- forAll $ genCoinB 0 (maxCoinValue `div` 2)
  y <- forAll $ genCoinB 0 (maxCoinValue `div` 2)
  let res = addCoins x y
  case res of
    Nothing -> failure
    Just z  -> fromEnum z === fromEnum x + fromEnum y

-- Test the 'overflow' case, aka the sad path
prop_addCoins_Overflow :: Property
prop_addCoins_Overflow = property $ do
  x <- forAll $ genCoinB 1 maxCoinValue
  let y = Coin maxCoinValue
  let res1 = addCoins x y
  assert (isNothing res1)
  let res2 = addCoins y x
  assert (isNothing res2)

-- Instead of having separate properties, we can combine them into a single
-- property test.
prop_addCoins_Combined :: Property
prop_addCoins_Combined = property $ do
  x <- forAll genCoin
  y <- forAll genCoin
  let res = addCoins x y
  if (fromEnum x) + (fromEnum y) > maxCoinValue
  then assert (isNothing res)
  else 
    case res of
      Nothing -> failure
      Just z -> fromEnum z === fromEnum x + fromEnum y

----------------------------------------------------------------------------------------------------


-- We're not ready to write a property test for inserting values into our binary
-- search tree.

-- To do this we need a 'model' that we know is correct to validate our
-- assumptions for the binary search tree. The simpler the better so we can be
-- confident that our representation is correct.
--
-- We're going to use an ordered list of pairs. Since it is a trivial data
-- structure and manipulating it is straightforward. We can be confident that
-- it is a good model for operations on our binary search tree.
--
-- fromList [(1, 'a'), (3,'c')] -> insert 2 'b' -> fromList [(1, 'a'), (2,'b'), (3,'c')]
--                   |                                            |
--                   |                                            |
--                   v                                            v
--          [(1, 'a'), (3,'c')] -> modelInsert (2,'b') -> [(1, 'a'), (2,'b'), (3,'c')]
--
prop_MyBTree_Insert :: Property
prop_MyBTree_Insert = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) genMyBTreeVal
  value <- forAll $ genMyBTreeVal
  test_List_Insert (sort xs) value

test_List_Insert :: (MonadTest m, Ord a) => [a] -> a -> m ()
test_List_Insert xs value = do
  assert (isSorted xs)
  let zs = Data.List.insert value xs
  assert (isSorted zs)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Now implement a test to ensure that we're correctly deleting elements within the tree.
prop_MyBTree_Delete :: Property
prop_MyBTree_Delete = property $ do
  xs <- nub <$> (forAll $ Gen.list (Range.linear 1 100) genMyBTreeVal)
  idx <- forAll $ Gen.int (Range.linear 0 (length xs - 1))
  let value = xs !! idx
  let ys = delete value xs
  test_List_Delete xs ys value

test_List_Delete :: (MonadTest m, Ord a) => [a] -> [a] -> a -> m ()
test_List_Delete orig upd val = do
  assert (val `elem` orig)
  assert (not (val `elem` upd))

----------------------------------------------------------------------------------------------------
  --
propertyTests :: TestTree
propertyTests = testGroup "Level00 - Property Tests"
  [ testProperty "Addition still works" prop_addTen
  , testProperty "Bad reverse is bad" prop_badReverse
  , testProperty "Add Coins (Normal)" prop_addCoins_Normal
  , testProperty "Add Coins (Overflow)" prop_addCoins_Overflow
  , testProperty "Add Coins (Combined)" prop_addCoins_Combined

  , testProperty "BST insert" prop_MyBTree_Insert
  , testProperty "BST delete" prop_MyBTree_Delete
  ]