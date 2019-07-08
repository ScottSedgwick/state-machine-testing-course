{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports #-}
{-# LANGUAGE RankNTypes #-}
module LawPropertiesBonus 
  ( myBTreePrismLaws
  , firstPrismLaw
  , secondPrismLaw
  , thirdPrismLaw
  , _Empty
  , _Node
  ) where

import           Control.Applicative (liftA3)
import           Control.Lens   (Prism', matching, preview, prism, review)
import           Control.Monad  (MonadPlus(..))

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           MyBTree        (MyBTree (..))
import           Generators

-- Complete the following prisms, then write properties that check the Prism laws hold.
_Empty :: Prism' (MyBTree k a) ()
_Empty = prism (\_ -> Empty) f
  where 
    f Empty = Right ()
    f t     = Left t

_Node :: Prism' (MyBTree k a) (MyBTree k a, (k, a), MyBTree k a)
_Node = prism (\(a,b,c) -> Node a b c) f
  where
    f Empty        = Left Empty
    f (Node a b c) = Right (a,b,c)

-- First, if I re or review a value with a Prism and then preview or use (^?),
-- I will get it back:
--
-- preview l (review l b) ≡ Just b
--
firstPrismLaw :: (Eq b, Show b) => Gen b -> Prism' a b -> Property
firstPrismLaw _gen _p = property $ do
  b <- forAll _gen
  preview _p (review _p b) === Just b

-- Second, if you can extract a value a using a Prism l from a value s, then
-- the value s is completely described by l and a:
--
-- preview l s ≡ Just a ==> review l a ≡ s
secondPrismLaw :: (Eq s, Show s) => Gen s -> Prism' s a -> Property
secondPrismLaw _gen _p = property $ do
  b <- forAll _gen
  case preview _p b of
    Nothing  -> success
    (Just a) -> review _p a === b

-- Third, if you get non-match t, you can convert it result back to s:
--
-- matching l s ≡ Left t ==> matching l t ≡ Left s
thirdPrismLaw :: (Eq a, Show a, Eq s, Show s) => Gen s -> Prism' s a -> Property
thirdPrismLaw _gen _p = property $ do
  b <- forAll _gen
  case matching _p b of
    Left t  -> (matching _p t) === (Left b)
    Right _ -> success

myBTreePrismLaws :: TestTree
myBTreePrismLaws = testGroup "Level00 - Law Properties"
  [ testProperty "First Prism Law on Empty" (firstPrismLaw (Gen.constant ()) _Empty)
  , testProperty "Second Prism Law on Empty" (secondPrismLaw (genTree genMyBTreeVal) _Empty)
  , testProperty "Third Prism Law on Empty" (thirdPrismLaw (genTree genMyBTreeVal) _Empty)
  , testProperty "First Prism Law on Node" (firstPrismLaw genTreeTriple _Node)
  , testProperty "Second Prism Law on Node" (secondPrismLaw (genTree genMyBTreeVal) _Node)
  , testProperty "Third Prism Law on Node" (thirdPrismLaw (genTree genMyBTreeVal) _Node)
  ]

genTreeTriple :: Gen (MyBTree Int Char, (Int, Char), MyBTree Int Char)
genTreeTriple = liftA3 (,,) (genTree genMyBTreeVal) genMyBTreeVal (genTree genMyBTreeVal)