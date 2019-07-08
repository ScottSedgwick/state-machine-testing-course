module Generators where


import           Control.Applicative (liftA2)
import           Hedgehog            (MonadGen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           MyBTree

-- Use the binary search tree that is defined in the MyBTree module, to
-- complete the following functions.
--
-- These examples are lifted from a presentation by John Hughes: "Building on developer intuitions". 
-- Which may be viewed at: https://www.youtube.com/watch?v=NcJOiQlzlXQ

-- To test our assumptions, we'll need to generate random MyBTrees. Using the
-- constructor functions from the MyBTree module, write a generator that can use
-- a given generator to populate the tree.
genTree :: (Ord k, MonadGen m) => m (k,v) -> m (MyBTree k v)
genTree genKV = fromList <$> Gen.list (Range.linear 0 100) genKV

-- To populate our tree, we need to generate some keys and their respective
-- values. We will make Hedgehog do this for us by reusing some of the built-in
-- generators.
genMyBTreeVal :: MonadGen m => m (Int, Char)
genMyBTreeVal = liftA2 (,) (Gen.int (Range.linear (-100) 100)) (Gen.enum 'a' 'z')