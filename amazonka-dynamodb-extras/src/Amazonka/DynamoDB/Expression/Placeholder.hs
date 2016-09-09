{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Amazonka.DynamoDB.Expression.Condition
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Placeholders for Expression Names and Values.
--
-- /See:/
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the AWS documentation.
module Amazonka.DynamoDB.Expression.Placeholder
    ( Placeholders
    , placeholders
    , empty

    , Supply
    , supply
    , unique

    , replace

    , substitute
    , bisubstitute
    ) where

import Control.Lens                     (Lens', lens, modifying, uses, _1, _2)
import Control.Lens.Zoom                (zoom)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.State.Strict

import Data.Bitraversable     (Bitraversable (..))
import Data.Hashable          (Hashable (..))
import Data.HashMap.Strict    (HashMap)
import Data.Monoid            ((<>))
import Data.Text.Lazy.Builder (Builder)

import qualified Data.HashMap.Strict        as Map
import qualified Data.Text.Lazy.Builder.Int as Build

-- | An opaque mapping of placeholders with the invariant that two sets of
-- placeholders are distinct.
newtype Placeholders a = Placeholders { placeholders :: HashMap a Builder }

placeholdersL :: Lens' (Placeholders a) (HashMap a Builder)
placeholdersL = lens placeholders (\s a -> s { placeholders = a })

empty :: (Eq a, Hashable a) => Placeholders a
empty = Placeholders mempty

-- | A supply of values suitable for substitution.
newtype Supply = Supply [Builder]

-- | Construct an infinite (@< (maxBound :: Int)@) supply of positive integral values.
supply :: Supply
supply = Supply (map Build.decimal ([1..] :: [Int]))

-- | Get the next unique value from the 'Supply'.
unique :: Monad m
       => Builder
          -- ^ A (possibly empty) prefix to prepend to the returned unique value.
       -> StateT Supply m Builder
unique prefix = do
    Supply (x:xs) <- get
    put (Supply xs)
    pure (prefix <> x)

-- | Replace a value with a memoized placeholder.
replace :: (Monad m, Eq a, Hashable a)
        => (a -> m Builder)
           -- ^ A function to generate a placeholder, if none was previously memoized.
        -> a
        -> StateT (Placeholders a) m Builder
replace f x = do
    my <- uses placeholdersL (Map.lookup x)
    case my of
        Just y  -> pure y
        Nothing -> do
            y <- lift (f x)
            modifying placeholdersL (Map.insert x y)
            pure y

-- | Substitute the argument of a 'Traversable' with a placeholder.
substitute :: (Monad m, Traversable t, Eq a, Hashable a)
           => t a
           -> StateT (Placeholders a) m (t Builder)
substitute =
    mapStateT (flip evalStateT supply)
        . traverse (replace (next_ ":v"))

-- | Substitute both arguments of a 'Bitraversable' with placeholders.
bisubstitute :: (Monad m, Bitraversable p, Eq a, Eq b, Hashable a, Hashable b)
             => p a b
             -> StateT (Placeholders a, Placeholders b) m (p Builder Builder)
bisubstitute =
    mapStateT (flip evalStateT supply)
        . bitraverse (zoom _1 . replace (next_ "#n"))
                     (zoom _2 . replace (next_ ":v"))

next_ :: Monad m => Builder -> a -> StateT Supply m Builder
next_ = const . unique
