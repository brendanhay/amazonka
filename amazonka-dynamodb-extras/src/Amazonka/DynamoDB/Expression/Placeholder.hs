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
    ( substitute
    , bisubstitute

    , Supply
    , supply
    , next
    ) where

import Control.Lens                     (_1, _2)
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

substitute :: (Monad m, Traversable t, Eq a, Hashable a)
           => t a
           -> StateT (HashMap a Builder) m (t Builder)
substitute =
    mapStateT (flip evalStateT supply)
        . traverse (replace (next_ ":v"))

bisubstitute :: (Monad m, Bitraversable p, Eq a, Eq b, Hashable a, Hashable b)
             => p a b
             -> StateT (HashMap a Builder, HashMap b Builder) m (p Builder Builder)
bisubstitute =
    mapStateT (flip evalStateT supply)
        . bitraverse (zoom _1 . replace (next_ "#n"))
                     (zoom _2 . replace (next_ ":v"))

data Supply = Supply [Builder]

supply :: Supply
supply = Supply (map Build.decimal ([1..] :: [Int]))

next_ :: Monad m => Builder -> a -> StateT Supply m Builder
next_ = const . next

next :: Monad m => Builder -> StateT Supply m Builder
next p = do
    Supply (x:xs) <- get
    put (Supply xs)
    pure (p <> x)

replace :: (Monad m, Eq a, Hashable a)
        => (a -> m b)
        -> a
        -> StateT (HashMap a b) m b
replace f x = do
    my <- gets (Map.lookup x)
    case my of
        Just y  -> pure y
        Nothing -> do
            y <- lift (f x)
            modify' (Map.insert x y)
            pure y
