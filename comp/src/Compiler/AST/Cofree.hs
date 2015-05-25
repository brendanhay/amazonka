{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.AST.Cofree
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Cofree where

import           Compiler.Formatting
import           Compiler.Types
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Error
import           Control.Lens
import           Control.Monad.State
import           Data.Functor.Foldable  (Fix (..))
import qualified Data.HashMap.Strict    as Map

cofree :: Functor f => a -> Fix f -> Cofree f a
cofree x = go
  where
    go (Fix f) = x :< fmap go f

attach :: (Traversable t, HasId a, Monoid b)
       => (a -> b -> c)
       -> Map Id b
       -> Cofree t a
       -> Cofree t c
attach ctor m = extend (go . extract)
  where
    go x = ctor x . fromMaybe mempty $ Map.lookup (identifier x) m

-- | Allows the new annotation to be memoised separately
-- from the pre-existing annotation.
annotate :: (Traversable t, MonadState s m, HasId a)
         => (a -> b -> c)
         -> Lens' s (Map Id b)
         -> (Cofree t a -> m b)
         -> Cofree t a
         -> m (Cofree t c)
annotate ctor l f = sequence . extend go
  where
    go x@(a :< _) = ctor a <$> memoise l f x

memoise :: (MonadState s m, HasId a)
        => Lens' s (Map Id b)
        -> (a -> m b)
        -> a
        -> m b
memoise l f x = uses l (Map.lookup n) >>= maybe go return
  where
    n = identifier x

    go = do
        r <- f x
        l %= Map.insert n r
        return r

elaborate :: Show a => Map Id (ShapeF a) -> Either Error (Map Id (Shape Id))
elaborate m = Map.traverseWithKey shape m
  where
    shape :: Id -> ShapeF a -> Either Error (Shape Id)
    shape n = fmap (n :<) . traverseOf references ref

    ref :: RefF a -> Either Error (RefF (Shape Id))
    ref r = flip (set refAnn) r <$> (safe n >>= shape n)
      where
        n = r ^. refShape

    safe n = note
        (format ("Missing shape "       % iprimary %
                 ", possible matches: " % partial)
                n (n, m))
        (Map.lookup n m)
