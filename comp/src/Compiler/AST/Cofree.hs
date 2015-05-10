{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

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

import           Compiler.Types
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Lens
import           Control.Monad.State
import qualified Data.HashMap.Strict    as Map
import           Data.Maybe

newtype Mu f = Mu (f (Mu f))

cofree :: Functor f => a -> Mu f -> Cofree f a
cofree x = go
  where
    go (Mu f) = x :< fmap go f

data a :*: b = !a :*: !b
    deriving (Show)

infixr 5 :*:

class HasId a where
    identifier :: a -> Id

instance HasId Id where
     identifier = id

instance HasId a => HasId (a :*: b) where
    identifier (x :*: _) = identifier x

instance (Functor f, HasId a) => HasId (Cofree f a) where
     identifier = identifier . extract

attach :: (HasId a, Monoid b, Comonad w) => Map Id b -> w a -> w (a :*: b)
attach m = extend (go . extract)
  where
    go x = x :*: fromMaybe mempty (Map.lookup (identifier x) m)

annotate :: (Traversable t, MonadState s m, HasId a)
         => Lens' s (Map Id b)
         -> (Cofree t a -> m b)
         -> Cofree t a
         -> m (Cofree t (a :*: b))
annotate l f = sequence . go
  where
    go x@(a :< _) = extend (fmap (a :*:) . memoise l f) x

memoise :: (MonadState s m, HasId a)
        => Lens' s (Map Id b)
        -> (a -> m b)
        -> a
        -> m b
memoise l f x = uses l (Map.lookup n) >>= maybe go return
  where
    n = identifier x :: Id

    go = do
        r <- f x
        l %= Map.insert n r
        return r
