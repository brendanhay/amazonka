{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.AST.Override
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Override
    ( override
    ) where

import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.HashMap.Strict    as Map
import qualified Data.HashSet           as Set
import           Data.Monoid
import           Debug.Trace

data Env = Env
    { _renamed  :: Map Id Id
    , _replaced :: Map Id Replace
    , _memo     :: Map Id (Shape Related)
    }

makeLenses ''Env

env :: MonadState Env m => Getter Env (Map Id a) -> Id -> m (Maybe a)
env l n = uses l (Map.lookup k)
  where
    k | n == mkId "DetachNetworkInterfaceRequest" = trace (show n) n
      | n == mkId "DetachNetworkInterface"     = trace (show n) n
      | otherwise = n

save :: Shape Related -> MemoS (Shape Related)
save x = memo %= Map.insert (identifier x) x >> return x

-- | Apply the override rules to shapes and their respective fields.
override :: Functor f
         => Map Id Override
         -> Service f (RefF a) (Shape Related)
         -> Service f (RefF a) (Shape Related)
override ovs svc = do
   svc & operations . each %~ operation
       & shapes            .~ evalState ss (Env rename replace mempty)
  where
    ss = fmap Map.fromList
       . traverse (uncurry (overrideShape ovs))
       . Map.toList
       $ svc ^. shapes

    operation :: Functor f => Operation f (RefF a) -> Operation f (RefF a)
    operation o = o
        { _opInput  = ref <$> _opInput  o
        , _opOutput = ref <$> _opOutput o
        }

    ref :: RefF a -> RefF a
    ref r
        | Just x <- ptr rename  = r & refShape .~ x
        | Just x <- ptr replace = r & refShape .~ x ^. replaceName
        | otherwise             = r
      where
        ptr = Map.lookup (r ^. refShape)

    rename :: Map Id Id
    rename =  vMapMaybe _renamedTo ovs

    replace :: Map Id Replace
    replace = vMapMaybe _replacedBy ovs

type MemoS = State Env

overrideRelation :: Relation -> MemoS Relation
overrideRelation r = do
    rn <- use renamed
    rp <- use replaced
    return $! r & parents %~ f rn rp
  where
    f rn rp n
        | Just x <- Map.lookup n rn = f rn rp x
        | Just x <- Map.lookup n rp = f rn rp (x ^. replaceName)
        | otherwise                 = n

overrideShape :: Map Id Override
              -> Id
              -> Shape Related
              -> MemoS (Id, Shape Related)
overrideShape ovs n c@(_ :< s) = mayRemember
  where
    mayRemember = env memo     n >>= maybe mayRename        (return . (n,))
    mayRename   = env renamed  n >>= maybe mayReplace       (\x -> overrideShape ovs x c)
    mayReplace  = env replaced n >>= maybe ((n,) <$> shape) (fmap (n,) . save . pointer)

    Override{..} = fromMaybe defaultOverride (Map.lookup n ovs)

    d = c ^. annRelation

    pointer :: Replace -> Shape Related
    pointer r = Related False (r ^. replaceName) mempty
         :< Ptr (s ^. info) (r ^. replaceDeriving)

    shape :: MemoS (Shape Related)
    shape = do
       d' <- overrideRelation d
       traverseOf references ref s
           >>= rules
           >>= save . (Related False n d' :<)

    ref :: RefF (Shape Related) -> MemoS (RefF (Shape Related))
    ref r = flip (set refAnn) r . snd <$>
        overrideShape ovs (r ^. refShape) (r ^. refAnn)

    rules :: ShapeF a -> MemoS (ShapeF a)
    rules = rename . prefix . require . optional >=> retype

    require, optional :: ShapeF a -> ShapeF a
    require  = setRequired (<> _requiredFields)
    optional = setRequired (`Set.difference` _optionalFields)

    prefix :: ShapeF a -> ShapeF a
    prefix =
        case _enumPrefix of
            Nothing -> id
            Just  p -> _Enum . _2 . kvTraversal %~ first (prependId p)

    rename :: ShapeF a -> MemoS (ShapeF a)
    rename x = do
        rn <- use renamed
        let f k = fromMaybe k (Map.lookup k rn)
        return $! x
                & _Struct . members . each . _1
               %~ f

    retype :: ShapeF a -> MemoS (ShapeF a)
    retype x = do
        rp <- use replaced
        rn <- use renamed
        let f g m v = maybe v (flip (set refShape) v . g)
                            (Map.lookup (v ^. refShape) m)
        return $! x
                & references
               %~ f _replaceName rp . f id rn
