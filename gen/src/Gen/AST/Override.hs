{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.AST.Override
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Override
  ( override,
  )
where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Error
import Control.Lens hiding ((:<))
import Control.Monad.State
import Data.Bifunctor
import Data.HashMap.Strict qualified as Map
import Data.List ((\\))
import Gen.Types

data Env = Env
  { _renamed :: Map Id Id,
    _replaced :: Map Id Replace,
    _memo :: Map Id (Shape Related)
  }

makeLenses ''Env

-- | Apply the override rules to shapes and their respective fields.
override ::
  Functor f =>
  Map Id Override ->
  Service f (RefF a) (Shape Related) b ->
  Service f (RefF a) (Shape Related) b
override ovs svc =
  svc & operations . each %~ operation
    & shapes .~ evalState ss (Env rename replace mempty)
  where
    ss =
      fmap Map.fromList
        . traverse (uncurry (overrideShape ovs))
        . Map.toList
        $ svc ^. shapes

    operation :: Functor f => Operation f (RefF a) b -> Operation f (RefF a) b
    operation o =
      o
        { _opInput = ref <$> _opInput o,
          _opOutput = ref <$> _opOutput o
        }

    ref :: RefF a -> RefF a
    ref r
      | Just x <- Map.lookup ptr rename = r & refShape .~ x
      | Just x <- Map.lookup ptr replace = r & refShape .~ x ^. replaceName
      | otherwise = r
      where
        ptr = r ^. refShape

    rename :: Map Id Id
    rename = vMapMaybe _renamedTo ovs

    replace :: Map Id Replace
    replace = vMapMaybe _replacedBy ovs

type MemoS = State Env

overrideShape ::
  Map Id Override ->
  Id ->
  Shape Related ->
  MemoS (Id, Shape Related)
overrideShape ovs n c@(_ :< s) = go -- env memo n >>= maybe go (return . (n,))
  where
    go = do
      rp <- env replaced n
      rn <- env renamed n
      case (rp, rn) of
        (Nothing, Nothing) -> (n,) <$> shape
        (Just x, _) -> (n,) <$> pointer x
        (_, Just x)
          | x == n -> (n,) <$> shape
          | otherwise -> overrideShape ovs x c

    Override {..} = fromMaybe defaultOverride (Map.lookup n ovs)

    pointer :: Replace -> MemoS (Shape Related)
    pointer r =
      save $
        (extract c & annId .~ n) :< Ptr (s ^. info) (typeOf r)

    shape :: MemoS (Shape Related)
    shape = do
      let a = extract c & annId .~ n
      traverseOf references ref s
        >>= rules
        >>= save . (a :<)

    ref :: RefF (Shape Related) -> MemoS (RefF (Shape Related))
    ref r =
      flip (set refAnn) r . snd
        <$> overrideShape ovs (r ^. refShape) (r ^. refAnn)

    rules :: ShapeF a -> MemoS (ShapeF a)
    rules = retype . fields . require . optional

    require, optional :: ShapeF a -> ShapeF a
    require = setRequired (<> _requiredFields)
    optional = setRequired (\\ _optionalFields)

    fields :: ShapeF a -> ShapeF a
    fields = _Struct . members . kvTraversal %~ first f
      where
        f k = maybe k (replaceId k) (Map.lookup k _renamedFields)

    retype :: ShapeF a -> MemoS (ShapeF a)
    retype x = do
      rp <- use replaced
      rn <- use renamed
      let f g m v =
            maybe
              v
              (flip (set refShape) v . g)
              (Map.lookup (v ^. refShape) m)
      return $! x
        & references
        %~ f _replaceName rp . f id rn

env :: MonadState Env m => Getter Env (Map Id a) -> Id -> m (Maybe a)
env l n = uses l (Map.lookup n)

save :: Shape Related -> MemoS (Shape Related)
save x = memo %= Map.insert (identifier x) x >> return x
