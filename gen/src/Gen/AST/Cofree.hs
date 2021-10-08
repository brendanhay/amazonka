-- Module      : Gen.AST.Cofree
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Cofree where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Error
import Control.Lens hiding ((:<))
import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import Gen.Types

newtype Fix f = Fix (f (Fix f))

cofree :: Functor f => a -> Fix f -> Cofree f a
cofree x = go
  where
    go (Fix f) = x :< fmap go f

attach ::
  (Traversable t, HasId a, Monoid b) =>
  (a -> b -> c) ->
  Map Id b ->
  Cofree t a ->
  Cofree t c
attach ctor m = extend (go . extract)
  where
    go x = ctor x . fromMaybe mempty $ Map.lookup (identifier x) m

-- | Allows the new annotation to be memoised separately
-- from the pre-existing annotation.
annotate ::
  (Traversable t, MonadState s m, HasId a, Show b) =>
  (a -> b -> c) ->
  Lens' s (Map Id b) ->
  (Cofree t a -> m b) ->
  Cofree t a ->
  m (Cofree t c)
annotate ctor l f = sequence . extend go
  where
    go x@(a :< _) = ctor a <$> memoise l f x

memoise ::
  (MonadState s m, HasId a, Show b) =>
  Lens' s (Map Id b) ->
  (a -> m b) ->
  a ->
  m b
memoise l f x = uses l (Map.lookup n) >>= maybe go return
  where
    go = do
      r <- f x
      l %= Map.insert n r
      return r

    n = identifier x

type MemoE = StateT (Map Id (Shape Id)) (Either String)

elaborate :: Show a => Map Id (ShapeF a) -> Either String (Map Id (Shape Id))
elaborate m = evalStateT (Map.traverseWithKey (shape []) m) mempty
  where
    shape :: [Id] -> Id -> ShapeF a -> MemoE (Shape Id)
    shape seen n s
      | length seen > 30 = throwError $ depth seen
      | conseq seen = return $! n :< Ptr (s ^. info) (pointerTo n s)
      | otherwise = do
        ms <- gets (Map.lookup n)
        case ms of
          Just x -> return x
          Nothing -> do
            x <- (n :<) <$> traverseOf references (ref seen) s
            modify (Map.insert n x)
            return x

    ref :: [Id] -> RefF a -> MemoE (RefF (Shape Id))
    ref seen r = flip (set refAnn) r <$> (lift (safe n) >>= shape (n : seen) n)
      where
        n = r ^. refShape

    safe n =
      note
        ( "Missing shape " ++ Text.unpack (memberId n)
            ++ ", possible matches: "
            ++ partial n m
        )
        (Map.lookup n m)

    depth xs =
      "Too many cycles "
        ++ show (reverse (map memberId xs))

    conseq (x : ys@(y : z : _))
      | x == y = True
      | x == z = True
      | otherwise = conseq ys
    conseq _ = False
