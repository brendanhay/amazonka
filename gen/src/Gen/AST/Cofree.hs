-- |
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

import qualified Control.Comonad as Comonad
import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Gen.Prelude
import Gen.Types

newtype Fix f = Fix (f (Fix f))

cofree :: Functor f => a -> Fix f -> Cofree f a
cofree x = go
  where
    go (Fix f) = x :< fmap go f

attach ::
  (Traversable t, HasId a, Monoid b) =>
  (a -> b -> c) ->
  HashMap Id b ->
  Cofree t a ->
  Cofree t c
attach ctor m = Comonad.extend (go . Comonad.extract)
  where
    go x = ctor x . fromMaybe mempty $ HashMap.lookup (identifier x) m

-- | Allows the new annotation to be memoised separately
-- from the pre-existing annotation.
annotate ::
  (Traversable t, MonadState s m, HasId a, Show b) =>
  (a -> b -> c) ->
  Lens' s (HashMap Id b) ->
  (Cofree t a -> m b) ->
  Cofree t a ->
  m (Cofree t c)
annotate ctor l f = sequenceA . Comonad.extend go
  where
    go x@(a :< _) = ctor a <$> memoise l f x

memoise ::
  (MonadState s m, HasId a, Show b) =>
  Lens' s (HashMap Id b) ->
  (a -> m b) ->
  a ->
  m b
memoise l f x = Lens.uses l (HashMap.lookup n) >>= maybe go return
  where
    go = do
      r <- f x
      l %= HashMap.insert n r
      pure r

    n = identifier x

type MemoE = StateT (HashMap Id (Shape Id)) (Either String)

elaborate :: Show a => HashMap Id (ShapeF a) -> Either String (HashMap Id (Shape Id))
elaborate m = State.evalStateT (HashMap.traverseWithKey (shape []) m) mempty
  where
    shape :: [Id] -> Id -> ShapeF a -> MemoE (Shape Id)
    shape seen n s
      | length seen > 30 = Except.throwError $ depth seen
      | conseq seen = pure $! n :< Ptr (s ^. info) (pointerTo n s)
      | otherwise = do
        ms <- State.gets (HashMap.lookup n)
        case ms of
          Just x -> pure x
          Nothing -> do
            x <- (n :<) <$> Lens.traverseOf references (ref seen) s
            State.modify' (HashMap.insert n x)
            pure x

    ref :: [Id] -> RefF a -> MemoE (RefF (Shape Id))
    ref seen r = flip (Lens.set refAnn) r <$> (lift (safe n) >>= shape (n : seen) n)
      where
        n = r ^. refShape

    safe n =
      note
        ( "Missing shape " ++ Text.unpack (memberId n)
            ++ ", possible matches: "
            ++ partial n m
        )
        (HashMap.lookup n m)

    depth xs =
      "Too many cycles "
        ++ show (reverse (map memberId xs))

    conseq (x : ys@(y : z : _))
      | x == y = True
      | x == z = True
      | otherwise = conseq ys
    conseq _ = False
