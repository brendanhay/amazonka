module Gen.AST.Cofree where

import qualified Control.Comonad as Comonad
import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import Gen.Prelude
import Gen.Types

newtype Fix f = Fix (f (Fix f))

cofree :: (Functor f) => a -> Fix f -> Cofree f a
cofree x = go
  where
    go (Fix f) = x :< fmap go f

attach ::
  (Traversable t, HasId a, Monoid b) =>
  (a -> b -> c) ->
  HashMap Id b ->
  Cofree t a ->
  Cofree t c
attach ctor m = fmap go
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

-- | Memoise the shapes elaborated so far, so that each shape is elaborated
-- once and its tree is reused wherever the shape appears.
--
-- The tree kept for a shape is the first one built, so where its 'Ptr'
-- loop-breakers sit depends on the ancestors it was first reached through,
-- and two occurrences of an 'Id' can differ in that respect. Types do not
-- depend on it, because "Gen.AST.Solve" assigns them per 'Id'. Code that
-- inspects a node's constructor still sees the 'Ptr' in place of the list,
-- map or struct that it stands in for.
type MemoE = StateT (HashMap Id (Shape Id)) (Either String)

runMemoE :: MemoE a -> Either String a
runMemoE = flip State.evalStateT mempty

-- | Elaborate a map of 'ShapeF's into a 'Cofree' tree, by looking up
-- all references in the input map. The 'Cofree' allows us to inline
-- nested structure definitions, but mutually-recursive shape
-- references are broken by returning 'Ptr's as loop breakers.
--
-- We never return a 'Ptr' in the first layer of the 'HashMap''s
-- values.
elaborate ::
  forall a.
  (Show a) =>
  HashMap Id (ShapeF a) ->
  Either String (HashMap Id (Shape Id))
elaborate m = runMemoE $ HashMap.traverseWithKey (shape mempty) m
  where
    shape :: Set Id -> Id -> ShapeF a -> MemoE (Shape Id)
    shape seen n s
      | Set.member n seen = pure $! n :< Ptr (s ^. info)
      | otherwise = do
          ms <- State.gets (HashMap.lookup n)
          case ms of
            Just x -> pure x
            Nothing -> do
              x <- (n :<) <$> Lens.traverseOf references (ref (Set.insert n seen)) s
              State.modify' (HashMap.insert n x)
              pure x

    ref :: Set Id -> RefF a -> MemoE (RefF (Shape Id))
    ref seen r = do
      let n = r ^. refShape
      s <- findShape n >>= shape seen n
      pure $ r & refAnn .~ s

    findShape :: Id -> MemoE (ShapeF a)
    findShape n = case HashMap.lookup n m of
      Nothing ->
        Except.throwError $
          unwords
            [ "Missing shape ",
              Text.unpack (memberId n),
              ", possible matches: ",
              partial n m
            ]
      Just s -> pure s
