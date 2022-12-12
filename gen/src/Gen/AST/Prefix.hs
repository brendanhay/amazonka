{-# LANGUAGE TemplateHaskell #-}

module Gen.AST.Prefix
  ( prefixes,
  )
where

import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Gen.AST.Cofree
import Gen.Prelude
import Gen.Text
import Gen.Types

type Seen = Map (CI Text) (HashSet (CI Text))

data Env = Env
  { _memo :: Map Id (Maybe Text),
    _branches :: Seen,
    _fields :: Seen
  }

$(Lens.makeLenses ''Env)

type MemoP = StateT Env (Either String)

prefixes :: Map Id (Shape Related) -> Either String (Map Id (Shape Prefixed))
prefixes ss = State.evalStateT (traverse assignPrefix ss) env
  where
    env = Env mempty mempty (smartCtors ss)

-- | Record projected smart constructors in set of seen field names.
smartCtors :: Map Id (Shape a) -> Seen
smartCtors = Map.fromListWith (<>) . mapMaybe go . Map.toList
  where
    go :: (Id, Shape a) -> Maybe (CI Text, HashSet (CI Text))
    go (s, _ :< Struct {}) = Just (k, HashSet.singleton v)
      where
        n = smartCtorId s
        k = CI.mk (Text.takeWhile Char.isLower n)
        v = CI.mk (stripTilUpper n)
    go _ = Nothing

assignPrefix :: Shape Related -> MemoP (Shape Prefixed)
assignPrefix = annotate Prefixed memo go
  where
    go :: (HasId a, HasRelation a) => Shape a -> MemoP (Maybe Text)
    go (x :< s) =
      let n = typeId (identifier x)
          r = x ^. relation
       in case s of
            Enum _ vs ->
              Just <$> do
                let hs = acronymPrefixes r n
                    ks = keys vs
                unique r branches n hs ks
            Struct st ->
              Just <$> do
                let hs = acronymPrefixes r n
                    ks = keys (st ^. members)
                unique r fields n hs ks
            _ -> return Nothing

    unique ::
      Relation ->
      Lens' Env Seen ->
      Text ->
      [CI Text] ->
      HashSet (CI Text) ->
      MemoP Text
    unique r seen n [] ks = do
      s <- Lens.use seen

      let line x =
            "\n" ++ Text.unpack (CI.original x)
              ++ " => "
              ++ show (Map.lookup x s)

      Except.throwError $
        "Error prefixing: " ++ Text.unpack n
          ++ ", fields: "
          ++ show (HashSet.toList ks)
          ++ concatMap line (acronymPrefixes r n)
    --
    unique r seen n (h : hs) ks = do
      m <- Lens.uses seen (Map.lookup h)
      -- Find if this particular naming heuristic is used already, and if
      -- it is, then is there overlap with this set of ks?
      case m of
        Just ys
          | overlap ys ks ->
            unique r seen n hs ks
        _ -> do
          seen %= Map.insertWith (<>) h ks
          return (CI.original h)

overlap :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
overlap xs ys = not . HashSet.null $ HashSet.intersection xs ys

keys :: Map Id a -> HashSet (CI Text)
keys = HashSet.fromList . map (CI.mk . typeId) . Map.keys

acronymPrefixes :: Relation -> Text -> [CI Text]
acronymPrefixes _relation name = [CI.mk (upperHead name)]
