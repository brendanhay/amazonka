{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.AST.Prefix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.AST.Prefix
  ( prefixes,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Hashable as Hashable
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Manipulate as Manipulate
import Gen.AST.Cofree (annotate)
import Gen.Prelude
import Gen.Text
import Gen.Types

type Seen = InsOrdHashMap Text (HashSet Text)

data Env = Env
  { _memo :: InsOrdHashMap Id (Maybe Text),
    _patterns :: Seen,
    _lenses :: Seen
  }

$(Lens.makeLenses ''Env)

type MemoP = StateT Env (Either String)

prefixes ::
  InsOrdHashMap Id (Shape Related) ->
  Either String (InsOrdHashMap Id (Shape Prefixed))
prefixes ss =
  State.evalStateT (traverse assignPrefix ss) $
    Env
      { _memo = mempty,
        _patterns = mempty,
        _lenses = mempty
      }

-- -- | Record projected smart constructors in set of seen field names.
-- smartCtors :: InsOrdHashMap Id (Shape a) -> Seen
-- smartCtors = HashMap.fromListWith (<>) . mapMaybe go . HashMap.toList
--   where
--     go :: (Id, Shape a) -> Maybe (CI Text, HashSet (CI Text))
--     go (s, _ :< Struct {}) = Just (k, HashSet.singleton v)
--       where
--         n = smartCtorId s
--         k = CI.mk (Text.takeWhile Char.isLower n)
--         v = CI.mk (dropLower n)
--     go _ = Nothing

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
                let hs = patternPrefixes n
                    ks = keys vs
                unique r patterns n hs ks
            Struct st ->
              Just <$> do
                let hs = lensPrefixes r n
                    ks = keys (st ^. members)
                unique r lenses n hs ks
            _ -> pure Nothing

    unique ::
      Relation ->
      Lens' Env Seen ->
      Text ->
      [Text] ->
      HashSet Text ->
      MemoP Text
    unique r seen n [] ks = do
      s <- Lens.use seen

      let hs = lensPrefixes r n
          f x = show x ++ " => " ++ show (HashMap.lookup x s)

      Except.throwError $
        "Error prefixing: " ++ show n
          ++ ", lenses: "
          ++ show (HashSet.toList ks)
          ++ show (map f hs)
    --
    unique r seen n (h : hs) ks =
      -- Find if this particular naming heuristic is used already, and if
      -- it is, then is there overlap with this set of ks?
      Lens.uses seen (HashMap.lookup h) >>= \case
        Just ys
          | overlap ys ks ->
            unique r seen n hs ks
        _ -> do
          seen %= HashMap.insertWith (<>) h ks
          pure h

overlap :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
overlap xs ys = not . HashSet.null $ HashSet.intersection xs ys

keys :: InsOrdHashMap Id a -> HashSet Text
keys = HashSet.fromList . map typeId . HashMap.keys

patternPrefixes :: Text -> [Text]
patternPrefixes n = [n]

-- | Acronym preference list.
lensPrefixes :: Relation -> Text -> [Text]
lensPrefixes r n
  | isOrphan r,
    Uni d <- _relMode r =
    case d of
      Input -> xs
      Output -> rs
  | otherwise = xs
  where
    rs = map (<> "rs") xs

    xs =
      map Text.toLower $
        catMaybes [r2, r4, r5, rN "f", rN "g", rN "h", rN "l", r6]

    a = camelAcronym n

    -- VpcPeeringInfo -> VPI
    r2 = Manipulate.toAcronym a

    -- SomeTestType -> S
    r4 = safeHead n

    -- SomeTypes -> STs (retain pural)
    r5
      | Text.isSuffixOf "s" n = flip Text.snoc 's' <$> r2
      | otherwise = Nothing

    rN suffix = (<> suffix) <$> (r2 <|> r4)

    -- SomeTestType -> SomeTestType
    r6 = Just n
