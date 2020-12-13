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
    acronymPrefixes,
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

type Seen = InsOrdHashMap (CI Text) (HashSet (CI Text))

data Env = Env
  { _memo :: InsOrdHashMap Id (Maybe Text),
    _branches :: Seen,
    _fields :: Seen
  }

$(Lens.makeLenses ''Env)

type MemoP = StateT Env (Either String)

prefixes :: InsOrdHashMap Id (Shape Related) -> Either String (InsOrdHashMap Id (Shape Prefixed))
prefixes ss = State.evalStateT (traverse assignPrefix ss) env
  where
    env = Env mempty mempty mempty

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
                let hs = mempty : acronymPrefixes r n
                    ks = keys vs
                unique r branches n hs ks
            Struct st ->
              Just <$> do
                let hs = acronymPrefixes r n
                    ks = keys (st ^. members)
                unique r fields n hs ks
            _ -> pure Nothing

    unique ::
      Relation ->
      Lens' Env Seen ->
      Text ->
      [CI Text] ->
      HashSet (CI Text) ->
      MemoP Text
    unique r seen n [] ks = do
      s <- Lens.use seen

      let hs = acronymPrefixes r n
          f x = show x ++ " => " ++ show (HashMap.lookup x s)

      Except.throwError $
        "Error prefixing: " ++ show n
          ++ ", fields: "
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
          pure (CI.original h)

overlap :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
overlap xs ys = not . HashSet.null $ HashSet.intersection xs ys

keys :: InsOrdHashMap Id a -> HashSet (CI Text)
keys = HashSet.fromList . map (CI.mk . typeId) . HashMap.keys

-- | Acronym preference list.
--
-- Prefixing occurs as follows:
-- * Requests  - prefer acronyms
-- * Responses - prefer acronyms, and append 'rs'
-- * Shapes    - prefer single letters, then acronyms
acronymPrefixes :: Relation -> Text -> [CI Text]
acronymPrefixes r (stripSuffix "Response" -> n)
  | isOrphan r,
    Uni d <- _relMode r =
    case d of
      Input -> ci xs
      Output -> ci rs
  | otherwise = ci xs
  where
    rs = map (<> "rs") xs

    ci = map CI.mk

    xs = catMaybes [r2, r3, r4, r5, rN "f", rN "g", rN "h", rN "l", r6]

    a = camelAcronym n
    a' = upperAcronym n

    -- VpcPeeringInfo -> VPI
    r2 = Manipulate.toAcronym a

    -- VpcPeeringInfo -> VPCPI
    r3
      | x /= r2 = x
      | otherwise = Nothing
      where
        x = Manipulate.toAcronym a'

    -- SomeTestType -> S
    r4 = Text.toUpper <$> safeHead n

    -- SomeTypes -> STs (retain pural)
    r5
      | Text.isSuffixOf "s" n = flip Text.snoc 's' <$> (r2 <|> r3)
      | otherwise = Nothing

    rN suffix = (<> suffix) <$> (r2 <|> r4)

    -- SomeTestType -> SomeTestType
    r6 = Just n
