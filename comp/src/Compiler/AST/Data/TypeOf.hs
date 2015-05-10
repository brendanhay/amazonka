{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.AST.Data.TypeOf
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data.TypeOf where

import           Compiler.AST.Data.Syntax
import           Compiler.Types
import           Control.Lens
import qualified Data.Foldable                as Fold
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit)

internal :: TType -> Type
internal = \case
    TType      x   -> tycon x
    TLit       x   -> literal True x
    TNatural       -> tycon "Nat"
    TMaybe     x   -> TyApp (tycon "Maybe") (internal x)
    TSensitive x   -> TyApp (tycon "Sensitive") (internal x)
    TList      x   -> TyApp (tycon "List") (internal x)
    TList1     x   -> TyApp (tycon "List1") (internal x)
    TMap       k v -> TyApp (TyApp (tycon "Map") (internal k)) (internal v)

     -- TList      i x       -> TyApp (TyApp (tycon "List") (singleton i)) (internal x)
    -- TList1     i x       -> TyApp (TyApp (tycon "List1") (singleton i)) (internal x)
    -- TMap   (e, i, j) k v ->
    --     TyApp
    --       (TyApp
    --         (TyApp
    --            (TyApp
    --               (TyApp (tycon "EMap") (singleton e))
    --               (singleton i))
    --            (singleton j))
    --         (internal k))
    --       (internal v)

external :: TType -> Type
external = \case
    TType      x   -> tycon x
    TLit       x   -> literal False x
    TNatural       -> tycon "Natural"
    TMaybe     x   -> TyApp (tycon "Maybe") (external x)
    TSensitive x   -> external x
    TList      x   -> TyList (external x)
    TList1     x   -> TyApp (tycon "NonEmpty") (external x)
    TMap       k v -> TyApp (TyApp (tycon "HashMap") (external k)) (external v)

literal :: Bool -> Lit -> Type
literal i = tycon . \case
    Int         -> "Int"
    Long        -> "Integer"
    Double      -> "Double"
    Text        -> "Text"
    Blob        -> "Base64"
    Bool        -> "Bool"
    -- Time (Just x) -- FIXME:
    --     | not i -> Text.pack (show x)
    Time        -> "UTCTime"

singleton :: Text -> Type
singleton = tycon -- . ("\"" <>) . (<> "\"")

mapping :: TType -> (Exp -> Exp)
mapping = compose . iso'
  where
    compose xs e = Fold.foldl' (\y -> InfixApp y (qop ".")) e xs

    iso' = \case
        TLit  (Time {}) -> [var "_Time"]
        TNatural        -> [var "_Nat"]
        TMaybe     x    -> case iso' x of; [] -> []; xs -> var "mapping" : xs
--        TFlatten   x    -> var "_Flatten"   : iso' x
        TSensitive x    -> var "_Sensitive" : iso' x
        TList      {}   -> [var "_List"]  -- Coercible.
        TList1     {}   -> [var "_List1"] -- Coercible.
        TMap       {}   -> [var "_Map"]   -- Coercible.
        _               -> []
