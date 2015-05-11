{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.AST.Data.Syntax
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data.Syntax where

import           Compiler.Types
import           Control.Lens                 hiding (mapping)
import qualified Data.Foldable                as Fold
import qualified Data.HashSet                 as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Language.Haskell.Exts        as Exts
import           Language.Haskell.Exts.Build  (app, infixApp, lamE, paren, sfun)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit)

data Field = Field
    { fieldId      :: Id
    , fieldOrdinal :: !Int
    , fieldType    :: TType
    , fieldReq     :: !Bool
    , fieldDerive  :: [Derive]
    , fieldComment :: Help
    }

fieldParam :: Field -> Name
fieldParam = Ident . mappend "p" . show . fieldOrdinal

ctorSig :: Id -> [Field] -> Decl
ctorSig n = TypeSig noLoc [n ^. smartCtorId . to ident]
    . Fold.foldr' (TyFun . external) (n ^. typeId . to tycon)
    . map fieldType
    . filter fieldReq

ctorDecl :: Maybe Text -> Id -> [Field] -> Decl
ctorDecl p n fs =
    sfun noLoc (n ^. smartCtorId . to ident) ps (UnGuardedRhs rhs) (BDecls [])
  where
    ps :: [Name]
    ps = map fieldParam (filter fieldReq fs)

    rhs :: Exp
    rhs = RecConstr (n ^. typeId . to unqual) (map upd fs)

    upd :: Field -> FieldUpdate
    upd f@Field{..} = FieldUpdate (fieldId ^. accessorId p . to unqual) def
      where
        def | not fieldReq               = var "Nothing"
            | DMonoid `elem` fieldDerive = var "mempty"
            | otherwise                  = Var (UnQual (fieldParam f))

lensSig :: Maybe Text -> TType -> Field -> Decl
lensSig p t Field{..} = TypeSig noLoc [ident (fieldId ^. lensId p)] $
    TyApp (TyApp (tycon "Lens'")
                 (external t))
          (external fieldType)

lensDecl :: Maybe Text -> Field -> Decl
lensDecl p Field{..} =
    sfun noLoc (ident l) [] (UnGuardedRhs rhs) (BDecls [])
  where
    l = fieldId ^. lensId p
    a = fieldId ^. accessorId p

    rhs = mapping fieldType $
        app (app (var "lens") (var a))
            (paren (lamE noLoc [pvar "s", pvar "a"]
                   (RecUpdate (var "s") [FieldUpdate (unqual a) (var "a")])))

dataDecl :: Id -> [QualConDecl] -> [Derive] -> Decl
dataDecl n fs cs = DataDecl noLoc arity [] (ident (n ^. typeId)) [] fs ds
  where
    arity = case fs of
        [QualConDecl _ _ _ (RecDecl _ [_])] -> NewType
        _                                   -> DataType

    ds = map ((,[]) . UnQual . Ident . drop 1 . show) cs

conDecl :: Text -> QualConDecl
conDecl n = QualConDecl noLoc [] [] (ConDecl (ident n) [])

recDecl :: Maybe Text -> Id -> [Field] -> QualConDecl
recDecl p n = QualConDecl noLoc [] []
    . RecDecl (ident (n ^. typeId)) -- Record has the same ctor as the type.
    . map (\f -> ([fieldId f ^. accessorId p . to ident], internal (fieldType f)))

-- instDecl :: Text -> Text -> Text -> Text -> [Text] -> Decl
-- instDecl c f o t fs = InstDecl noLoc Nothing [] [] (UnQual (ident c)) [tycon t]
--     [InsDecl (sfun noLoc (ident f) [ident v] (UnGuardedRhs rhs) (BDecls []))]
--   where
--     rhs = case fs of
--         []   -> var "mzero"
--         [x]  -> first x
--         x:xs -> Fold.foldl' (flip rest) (first x) xs

--     first :: Text -> Exp
--     first x = infixApp (con t) (qop "<$>") (loc x)

--     rest :: Text -> Exp -> Exp
--     rest x e = infixApp e (qop "<*>") (loc x)

--     loc :: Text -> Exp
--     loc = paren . infixApp (var v) (qop o) . str

--     v = "x"

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
literal _ = tycon . \case
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

tycon :: Text -> Type
tycon = TyCon . unqual

con :: Text -> Exp
con = Con . unqual

str :: Text -> Exp
str = Exts.Lit . String . Text.unpack

pvar :: Text -> Pat
pvar = Exts.pvar . ident

var :: Text -> Exp
var = Exts.var . ident

qop :: Text -> QOp
qop = Exts.op . Exts.sym . Text.unpack

param :: Int -> Name
param = Ident . mappend "p" . show

unqual :: Text -> QName
unqual = UnQual . ident

ident :: Text -> Name
ident = Ident . Text.unpack
