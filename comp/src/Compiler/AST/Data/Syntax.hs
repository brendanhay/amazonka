{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

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

import           Compiler.AST.Data.Field
import           Compiler.AST.TypeOf
import           Compiler.Protocol
import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Lens                 hiding (mapping, op)
import qualified Data.Foldable                as Fold
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Language.Haskell.Exts        as Exts
import           Language.Haskell.Exts.Build  (app, infixApp, lamE, paren, sfun)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit)

ctorSig :: Id -> [Field] -> Decl
ctorSig n = TypeSig noLoc [n ^. smartCtorId . to ident]
    . Fold.foldr' TyFun (n ^. typeId . to tycon)
    . map external
    . filter (^. fieldRequired)

ctorDecl :: Id -> [Field] -> Decl
ctorDecl n fs = sfun noLoc name ps (UnGuardedRhs rhs) (BDecls [])
  where
    name :: Name
    name = n ^. smartCtorId . to ident

    ps :: [Name]
    ps = map (view fieldParam) (filter (view fieldRequired) fs)

    rhs :: Exp
    rhs | null fs   = var (n ^. typeId)
        | otherwise = RecConstr (n ^. typeId . to unqual) (map upd fs)

    upd :: Field -> FieldUpdate
    upd f = FieldUpdate (f ^. fieldAccessor . to unqual) def
      where
        def | opt, f ^. fieldMonoid = var "mempty"
            | opt                   = var "Nothing"
            | otherwise             = Var (UnQual (f ^. fieldParam))

        opt = not (f ^. fieldRequired)

lensSig :: TType -> Field -> Decl
lensSig t f = TypeSig noLoc [ident (f ^. fieldLens)] $
    TyApp (TyApp (tycon "Lens'")
                 (external t))          (external (typeOf f))

lensDecl :: Field -> Decl
lensDecl f = sfun noLoc (ident l) [] (UnGuardedRhs rhs) (BDecls [])
  where
    l = f ^. fieldLens
    a = f ^. fieldAccessor

    rhs = mapping (typeOf f) $
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

recDecl :: Id -> [Field] -> QualConDecl
recDecl n [] = conDecl (n ^. typeId)
recDecl n fs = QualConDecl noLoc [] [] $ RecDecl (ident (n ^. typeId)) (map g fs)
  where
    g f = ([f ^. fieldAccessor . to ident], internal f)

instanceExp :: Protocol -> Instance -> Field -> Exp
instanceExp proto i f =
    case unwrap (f ^. fieldRef . refAnn) of
        List l -> expr (parent : maybeToList item)
          where
            (parent, item) =
                listName proto dir (f ^. fieldId) (f ^. fieldRef) l

        _ -> expr [member]
  where
    expr :: [Text] -> Exp
    expr = case dir of
        Output -> Fold.foldl' (\acc -> infixApp acc op . str) (var "x")
        Input  -> Fold.foldr' (\x   -> infixApp (str x) op)   (var accessor)

    accessor, member :: Text
    accessor = f ^. fieldAccessor
    member   = memberName (f ^. fieldId) (f ^. fieldRef)

    op :: QOp
    op = qop (operator i req)

    dir :: Direction
    dir = direction i

    req :: Bool
    req = f ^. fieldRequired

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

internal :: TypeOf a => a -> Type
internal (typeOf -> t) =
    case t of
        TType      x   -> tycon x
        TLit       x   -> literal True x
        TNatural       -> tycon "Nat"
        TMaybe     x   -> TyApp (tycon "Maybe") (internal x)
        TSensitive x   -> TyApp (tycon "Sensitive") (internal x)
        TList      x   -> TyApp (tycon "List") (internal x)
        TList1     x   -> TyApp (tycon "List1") (internal x)
        TMap       k v -> TyApp (TyApp (tycon "Map") (internal k)) (internal v)

external :: TypeOf a => a -> Type
external (typeOf -> t) =
    case t of
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

mapping :: TType -> Exp -> Exp
mapping = compose . iso'
  where
    compose xs e = Fold.foldl' (\y -> InfixApp y (qop ".")) e xs

    iso' = \case
        TLit  (Time {}) -> [var "_Time"]
        TNatural        -> [var "_Nat"]
        TMaybe     x    -> case iso' x of; [] -> []; xs -> var "mapping" : xs
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
