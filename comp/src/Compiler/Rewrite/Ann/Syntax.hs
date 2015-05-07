{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.Rewrite.Ann.Syntax
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Ann.Syntax where

import           Compiler.Types
import qualified Data.Foldable                as Fold
import qualified Data.HashSet                 as Set
import           Data.List                    (sort)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Language.Haskell.Exts        as Exts
import           Language.Haskell.Exts.Build  (app, lamE, paren, sfun)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit)

typeSig :: Text -> Type -> [Type] -> Decl
typeSig n t = TypeSig noLoc [ident n] . Fold.foldr' TyFun t

dataDecl :: Text -> [QualConDecl] -> Set Constraint -> Decl
dataDecl n fs cs = DataDecl noLoc arity [] (ident n) [] fs ds
  where
    arity = case fs of
        [QualConDecl _ _ _ (RecDecl _ [_])] -> NewType
        _                                   -> DataType

    ds = map ((,[]) . UnQual . Ident . drop 1 . show)
       . sort
       $ Set.toList cs

funDecl :: Text -> [Name] -> Exp -> Decl
funDecl n ps f = sfun noLoc (ident n) ps (UnGuardedRhs f) (BDecls [])

lensSig :: Text -> Type -> Type -> Decl
lensSig n x y = typeSig n (TyApp (TyApp (tycon "Lens'") x) y) []

lensBody :: Text -> Exp
lensBody n =
    app (app (var "lens") (var n))
        (paren (lamE noLoc [pvar "s", pvar "a"]
               (RecUpdate (var "s") [FieldUpdate (unqual n) (var "a")])))

conDecl :: Text -> QualConDecl
conDecl n = QualConDecl noLoc [] [] (ConDecl (ident n) [])

recDecl :: Text -> [([Name], Type)] -> QualConDecl
recDecl n = QualConDecl noLoc [] [] . RecDecl (ident n)

update :: Text -> Name -> Bool -> Set Constraint -> FieldUpdate
update n p req cs = FieldUpdate (unqual n) f
  where
    f | not req               = var "Nothing"
      | Set.member CMonoid cs = var "mempty"
      | otherwise             = Var (UnQual p)

tycon :: Text -> Type
tycon = TyCon . unqual

pvar :: Text -> Pat
pvar = Exts.pvar . ident

var :: Text -> Exp
var = Exts.var . ident

qop :: String -> QOp
qop = Exts.op . Exts.sym

param :: Int -> Name
param = Ident . mappend "p" . show

unqual :: Text -> QName
unqual = UnQual . ident

ident :: Text -> Name
ident = Ident . Text.unpack
