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
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit)

typeSig :: Name -> Type -> [Type] -> Decl
typeSig n t = TypeSig noLoc [n] . Fold.foldr' TyFun t

dataDecl :: Name -> [QualConDecl] -> [Deriving] -> Decl
dataDecl n cs = DataDecl noLoc arity [] n [] cs
  where
    arity = case cs of
        [QualConDecl _ _ _ (RecDecl _ [_])] -> NewType
        _                                   -> DataType

derivings :: Set Constraint -> [Deriving]
derivings = map ((,[]) . UnQual . Ident . drop 1 . show) . sort . Set.toList

-- itycon :: Id -> Type
-- itycon = TyCon . UnQual . iident

-- conId :: Getter Id Type
-- conId = qtypeId . to TyCon

-- qtypeId :: Getter Id QName
-- qtypeId = ctorId . to unqual

tycon :: Text -> Type
tycon = TyCon . unqual

pvar :: Text -> Pat
pvar = Exts.pvar . ident

var :: Text -> Exp
var = Exts.var . ident

symop :: String -> QOp
symop = Exts.op . Exts.sym

unqual :: Text -> QName
unqual = UnQual . ident

ident :: Text -> Name
ident = Ident . Text.unpack
