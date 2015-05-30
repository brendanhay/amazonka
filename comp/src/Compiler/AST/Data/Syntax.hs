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
import           Compiler.AST.Data.Instance
import           Compiler.AST.TypeOf
import           Compiler.Formatting
import           Compiler.Protocol
import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Error
import           Control.Lens                 hiding (iso, mapping, op)
import qualified Data.Foldable                as Fold
import           Data.Function                ((&))
import qualified Data.HashMap.Strict          as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Language.Haskell.Exts        as Exts
import           Language.Haskell.Exts.Build  (app, appFun, infixApp, lamE,
                                               paren, sfun)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit, Var)

ctorSig :: Timestamp -> Id -> [Field] -> Decl
ctorSig ts n = TypeSig noLoc [n ^. smartCtorId . to ident]
    . Fold.foldr' TyFun (n ^. typeId . to tycon)
    . map (external ts)
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
        | otherwise = RecConstr (n ^. typeId . to unqual) (map fieldUpdate fs)

fieldUpdate :: Field -> FieldUpdate
fieldUpdate f = FieldUpdate (f ^. fieldAccessor . to unqual) set'
  where
    set' | opt, f ^. fieldMonoid    = var "mempty"
         | opt                      = var "Nothing"
         | Just v <- iso (typeOf f) = infixApp v (qop "#") p
         | otherwise                = p

    opt = not (f ^. fieldRequired)

    p = Exts.Var (UnQual (f ^. fieldParam))

lensSig :: Timestamp -> TType -> Field -> Decl
lensSig ts t f = TypeSig noLoc [ident (f ^. fieldLens)] $
    TyApp (TyApp (tycon "Lens'")
                 (external ts t)) (external ts (typeOf f))

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

recDecl :: Timestamp -> Id -> [Field] -> QualConDecl
recDecl _  n [] = conDecl (n ^. typeId)
recDecl ts n fs = QualConDecl noLoc [] [] $ RecDecl (ident (n ^. typeId)) (map g fs)
  where
    g f = ([f ^. fieldAccessor . to ident], internal ts f)

responseExps :: Protocol -> [Field] -> [Exp]
responseExps p = map go
  where
    go f = case f ^. fieldLocation of
        Just Headers     -> fromHeadersExp p f
        Just Header      -> fromHeadersExp p f
        Just StatusCode  -> app (var "pure") (var "s")
        Just Body        -> app (var "pure") (var "x")
        Nothing
            | f ^. fieldPayload
                         -> app (var "pure") (var "x")
        _                ->
            case p of
                JSON     -> fromJSONExp p f
                RestJSON -> fromJSONExp p f
                _        -> fromXMLExp  p f

instanceExps :: Protocol -> Inst -> [Exp]
instanceExps p = \case
    FromXML   fs -> map (fromXMLExp   p) fs
    ToXML     fs -> map (toXMLExp     p) fs
    ToHeaders fs -> map (toHeadersExp p) fs
    ToQuery   es -> map (either str (toQueryExp p)) es
    ToPath    es -> map (either str toPathExp)      es
    ToBody    f  -> [var (f ^. fieldAccessor)]

fromXMLExp, fromJSONExp, fromHeadersExp, toXMLExp,
 toJSONExp, toHeadersExp, toQueryExp :: Protocol -> Field -> Exp
fromXMLExp     = decodeExp '@'
fromJSONExp    = decodeExp ':'
fromHeadersExp = decodeExp '?'
toXMLExp       = encodeExp '@'
toJSONExp      = encodeExp '.'
toHeadersExp   = encodeExp '~'
toQueryExp     = encodeExp '?'

toPathExp :: Field -> Exp
toPathExp = app (var "toText") . var . view fieldAccessor

decodeExp :: Char -> Protocol -> Field -> Exp
decodeExp c p f
    | Just i <- m        = infixApp v (decodeListOp  c) (infixApp n (decodeOp c) i)
    | f ^. fieldRequired = infixApp v (decodeOp      c) n
    | f ^. fieldMonoid   = infixApp v (decodeOp      c) (infixApp n (decodeDefOp c) (var "mempty"))
    | otherwise          = infixApp v (decodeMaybeOp c) n
  where
    (n, m) = memberNames p f

    v = var "x"

encodeExp :: Char -> Protocol -> Field -> Exp
encodeExp c p f
    | Just i <- m = infixApp n o (infixApp i o v)
    | otherwise   = infixApp n o v
  where
    (n, m) = memberNames p f

    o = encodeOp c
    v = var (f ^. fieldAccessor)

decodeOp, decodeMaybeOp, decodeListOp, decodeDefOp, encodeOp :: Char -> QOp
decodeOp      c = Exts.op (Exts.sym ['.', c])
decodeMaybeOp c = Exts.op (Exts.sym ['.', c, '?'])
decodeListOp  c = Exts.op (Exts.sym ['.', c, c])
decodeDefOp   c = Exts.op (Exts.sym ['.', '!', c])
encodeOp      c = Exts.op (Exts.sym [c, '='])

memberNames :: Protocol -> Field -> (Exp, Maybe Exp)
memberNames p f =
    ( str  $  memberName p Input (f ^. fieldId) (f ^. fieldRef)
    , str <$> item
    )
  where
    item = case unwrap (f ^. fieldRef . refAnn) of
        List l -> listItemName p Input l
        _      -> Nothing

internal :: TypeOf a => Timestamp -> a -> Type
internal ts (typeOf -> t) = case t of
    TType      x   -> tycon x
    TLit       x   -> literal True ts x
    TNatural       -> tycon "Nat"
    TStream        -> tycon "Stream"
    TMaybe     x   -> TyApp  (tycon "Maybe") (internal ts x)
    TSensitive x   -> TyApp  (tycon "Sensitive") (internal ts x)
    TList      x   -> TyList (internal ts x)
    TList1     x   -> TyApp  (tycon "NonEmpty") (internal ts x)
    TMap       k v -> TyApp  (TyApp (tycon "HashMap") (internal ts k)) (internal ts v)

external :: TypeOf a => Timestamp -> a -> Type
external ts (typeOf -> t) = case t of
    TType      x   -> tycon x
    TLit       x   -> literal False ts x
    TNatural       -> tycon "Natural"
    TStream        -> tycon "Stream"
    TMaybe     x   -> TyApp  (tycon "Maybe") (external ts x)
    TSensitive x   -> external ts x
    TList      x   -> TyList (external ts x)
    TList1     x   -> TyApp  (tycon "NonEmpty") (external ts x)
    TMap       k v -> TyApp  (TyApp (tycon "HashMap") (external ts k)) (external ts v)

mapping :: TType -> Exp -> Exp
mapping t e = Fold.foldl' (\y -> InfixApp y (qop ".")) e (go t)
  where
    go = \case
        TSensitive x -> var "_Sensitive" : go x
        TMaybe     x -> coerce (go x)
        x            -> maybeToList (iso x)

    coerce (x:xs) = app (var "mapping") x : xs
    coerce []     = []

iso :: TType -> Maybe Exp
iso = \case
    TLit Time    -> Just (var "_Time")
    TNatural     -> Just (var "_Nat")
    TSensitive _ -> Just (var "_Sensitive")
    _            -> Nothing

literal :: Bool -> Timestamp -> Lit -> Type
literal i ts = tycon . \case
    Int              -> "Int"
    Long             -> "Integer"
    Double           -> "Double"
    Text             -> "Text"
    Blob             -> "Base64"
    Bool             -> "Bool"
    Time | i         -> tsToText ts
         | otherwise -> "UTCTime"

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
