{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
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
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Language.Haskell.Exts        as Exts
import           Language.Haskell.Exts.Build  hiding (pvar, var)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit, Var)

-- FIXME: remove .Internal Data namespace, and import explicitly
-- like Headers/Path/etc to types, whatnot? Or make an import scheme
-- based on protocol name?

ctorSig :: Timestamp -> Id -> [Field] -> Decl
ctorSig ts n = TypeSig noLoc [n ^. smartCtorId . to ident]
    . Fold.foldr' TyFun (n ^. typeId . to tycon)
    . map (external ts)
    . filter (^. fieldRequired)

ctorDecl :: Id -> [Field] -> Decl
ctorDecl n fs = sfun noLoc name ps (UnGuardedRhs rhs) noBinds
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
         | Just v <- iso (typeOf f) = infixApp v "#" p
         | otherwise                = p

    opt = not (f ^. fieldRequired)

    p = Exts.Var (UnQual (f ^. fieldParam))

lensSig :: Timestamp -> TType -> Field -> Decl
lensSig ts t f = TypeSig noLoc [ident (f ^. fieldLens)] $
    TyApp (TyApp (tycon "Lens'")
                 (external ts t)) (external ts (typeOf f))

lensDecl :: Field -> Decl
lensDecl f = sfun noLoc (ident l) [] (UnGuardedRhs rhs) noBinds
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
responseExps p =  const [] -- map go
  where
    -- go f = case f ^. fieldLocation of
    --     Just Headers     -> fromHeadersExp p f
    --     Just Header      -> fromHeadersExp p f
    --     Just StatusCode  -> app (var "pure") (var "s")
    --     Just Body        -> app (var "pure") (var "x")
    --     Nothing
    --         | f ^. fieldPayload
    --                      -> app (var "pure") (var "x")
    --     _                ->
    --         case p of
    --             JSON     -> fromJSONExp p f
    --             RestJSON -> fromJSONExp p f
    --             _        -> fromXMLExp  p f

instanceD :: Protocol -> Id -> Inst -> Decl
instanceD p n = \case
    FromXML   fs -> fromXMLD   p n fs
    FromJSON  fs -> fromJSOND  p n fs
    ToElement f  -> toElementD p n f
    ToXML     fs -> toXMLD     p n fs
    ToJSON    fs -> toJSOND   p n fs
    ToHeaders fs -> toHeadersD p n fs
    ToPath    es -> toPathD      n es
    ToQuery   es -> toQueryD   p n es
    ToBody    f  -> toBodyD    p n f

fromXMLD :: Protocol -> Id -> [Field] -> Decl
fromXMLD p n fs = instD "FromXML" n (funArgsD "parseXML" ["x"] exps)
  where
    exps = seqE (n ^. typeId . to var) (map (fromXMLE p) fs)

fromJSOND :: Protocol -> Id -> [Field] -> Decl
fromJSOND p n fs = instD "FromJSON" n (funArgsD "parseJSON" ["x"] exps)
  where
    exps = seqE (n ^. typeId . to var) (map (fromJSONE p) fs)

toElementD :: Protocol -> Id -> Field -> Decl
toElementD p n = instD "ToElement" n . funD "toElement" . toElementE p

toXMLD :: Protocol -> Id -> [Field] -> Decl
toXMLD p n fs = instD "ToXML" n (wildcardD n "toXML" exps)
  where
    exps  = listE (map (toXMLE p) fs)

toJSOND :: Protocol -> Id -> [Field] -> Decl
toJSOND p n fs = instD "ToJSON" n (wildcardD n "toJSON" exps)
  where
    exps  = listE (map (toJSONE p) fs)

toHeadersD :: Protocol -> Id -> [Field] -> Decl
toHeadersD p n fs = instD "ToHeaders" n decl
  where
    decl = wildcardD n "toHeaders" exps
    exps = listE (map (toHeadersE p) fs)

toQueryD :: Protocol -> Id -> [Either Text Field] -> Decl
toQueryD p n es = instD "ToQuery" n decl
  where
    decl = wildcardD n "toQuery" exps
    exps = listE (map (toQueryE p) es)

toPathD :: Id -> [Either Text Field] -> Decl
toPathD n es
    | [Left t] <- es = ins (single t)
    | otherwise      = ins multi
  where
    ins = instD "ToPath" n
    fun = "toPath"

    single = funD fun . app (var "const") . str
    multi  = wildcardD n fun . app (var "mconcat") . listE $ map toPathE es

toBodyD :: Protocol -> Id -> Field -> Decl
toBodyD p n = instD "ToBody" n . funD "toBody" . toBodyE

instD :: Text -> Id -> InstDecl -> Decl
instD c n d = InstDecl noLoc Nothing [] [] (unqual c) [n ^. typeId . to tycon] [d]

funD :: Text -> Exp -> InstDecl
funD f = InsDecl . patBind noLoc (pvar f)

funArgsD :: Text -> [Text] -> Exp -> InstDecl
funArgsD f as e =
    InsDecl (sfun noLoc (ident f) (map ident as) (UnGuardedRhs e) noBinds)

wildcardD :: Id -> Text -> Exp -> InstDecl
wildcardD n f e = InsDecl (FunBind [match])
  where
    match = Match noLoc (ident f) [pat] Nothing rhs noBinds
    pat   = PRec (n ^. typeId . to unqual) [PFieldWildcard]
    rhs   = UnGuardedRhs e

seqE :: Exp -> [Exp] -> Exp
seqE l []     = app (var "pure") l
seqE l (r:rs) = infixApp l "<$>" (infixE r "<*>" rs)

infixE :: Exp -> QOp -> [Exp] -> Exp
infixE l _ []     = l
infixE l o (r:rs) = infixE (infixApp l o r) o rs

fromXMLE, fromJSONE, fromHeadersE :: Protocol -> Field -> Exp
fromXMLE     = decodeE (Dec ".@" ".@?" ".!@")
fromJSONE    = decodeE (Dec ".:" ".:?" ".!=")
fromHeadersE = decodeE (Dec ".#" ".#?" ".!#")

toXMLE, toJSONE, toHeadersE :: Protocol -> Field -> Exp
toXMLE     = encodeE (Enc "@=" "@@=")
toJSONE    = encodeE (Enc ".=" ".=")
toHeadersE = encodeE (Enc "=#" "=##")

toQueryE :: Protocol -> Either Text Field -> Exp
toQueryE p = either str (encodeE (Enc "=:" "=:") p)

toElementE :: Protocol -> Field -> Exp
toElementE p f = appFun (var "mkElement")
    [ str name
    , var "."
    , var "toXML"
    , var "."
    , var (f ^. fieldAccessor)
    ]
  where
    name | Just ns <- f ^. fieldNamespace = "{" <> ns <> "}" <> n
         | otherwise                      = n

    n = memberName p Input (f ^. fieldId) (f ^. fieldRef)

toPathE :: Either Text Field -> Exp
toPathE = either str (app (var "toText") . var . view fieldAccessor)

toBodyE :: Field -> Exp
toBodyE f = var (f ^. fieldAccessor)

data Dec = Dec
    { decodeOp      :: QOp
    , decodeMaybeOp :: QOp
    , decodeDefOp   :: QOp
    }

decodeE :: Dec -> Protocol -> Field -> Exp
decodeE o p f
   | Just i <- m        = paren (infixApp monoid ">>=" (parse i))
   | f ^. fieldMonoid   = app (parse n) v
   | f ^. fieldRequired = infixApp v (decodeOp      o) n
   | otherwise          = infixApp v (decodeMaybeOp o) n
  where
    monoid = infixApp v (decodeMaybeOp o) $
        infixApp n (decodeDefOp o) (var "mempty")

    parse i
        | fieldList1 f = app (var "parseList1") i
        | fieldList  f = app (var "parseList")  i
        | fieldMap   f = app (var "parseMap")   i

    (n, m) = memberNames p f

    v = var "x"

data Enc = Enc
    { encodeOp     :: QOp
    , encodeListOp :: QOp
    }

encodeE :: Enc -> Protocol -> Field -> Exp
encodeE o p f
    | Just i <- m  = infixApp n (encodeOp     o) (infixApp i (encodeListOp o) v)
    | fieldList1 f = infixApp n (encodeListOp o) v
    | fieldList  f = infixApp n (encodeListOp o) v
--    | fieldMap   f = infixApp n (encodeOp c) v
    | otherwise    = infixApp n (encodeOp     o) v
  where
    (n, m) = memberNames p f

    v = var (f ^. fieldAccessor)

memberNames :: Protocol -> Field -> (Exp, Maybe Exp)
memberNames p f =
    ( str  $  memberName p Input (f ^. fieldId) (f ^. fieldRef)
    , str <$> item
    )
  where
    item = case unwrap (f ^. fieldRef . refAnn) of
        List l -> listItemName p Input l
        _      -> Nothing

responseFun :: Protocol -> [Field] -> Text
responseFun p fs = "response" <> f
  where
    f | any (view fieldStream) fs = "Body"
      | otherwise                 =
          case p of
              JSON     -> "JSON"
              RestJSON -> "JSON"
              XML      -> "XML"
              RestXML  -> "XML"
              Query    -> "XML"
              EC2      -> "XML"

requestFun :: HTTP Identity -> [Field] -> Text
requestFun h fs
    | any (view fieldStream) fs = "stream"
    | otherwise                 = methodToText (h ^. method)

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
mapping t e = infixE e "." (go t)
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

-- qop :: Text -> QOp
-- qop = Exts.op . Exts.sym . Text.unpack

param :: Int -> Name
param = Ident . mappend "p" . show

unqual :: Text -> QName
unqual = UnQual . ident

ident :: Text -> Name
ident = Ident . Text.unpack
