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
import           Compiler.Text
import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Error
import           Control.Lens                 hiding (iso, mapping, op)
import qualified Data.Foldable                as Fold
import           Data.Function                ((&))
import qualified Data.HashMap.Strict          as Map
import           Data.List                    (nub)
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

requestD :: HasMetadata a f
         => a
         -> HTTP Identity
         -> (Id, [Inst])
         -> (Id, [Field])
         -> Decl
requestD m h (a, as) (b, bs) = instD "AWSRequest" a
    [ assocTyD a "Sv" (m ^. serviceAbbrev)
    , assocTyD a "Rs" (b ^. typeId)
    , funD "request"  (requestF h as)
    , funD "response" (responseE (m ^. protocol) h b bs)
    ]

responseE :: Protocol -> HTTP Identity -> Id -> [Field] -> Exp
responseE p h n fs = app (responseF p h fs) lam
  where
    lam :: Exp
    lam = lamE noLoc [pvar "s", pvar "h", pvar "x"] (dataE n $ map go fs)

    go :: Field -> Exp
    go x = case l of
        Just Headers     -> parseHeadersE p x
        Just Header      -> parseHeadersE p x
        Just StatusCode  -> app (var "pure") (var "s")
        Just Body        -> app (var "pure") (var "x")
        Nothing      | b -> app (var "pure") (var "x")
        _                -> proto x
     where
        l = x ^. fieldLocation
        b = x ^. fieldPayload

    proto :: Field -> Exp
    proto = case p of
        JSON     -> parseJSONE p
        RestJSON -> parseJSONE p
        _        -> parseXMLE  p

instanceD :: Protocol -> Id -> Inst -> Decl
instanceD p n = \case
    FromXML   fs -> fromXMLD   p n fs
    FromJSON  fs -> fromJSOND  p n fs
    ToElement f  -> toElementD p n f
    ToXML     fs -> toXMLD     p n fs
    ToJSON    fs -> toJSOND    p n fs
    ToHeaders es -> toHeadersD p n es
    ToPath    es -> toPathD      n es
    ToQuery   es -> toQueryD   p n es
    ToBody    f  -> toBodyD      n f

fromXMLD, fromJSOND :: Protocol -> Id -> [Field] -> Decl
fromXMLD  p n = decodeD "FromXML"  n "parseXML"  (dataE n) . map (parseXMLE  p)
fromJSOND p n = decodeD "FromJSON" n "parseJSON" (dataE n) . map (parseJSONE p)

toElementD :: Protocol -> Id -> Field -> Decl
toElementD p n = instD1 "ToElement" n . funD "toElement" . toElementE p

toXMLD, toJSOND :: Protocol -> Id -> [Field] -> Decl
toXMLD     p n = encodeD "ToXML" n "toXML" (app (var "mconcat") . listE) . map (toXMLE p)
toJSOND    p n = encodeD "ToJSON"    n "toJSON"    listE . map (toJSONE    p)

toHeadersD :: Protocol -> Id -> [Either (Text, Text) Field] -> Decl
toHeadersD p n = instD1 "ToHeaders" n  . \case
    [] -> constMemptyD "toHeaders"
    es -> wildcardD  n "toHeaders" . listE $ map (toHeadersE p) es

toQueryD :: Protocol -> Id -> [Either (Text, Maybe Text) Field] -> Decl
toQueryD p n = instD1 "ToQuery" n  . \case
    [] -> constMemptyD "toQuery"
    es -> wildcardD  n "toQuery" . listE $ map (toQueryE p) es

toPathD :: Id -> [Either Text Field] -> Decl
toPathD n = instD1 "ToPath" n . \case
    [Left t] -> funD "toQuery" . app (var "const") $ str t
    es       -> wildcardD n "toQuery" . app (var "mconcat") . listE $ map toPathE es

toBodyD :: Id -> Field -> Decl
toBodyD n f = instD "ToBody" n [funD "toBody" (toBodyE f)]

instD1 :: Text -> Id -> InstDecl -> Decl
instD1 c n = instD c n . (:[])

instD :: Text -> Id -> [InstDecl] -> Decl
instD c n = InstDecl noLoc Nothing [] [] (unqual c) [n ^. typeId . to tycon]

funD :: Text -> Exp -> InstDecl
funD f = InsDecl . patBind noLoc (pvar f)

funArgsD :: Text -> [Text] -> Exp -> InstDecl
funArgsD f as e = InsDecl $
    sfun noLoc (ident f) (map ident as) (UnGuardedRhs e) noBinds

-- wildcardD :: Id -> Text -> ([Exp] -> Exp) -> [Exp] -> InstDecl
-- wildcardD n f rhs [] = funArgsD f [n ^. typeId] (rhs [])
wildcardD n f e = InsDecl (FunBind [match])
  where
    match = Match noLoc (ident f) [pat] Nothing (UnGuardedRhs e) noBinds
    pat   = PRec (n ^. typeId . to unqual) [PFieldWildcard]

assocTyD :: Id -> Text -> Text -> InstDecl
assocTyD n x y = InsType noLoc (TyApp (tycon x) (n ^. typeId . to tycon)) (tycon y)

decodeD c n f dec = instD1 c n . \case
    [] -> funD f . app (var "const") $ dec []
    es -> funArgsD f ["x"] (dec es)

-- listEncodeD :: Text -> Id -> Text -> (Field -> Exp) -> Decl
encodeD c n f enc = instD c n . (:[]) . \case
    [] -> constMemptyD f
    es -> wildcardD n f (enc es)

constMemptyD :: Text -> InstDecl
constMemptyD f = funArgsD f [] $ app (var "const") (var "mempty")

dataE :: Id -> [Exp] -> Exp
dataE n = seqE (n ^. typeId . to var)

seqE :: Exp -> [Exp] -> Exp
seqE l []     = app (var "pure") l
seqE l (r:rs) = infixApp l "<$>" (infixE r "<*>" rs)

infixE :: Exp -> QOp -> [Exp] -> Exp
infixE l _ []     = l
infixE l o (r:rs) = infixE (infixApp l o r) o rs

parseXMLE, parseJSONE, parseHeadersE :: Protocol -> Field -> Exp
parseXMLE     = decodeE (Dec ".@" ".@?" ".!@")
parseJSONE    = decodeE (Dec ".:" ".:?" ".!=")
parseHeadersE = decodeE (Dec ".#" ".#?" ".!#")

toXMLE, toJSONE :: Protocol -> Field -> Exp
toXMLE  = encodeE (Enc "@=" "@@=")
toJSONE = encodeE (Enc ".=" ".=")

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

toHeadersE :: Protocol -> Either (Text, Text) Field -> Exp
toHeadersE p = either pair (encodeE (Enc "=#" "=##") p)
  where
    pair (k, v) = infixApp (str k) "=#" (str v)

toQueryE :: Protocol -> Either (Text, Maybe Text) Field -> Exp
toQueryE p = either pair (encodeE (Enc "=:" "=:") p)
  where
    pair (k, Nothing) = str k
    pair (k, Just v)  = infixApp (str k) "=:" (str v)

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

requestF :: HTTP Identity -> [Inst] -> Exp
requestF h is = var v
  where
    v = mappend (methodToText (h ^. method))
      . fromMaybe mempty
      . listToMaybe
      $ mapMaybe f is

    f = \case
        ToBody    {} -> Just "Body"
        ToJSON    {} -> Just "JSON"
        ToElement {} -> Just "XML"
        _            -> Nothing

-- FIXME: take method into account for responses, such as HEAD etc, particuarly
-- when the body might be totally empty.
responseF :: Protocol -> HTTP Identity -> [Field] -> Exp
responseF p h fs = var ("receive" <> f)
  where
    f | any (view fieldStream) fs = "Body"
      | otherwise                 =
          case p of
              JSON     -> "JSON"
              RestJSON -> "JSON"
              RestXML  -> "XML"
              Query    -> "XML"
              EC2      -> "XML"

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
