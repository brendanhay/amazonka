{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Gen.AST.Data.Syntax
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Data.Syntax where

import Control.Comonad
import Control.Error
import Control.Lens hiding (iso, mapping, op, strict)
import Data.Char qualified as Char
import Data.Foldable (foldl', foldr')
import Data.Foldable qualified as Fold
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty (..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Gen.AST.Data.Field
import Gen.AST.Data.Instance
import Gen.Protocol (Names (..))
import Gen.Protocol qualified as Proto
import Gen.Types
import Language.Haskell.Exts qualified as Exts

type Name = Exts.Name ()

type QName = Exts.QName ()

type Type = Exts.Type ()

type Decl = Exts.Decl ()

type Exp = Exts.Exp ()

type Pat = Exts.Pat ()

type QOp = Exts.QOp ()

type Rhs = Exts.Rhs ()

type GuardedRhs = Exts.GuardedRhs ()

type InstDecl = Exts.InstDecl ()

type QualConDecl = Exts.QualConDecl ()

type ConDecl = Exts.ConDecl ()

type FieldUpdate = Exts.FieldUpdate ()

pX, pXMay, pXDef :: QOp
pX = ".@"
pXMay = ".@?"
pXDef = ".!@"

pJ, pJMay, pJDef :: QOp
pJ = ".:"
pJMay = ".:?"
pJDef = ".!="

pJE, pJEMay, pJEDef :: QOp
pJE = ".:>"
pJEMay = ".?>"
pJEDef = pXDef

pH, pHMay :: QOp
pH = ".#"
pHMay = ".#?"

pXMap, pXList, pXList1, pHMap :: Exp
pXMap = var "parseXMLMap"
pXList = var "parseXMLList"
pXList1 = var "parseXMLList1"
pHMap = var "parseHeadersMap"

toX, toXAttr, toJ, toQ, toH :: QOp
toX = "@="
toXAttr = "@@="
toJ = ".="
toQ = "=:"
toH = "=#"

toQList, toXList :: Exp
toQList = var "toQueryList"
toXList = var "toXMLList"

toXMap, toQMap :: Exp
toXMap = var "toXMLMap"
toQMap = var "toQueryMap"

ctorS :: HasMetadata a Identity => a -> Id -> [Field] -> Decl
ctorS m n fs = Exts.TypeSig () [ident (smartCtorId n)] ty
  where
    ty = foldr' (Exts.TyFun ()) (tycon (typeId n)) ps

    ps = map (external m) (filter fieldIsParam fs)

ctorD :: Id -> [Field] -> Decl
ctorD n fs = Exts.sfun (ident (smartCtorId n)) ps (unguarded rhs) Exts.noBinds
  where
    ps = map fieldParamName (filter fieldIsParam fs)

    rhs
      | null fs = var (ctorId n)
      | otherwise = recconstr (unqual (ctorId n)) (map fieldUpdate fs)

fieldUpdate :: Field -> FieldUpdate
fieldUpdate f = field (unqual (fieldAccessor f)) rhs
  where
    rhs
      | fieldMaybe f = nothingE
      | fieldMonoid f = memptyE
      | Just v <- iso (typeOf f) = Exts.infixApp v "#" pat
      | otherwise = pat

    pat = Exts.Var () (Exts.UnQual () (fieldParamName f))

lensS :: HasMetadata a Identity => a -> TType -> Field -> Decl
lensS m t f =
  Exts.TypeSig () [ident (fieldLens f)] $
    tyapp
      ( tyapp
          (tycon "Lens'")
          (signature m t)
      )
      (external m f)

lensD :: Field -> Decl
lensD f = Exts.sfun (ident l) [] (unguarded rhs) Exts.noBinds
  where
    l = fieldLens f
    a = fieldAccessor f

    rhs =
      mapping (typeOf f) $
        Exts.app
          (Exts.app (var "lens") (var a))
          ( Exts.paren
              ( Exts.lamE
                  [pvar "s", pvar "a"]
                  (Exts.RecUpdate () (var "s") [field (unqual a) (var "a")])
              )
          )

errorS :: Text -> Decl
errorS n =
  let cxt = Exts.CxSingle () (Exts.ClassA () (unqual "AsError") [tyvar "a"])
      forall = Exts.TyForall () Nothing (Just cxt)
   in Exts.TypeSig () [ident n] . forall $
        tyapp
          ( tyapp
              ( tyapp
                  (tycon "Getting")
                  (tyapp (tycon "First") (tycon "ServiceError"))
              )
              (tyvar "a")
          )
          (tycon "ServiceError")

errorD :: HasMetadata a Identity => a -> Text -> Maybe Int -> Text -> Decl
errorD m n s c =
  Exts.sfun (ident n) [] (unguarded (maybe rhs status s)) Exts.noBinds
  where
    status i =
      Exts.infixApp rhs "." $
        var "hasStatus"
          `Exts.app` Exts.intE (fromIntegral i)

    rhs = Exts.appFun (var "_MatchServiceError") [var (m ^. serviceConfig), str c]

dataD :: Id -> [QualConDecl] -> [Derive] -> Decl
dataD n fs cs = Exts.DataDecl () arity Nothing head' fs [derives]
  where
    arity =
      case fs of
        [Exts.QualConDecl _ _ _ (Exts.RecDecl _ _ [_])] ->
          Exts.NewType ()
        _ -> Exts.DataType ()

    head' =
      Exts.DHead () (ident (typeId n))

    derives =
      Exts.Deriving () Nothing $
        map (rule . Text.pack) (mapMaybe derivingName cs)

    rule c =
      Exts.IRule () Nothing Nothing (Exts.IHCon () (unqual c))

recordD :: HasMetadata a Identity => a -> Id -> [Field] -> QualConDecl
recordD m n =
  conD . \case
    [] -> Exts.ConDecl () c []
    [x] -> Exts.RecDecl () c [fieldDecl (internal m) x]
    xs -> Exts.RecDecl () c (map (fieldDecl (strict . internal m)) xs)
  where
    fieldDecl h f = Exts.FieldDecl () [ident (fieldAccessor f)] (h f)

    c = ident (ctorId n)

conD :: ConDecl -> QualConDecl
conD = Exts.QualConDecl () Nothing Nothing

serviceS :: HasMetadata a Identity => a -> Decl
serviceS m = Exts.TypeSig () [ident (m ^. serviceConfig)] (tycon "Service")

serviceD :: HasMetadata a Identity => a -> Retry -> Decl
serviceD m r = Exts.patBindWhere (pvar n) rhs bs
  where
    bs = [try Exts.noBinds, chk Exts.noBinds]

    rhs =
      recconstr
        (unqual "Service")
        [ field (unqual "_svcAbbrev") (str abbrev),
          field (unqual "_svcSigner") (var sig),
          field (unqual "_svcPrefix") (m ^. endpointPrefix . to str),
          field (unqual "_svcVersion") (m ^. apiVersion . to str),
          field (unqual "_svcEndpoint") (Exts.app (var "defaultEndpoint") (var n)),
          field (unqual "_svcTimeout") (Exts.app justE (Exts.intE 70)),
          field (unqual "_svcCheck") (var "statusSuccess"),
          field (unqual "_svcError") (var (serviceError m) `Exts.app` str abbrev),
          field (unqual "_svcRetry") (var "retry")
        ]

    try =
      Exts.sfun (ident "retry") [] . unguarded $
        recconstr
          (r ^. delayType . to unqual)
          [ field (unqual "_retryBase") (r ^. delayBase . to frac),
            field (unqual "_retryGrowth") (r ^. delayGrowth . to Exts.intE),
            field (unqual "_retryAttempts") (r ^. retryAttempts . to Exts.intE),
            field (unqual "_retryCheck") (var "check")
          ]

    chk =
      Exts.sfun (ident "check") [ident "e"] . Exts.GuardedRhss () $
        mapMaybe policy (r ^.. retryPolicies . kvTraversal) ++ [otherE nothingE]
      where
        policy (k, v) = (`guardE` Exts.app justE (str k)) <$> policyE v

    n = m ^. serviceConfig
    abbrev = m ^. serviceAbbrev
    sig = m ^. signatureVersion . to sigToText

policyE :: Policy -> Maybe Exp
policyE = \case
  When (WhenStatus (Just c) s) ->
    Just $
      Exts.appFun
        (var "has")
        [ Exts.paren $
            Exts.infixApp
              (Exts.app (var "hasCode") (str c))
              "."
              (Exts.app (var "hasStatus") (Exts.intE s)),
          var "e"
        ]
  When (WhenStatus Nothing s) ->
    Just $
      Exts.appFun
        (var "has")
        [ Exts.paren $ Exts.app (var "hasStatus") (Exts.intE s),
          var "e"
        ]
  _ -> Nothing

pagerD :: Id -> Pager Field -> Decl
pagerD n p =
  instD
    "AWSPager"
    n
    [ Exts.InsDecl () $
        Exts.sfun (ident "page") [ident "rq", ident "rs"] (rhs p) Exts.noBinds
    ]
  where
    rhs = \case
      Only t ->
        Exts.GuardedRhss
          ()
          [ stop (notationE (_tokenOutput t)),
            other [t]
          ]
      Next ks t ->
        Exts.GuardedRhss () $
          stop (notationE (_tokenOutput t)) :
          map (stop . notationE) (Fold.toList ks)
            ++ [other [t]]
      Many k (t :| ts) ->
        Exts.GuardedRhss
          ()
          [ stop (notationE k),
            check t ts,
            other (t : ts)
          ]

    stop x = guardE (Exts.app (var "stop") (rs x)) nothingE

    other = otherE . foldl' f rq
      where
        f :: Exp -> Token Field -> Exp
        f e x =
          Exts.infixApp e "&"
            . Exts.infixApp (x ^. tokenInput . to notationE) ".~"
            $ rs (x ^. tokenOutput . to notationE)

    check t ts = guardE (foldl' f (g t) ts) nothingE
      where
        f x = Exts.infixApp x "&&" . g
        g y = Exts.app (var "isNothing") $ rs (y ^. tokenOutput . to notationE)

    rq = Exts.infixApp justE "$" (var "rq")
    rs x = Exts.infixApp (var "rs") (qop (getterN x)) x

getterN :: Exp -> Text
getterN e = if go e then "^?" else "^."
  where
    go = \case
      Exts.App _ x y -> go x || go y
      Exts.InfixApp _ x _ y -> go x || go y
      Exts.Var _ (Exts.UnQual _ (Exts.Ident _ "_last")) -> True
      Exts.Var _ (Exts.UnQual _ (Exts.Ident _ "_Just")) -> True
      _ -> False

-- FIXME: doesn't support Maybe fields currently.
notationE :: Notation Field -> Exp
notationE = \case
  NonEmptyText k -> Exts.app (var "nonEmptyText") (label False k)
  IsEmptyList (k :| ks) -> Exts.app (var "isEmptyList") (labels k ks)
  NonEmptyList (k :| ks) -> labels k ks
  Access (k :| ks) -> labels k ks
  Choice x y -> Exts.appFun (var "choice") [branch x, branch y]
  where
    branch x =
      let e = notationE x
       in Exts.paren (Exts.app (var (getterN e)) e)

    labels k [] = label False k
    labels k ks = foldl' f (label True k) ks
      where
        f e x = Exts.infixApp e "." (label True x)

    label b = \case
      Key f -> key b f
      Each f -> Exts.app (var "folding") . Exts.paren $ Exts.app (var "concatOf") (key False f)
      Last f -> Exts.infixApp (key False f) "." (var "_last")

    key False f = var (fieldLens f)
    key True f
      | fieldMonoid f = key False f
      | fieldMaybe f = Exts.infixApp (key False f) "." (var "_Just")
      | otherwise = key False f

requestD ::
  HasMetadata a Identity =>
  Config ->
  a ->
  HTTP ->
  (Ref, [Inst]) ->
  (Ref, [Field]) ->
  Decl
requestD c m h (a, as) (b, bs) =
  instD
    "AWSRequest"
    (identifier a)
    [ assocD (identifier a) "Rs" (typeId (identifier b)),
      funD "request" (requestF c m h a as),
      funD "response" (responseE (m ^. protocol) b bs)
    ]

responseE :: Protocol -> Ref -> [Field] -> Exp
responseE p r fs = Exts.app (responseF p r fs) bdy
  where
    n = r ^. to identifier
    s = r ^. refAnn . to extract

    bdy :: Exp
    bdy
      | null fs = var (ctorId n)
      | isShared s, all fieldBody fs = lam parseAll
      | otherwise = lam . ctorE n $ map parseField fs

    lam :: Exp -> Exp
    lam = Exts.lamE [Exts.pvar "s", Exts.pvar "h", Exts.pvar "x"]

    parseField :: Field -> Exp
    parseField x =
      case fieldLocation x of
        Just Headers -> parseHeadersE p x
        Just Header -> parseHeadersE p x
        Just StatusCode -> parseStatusE x
        Just Body | body -> Exts.app pureE (var "x")
        Nothing | body -> Exts.app pureE (var "x")
        _ -> parseProto x

    parseProto :: Field -> Exp
    parseProto f =
      case p of
        _ | f ^. fieldPayload -> parseOne f
        JSON -> parseJSONE p pJE pJEMay pJEDef f
        RestJSON -> parseJSONE p pJE pJEMay pJEDef f
        APIGateway -> parseJSONE p pJE pJEMay pJEDef f
        _ -> parseXMLE p f

    parseOne :: Field -> Exp
    parseOne f
      | fieldLit f =
        if fieldIsParam f
          then Exts.app (var "pure") (var "x")
          else Exts.app (var "pure") (Exts.paren (Exts.app (var "Just") (var "x")))
      -- This ensures anything which is set as a payload,
      -- but is a primitive type is just consumed as a bytestring.
      | otherwise = parseAll

    parseAll :: Exp
    parseAll =
      flip Exts.app (var "x") $
        if any fieldLitPayload fs
          then var "pure"
          else case p of
            JSON -> var "eitherParseJSON"
            RestJSON -> var "eitherParseJSON"
            APIGateway -> var "eitherParseJSON"
            _ -> var "parseXML"

    body = any fieldStream fs

instanceD :: Protocol -> Id -> Inst -> Decl
instanceD p n = \case
  FromXML fs -> fromXMLD p n fs
  FromJSON fs -> fromJSOND p n fs
  ToElement ns e -> toElementD p n ns e
  ToXML fs -> toXMLD p n fs
  ToJSON fs -> toJSOND p n fs
  ToHeaders es -> toHeadersD p n es
  ToPath es -> toPathD n es
  ToQuery es -> toQueryD p n es
  ToBody f -> toBodyD n f
  IsHashable -> hashableD n
  IsNFData -> nfDataD n

hashableD, nfDataD :: Id -> Decl
hashableD n = instD "Hashable" n []
nfDataD n = instD "NFData" n []

-- FIXME: merge D + E constructors where possible
fromXMLD :: Protocol -> Id -> [Field] -> Decl
fromXMLD p n = decodeD "FromXML" n "parseXML" (ctorE n) . map (parseXMLE p)

fromJSOND :: Protocol -> Id -> [Field] -> Decl
fromJSOND p n fs = instD1 "FromJSON" n with
  where
    with =
      funD "parseJSON" $
        Exts.app
          (Exts.app (var "withObject") (str (typeId n)))
          (Exts.lamE [Exts.pvar "x"] es)

    es = ctorE n $ map (parseJSONE p pJ pJMay pJDef) fs

toElementD :: Protocol -> Id -> Maybe Text -> Either Text Field -> Decl
toElementD p n ns = instD1 "ToElement" n . funD "toElement" . toElementE p ns

toXMLD :: Protocol -> Id -> [Field] -> Decl
toXMLD p n =
  instD1 "ToXML" n
    . wildcardD n "toXML" enc memptyE
    . map (Right . toXMLE p)
  where
    enc = mconcatE . map (either id id)

toJSOND :: Protocol -> Id -> [Field] -> Decl
toJSOND p n =
  instD1 "ToJSON" n
    . wildcardD n "toJSON" enc (Exts.paren $ Exts.app (var "Object") memptyE)
    . map (Right . toJSONE p)
  where
    enc =
      Exts.app (var "object")
        . Exts.app (var "catMaybes")
        . Exts.listE
        . map (either id id)

toHeadersD :: Protocol -> Id -> [Either (Text, Text) Field] -> Decl
toHeadersD p n = instD1 "ToHeaders" n . wildcardD n "toHeaders" enc memptyE
  where
    enc = mconcatE . map (toHeadersE p)

toQueryD :: Protocol -> Id -> [Either (Text, Maybe Text) Field] -> Decl
toQueryD p n = instD1 "ToQuery" n . wildcardD n "toQuery" enc memptyE
  where
    enc = mconcatE . map (toQueryE p)

toPathD :: Id -> [Either Text Field] -> Decl
toPathD n =
  instD1 "ToPath" n . \case
    [Left t] -> funD "toPath" . Exts.app (var "const") $ str t
    es -> wildcardD n "toPath" enc memptyE es
  where
    enc = mconcatE . map toPathE

toBodyD :: Id -> Field -> Decl
toBodyD n f = instD "ToBody" n [funD "toBody" (toBodyE f)]

wildcardD ::
  Id ->
  Text ->
  ([Either a b] -> Exp) ->
  Exp ->
  [Either a b] ->
  InstDecl
wildcardD n f enc xs = \case
  [] -> constD f xs
  es
    | not (any isRight es) -> funD f $ Exts.app (var "const") (enc es)
    | otherwise -> Exts.InsDecl () (Exts.FunBind () [match prec es])
  where
    match p es =
      Exts.Match () (ident f) [p] (unguarded (enc es)) Exts.noBinds

    prec = Exts.PRec () (unqual (ctorId n)) [Exts.PFieldWildcard ()]

instD1 :: Text -> Id -> InstDecl -> Decl
instD1 c n = instD c n . (: [])

instD :: Text -> Id -> [InstDecl] -> Decl
instD c n = Exts.InstDecl () Nothing rule . Just
  where
    rule =
      Exts.IRule () Nothing Nothing $
        Exts.IHApp () (Exts.IHCon () (unqual c)) (tycon (typeId n))

funD :: Text -> Exp -> InstDecl
funD f = Exts.InsDecl () . Exts.patBind (pvar f)

funArgsD :: Text -> [Text] -> Exp -> InstDecl
funArgsD f as e =
  Exts.InsDecl () $
    Exts.sfun (ident f) (map ident as) (unguarded e) Exts.noBinds

assocD :: Id -> Text -> Text -> InstDecl
assocD n x y = Exts.InsType () (tyapp (tycon x) (tycon (typeId n))) (tycon y)

decodeD :: Text -> Id -> Text -> ([a] -> Exp) -> [a] -> Decl
decodeD c n f dec =
  instD1 c n . \case
    [] -> funD f . Exts.app (var "const") $ dec []
    es -> funArgsD f ["x"] (dec es)

constD :: Text -> Exp -> InstDecl
constD f = funArgsD f [] . Exts.app (var "const")

parseXMLE :: Protocol -> Field -> Exp
parseXMLE p f = case outputNames p f of
  NMap mn e k v -> unflatE mn pXMap [str e, str k, str v]
  NList mn i
    | fieldMonoid f -> unflatE mn pXList [str i]
    | otherwise -> unflatE mn pXList1 [str i]
  NName n
    | req -> decodeE x pX n
    | otherwise -> decodeE x pXMay n
  where
    unflatE Nothing g xs
      | req = Exts.appFun g (xs ++ [x])
      | otherwise = Exts.app (may (Exts.appFun g xs)) x
    unflatE (Just n) g xs =
      Exts.infixApp (defaultMonoidE x n pXMay pXDef) ">>=" $
        if req
          then Exts.appFun g xs
          else may (Exts.appFun g xs)

    may = Exts.app (var "may")
    x = var "x"

    req = not (fieldMaybe f)

parseJSONE :: Protocol -> QOp -> QOp -> QOp -> Field -> Exp
parseJSONE p d dm dd f
  | fieldMonoid f = defaultMonoidE x n dm dd
  | fieldMaybe f = decodeE x dm n
  | otherwise = decodeE x d n
  where
    n = memberName p Output f
    x = var "x"

parseHeadersE :: Protocol -> Field -> Exp
parseHeadersE p f
  | TMap {} <- typeOf f = Exts.appFun pHMap [str n, h]
  | fieldMaybe f = decodeE h pHMay n
  | otherwise = decodeE h pH n
  where
    n = memberName p Output f
    h = var "h"

parseStatusE :: Field -> Exp
parseStatusE f
  | fieldMaybe f = Exts.app pureE (Exts.app justE v)
  | otherwise = Exts.app pureE v
  where
    v = Exts.paren $ Exts.app (var "fromEnum") (var "s")

toXMLE :: Protocol -> Field -> Exp
toXMLE p f = toGenericE p opX "toXML" toXMap toXList f
  where
    opX
      | f ^. fieldRef . refXMLAttribute = toXAttr
      | otherwise = toX

toElementE :: Protocol -> Maybe Text -> Either Text Field -> Exp
toElementE p ns = either (`root` []) node
  where
    root n = Exts.appFun (var "mkElement") . (str (qual n) :)

    node f = root n [var ".", var (fieldAccessor f)]
      where
        n = memberName p Input f

    qual n
      | Just x <- ns = "{" <> x <> "}" <> n
      | otherwise = n

toJSONE :: Protocol -> Field -> Exp
toJSONE p f
  | fieldMaybe f = Exts.infixApp (Exts.paren (Exts.app (str n) o)) "<$>" a
  | otherwise = Exts.app (var "Just") (encodeE n toJ a)
  where
    n = memberName p Input f
    a = var (fieldAccessor f)
    o = var (Text.pack (Exts.prettyPrint toJ))

toHeadersE :: Protocol -> Either (Text, Text) Field -> Exp
toHeadersE p = either pair field'
  where
    pair (k, v) = encodeE k toH $ impliesE v (var "ByteString")

    field' f = encodeE (memberName p Input f) toH $ var (fieldAccessor f)

toQueryE :: Protocol -> Either (Text, Maybe Text) Field -> Exp
toQueryE p = either pair field'
  where
    pair (k, Nothing) = str k
    pair (k, Just v) = encodeE k toQ $ impliesE v (var "ByteString")

    field' = toGenericE p toQ "toQuery" toQMap toQList

toPathE :: Either Text Field -> Exp
toPathE = either str (Exts.app (var "toBS") . var . fieldAccessor)

toBodyE :: Field -> Exp
toBodyE = Exts.infixApp (var "toBody") "." . var . fieldAccessor

toGenericE :: Protocol -> QOp -> Text -> Exp -> Exp -> Field -> Exp
toGenericE p toO toF toM toL f = case inputNames p f of
  NMap mn e k v
    | fieldMaybe f -> flatE mn toO . Exts.app (var toF) $ Exts.appFun toM [str e, str k, str v, var "<$>", a]
    | otherwise -> flatE mn toO $ Exts.appFun toM [str e, str k, str v, a]
  NList mn i
    | fieldMaybe f -> flatE mn toO . Exts.app (var toF) $ Exts.appFun toL [str i, var "<$>", a]
    | otherwise -> flatE mn toO $ Exts.appFun toL [str i, a]
  NName n -> encodeE n toO a
  where
    a = var (fieldAccessor f)

pureE :: Exp
pureE = var "pure"

nothingE :: Exp
nothingE = var "Nothing"

justE :: Exp
justE = var "Just"

otherE :: Exp -> GuardedRhs
otherE = guardE (var "otherwise")

guardE :: Exp -> Exp -> GuardedRhs
guardE x = Exts.GuardedRhs () [Exts.qualStmt x]

ctorE :: Id -> [Exp] -> Exp
ctorE n = seqE (var (ctorId n)) . map Exts.paren

memptyE :: Exp
memptyE = var "mempty"

mconcatE :: [Exp] -> Exp
mconcatE = Exts.app (var "mconcat") . Exts.listE

seqE :: Exp -> [Exp] -> Exp
seqE l [] = Exts.app pureE l
seqE l (r : rs) = Exts.infixApp l "<$>" (infixE r "<*>" rs)

infixE :: Exp -> QOp -> [Exp] -> Exp
infixE l _ [] = l
infixE l o (r : rs) = infixE (Exts.infixApp l o r) o rs

impliesE :: Text -> Exp -> Exp
impliesE x y = Exts.paren (Exts.infixApp (str x) "::" y)

flatE :: Maybe Text -> QOp -> Exp -> Exp
flatE (Just n) o = encodeE n o
flatE Nothing _ = id

defaultMonoidE :: Exp -> Text -> QOp -> QOp -> Exp
defaultMonoidE v n dm dd =
  Exts.infixApp (Exts.infixApp v dm (str n)) dd memptyE

encodeE :: Text -> QOp -> Exp -> Exp
encodeE n = Exts.infixApp (str n)

decodeE :: Exp -> QOp -> Text -> Exp
decodeE v o = Exts.infixApp v o . str

memberName :: Protocol -> Direction -> Field -> Text
memberName p d f = Proto.memberName p d (f ^. fieldId) (f ^. fieldRef)

inputNames, outputNames :: Protocol -> Field -> Names
inputNames p f = Proto.nestedNames p Input (f ^. fieldId) (f ^. fieldRef)
outputNames p f = Proto.nestedNames p Output (f ^. fieldId) (f ^. fieldRef)

requestF ::
  HasMetadata a Identity =>
  Config ->
  a ->
  HTTP ->
  Ref ->
  [Inst] ->
  Exp
requestF c meta h r is = maybe e (foldr' plugin e) ps
  where
    plugin x = Exts.infixApp (var x) "."

    ps = Map.lookup (identifier r) (c ^. operationPlugins)

    e = Exts.app v (var n)

    v =
      var
        . mappend (methodToText m)
        . fromMaybe mempty
        . listToMaybe
        $ mapMaybe f is

    f = \case
      ToBody {} -> Just "Body"
      ToJSON {} -> Just "JSON"
      ToElement {} -> Just "XML"
      _
        | p == Query,
          m == POST ->
          Just "Query"
      _
        | p == EC2,
          m == POST ->
          Just "Query"
      _ -> Nothing

    m = h ^. method
    p = meta ^. protocol
    n = meta ^. serviceConfig

-- FIXME: take method into account for responses, such as HEAD etc, particuarly
-- when the body might be totally empty.
responseF :: Protocol -> RefF a -> [Field] -> Exp
responseF p r fs
  | null fs = var "receiveNull"
  | any fieldStream fs = var "receiveBody"
  | any fieldLitPayload fs = var "receiveBytes"
  | Just x <- r ^. refResultWrapper = Exts.app (var (suf <> "Wrapper")) (str x)
  | all (not . fieldBody) fs = var "receiveEmpty"
  | otherwise = var suf
  where
    suf = "receive" <> Proto.suffix p

waiterS :: Id -> Waiter a -> Decl
waiterS n w = Exts.TypeSig () [ident c] $ tyapp (tycon "Wait") (tycon k)
  where
    k = w ^. waitOperation . to typeId
    c = smartCtorId n

waiterD :: Id -> Waiter Field -> Decl
waiterD n w = Exts.sfun (ident c) [] (unguarded rhs) Exts.noBinds
  where
    c = smartCtorId n

    rhs =
      recconstr
        (unqual "Wait")
        [ field (unqual "_waitName") (str (memberId n)),
          field (unqual "_waitAttempts") (w ^. waitAttempts . to Exts.intE),
          field (unqual "_waitDelay") (w ^. waitDelay . to Exts.intE),
          field (unqual "_waitAcceptors")
            . Exts.listE
            $ map match (w ^. waitAcceptors)
        ]

    match x =
      case (_acceptMatch x, _acceptArgument x) of
        (_, Just (NonEmptyList _)) ->
          Exts.appFun (var "matchNonEmpty") (expect x : criteria x : argument' x)
        (Path, _) ->
          Exts.appFun (var "matchAll") (expect x : criteria x : argument' x)
        (PathAll, _) ->
          Exts.appFun (var "matchAll") (expect x : criteria x : argument' x)
        (PathAny, _) ->
          Exts.appFun (var "matchAny") (expect x : criteria x : argument' x)
        (Status, _) ->
          Exts.appFun (var "matchStatus") (expect x : criteria x : argument' x)
        (Error, _) ->
          Exts.appFun (var "matchError") (expect x : criteria x : argument' x)

    expect x =
      case _acceptExpect x of
        Status' i -> Exts.intE i
        Boolean b -> con . Text.pack $ show b
        Textual t -> str t

    criteria x =
      case _acceptCriteria x of
        Retry -> var "AcceptRetry"
        Success -> var "AcceptSuccess"
        Failure -> var "AcceptFailure"

    argument' x = go <$> maybeToList (notationE <$> _acceptArgument x)
      where
        go = case _acceptExpect x of
          Textual {} ->
            \y -> Exts.infixApp y "." (Exts.app (var "to") (var "toTextCI"))
          _ -> id

signature :: HasMetadata a Identity => a -> TType -> Type
signature m = directed False m Nothing

internal, external :: HasMetadata a Identity => a -> Field -> Type
internal m f = directed True m (_fieldDirection f) f
external m f = directed False m (_fieldDirection f) f

-- FIXME: split again into internal/external
directed ::
  ( HasMetadata a Identity,
    TypeOf b
  ) =>
  Bool ->
  a ->
  Maybe Direction ->
  b ->
  Type
directed i m d (typeOf -> t) = case t of
  TType x _ -> tycon x
  TLit x -> literal i (m ^. timestampFormat . _Identity) x
  TNatural -> tycon nat
  TStream -> tycon stream
  TSensitive x -> sensitive (go x)
  TMaybe x -> may x
  TList x -> Exts.TyList () (go x)
  TList1 x -> list1 (go x)
  TMap k v -> hmap k v
  where
    go = directed i m d

    nat
      | i = "Nat"
      | otherwise = "Natural"

    sensitive
      | i = tyapp (tycon "Sensitive")
      | otherwise = id

    may x@(TMap {}) | not i = go x
    may x@(TList {}) | not i = go x
    may x = tycon "Maybe" `tyapp` go x

    list1
      | i = tyapp (tycon "List1")
      | otherwise = tyapp (tycon "NonEmpty")

    hmap k v
      | i = tycon "Map" `tyapp` go k `tyapp` go v
      | otherwise = tycon "HashMap" `tyapp` go k `tyapp` go v

    stream = case d of
      Nothing -> "RsBody"
      Just Output -> "RsBody" -- Response stream.
      Just Input
        | m ^. signatureVersion == S3 ->
          "RqBody" -- If the signer supports chunked encoding, both body types are accepted.
        | otherwise -> "HashedBody" -- Otherwise only a pre-hashed body is accepted.

mapping :: TType -> Exp -> Exp
mapping t e = infixE e "." (go t)
  where
    go = \case
      TSensitive x -> var "_Sensitive" : go x
      TMaybe x@(TMap {}) -> var "_Default" : go x
      TMaybe x@(TList {}) -> var "_Default" : go x
      TMaybe x -> nest (go x)
      x -> maybeToList (iso x)

    nest [] = []
    nest (x : xs) = [Exts.app (var "mapping") (infixE x "." xs)]

iso :: TType -> Maybe Exp
iso = \case
  TLit Time -> Just (var "_Time")
  TLit Base64 -> Just (var "_Base64")
  TNatural -> Just (var "_Nat")
  TSensitive x -> Just (infixE (var "_Sensitive") "." (maybeToList (iso x)))
  TList1 {} -> Just (var "_List1")
  TList {} -> Just (var "_Coerce")
  TMap {} -> Just (var "_Map")
  _ -> Nothing

literal :: Bool -> Timestamp -> Lit -> Type
literal i ts = \case
  Bool -> tycon "Bool"
  Int -> tycon "Int"
  Long -> tycon "Integer"
  Double -> tycon "Double"
  Text -> tycon "Text"
  Bytes -> tycon "ByteString"
  Base64
    | i -> tycon "Base64"
    | otherwise -> tycon "ByteString"
  Time
    | i -> tycon (tsToText ts)
    | otherwise -> tycon "UTCTime"
  Json -> tycon "ByteString"

strict :: Type -> Type
strict =
  Exts.TyBang () (Exts.BangedTy ()) (Exts.NoUnpackPragma ()) . \case
    t@Exts.TyApp {} -> Exts.TyParen () t
    t -> t

tyvar :: Text -> Type
tyvar = Exts.TyVar () . ident

tycon :: Text -> Type
tycon x =
  -- Fix/hack for replacedBy overrides having unquoted identifiers.
  (if Text.any Char.isSpace x then Exts.TyParen () else id) $
    Exts.TyCon () (unqual x)

con :: Text -> Exp
con = Exts.Con () . unqual

qop :: Text -> QOp
qop = fromString . Text.unpack

frac :: Rational -> Exp
frac n = Exts.Lit () (Exts.Frac () n (show n))

str :: Text -> Exp
str = Exts.strE . Text.unpack

pvar :: Text -> Pat
pvar = Exts.pvar . ident

var :: Text -> Exp
var = Exts.var . ident

param :: Int -> Name
param = Exts.name . mappend "p" . show

unqual :: Text -> QName
unqual = Exts.UnQual () . ident

ident :: Text -> Name
ident = Exts.name . Text.unpack

tyapp :: Type -> Type -> Type
tyapp a =
  Exts.TyApp () a . \case
    b@Exts.TyApp {} -> Exts.TyParen () b
    b -> b

field :: QName -> Exp -> FieldUpdate
field = Exts.FieldUpdate ()

unguarded :: Exp -> Rhs
unguarded = Exts.UnGuardedRhs ()

recconstr :: QName -> [FieldUpdate] -> Exp
recconstr = Exts.RecConstr ()

-- Orphans

instance IsString QOp where
  fromString = Exts.op . Exts.sym

instance IsString Name where
  fromString = ident . fromString
