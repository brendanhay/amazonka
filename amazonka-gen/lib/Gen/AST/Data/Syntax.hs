{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Gen.AST.Data.Syntax
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.AST.Data.Syntax where

import qualified Control.Comonad as Comonad
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import Gen.AST.Data.Field
import Gen.AST.Data.Instance
import Gen.Prelude
import Gen.Protocol (Names (..))
import qualified Gen.Protocol as Protocol
import Gen.Types
import qualified Language.Haskell.Exts as Exts

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
pX = "Lude..@"
pXMay = "Lude..@?"
pXDef = "Lude..!@"

pJ, pJMay, pJDef :: QOp
pJ = "Lude..:"
pJMay = "Lude..:?"
pJDef = "Lude..!="

pJE, pJEMay, pJEDef :: QOp
pJE = "Lude..:>"
pJEMay = "Lude..?>"
pJEDef = pXDef

pH, pHMay :: QOp
pH = "Lude..#"
pHMay = "Lude..#?"

pXMap, pXList, pXList1, pHMap :: Exp
pXMap = var "Lude.parseXMLMap"
pXList = var "Lude.parseXMLList"
pXList1 = var "Lude.parseXMLNonEmpty"
pHMap = var "Lude.parseHeadersMap"

toX, toXAttr, toJ, toQ, toH :: QOp
toX = "Lude.@="
toXAttr = "Lude.@@="
toJ = "Lude..="
toQ = "Lude.=:"
toH = "Lude.=#"

toQList, toXList :: Exp
toQList = var "Lude.toQueryList"
toXList = var "Lude.toXMLList"

toXMap, toQMap :: Exp
toXMap = var "Lude.toXMLMap"
toQMap = var "Lude.toQueryMap"

ctorS :: HasMetadata a Identity => a -> Id -> [Field] -> Decl
ctorS m n fs = Exts.TypeSig () [ident (smartCtorId n)] ty
  where
    ty = Foldable.foldr' (Exts.TyFun ()) (tycon (typeId n)) ps

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
      -- Just v <- iso (typeOf f) = Exts.infixApp v "Lens.#" pat
      | otherwise = pat

    pat = Exts.Var () (Exts.UnQual () (fieldParamName f))

lensS :: HasMetadata a Identity => a -> TType -> Field -> Decl
lensS m t f =
  Exts.TypeSig () [ident (fieldLens f)] $
    tyapp
      ( tyapp
          (tycon "Lens.Lens'")
          (signature m t)
      )
      (external m f)

lensD :: HasMetadata a Identity => a -> TType -> Field -> Decl
lensD m t f =
  Exts.sfun (ident (fieldLens f)) [] (unguarded rhs) Exts.noBinds
  where
    accessor = fieldAccessor f

    rhs =
      -- mapping (typeOf f) $
      Exts.app (Exts.app (var "Lens.lens") get) set

    get =
      Exts.ExpTypeSig () (var accessor) (Exts.TyFun () sig (internal m f))

    set =
      Exts.ExpTypeSig () lam sig

    lam =
      Exts.lamE [pvar "s", pvar "a"] $
        Exts.RecUpdate () (var "s") [field (unqual accessor) (var "a")]

    sig = signature m t

errorS :: Text -> Decl
errorS n =
  Exts.TypeSig () [ident n] . forall $
    tyapp
      ( tyapp
          ( tyapp
              (tycon "Lens.Getting")
              (tyapp (tycon "Lude.First") (tycon "Lude.ServiceError"))
          )
          (tyvar "a")
      )
      (tycon "Lude.ServiceError")
  where
    cxt = Exts.CxSingle () (Exts.TypeA () (tycon "Lude.AsError" `tyapp` tyvar "a"))

    forall = Exts.TyForall () Nothing (Just cxt)

errorD :: HasMetadata a Identity => a -> Text -> Maybe Int -> Text -> Decl
errorD m n s c =
  Exts.sfun (ident n) [] (unguarded (maybe rhs status s)) Exts.noBinds
  where
    status i =
      Exts.infixApp rhs "Lude.." $
        var "Lude.hasStatus"
          `Exts.app` Exts.intE (fromIntegral i)

    rhs =
      Exts.appFun
        (var "Lude._MatchServiceError")
        [ var (m ^. serviceConfig),
          str c
        ]

dataD :: Id -> [QualConDecl] -> [Derive] -> Decl
dataD n fs cs =
  Exts.DataDecl () arity Nothing head' fs (stocks ++ newtypes)
  where
    arity
      | isNewtype = Exts.NewType ()
      | otherwise = Exts.DataType ()

    isNewtype =
      case fs of
        [Exts.QualConDecl _ _ _ (Exts.RecDecl _ _ [_])] -> True
        [Exts.QualConDecl _ _ _ (Exts.ConDecl _ _ [_])] -> True
        _ -> False

    head' =
      Exts.DHead () (ident (typeId n))

    (stocks, newtypes) =
      bimap
        (derive Exts.DerivStock)
        (derive (if isNewtype then Exts.DerivNewtype else Exts.DerivAnyclass))
        (partitionEithers (map derivingStrategy cs))

    derive strategy = \case
      [] -> []
      xs -> [Exts.Deriving () (Just (strategy ())) (map rule xs)]

    rule c =
      Exts.IRule () Nothing Nothing $
        Exts.IHCon () (unqual (mappend "Lude." c))

recordD :: HasMetadata a Identity => a -> Id -> [Field] -> QualConDecl
recordD m n =
  conD . \case
    [] -> Exts.ConDecl () c []
    [x] -> Exts.RecDecl () c [fieldDecl (internal m) x]
    xs -> Exts.RecDecl () c (map (fieldDecl (internal m)) xs)
  where
    fieldDecl h f = Exts.FieldDecl () [ident (fieldAccessor f)] (h f)

    c = ident (ctorId n)

conD :: ConDecl -> QualConDecl
conD = Exts.QualConDecl () Nothing Nothing

serviceS :: HasMetadata a Identity => a -> Decl
serviceS m =
  Exts.TypeSig () [ident (m ^. serviceConfig)] (tycon "Lude.Service")

serviceD :: HasMetadata a Identity => a -> Retry -> Decl
serviceD m r = Exts.patBindWhere (pvar name) record binds
  where
    record =
      recconstr
        (unqual "Lude.Service")
        [ field
            (unqual "Lude._svcAbbrev")
            (str abbrev),
          field
            (unqual "Lude._svcSigner")
            (var signer),
          field
            (unqual "Lude._svcPrefix")
            (m ^. endpointPrefix . Lens.to str),
          field
            (unqual "Lude._svcVersion")
            (m ^. apiVersion . Lens.to str),
          field
            (unqual "Lude._svcEndpoint")
            (Exts.app (var "Lude.defaultEndpoint") (var name)),
          field
            (unqual "Lude._svcTimeout")
            (Exts.app justE (Exts.intE 70)),
          field
            (unqual "Lude._svcCheck")
            (var "Lude.statusSuccess"),
          field
            (unqual "Lude._svcError")
            (var ("Lude." <> serviceError m) `Exts.app` str abbrev),
          field
            (unqual "Lude._svcRetry")
            (var "retry")
        ]

    binds = [retry Exts.noBinds, check Exts.noBinds]

    retry =
      Exts.sfun (ident "retry") [] . unguarded $
        recconstr
          (r ^. delayType . Lens.to (unqual . mappend "Lude."))
          [ field (unqual "Lude._retryBase") (r ^. delayBase . Lens.to frac),
            field (unqual "Lude._retryGrowth") (r ^. delayGrowth . Lens.to Exts.intE),
            field (unqual "Lude._retryAttempts") (r ^. retryAttempts . Lens.to Exts.intE),
            field (unqual "Lude._retryCheck") (var "check")
          ]

    check =
      Exts.sfun (ident "check") [ident "e"] . Exts.GuardedRhss () $
        mapMaybe policy (r ^.. retryPolicies . kvTraversal) ++ [otherE nothingE]
      where
        policy (k, v) = (`guardE` Exts.app justE (str k)) <$> policyE v

    name = m ^. serviceConfig

    abbrev = m ^. serviceAbbrev

    signer = m ^. signatureVersion . Lens.to (mappend "Sign." . sigToText)

policyE :: Policy -> Maybe Exp
policyE = \case
  When (WhenStatus (Just c) s) ->
    Just $
      Exts.appFun
        (var "Lens.has")
        [ Exts.paren $
            Exts.infixApp
              (Exts.app (var "Lude.hasCode") (str c))
              "Lude.."
              (Exts.app (var "Lude.hasStatus") (Exts.intE s)),
          var "e"
        ]
  When (WhenStatus Nothing s) ->
    Just $
      Exts.appFun
        (var "Lens.has")
        [ Exts.paren $ Exts.app (var "Lude.hasStatus") (Exts.intE s),
          var "e"
        ]
  _ -> Nothing

pagerD :: Id -> Pager Field -> Decl
pagerD n p =
  instD
    "Page.AWSPager"
    n
    [ Exts.InsDecl () $
        Exts.sfun (ident "page") [ident "rq", ident "rs"] (rhs p) Exts.noBinds
    ]
  where
    rhs = \case
      Only t ->
        Exts.GuardedRhss
          ()
          [ stop (notationE False (_tokenOutput t)),
            other [t]
          ]
      Next ks t ->
        Exts.GuardedRhss () $
          stop (notationE False (_tokenOutput t)) :
          map (stop . notationE False) (Foldable.toList ks)
            ++ [other [t]]
      Many k (t :| ts) ->
        Exts.GuardedRhss
          ()
          [ stop (notationE False k),
            check t ts,
            other (t : ts)
          ]

    stop x = guardE (Exts.app (var "Page.stop") (rs x)) nothingE

    other = otherE . Foldable.foldl' f rq
      where
        f :: Exp -> Token Field -> Exp
        f e x =
          Exts.infixApp e "Lude.&"
            . Exts.infixApp (x ^. tokenInput . Lens.to (notationE False)) "Lens..~"
            $ rs (x ^. tokenOutput . Lens.to (notationE False))

    check t ts = guardE (Foldable.foldl' f (g t) ts) nothingE
      where
        f x = Exts.infixApp x "Lude.&&" . g
        g y = Exts.app (var "Lude.isNothing") $ rs (y ^. tokenOutput . Lens.to (notationE False))

    rq = Exts.infixApp justE "Lude.$" (var "rq")
    rs x = Exts.infixApp (var "rs") (qop (getterN x)) x

getterN :: Exp -> Text
getterN e = if go e then "Lens.^?" else "Lens.^."
  where
    go = \case
      Exts.App _ x y -> go x || go y
      Exts.InfixApp _ x _ y -> go x || go y
      Exts.Var _ (Exts.UnQual _ (Exts.Ident _ "Lens._last")) -> True
      Exts.Var _ (Exts.UnQual _ (Exts.Ident _ "Lens._Just")) -> True
      _ -> False

-- FIXME: doesn't support Maybe fields currently.
notationE :: Bool -> Notation Field -> Exp
notationE force = \case
  Deref (k :| ks) -> labels k ks
  Infix _lens x -> notationE force x
  Choice x y -> Exts.appFun (var "Lude.choice") [branch x, branch y]
  where
    branch x =
      let e = notationE force x
       in Exts.paren (Exts.app (var (getterN e)) e)

    labels k [] = label force k
    labels k ks = Foldable.foldl' f (label True k) ks
      where
        f e x = Exts.infixApp e "Lude.." (label True x)

    label b = \case
      Key f -> key b f
      Last f -> Exts.infixApp (key False f) "Lude.." (var "Lens._last")
      Each f ->
        Exts.app (var "Lens.folding")
          . Exts.paren
          . Exts.app (var "Lens.concatOf")
          $ Exts.infixApp (key b f) "Lude.." (Exts.app (var "Lens.to") (var "Lude.toList"))

    key False f = var (fieldLens f)
    key True f
      | fieldMaybe f = Exts.infixApp (key False f) "Lude.." (var "Lens._Just")
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
    "Lude.AWSRequest"
    (identifier a)
    [ assocD (identifier a) "Rs" (typeId (identifier b)),
      funD "request" (requestF c m h a as),
      funD "response" (responseE (m ^. protocol) b bs)
    ]

responseE :: Protocol -> Ref -> [Field] -> Exp
responseE p r fs = Exts.app (responseF p r fs) bdy
  where
    n = r ^. Lens.to identifier
    s = r ^. refAnn . Lens.to Comonad.extract

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
          then Exts.app (var "Lude.pure") (var "x")
          else Exts.app (var "Lude.pure") (Exts.paren (Exts.app (var "Lude.Just") (var "x")))
      -- This ensures anything which is set as a payload,
      -- but is a primitive type is just consumed as a bytestring.
      | otherwise = parseAll

    parseAll :: Exp
    parseAll =
      flip Exts.app (var "x") $
        if any fieldLitPayload fs
          then var "Lude.pure"
          else case p of
            JSON -> var "Lude.eitherParseJSON"
            RestJSON -> var "Lude.eitherParseJSON"
            APIGateway -> var "Lude.eitherParseJSON"
            _ -> var "Lude.parseXML"

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

-- FIXME: merge D + E constructors where possible
fromXMLD :: Protocol -> Id -> [Field] -> Decl
fromXMLD p n = decodeD "Lude.FromXML" n "parseXML" (ctorE n) . map (parseXMLE p)

fromJSOND :: Protocol -> Id -> [Field] -> Decl
fromJSOND p n fs = instD1 "Lude.FromJSON" n with
  where
    with =
      funD "parseJSON" $
        Exts.app
          (Exts.app (var "Lude.withObject") (str (typeId n)))
          (Exts.lamE [Exts.pvar "x"] es)

    es = ctorE n $ map (parseJSONE p pJ pJMay pJDef) fs

toElementD :: Protocol -> Id -> Maybe Text -> Either Text Field -> Decl
toElementD p n ns =
  instD1 "Lude.ToElement" n . funD "toElement" . toElementE p ns

toXMLD :: Protocol -> Id -> [Field] -> Decl
toXMLD p n =
  instD1 "Lude.ToXML" n
    . wildcardD n "toXML" enc memptyE
    . map (Right . toXMLE p)
  where
    enc = mconcatE . map (either id id)

toJSOND :: Protocol -> Id -> [Field] -> Decl
toJSOND p n =
  instD1 "Lude.ToJSON" n
    . wildcardD n "toJSON" enc (Exts.paren $ Exts.app (var "Lude.Object") memptyE)
    . map (Right . toJSONE p)
  where
    enc =
      Exts.app (var "Lude.object")
        . Exts.app (var "Lude.catMaybes")
        . Exts.listE
        . map (either id id)

toHeadersD :: Protocol -> Id -> [Either (Text, Text) Field] -> Decl
toHeadersD p n =
  instD1 "Lude.ToHeaders" n . wildcardD n "toHeaders" enc memptyE
  where
    enc = mconcatE . map (toHeadersE p)

toQueryD :: Protocol -> Id -> [Either (Text, Maybe Text) Field] -> Decl
toQueryD p n = instD1 "Lude.ToQuery" n . wildcardD n "toQuery" enc memptyE
  where
    enc = mconcatE . map (toQueryE p)

toPathD :: Id -> [Either Text Field] -> Decl
toPathD n =
  instD1 "Lude.ToPath" n . \case
    [Left t] -> funD "toPath" . Exts.app (var "Lude.const") $ str t
    es -> wildcardD n "toPath" enc memptyE es
  where
    enc = mconcatE . map toPathE

toBodyD :: Id -> Field -> Decl
toBodyD n f = instD "Lude.ToBody" n [funD "toBody" (toBodyE f)]

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
    | not (any isRight es) -> funD f $ Exts.app (var "Lude.const") (enc es)
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
    [] -> funD f . Exts.app (var "Lude.const") $ dec []
    es -> funArgsD f ["x"] (dec es)

constD :: Text -> Exp -> InstDecl
constD f = funArgsD f [] . Exts.app (var "Lude.const")

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
    fmapDecodeE = "953gg"

    unflatE Nothing g xs
      | req = Exts.appFun g (xs ++ [x])
      | otherwise = Exts.app (may (Exts.appFun g xs)) x
    unflatE (Just n) g xs =
      Exts.infixApp (defaultMonoidE x n pXMay pXDef) "Lude.>>=" $
        if req
          then Exts.appFun g xs
          else may (Exts.appFun g xs)

    may = Exts.app (var "Lude.may")
    req = not (fieldMaybe f)

    x = var "x"

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
    v = Exts.paren $ Exts.app (var "Lude.fromEnum") (var "s")

toXMLE :: Protocol -> Field -> Exp
toXMLE p f = toGenericE p opX "Lude.toXML" toXMap toXList f
  where
    opX
      | f ^. fieldRef . refXMLAttribute = toXAttr
      | otherwise = toX

toElementE :: Protocol -> Maybe Text -> Either Text Field -> Exp
toElementE p ns = either (`root` []) node
  where
    root n = Exts.appFun (var "Lude.mkElement") . (str (qual n) :)

    node f = root n [var "Lude..", var (fieldAccessor f)]
      where
        n = memberName p Input f

    qual n
      | Just x <- ns = "{" <> x <> "}" <> n
      | otherwise = n

toJSONE :: Protocol -> Field -> Exp
toJSONE p f
  | fieldMaybe f = Exts.infixApp (Exts.paren (Exts.app (str n) o)) "Lude.<$>" a
  | otherwise = Exts.app (var "Lude.Just") (encodeE n toJ a)
  where
    n = memberName p Input f
    a = var (fieldAccessor f)
    o = var (Text.pack (Exts.prettyPrint toJ))

toHeadersE :: Protocol -> Either (Text, Text) Field -> Exp
toHeadersE p = either pair field'
  where
    pair (k, v) = encodeE k toH $ impliesE v (var "Lude.ByteString")

    field' f = encodeE (memberName p Input f) toH $ var (fieldAccessor f)

toQueryE :: Protocol -> Either (Text, Maybe Text) Field -> Exp
toQueryE p = either pair field'
  where
    pair (k, Nothing) = str k
    pair (k, Just v) = encodeE k toQ $ impliesE v (var "Lude.ByteString")

    field' = toGenericE p toQ "Lude.toQuery" toQMap toQList

toPathE :: Either Text Field -> Exp
toPathE = either str (Exts.app (var "Lude.toBS") . var . fieldAccessor)

toBodyE :: Field -> Exp
toBodyE = Exts.infixApp (var "Lude.toBody") "Lude.." . var . fieldAccessor

toGenericE :: Protocol -> QOp -> Text -> Exp -> Exp -> Field -> Exp
toGenericE p toO toF toM toL f =
  case inputNames p f of
    NMap mn e k v
      | fieldMaybe f ->
        flatE mn toO . Exts.app (var toF) $
          Exts.appFun toM [str e, str k, str v, var "Lude.<$>", a]
      | otherwise ->
        flatE mn toO $
          Exts.appFun toM [str e, str k, str v, a]
    NList mn i
      | fieldMaybe f ->
        flatE mn toO . Exts.app (var toF) $
          Exts.appFun toL [str i, var "Lude.<$>", a]
      | otherwise ->
        flatE mn toO $
          Exts.appFun toL [str i, a]
    NName n ->
      encodeE n toO a
  where
    a = var (fieldAccessor f)

pureE :: Exp
pureE = var "Lude.pure"

nothingE :: Exp
nothingE = var "Lude.Nothing"

justE :: Exp
justE = var "Lude.Just"

otherE :: Exp -> GuardedRhs
otherE = guardE (var "Lude.otherwise")

guardE :: Exp -> Exp -> GuardedRhs
guardE x = Exts.GuardedRhs () [Exts.qualStmt x]

ctorE :: Id -> [Exp] -> Exp
ctorE n = seqE (var (ctorId n)) . map Exts.paren

memptyE :: Exp
memptyE = var "Lude.mempty"

mconcatE :: [Exp] -> Exp
mconcatE = Exts.app (var "Lude.mconcat") . Exts.listE

seqE :: Exp -> [Exp] -> Exp
seqE l [] = Exts.app pureE l
seqE l (r : rs) = Exts.infixApp l "Lude.<$>" (infixE r "Lude.<*>" rs)

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
memberName p d f = Protocol.memberName p d (f ^. fieldId) (f ^. fieldRef)

inputNames, outputNames :: Protocol -> Field -> Names
inputNames p f = Protocol.nestedNames p Input (f ^. fieldId) (f ^. fieldRef)
outputNames p f = Protocol.nestedNames p Output (f ^. fieldId) (f ^. fieldRef)

requestF ::
  HasMetadata a Identity =>
  Config ->
  a ->
  HTTP ->
  Ref ->
  [Inst] ->
  Exp
requestF cfg meta h ref instances =
  maybe expr (Foldable.foldr' plugin expr) plugins
  where
    plugin x = Exts.infixApp (var x) "Lude.."
    plugins = HashMap.lookup (identifier ref) (cfg ^. operationPlugins)

    expr = Exts.app verb (var name)

    verb =
      var
        . mappend "Req."
        . mappend (methodToText method')
        . fromMaybe mempty
        . listToMaybe
        $ mapMaybe contentType instances

    contentType = \case
      ToBody {} -> Just "Body"
      ToJSON {} -> Just "JSON"
      ToElement {} -> Just "XML"
      _
        | protocol' == Query,
          method' == POST ->
          Just "Query"
      _
        | protocol' == EC2,
          method' == POST ->
          Just "Query"
      _ -> Nothing

    method' = h ^. method
    protocol' = meta ^. protocol
    name = meta ^. serviceConfig

-- FIXME: take method into account for responses, such as HEAD etc, particuarly
-- when the body might be totally empty.
responseF :: Protocol -> RefF a -> [Field] -> Exp
responseF p r fs
  | null fs = var "Res.receiveNull"
  | any fieldStream fs = var "Res.receiveBody"
  | any fieldLitPayload fs = var "Res.receiveBytes"
  | Just x <- r ^. refResultWrapper = Exts.app (var (suf <> "Wrapper")) (str x)
  | all (not . fieldBody) fs = var "Res.receiveEmpty"
  | otherwise = var suf
  where
    suf = "Res.receive" <> Protocol.suffix p

waiterS :: Id -> Waiter a -> Decl
waiterS n w = Exts.TypeSig () [ident c] $ tyapp (tycon "Wait.Wait") (tycon k)
  where
    k = w ^. waitOperation . Lens.to typeId
    c = smartCtorId n

waiterD :: Id -> Waiter Field -> Decl
waiterD n w = Exts.sfun (ident c) [] (unguarded rhs) Exts.noBinds
  where
    c = smartCtorId n

    rhs =
      recconstr
        (unqual "Wait.Wait")
        [ field (unqual "Wait._waitName") (str (memberId n)),
          field (unqual "Wait._waitAttempts") (w ^. waitAttempts . Lens.to Exts.intE),
          field (unqual "Wait._waitDelay") (w ^. waitDelay . Lens.to Exts.intE),
          field (unqual "Wait._waitAcceptors")
            . Exts.listE
            $ map match (w ^. waitAcceptors)
        ]

    match x =
      case (_acceptMatch x, _acceptArgument x) of
        (_, Just (Infix lens _)) ->
          Exts.appFun (var ("Wait." <> lens)) (expect x : criteria x : argument' x)
        (Path, _) ->
          Exts.appFun (var "Wait.matchAll") (expect x : criteria x : argument' x)
        (PathAll, _) ->
          Exts.appFun (var "Wait.matchAll") (expect x : criteria x : argument' x)
        (PathAny, _) ->
          Exts.appFun (var "Wait.matchAny") (expect x : criteria x : argument' x)
        (Status, _) ->
          Exts.appFun (var "Wait.matchStatus") (expect x : criteria x : argument' x)
        (Error, _) ->
          Exts.appFun (var "Wait.matchError") (expect x : criteria x : argument' x)

    expect x =
      case _acceptExpect x of
        Status' i -> Exts.intE i
        Boolean b -> con $ mappend "Lude." $ Text.pack $ show b
        Textual t -> str t

    criteria x =
      case _acceptCriteria x of
        Retry -> var "Wait.AcceptRetry"
        Success -> var "Wait.AcceptSuccess"
        Failure -> var "Wait.AcceptFailure"

    argument' x = go <$> maybeToList (notationE True <$> _acceptArgument x)
      where
        go y =
          case _acceptExpect x of
            Textual {} ->
              Exts.infixApp y "Lude.." (Exts.app (var "Lens.to") (var "Lude.toText"))
            _ -> y

signature :: HasMetadata a Identity => a -> TType -> Type
signature m = directed m Nothing

internal, external :: HasMetadata a Identity => a -> Field -> Type
internal m f = directed m (_fieldDirection f) f
external m f = directed m (_fieldDirection f) f

directed ::
  ( HasMetadata a Identity,
    TypeOf b
  ) =>
  a ->
  Maybe Direction ->
  b ->
  Type
directed m d (typeOf -> t) =
  case t of
    TType x _ -> tycon x
    TLit x -> literal (m ^. timestampFormat . _Identity) x
    TNatural -> tycon "Lude.Natural"
    TStream -> tycon stream
    TSensitive x -> sensitive (go x)
    TMaybe x -> may x
    TList x -> Exts.TyList () (go x)
    TList1 x -> list1 (go x)
    TMap k v -> hmap k v
  where
    go = directed m d

    sensitive = tyapp (tycon "Lude.Sensitive")

    may = tyapp (tycon "Lude.Maybe") . go

    list1 = tyapp (tycon "Lude.NonEmpty")

    hmap k v =
      tyapp (tyapp (tycon "Lude.HashMap") (go k)) (Exts.TyParen () (go v))

    stream =
      case d of
        Nothing -> "Lude.RsBody"
        Just Output -> "Lude.RsBody" -- Response stream.
        Just Input
          -- If the signer supports chunked encoding, both body types are accepted.
          | m ^. signatureVersion == S3 -> "Lude.RqBody"
          -- Otherwise only a pre-hashed body is accepted.
          | otherwise -> "Lude.HashedBody"

literal :: Timestamp -> Lit -> Type
literal ts = \case
  Bool -> tycon "Lude.Bool"
  Int -> tycon "Lude.Int"
  Long -> tycon "Lude.Integer"
  Double -> tycon "Lude.Double"
  Text -> tycon "Lude.Text"
  Bytes -> tycon "Lude.ByteString"
  Base64 -> tycon "Lude.Base64"
  Time -> tycon ("Lude." <> tsToText ts)
  Json -> tycon "Lude.ByteString"

tyvar :: Text -> Type
tyvar = Exts.TyVar () . ident

tycon :: Text -> Type
tycon = Exts.TyCon () . unqual

tyapp :: Type -> Type -> Type
tyapp a b = Exts.TyApp () a (typaren b)

typaren :: Type -> Type
typaren = \case
  t@Exts.TyApp {} -> Exts.TyParen () t
  t -> t

con :: Text -> Exp
con = Exts.Con () . unqual

qop :: Text -> QOp
qop = fromString . Text.unpack

frac :: Rational -> Exp
frac n = Exts.Lit () (Exts.Frac () n (show n))

str :: Text -> Exp
str = Exts.strE . Text.unpack

int :: Integer -> Exp
int = Exts.intE

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
