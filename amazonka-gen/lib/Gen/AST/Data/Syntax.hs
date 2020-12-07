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
pX = "Prelude..@"
pXMay = "Prelude..@?"
pXDef = "Prelude..!@"

pJ, pJMay, pJDef :: QOp
pJ = "Prelude..:"
pJMay = "Prelude..:?"
pJDef = "Prelude..!="

pJE, pJEMay, pJEDef :: QOp
pJE = "Prelude..:>"
pJEMay = "Prelude..?>"
pJEDef = pXDef

pH, pHMay :: QOp
pH = "Prelude..#"
pHMay = "Prelude..#?"

pXMap, pXList, pXList1, pHMap :: Exp
pXMap = var "Prelude.parseXMLMap"
pXList = var "Prelude.parseXMLList"
pXList1 = var "Prelude.parseXMLNonEmpty"
pHMap = var "Prelude.parseHeadersMap"

toX, toXAttr, toJ, toQ, toH :: QOp
toX = "Prelude.@="
toXAttr = "Prelude.@@="
toJ = "Prelude..="
toQ = "Prelude.=:"
toH = "Prelude.=#"

toQList, toXList :: Exp
toQList = var "Prelude.toQueryList"
toXList = var "Prelude.toXMLList"

toXMap, toQMap :: Exp
toXMap = var "Prelude.toXMLMap"
toQMap = var "Prelude.toQueryMap"

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
      | Just v <- iso (typeOf f) = Exts.infixApp v "Lens.#" pat
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

lensD :: Field -> Decl
lensD f =
  Exts.sfun (ident (fieldLens f)) [] (unguarded rhs) Exts.noBinds
  where
    accessor = fieldAccessor f

    rhs =
      mapping (typeOf f) $
        Exts.app (Exts.app (var "Lens.lens") get) set

    get =
      var accessor

    set =
      Exts.lamE [pvar "s", pvar "a"] $
        Exts.RecUpdate () (var "s") [field (unqual accessor) (var "a")]

errorS :: Text -> Decl
errorS n =
  Exts.TypeSig () [ident n] . forall $
    tyapp
      ( tyapp
          ( tyapp
              (tycon "Lens.Getting")
              (tyapp (tycon "Prelude.First") (tycon "Prelude.ServiceError"))
          )
          (tyvar "a")
      )
      (tycon "Prelude.ServiceError")
  where
    cxt = Exts.CxSingle () (Exts.TypeA () (tycon "Prelude.AsError" `tyapp` tyvar "a"))

    forall = Exts.TyForall () Nothing (Just cxt)

errorD :: HasMetadata a Identity => a -> Text -> Maybe Int -> Text -> Decl
errorD m n s c =
  Exts.sfun (ident n) [] (unguarded (maybe rhs status s)) Exts.noBinds
  where
    status i =
      Exts.infixApp rhs "Prelude.." $
        var "Prelude.hasStatus"
          `Exts.app` Exts.intE (fromIntegral i)

    rhs =
      Exts.appFun
        (var "Prelude._MatchServiceError")
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
      Exts.IRule () Nothing Nothing (Exts.IHCon () (unqual c))

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
  Exts.TypeSig () [ident (m ^. serviceConfig)] (tycon "Prelude.Service")

serviceD :: HasMetadata a Identity => a -> Retry -> Decl
serviceD m r = Exts.patBindWhere (pvar name) record binds
  where
    record =
      recconstr
        (unqual "Prelude.Service")
        [ field
            (unqual "Prelude._svcAbbrev")
            (str abbrev),
          field
            (unqual "Prelude._svcSigner")
            (var signer),
          field
            (unqual "Prelude._svcPrefix")
            (m ^. endpointPrefix . Lens.to str),
          field
            (unqual "Prelude._svcVersion")
            (m ^. apiVersion . Lens.to str),
          field
            (unqual "Prelude._svcEndpoint")
            (Exts.app (var "Prelude.defaultEndpoint") (var name)),
          field
            (unqual "Prelude._svcTimeout")
            (Exts.app justE (Exts.intE 70)),
          field
            (unqual "Prelude._svcCheck")
            (var "Prelude.statusSuccess"),
          field
            (unqual "Prelude._svcError")
            (var (serviceError m) `Exts.app` str abbrev),
          field
            (unqual "Prelude._svcRetry")
            (var "retry")
        ]

    binds = [retry Exts.noBinds, check Exts.noBinds]

    retry =
      Exts.sfun (ident "retry") [] . unguarded $
        recconstr
          (r ^. delayType . Lens.to (unqual . mappend "Prelude."))
          [ field (unqual "Prelude._retryBase") (r ^. delayBase . Lens.to frac),
            field (unqual "Prelude._retryGrowth") (r ^. delayGrowth . Lens.to Exts.intE),
            field (unqual "Prelude._retryAttempts") (r ^. retryAttempts . Lens.to Exts.intE),
            field (unqual "Prelude._retryCheck") (var "check")
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
              (Exts.app (var "Prelude.hasCode") (str c))
              "Prelude.."
              (Exts.app (var "Prelude.hasStatus") (Exts.intE s)),
          var "e"
        ]
  When (WhenStatus Nothing s) ->
    Just $
      Exts.appFun
        (var "Lens.has")
        [ Exts.paren $ Exts.app (var "Prelude.hasStatus") (Exts.intE s),
          var "e"
        ]
  _ -> Nothing

pagerD :: Id -> Pager Field -> Decl
pagerD n p =
  instD
    "Prelude.AWSPager"
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

    stop x = guardE (Exts.app (var "stop") (rs x)) nothingE

    other = otherE . Foldable.foldl' f rq
      where
        f :: Exp -> Token Field -> Exp
        f e x =
          Exts.infixApp e "Prelude.&"
            . Exts.infixApp (x ^. tokenInput . Lens.to (notationE False)) "Prelude..~"
            $ rs (x ^. tokenOutput . Lens.to (notationE False))

    check t ts = guardE (Foldable.foldl' f (g t) ts) nothingE
      where
        f x = Exts.infixApp x "Prelude.&&" . g
        g y = Exts.app (var "Prelude.isNothing") $ rs (y ^. tokenOutput . Lens.to (notationE False))

    rq = Exts.infixApp justE "Prelude.$" (var "rq")
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
  Choice x y -> Exts.appFun (var "Prelude.choice") [branch x, branch y]
  where
    branch x =
      let e = notationE force x
       in Exts.paren (Exts.app (var (getterN e)) e)

    labels k [] = label force k
    labels k ks = Foldable.foldl' f (label True k) ks
      where
        f e x = Exts.infixApp e "Prelude.." (label True x)

    label b = \case
      Key f -> key b f
      Last f -> Exts.infixApp (key False f) "Prelude.." (var "Lens._last")
      Each f ->
        Exts.app (var "Lens.folding")
          . Exts.paren
          . Exts.app (var "Lens.concatOf")
          $ Exts.infixApp (key b f) "Prelude.." (Exts.app (var "Lens.to") (var "Prelude.toList"))

    key False f = var (fieldLens f)
    key True f
      | fieldMonoid f = key False f
      | fieldMaybe f = Exts.infixApp (key False f) "Prelude.." (var "Lens._Just")
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
    "Prelude.AWSRequest"
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
          then Exts.app (var "Prelude.pure") (var "x")
          else Exts.app (var "Prelude.pure") (Exts.paren (Exts.app (var "Prelude.Just") (var "x")))
      -- This ensures anything which is set as a payload,
      -- but is a primitive type is just consumed as a bytestring.
      | otherwise = parseAll

    parseAll :: Exp
    parseAll =
      flip Exts.app (var "x") $
        if any fieldLitPayload fs
          then var "Prelude.pure"
          else case p of
            JSON -> var "Prelude.eitherParseJSON"
            RestJSON -> var "Prelude.eitherParseJSON"
            APIGateway -> var "Prelude.eitherParseJSON"
            _ -> var "Prelude.parseXML"

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
fromXMLD p n = decodeD "Prelude.FromXML" n "parseXML" (ctorE n) . map (parseXMLE p)

fromJSOND :: Protocol -> Id -> [Field] -> Decl
fromJSOND p n fs = instD1 "Prelude.FromJSON" n with
  where
    with =
      funD "parseJSON" $
        Exts.app
          (Exts.app (var "Prelude.withObject") (str (typeId n)))
          (Exts.lamE [Exts.pvar "x"] es)

    es = ctorE n $ map (parseJSONE p pJ pJMay pJDef) fs

toElementD :: Protocol -> Id -> Maybe Text -> Either Text Field -> Decl
toElementD p n ns =
  instD1 "Prelude.ToElement" n . funD "toElement" . toElementE p ns

toXMLD :: Protocol -> Id -> [Field] -> Decl
toXMLD p n =
  instD1 "Prelude.ToXML" n
    . wildcardD n "toXML" enc memptyE
    . map (Right . toXMLE p)
  where
    enc = mconcatE . map (either id id)

toJSOND :: Protocol -> Id -> [Field] -> Decl
toJSOND p n =
  instD1 "Prelude.ToJSON" n
    . wildcardD n "toJSON" enc (Exts.paren $ Exts.app (var "Prelude.Object") memptyE)
    . map (Right . toJSONE p)
  where
    enc =
      Exts.app (var "Prelude.object")
        . Exts.app (var "Prelude.catMaybes")
        . Exts.listE
        . map (either id id)

toHeadersD :: Protocol -> Id -> [Either (Text, Text) Field] -> Decl
toHeadersD p n =
  instD1 "Prelude.ToHeaders" n . wildcardD n "toHeaders" enc memptyE
  where
    enc = mconcatE . map (toHeadersE p)

toQueryD :: Protocol -> Id -> [Either (Text, Maybe Text) Field] -> Decl
toQueryD p n = instD1 "Prelude.ToQuery" n . wildcardD n "toQuery" enc memptyE
  where
    enc = mconcatE . map (toQueryE p)

toPathD :: Id -> [Either Text Field] -> Decl
toPathD n =
  instD1 "Prelude.ToPath" n . \case
    [Left t] -> funD "toPath" . Exts.app (var "Prelude.const") $ str t
    es -> wildcardD n "toPath" enc memptyE es
  where
    enc = mconcatE . map toPathE

toBodyD :: Id -> Field -> Decl
toBodyD n f = instD "Prelude.ToBody" n [funD "toBody" (toBodyE f)]

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
    | not (any isRight es) -> funD f $ Exts.app (var "Prelude.const") (enc es)
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
    [] -> funD f . Exts.app (var "Prelude.const") $ dec []
    es -> funArgsD f ["x"] (dec es)

constD :: Text -> Exp -> InstDecl
constD f = funArgsD f [] . Exts.app (var "Prelude.const")

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
      Exts.infixApp (defaultMonoidE x n pXMay pXDef) "Prelude.>>=" $
        if req
          then Exts.appFun g xs
          else may (Exts.appFun g xs)

    may = Exts.app (var "Prelude.may")
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
    v = Exts.paren $ Exts.app (var "Prelude.fromEnum") (var "s")

toXMLE :: Protocol -> Field -> Exp
toXMLE p f = toGenericE p opX "Prelude.toXML" toXMap toXList f
  where
    opX
      | f ^. fieldRef . refXMLAttribute = toXAttr
      | otherwise = toX

toElementE :: Protocol -> Maybe Text -> Either Text Field -> Exp
toElementE p ns = either (`root` []) node
  where
    root n = Exts.appFun (var "Prelude.mkElement") . (str (qual n) :)

    node f = root n [var "Prelude..", var (fieldAccessor f)]
      where
        n = memberName p Input f

    qual n
      | Just x <- ns = "{" <> x <> "}" <> n
      | otherwise = n

toJSONE :: Protocol -> Field -> Exp
toJSONE p f
  | fieldMaybe f = Exts.infixApp (Exts.paren (Exts.app (str n) o)) "Prelude.<$>" a
  | otherwise = Exts.app (var "Prelude.Just") (encodeE n toJ a)
  where
    n = memberName p Input f
    a = var (fieldAccessor f)
    o = var (Text.pack (Exts.prettyPrint toJ))

toHeadersE :: Protocol -> Either (Text, Text) Field -> Exp
toHeadersE p = either pair field'
  where
    pair (k, v) = encodeE k toH $ impliesE v (var "Prelude.ByteString")

    field' f = encodeE (memberName p Input f) toH $ var (fieldAccessor f)

toQueryE :: Protocol -> Either (Text, Maybe Text) Field -> Exp
toQueryE p = either pair field'
  where
    pair (k, Nothing) = str k
    pair (k, Just v) = encodeE k toQ $ impliesE v (var "Prelude.ByteString")

    field' = toGenericE p toQ "Prelude.toQuery" toQMap toQList

toPathE :: Either Text Field -> Exp
toPathE = either str (Exts.app (var "Prelude.toBS") . var . fieldAccessor)

toBodyE :: Field -> Exp
toBodyE = Exts.infixApp (var "Prelude.toBody") "Prelude.." . var . fieldAccessor

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
pureE = var "Prelude.pure"

nothingE :: Exp
nothingE = var "Prelude.Nothing"

justE :: Exp
justE = var "Prelude.Just"

otherE :: Exp -> GuardedRhs
otherE = guardE (var "Prelude.otherwise")

guardE :: Exp -> Exp -> GuardedRhs
guardE x = Exts.GuardedRhs () [Exts.qualStmt x]

ctorE :: Id -> [Exp] -> Exp
ctorE n = seqE (var (ctorId n)) . map Exts.paren

memptyE :: Exp
memptyE = var "Prelude.mempty"

mconcatE :: [Exp] -> Exp
mconcatE = Exts.app (var "Prelude.mconcat") . Exts.listE

seqE :: Exp -> [Exp] -> Exp
seqE l [] = Exts.app pureE l
seqE l (r : rs) = Exts.infixApp l "Prelude.<$>" (infixE r "Prelude.<*>" rs)

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
requestF c meta h r is = maybe e (Foldable.foldr' plugin e) ps
  where
    plugin x = Exts.infixApp (var x) "Prelude.."

    ps = HashMap.lookup (identifier r) (c ^. operationPlugins)

    e = Exts.app v (var n)

    v =
      var
        . mappend (methodToText m)
        . fromMaybe mempty
        . listToMaybe
        $ mapMaybe f is

    f = \case
      ToBody {} -> Just "Prelude.Body"
      ToJSON {} -> Just "Prelude.JSON"
      ToElement {} -> Just "Prelude.XML"
      _
        | p == Query,
          m == POST ->
          Just "Prelude.Query"
      _
        | p == EC2,
          m == POST ->
          Just "Prelude.Query"
      _ -> Nothing

    m = h ^. method
    p = meta ^. protocol
    n = meta ^. serviceConfig

-- FIXME: take method into account for responses, such as HEAD etc, particuarly
-- when the body might be totally empty.
responseF :: Protocol -> RefF a -> [Field] -> Exp
responseF p r fs
  | null fs = var "Prelude.receiveNull"
  | any fieldStream fs = var "Prelude.receiveBody"
  | any fieldLitPayload fs = var "Prelude.receiveBytes"
  | Just x <- r ^. refResultWrapper = Exts.app (var (suf <> "Prelude.Wrapper")) (str x)
  | all (not . fieldBody) fs = var "Prelude.receiveEmpty"
  | otherwise = var suf
  where
    suf = "Prelude.receive" <> Protocol.suffix p

waiterS :: Id -> Waiter a -> Decl
waiterS n w = Exts.TypeSig () [ident c] $ tyapp (tycon "Prelude.Wait") (tycon k)
  where
    k = w ^. waitOperation . Lens.to typeId
    c = smartCtorId n

waiterD :: Id -> Waiter Field -> Decl
waiterD n w = Exts.sfun (ident c) [] (unguarded rhs) Exts.noBinds
  where
    c = smartCtorId n

    rhs =
      recconstr
        (unqual "Prelude.Wait")
        [ field (unqual "Prelude._waitName") (str (memberId n)),
          field (unqual "Prelude._waitAttempts") (w ^. waitAttempts . Lens.to Exts.intE),
          field (unqual "Prelude._waitDelay") (w ^. waitDelay . Lens.to Exts.intE),
          field (unqual "Prelude._waitAcceptors")
            . Exts.listE
            $ map match (w ^. waitAcceptors)
        ]

    match x =
      case (_acceptMatch x, _acceptArgument x) of
        (_, Just (Infix lens _)) ->
          Exts.appFun (var lens) (expect x : criteria x : argument' x)
        (Path, _) ->
          Exts.appFun (var "Prelude.matchAll") (expect x : criteria x : argument' x)
        (PathAll, _) ->
          Exts.appFun (var "Prelude.matchAll") (expect x : criteria x : argument' x)
        (PathAny, _) ->
          Exts.appFun (var "Prelude.matchAny") (expect x : criteria x : argument' x)
        (Status, _) ->
          Exts.appFun (var "Prelude.matchStatus") (expect x : criteria x : argument' x)
        (Error, _) ->
          Exts.appFun (var "Prelude.matchError") (expect x : criteria x : argument' x)

    expect x =
      case _acceptExpect x of
        Status' i -> Exts.intE i
        Boolean b -> con . Text.pack $ show b
        Textual t -> str t

    criteria x =
      case _acceptCriteria x of
        Retry -> var "Prelude.AcceptRetry"
        Success -> var "Prelude.AcceptSuccess"
        Failure -> var "Prelude.AcceptFailure"

    argument' x = go <$> maybeToList (notationE True <$> _acceptArgument x)
      where
        go y =
          case _acceptExpect x of
            Textual {} ->
              Exts.infixApp y "Prelude.." (Exts.app (var "Prelude.to") (var "Prelude.toText"))
            _ -> y

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

    nat = "Prelude.Natural"

    sensitive
      | i = tyapp (tycon "Prelude.Sensitive")
      | otherwise = id

    may = tyapp (tycon "Prelude.Maybe") . go

    list1 = tyapp (tycon "Prelude.NonEmpty")

    hmap k v =
      tyapp (tyapp (tycon "Prelude.HashMap") (go k)) (Exts.TyParen () (go v))

    stream = case d of
      Nothing -> "Prelude.RsBody"
      Just Output -> "Prelude.RsBody" -- Response stream.
      Just Input
        | m ^. signatureVersion == S3 ->
          "Prelude.RqBody" -- If the signer supports chunked encoding, both body types are accepted.
        | otherwise -> "Prelude.HashedBody" -- Otherwise only a pre-hashed body is accepted.

mapping :: TType -> Exp -> Exp
mapping t e = infixE e "Prelude.." (go t)
  where
    go = \case
      TSensitive x -> var "Prelude._Sensitive" : go x
      TMaybe x -> nest (go x)
      x -> maybeToList (iso x)

    nest [] = []
    nest (x : xs) = [Exts.app (var "Lens.mapping") (infixE x "Prelude.." xs)]

iso :: TType -> Maybe Exp
iso = \case
  TLit Time -> Just (var "Prelude._Time")
  TLit Base64 -> Just (var "Prelude._Base64")
  TSensitive x -> Just (infixE (var "Prelude._Sensitive") "Prelude.." (maybeToList (iso x)))
  _ -> Nothing

literal :: Bool -> Timestamp -> Lit -> Type
literal i ts = \case
  Bool -> tycon "Prelude.Bool"
  Int -> tycon "Prelude.Int"
  Long -> tycon "Prelude.Integer"
  Double -> tycon "Prelude.Double"
  Text -> tycon "Prelude.Text"
  Bytes -> tycon "Prelude.ByteString"
  Base64
    | i -> tycon "Prelude.Base64"
    | otherwise -> tycon "Prelude.ByteString"
  Time
    | i -> tycon ("Prelude." <> tsToText ts)
    | otherwise -> tycon "Prelude.UTCTime"
  Json -> tycon "Prelude.ByteString"

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
