{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.AST.Data.Syntax where

import qualified Control.Comonad as Comonad
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Foldable as Fold
import qualified Data.Map.Strict as Map
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Gen.AST.Data.Field
import Gen.AST.Data.Instance
import Gen.Prelude
import Gen.Protocol (Names (..))
import qualified Gen.Protocol as Proto
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
pX = "Data..@"
pXMay = "Data..@?"
pXDef = "Core..!@"

pJ, pJMay, pJDef :: QOp
pJ = "Data..:"
pJMay = "Data..:?"
pJDef = "Data..!="

pJE, pJEMay, pJEDef :: QOp
pJE = "Data..:>"
pJEMay = "Data..?>"
pJEDef = pXDef

pH, pHMay :: QOp
pH = "Data..#"
pHMay = "Data..#?"

pXMap, pXList, pXList1, pHMap :: Exp
pXMap = var "Data.parseXMLMap"
pXList = var "Data.parseXMLList"
pXList1 = var "Data.parseXMLList1"
pHMap = var "Data.parseHeadersMap"

toX, toXAttr, toJ, toQ, toH :: QOp
toX = "Data.@="
toXAttr = "Data.@@="
toJ = "Data..="
toQ = "Data.=:"
toH = "Data.=#"

toQList, toXList :: Exp
toQList = var "Data.toQueryList"
toXList = var "Data.toXMLList"

toXMap, toQMap :: Exp
toXMap = var "Data.toXMLMap"
toQMap = var "Data.toQueryMap"

ctorS :: HasMetadata a Identity => a -> Id -> [Field] -> Decl
ctorS m n fs = Exts.TypeSig () [ident (smartCtorId n)] ty
  where
    ty = foldr (Exts.TyFun ()) (tycon (typeId n)) ps

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
lensS m type' f =
  Exts.TypeSig () [ident (fieldLens f)] $
    tyapp
      ( tyapp
          (tycon "Lens.Lens'")
          (signature m type')
      )
      (external m f)

lensD :: Id -> Field -> Decl
lensD type' f = Exts.sfun (ident l) [] (unguarded rhs) Exts.noBinds
  where
    l = fieldLens f
    a = fieldAccessor f

    rhs =
      mapping (typeOf f) $
        var "Lens.lens"
          `Exts.app` Exts.lamE [recordPat [Exts.PFieldPun () (unqual a)]] (var a)
          `Exts.app` Exts.lamE
            [Exts.PAsPat () (ident "s") (recordPat []), pvar "a"]
            (recordSig (Exts.RecUpdate () (var "s") [field (unqual a) (var "a")]))

    recordPat = Exts.PRec () (unqual (ctorId type'))
    recordSig e = Exts.ExpTypeSig () e (tycon (typeId type'))

errorS :: Text -> Decl
errorS n =
  let cxt = Exts.CxSingle () (Exts.TypeA () $ tycon "Core.AsError" `tyapp` tyvar "a")
      forall = Exts.TyForall () Nothing (Just cxt)
   in Exts.TypeSig () [ident n] . forall $
        tycon "Lens.Fold"
          `tyapp` tyvar "a"
          `tyapp` tycon "Core.ServiceError"

errorD :: HasMetadata a Identity => a -> Text -> Maybe Int -> Text -> Decl
errorD m n s c =
  Exts.sfun (ident n) [] (unguarded (maybe rhs status s)) Exts.noBinds
  where
    status i =
      Exts.infixApp rhs "Prelude.." $
        var "Core.hasStatus"
          `Exts.app` Exts.intE (fromIntegral i)

    rhs = Exts.appFun (var "Core._MatchServiceError") [var (m ^. serviceConfig), str c]

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
        map (rule . mappend "Prelude." . Text.pack) (mapMaybe derivingName cs)

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
serviceS m = Exts.TypeSig () [ident (m ^. serviceConfig)] (tycon "Core.Service")

serviceD :: HasMetadata a Identity => a -> Retry -> Decl
serviceD m r = Exts.patBindWhere (pvar n) rhs bs
  where
    bs = [try Exts.noBinds, chk Exts.noBinds]

    rhs =
      recconstr
        (unqual "Core.Service")
        [ field (unqual "Core.abbrev") (str abbrev),
          field (unqual "Core.signer") (var sig),
          field (unqual "Core.endpointPrefix") (m ^. endpointPrefix . Lens.to str),
          field (unqual "Core.signingName") (m ^. signingName . Lens.to str),
          field (unqual "Core.version") (m ^. apiVersion . Lens.to str),
          field (unqual "Core.s3AddressingStyle") (var "Core.S3AddressingStyleAuto"),
          field (unqual "Core.endpoint") (Exts.app (var "Core.defaultEndpoint") (var n)),
          field (unqual "Core.timeout") (Exts.app justE (Exts.intE 70)),
          field (unqual "Core.check") (var "Core.statusSuccess"),
          field (unqual "Core.error") (var ("Core." <> serviceError m) `Exts.app` str abbrev),
          field (unqual "Core.retry") (var "retry")
        ]

    try =
      Exts.sfun (ident "retry") [] . unguarded $
        recconstr
          (r ^. delayType . Lens.to (unqual . mappend "Core."))
          [ field (unqual "Core.base") (r ^. delayBase . Lens.to frac),
            field (unqual "Core.growth") (r ^. delayGrowth . Lens.to Exts.intE),
            field (unqual "Core.attempts") (r ^. retryAttempts . Lens.to Exts.intE),
            field (unqual "Core.check") (var "check")
          ]

    chk =
      Exts.sfun (ident "check") [ident "e"] . Exts.GuardedRhss () $
        mapMaybe policy (r ^.. retryPolicies . kvTraversal) ++ [otherE nothingE]
      where
        policy (k, v) = (`guardE` Exts.app justE (str k)) <$> policyE v

    n = m ^. serviceConfig
    abbrev = m ^. serviceAbbrev
    sig = "Sign." <> sigToText (m ^. signatureVersion)

policyE :: Policy -> Maybe Exp
policyE = \case
  When (WhenStatus (Just c) s) ->
    Just $
      Exts.appFun
        (var "Lens.has")
        [ Exts.paren $
            Exts.infixApp
              (Exts.app (var "Core.hasCode") (str c))
              "Prelude.."
              (Exts.app (var "Core.hasStatus") (Exts.intE s)),
          var "e"
        ]
  When (WhenStatus Nothing s) ->
    Just $
      Exts.appFun
        (var "Lens.has")
        [ Exts.paren $ Exts.app (var "Core.hasStatus") (Exts.intE s),
          var "e"
        ]
  _ -> Nothing

pagerD :: Id -> Pager Field -> Decl
pagerD n p =
  instD "Core.AWSPager" n $
    Just
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
      --
      Next ks t ->
        Exts.GuardedRhss () $
          stop (notationE (_tokenOutput t)) :
          map (stop . notationE) (Fold.toList ks)
            ++ [other [t]]
      --
      Many k (t :| ts) ->
        Exts.GuardedRhss
          ()
          [ stop (notationE k),
            check t ts,
            other (t : ts)
          ]

    stop x = guardE (Exts.app (var "Core.stop") (rs x)) nothingE

    other = otherE . foldl' f rq
      where
        f :: Exp -> Token Field -> Exp
        f e x =
          Exts.infixApp e "Prelude.&"
            . Exts.infixApp (x ^. tokenInput . Lens.to (notationE' False)) "Lens..~"
            $ rs (x ^. tokenOutput . Lens.to notationE)

    check t ts = guardE (foldl' f (g t) ts) nothingE
      where
        f x = Exts.infixApp x "Prelude.&&" . g
        g y = Exts.app (var "Prelude.isNothing") $ rs (y ^. tokenOutput . Lens.to notationE)

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

notationE :: Notation Field -> Exp
notationE = notationE' True

-- FIXME: doesn't support Maybe fields properly.
notationE' :: Bool -> Notation Field -> Exp
notationE' withLensIso = \case
  NonEmptyText k -> Exts.app (var "Core.nonEmptyText") (label k)
  IsEmptyList (k :| ks) -> Exts.app (var "Core.isEmptyList") (labels k ks)
  NonEmptyList (k :| ks) -> labels k ks
  Access (k :| ks) -> labels k ks
  Choice x y -> Exts.appFun (var "Core.choice") [branch x, branch y]
  where
    branch x =
      let e = notationE' withLensIso x
       in Exts.paren (Exts.app (var (getterN e)) e)

    labels k [] = label k
    labels k ks = foldl' f (label k) ks
      where
        f e x = Exts.infixApp e "Prelude.." (label x)

    label = \case
      Key f -> accessors f
      Each f -> Exts.app (var "Lens.folding") . Exts.paren $ Exts.app (var "Lens.concatOf") (accessors f)
      Last f -> Exts.infixApp (accessors f) "Prelude.." (var "Lens._last")

    accessors f
      | not withLensIso = var (fieldLens f)
      | otherwise =
        foldl' (\a b -> Exts.infixApp a "Prelude.." b) (var (fieldLens f)) $
          lensIso (typeOf f)

    lensIso = \case
      TList1 x -> Exts.app (var "Lens.to") (var "Prelude.toList") : lensIso x
      TMaybe x -> var "Lens._Just" : lensIso x
      _other -> []

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
    "Core.AWSRequest"
    (identifier a)
    $ Just
      [ assocD (identifier a) "AWSResponse" (typeId (identifier b)),
        funArgsD "request" ["overrides"] (requestF c m h a as),
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
          else -- Coerce is inserted here to handle newtypes such as Sensitive.

            Exts.app (var "Prelude.pure")
              . Exts.paren
              . Exts.app justE
              . Exts.paren
              . Exts.app (var "Prelude.coerce")
              $ var "x"
      -- This ensures anything which is set as a payload,
      -- but is a primitive type is just consumed as a bytestring.
      | otherwise = parseAll

    parseAll :: Exp
    parseAll =
      flip Exts.app (var "x") $
        if any fieldLitPayload fs
          then var "Prelude.pure"
          else case p of
            JSON -> var "Data.eitherParseJSON"
            RestJSON -> var "Data.eitherParseJSON"
            APIGateway -> var "Data.eitherParseJSON"
            _ -> var "Data.parseXML"

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
  IsHashable fs -> hashableD n fs
  IsNFData fs -> nfDataD n fs

hashableD :: Id -> [Field] -> Decl
hashableD n fs =
  instD1 "Prelude.Hashable" n (Exts.InsDecl () (Exts.FunBind () [match]))
  where
    match = Exts.Match () (ident "hashWithSalt") [Exts.pvar "_salt", lhs] (unguarded rhs) Exts.noBinds

    lhs
      | null fs = Exts.PWildCard ()
      | otherwise = Exts.PRec () (unqual (ctorId n)) [Exts.PFieldWildcard ()]

    rhs
      | null fs = hashWithSaltE (Exts.var "_salt") (Exts.tuple [])
      | otherwise =
        foldl' hashWithSaltE (Exts.var "_salt") $
          var . fieldAccessor <$> fs

    hashWithSaltE l r = Exts.infixApp l "`Prelude.hashWithSalt`" r

nfDataD :: Id -> [Field] -> Decl
nfDataD n fs =
  instD1 "Prelude.NFData" n (Exts.InsDecl () (Exts.FunBind () [match]))
  where
    match = Exts.Match () (ident "rnf") [lhs] (unguarded rhs) Exts.noBinds

    lhs
      | null fs = Exts.PWildCard ()
      | otherwise = Exts.PRec () (unqual (ctorId n)) [Exts.PFieldWildcard ()]

    rhs = case NE.nonEmpty $ rnfE . var . fieldAccessor <$> fs of
      Nothing -> Exts.tuple []
      Just rnfs -> foldr1 seqE rnfs

    rnfE = Exts.app (var "Prelude.rnf")

    seqE l r = Exts.infixApp l "`Prelude.seq`" r

-- FIXME: merge D + E constructors where possible
fromXMLD :: Protocol -> Id -> [Field] -> Decl
fromXMLD p n = decodeD "Data.FromXML" n "parseXML" (ctorE n) . map (parseXMLE p)

fromJSOND :: Protocol -> Id -> [Field] -> Decl
fromJSOND p n fs = instD1 "Data.FromJSON" n with
  where
    with =
      funD "parseJSON" $
        Exts.app
          (Exts.app (var "Data.withObject") (str (typeId n)))
          (Exts.lamE [Exts.pvar "x"] es)

    es = ctorE n $ map (parseJSONE p pJ pJMay pJDef) fs

toElementD :: Protocol -> Id -> Maybe Text -> Either Text Field -> Decl
toElementD p n ns ef =
  instD1 "Data.ToElement" n (if isLeft ef then funD "toElement" body else decl)
  where
    decl = Exts.InsDecl () (Exts.FunBind () [match])
    match = Exts.Match () (ident "toElement") [prec] (unguarded body) Exts.noBinds
    prec = Exts.PRec () (unqual (ctorId n)) [Exts.PFieldWildcard ()]
    body = toElementE p ns ef

toXMLD :: Protocol -> Id -> [Field] -> Decl
toXMLD p n =
  instD1 "Data.ToXML" n
    . wildcardD n "toXML" enc memptyE
    . map (Right . toXMLE p)
  where
    enc = mconcatE . map (either id id)

toJSOND :: Protocol -> Id -> [Field] -> Decl
toJSOND p n fs =
  instD1 "Data.ToJSON" n
    . wildcardD n "toJSON" enc (Exts.paren $ Exts.app (var "Data.Object") memptyE)
    $ map (Right . toJSONE p) fs
  where
    enc = case find _fieldPayload fs of
      Nothing ->
        Exts.app (var "Data.object")
          . Exts.app (var "Prelude.catMaybes")
          . Exts.listE
          . map (either id id)
      Just f ->
        const $ Exts.app (var "Data.toJSON") (var $ fieldAccessor f)

toHeadersD :: Protocol -> Id -> [Either (Text, Text) Field] -> Decl
toHeadersD p n = instD1 "Data.ToHeaders" n . wildcardD n "toHeaders" enc memptyE
  where
    enc = mconcatE . map (toHeadersE p)

toQueryD :: Protocol -> Id -> [Either (Text, Maybe Text) Field] -> Decl
toQueryD p n = instD1 "Data.ToQuery" n . wildcardD n "toQuery" enc memptyE
  where
    enc = mconcatE . map (toQueryE p)

toPathD :: Id -> [Either Text Field] -> Decl
toPathD n =
  instD1 "Data.ToPath" n . \case
    [Left t] -> funD "toPath" . Exts.app (var "Prelude.const") $ str t
    es -> wildcardD n "toPath" enc memptyE es
  where
    enc = mconcatE . map toPathE

toBodyD :: Id -> Field -> Decl
toBodyD n f =
  instD1 "Data.ToBody" n decl
  where
    decl = Exts.InsDecl () (Exts.FunBind () [match])
    match = Exts.Match () (ident "toBody") [prec] (unguarded body) Exts.noBinds
    prec = Exts.PRec () (unqual (ctorId n)) [Exts.PFieldWildcard ()]
    body = Exts.app (var "Data.toBody") (var (fieldAccessor f))

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
    | all isLeft es -> funD f $ Exts.app (var "Prelude.const") (enc es)
    | otherwise -> Exts.InsDecl () (Exts.FunBind () [match prec es])
  where
    match p es =
      Exts.Match () (ident f) [p] (unguarded (enc es)) Exts.noBinds

    prec = Exts.PRec () (unqual (ctorId n)) [Exts.PFieldWildcard ()]

instD1 :: Text -> Id -> InstDecl -> Decl
instD1 c n = instD c n . Just . (: [])

instD :: Text -> Id -> Maybe [InstDecl] -> Decl
instD c n = Exts.InstDecl () Nothing rule
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
    | required -> decodeE x pX n
    | otherwise -> decodeE x pXMay n
  where
    unflatE Nothing g xs =
      Exts.app (wrapMay (wrapSensitive (Exts.appFun g xs))) x
    unflatE (Just n) g xs =
      Exts.infixApp (defaultMonoidE x n pXMay pXDef) "Prelude.>>=" $
        wrapMay (wrapSensitive (Exts.appFun g xs))

    x = var "x"

    wrapSensitive
      | sensitive =
        Exts.app
          ( Exts.app
              (var "Prelude.fmap")
              (Exts.app (var "Prelude.fmap") (var "Data.Sensitive"))
          )
      | otherwise = id

    wrapMay
      | required = id
      | otherwise = Exts.app (var "Core.may")

    required = not (fieldMaybe f)
    sensitive = fieldSensitive f

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
toXMLE p f = toGenericE p opX "Data.toXML" toXMap toXList f
  where
    opX
      | f ^. fieldRef . refXMLAttribute = toXAttr
      | otherwise = toX

toElementE :: Protocol -> Maybe Text -> Either Text Field -> Exp
toElementE p ns = either (`root` []) node
  where
    root n = Exts.appFun (var "Data.mkElement") . (str (qual n) :)

    node f = root n [var (fieldAccessor f)]
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

    field' = toGenericE p toQ "Data.toQuery" toQMap toQList

toPathE :: Either Text Field -> Exp
toPathE = either str (Exts.app (var "Data.toBS") . var . fieldAccessor)

toGenericE :: Protocol -> QOp -> Text -> Exp -> Exp -> Field -> Exp
toGenericE p toO toF toM toL f = case inputNames p f of
  NMap mn e k v
    | fieldMaybe f ->
      flatE mn toO . Exts.app (var toF) $ Exts.appFun toM [str e, str k, str v, var "Prelude.<$>", a]
    | otherwise ->
      flatE mn toO $ Exts.appFun toM [str e, str k, str v, a]
  NList mn i
    | fieldMaybe f ->
      flatE mn toO . Exts.app (var toF) $ Exts.appFun toL [str i, var "Prelude.<$>", a]
    | otherwise ->
      flatE mn toO $ Exts.appFun toL [str i, a]
  NName n ->
    encodeE n toO a
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
ctorE n = applicativeE (var (ctorId n)) . map Exts.paren

memptyE :: Exp
memptyE = var "Prelude.mempty"

mconcatE :: [Exp] -> Exp
mconcatE = Exts.app (var "Prelude.mconcat") . Exts.listE

applicativeE :: Exp -> [Exp] -> Exp
applicativeE l [] = Exts.app pureE l
applicativeE l (r : rs) = Exts.infixApp l "Prelude.<$>" (infixE r "Prelude.<*>" rs)

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
requestF c meta h r is =
  maybe e (foldr applyPlugin e) selectedPlugins
  where
    applyPlugin x =
      -- Plugin functions are of the form :: Request a -> Request a
      Exts.infixApp (var x) "Prelude.."

    selectedPlugins =
      -- Lookup a specific operationPlugins key before the wildcard.
      Map.lookup (identifier r) (c ^. operationPlugins)
        <|> Map.lookup (mkId "*") (c ^. operationPlugins)

    e = Exts.app v (Exts.app (var "overrides") (var $ meta ^. serviceConfig))

    v =
      var
        . mappend ("Request." <> methodToText m)
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

-- FIXME: take method into account for responses, such as HEAD etc, particuarly
-- when the body might be totally empty.
responseF :: Protocol -> RefF a -> [Field] -> Exp
responseF p r fs
  | null fs = var "Response.receiveNull"
  | any fieldStream fs = var "Response.receiveBody"
  | any fieldLitPayload fs = var "Response.receiveBytes"
  | Just x <- r ^. refResultWrapper = Exts.app (var (suf <> "Wrapper")) (str x)
  | all (not . fieldBody) fs = var "Response.receiveEmpty"
  | otherwise = var suf
  where
    suf = "Response.receive" <> Proto.suffix p

waiterS :: Id -> Waiter a -> Decl
waiterS n w = Exts.TypeSig () [ident c] $ tyapp (tycon "Core.Wait") (tycon k)
  where
    k = w ^. waitOperation . Lens.to typeId
    c = smartCtorId n

waiterD :: Id -> Waiter Field -> Decl
waiterD n w = Exts.sfun (ident c) [] (unguarded rhs) Exts.noBinds
  where
    c = smartCtorId n

    rhs =
      recconstr
        (unqual "Core.Wait")
        [ field (unqual "Core.name") (str (memberId n)),
          field (unqual "Core.attempts") (w ^. waitAttempts . Lens.to Exts.intE),
          field (unqual "Core.delay") (w ^. waitDelay . Lens.to Exts.intE),
          field (unqual "Core.acceptors")
            . Exts.listE
            $ map match (w ^. waitAcceptors)
        ]

    match x =
      case (_acceptMatch x, _acceptArgument x) of
        (_, Just (NonEmptyList _)) ->
          Exts.appFun (var "Core.matchNonEmpty") (expect x : criteria x : argument' x)
        (Path, _) ->
          Exts.appFun (var "Core.matchAll") (expect x : criteria x : argument' x)
        (PathAll, _) ->
          Exts.appFun (var "Core.matchAll") (expect x : criteria x : argument' x)
        (PathAny, _) ->
          Exts.appFun (var "Core.matchAny") (expect x : criteria x : argument' x)
        (Status, _) ->
          Exts.appFun (var "Core.matchStatus") (expect x : criteria x : argument' x)
        (Error, _) ->
          Exts.appFun (var "Core.matchError") (expect x : criteria x : argument' x)

    expect x =
      case _acceptExpect x of
        Status' i -> Exts.intE i
        Boolean b -> con . mappend "Prelude." . Text.pack $ show b
        Textual t -> str t

    criteria x =
      case _acceptCriteria x of
        Retry -> var "Core.AcceptRetry"
        Success -> var "Core.AcceptSuccess"
        Failure -> var "Core.AcceptFailure"

    argument' x = go <$> maybeToList (notationE <$> _acceptArgument x)
      where
        go = case _acceptExpect x of
          Textual {} ->
            \y -> Exts.infixApp y "Prelude.." (Exts.app (var "Lens.to") (var "Data.toTextCI"))
          _ -> id

signature :: HasMetadata a Identity => a -> TType -> Type
signature m = directed False m Nothing

internal, external :: HasMetadata a Identity => a -> Field -> Type
internal m f = directed True m (_fieldDirection f) f
external m f = directed False m (_fieldDirection f) f

-- FIXME: split again into internal/external
directed :: (HasMetadata a Identity, TypeOf b) => Bool -> a -> Maybe Direction -> b -> Type
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
      | i = tyapp (tycon "Data.Sensitive")
      | otherwise = id

    may x = tycon "Prelude.Maybe" `tyapp` go x

    list1 = tyapp (tycon "Prelude.NonEmpty")

    hmap k v = tycon "Prelude.HashMap" `tyapp` go k `tyapp` go v

    stream = case d of
      Nothing -> "Data.ResponseBody"
      Just Output -> "Data.ResponseBody" -- Response stream.
      Just Input
        | m ^. signatureVersion == S3 ->
          "Data.RequestBody" -- If the signer supports chunked encoding, both body types are accepted.
        | otherwise -> "Data.HashedBody" -- Otherwise only a pre-hashed body is accepted.

mapping :: TType -> Exp -> Exp
mapping t e = infixE e "Prelude.." (go t)
  where
    go = \case
      TSensitive x -> var "Data._Sensitive" : go x
      TMaybe x -> nest (go x)
      x -> maybeToList (iso x)

    nest [] = []
    nest (x : xs) = [Exts.app (var "Lens.mapping") (infixE x "Prelude.." xs)]

iso :: TType -> Maybe Exp
iso = \case
  TLit Time -> Just (var "Data._Time")
  TLit Base64 -> Just (var "Data._Base64")
  TMap {} -> Just (var "Lens.coerced")
  TList1 {} -> Just (var "Lens.coerced")
  TList {} -> Just (var "Lens.coerced")
  TSensitive x -> Just (infixE (var "Data._Sensitive") "Prelude.." (maybeToList (iso x)))
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
    | i -> tycon "Data.Base64"
    | otherwise -> tycon "Prelude.ByteString"
  Time
    | i -> tycon ("Data." <> tsToText ts)
    | otherwise -> tycon "Prelude.UTCTime"
  Json -> tycon "Prelude.ByteString"

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
