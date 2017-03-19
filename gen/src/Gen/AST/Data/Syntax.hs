{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.AST.Data.Syntax
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Data.Syntax where

import           Control.Comonad
import           Control.Error
import           Control.Lens                 hiding (iso, mapping, op, strict)

import           Data.Foldable                (foldl', foldr')
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Monoid                  ((<>))
import           Data.String
import           Data.Text                    (Text)

import           Gen.AST.Data.Field
import           Gen.AST.Data.Instance
import           Gen.Protocol                 (Names (..))
import           Gen.Types

import           Language.Haskell.Exts.Build  hiding (pvar, var)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax hiding (Int, List, Lit, Var)

import qualified Data.Foldable                as Fold
import qualified Data.HashMap.Strict          as Map
import qualified Data.Text                    as Text
import qualified Gen.Protocol                 as Proto
import qualified Language.Haskell.Exts        as Exts

pX, pXMay, pXDef :: QOp
pX      = ".@"
pXMay   = ".@?"
pXDef   = ".!@"

pJ, pJMay, pJDef :: QOp
pJ      = ".:"
pJMay   = ".:?"
pJDef   = ".!="

pJE, pJEMay, pJEDef :: QOp
pJE     = ".:>"
pJEMay  = ".?>"
pJEDef  = pXDef

pH, pHMay :: QOp
pH      = ".#"
pHMay   = ".#?"

pXMap, pXList, pXList1, pHMap :: Exp
pXMap   = var "parseXMLMap"
pXList  = var "parseXMLList"
pXList1 = var "parseXMLList1"
pHMap   = var "parseHeadersMap"

toX, toXAttr, toJ, toQ, toH :: QOp
toX     = "@="
toXAttr = "@@="
toJ     = ".="
toQ     = "=:"
toH     = "=#"

toQList, toXList ::  Exp
toQList = var "toQueryList"
toXList = var "toXMLList"

toXMap, toQMap :: Exp
toXMap  = var "toXMLMap"
toQMap  = var "toQueryMap"

ctorS :: HasMetadata a Identity => a -> Id -> [Field] -> Decl
ctorS m n fs = TypeSig noLoc [ident (smartCtorId n)] ty
  where
    ty = foldr' TyFun (tycon (typeId n)) ps
    ps = map (external m) (filter fieldIsParam fs)

ctorD :: Id -> [Field] -> Decl
ctorD n fs =
    sfun noLoc (ident (smartCtorId n)) ps (UnGuardedRhs rhs) noBinds
  where
    ps :: [Name]
    ps = map fieldParamName (filter fieldIsParam fs)

    rhs :: Exp
    rhs | null fs   = var (ctorId n)
        | otherwise = RecConstr (unqual (ctorId n)) (map fieldUpdate fs)

fieldUpdate :: Field -> FieldUpdate
fieldUpdate f = FieldUpdate (unqual (fieldAccessor f)) rhs
  where
    rhs :: Exp
    rhs | fieldMaybe f  = nothingE
        | fieldMonoid f = memptyE
        | Just v <- iso (typeOf f)
                        = infixApp v "#" p
        | otherwise     = p

    p :: Exp
    p = Exts.Var (UnQual (fieldParamName f))

lensS :: HasMetadata a Identity => a -> TType -> Field -> Decl
lensS m t f = TypeSig noLoc [ident (fieldLens f)] $
    TyApp (TyApp (tycon "Lens'")
                 (signature m t))
          (external m f)

lensD :: Field -> Decl
lensD f = sfun noLoc (ident l) [] (UnGuardedRhs rhs) noBinds
  where
    l = fieldLens f
    a = fieldAccessor f

    rhs = mapping (typeOf f) $
        app (app (var "lens") (var a))
            (paren (lamE noLoc [pvar "s", pvar "a"]
                   (RecUpdate (var "s") [FieldUpdate (unqual a) (var "a")])))

errorS :: Text -> Decl
errorS n = TypeSig noLoc [ident n] $
    TyForall Nothing [ClassA (unqual "AsError") [tyvar "a"]] $
        TyApp (TyApp (TyApp (tycon "Getting")
                            (TyApp (tycon "First") (tycon "ServiceError")))
                     (tyvar "a"))
              (tycon "ServiceError")

errorD :: HasMetadata a Identity => a -> Text -> Maybe Int -> Text -> Decl
errorD m n s c =
    sfun noLoc (ident n) [] (UnGuardedRhs (maybe rhs status s)) noBinds
  where
    status i = infixApp rhs "." (var "hasStatus" `app` intE (fromIntegral i))

    rhs = appFun (var "_MatchServiceError") [var (m ^. serviceConfig), str c]

dataD :: Id -> [QualConDecl] -> [Derive] -> Decl
dataD n fs cs = DataDecl noLoc arity [] (ident (typeId n)) [] fs ds
  where
    arity = case fs of
        [QualConDecl _ _ _ (RecDecl _ [_])] -> NewType
        _                                   -> DataType

    ds = map ((,[]) . UnQual . Ident) (mapMaybe derivingName cs)

recordD :: HasMetadata a Identity => a -> Id -> [Field] -> QualConDecl
recordD m n = conD . \case
    []  -> ConDecl c []
    [x] -> RecDecl c [g (internal m) x]
    xs  -> RecDecl c (map (g (strict . internal m)) xs)
  where
    g h f = ([ident (fieldAccessor f)], h f)

    c = ident (ctorId n)

conD :: ConDecl -> QualConDecl
conD = QualConDecl noLoc [] []

serviceS :: HasMetadata a Identity => a -> Decl
serviceS m = TypeSig noLoc [ident (m ^. serviceConfig)] (tycon "Service")

serviceD :: HasMetadata a Identity => a -> Retry -> Decl
serviceD m r = patBindWhere noLoc (pvar n) rhs bs
  where
    bs  = [try noBinds, chk noBinds]

    rhs =
        RecConstr (unqual "Service")
            [ FieldUpdate (unqual "_svcAbbrev")   (str abbrev)
            , FieldUpdate (unqual "_svcSigner")   (var sig)
            , FieldUpdate (unqual "_svcPrefix")   (m ^. endpointPrefix . to str)
            , FieldUpdate (unqual "_svcVersion")  (m ^. apiVersion . to str)
            , FieldUpdate (unqual "_svcEndpoint") (app (var "defaultEndpoint") (var n))
            , FieldUpdate (unqual "_svcTimeout")  (app justE (intE 70))
            , FieldUpdate (unqual "_svcCheck")    (var "statusSuccess")
            , FieldUpdate (unqual "_svcError")    (var (serviceError m) `app` str abbrev)
            , FieldUpdate (unqual "_svcRetry")    (var "retry")
            ]

    try = sfun noLoc (ident "retry") [] . UnGuardedRhs $
        RecConstr (r ^. delayType . to unqual)
            [ FieldUpdate (unqual "_retryBase")     (r ^. delayBase . to (Exts.Lit . Frac))
            , FieldUpdate (unqual "_retryGrowth")   (r ^. delayGrowth . to intE)
            , FieldUpdate (unqual "_retryAttempts") (r ^. retryAttempts . to intE)
            , FieldUpdate (unqual "_retryCheck")    (var "check")
            ]

    chk = sfun noLoc (ident "check") [ident "e"] . GuardedRhss $
        mapMaybe policy (r ^.. retryPolicies . kvTraversal) ++ [otherE nothingE]
      where
        policy (k, v) = (`guardE` app justE (str k)) <$> policyE v

    n      = m ^. serviceConfig
    abbrev = m ^. serviceAbbrev
    sig    = m ^. signatureVersion . to sigToText

policyE :: Policy -> Maybe Exp
policyE = \case
   When (WhenStatus (Just c) s)
       -> Just $ appFun (var "has")
           [ paren $ infixApp (app (var "hasCode") (str c))
                              "."
                              (app (var "hasStatus") (intE s))
           , var "e"
           ]

   When (WhenStatus Nothing  s)
       -> Just $ appFun (var "has")
           [ paren $ app (var "hasStatus") (intE s)
           , var "e"
           ]

   _   -> Nothing

pagerD :: Id -> Pager Field -> Decl
pagerD n p = instD "AWSPager" n
    [ InsDecl $ sfun noLoc (ident "page") [ident "rq", ident "rs"] (rhs p) noBinds
    ]
  where
    rhs = \case
        Only t -> GuardedRhss
            [ stop (notationE (_tokenOutput t))
            , other [t]
            ]

        Next ks t -> GuardedRhss
            $ stop (notationE (_tokenOutput t))
            : map  (stop . notationE) (Fold.toList ks)
           ++ [other [t]]

        Many k (t :| ts) -> GuardedRhss
            [ stop  (notationE k)
            , check t ts
            , other (t:ts)
            ]

    stop x = guardE (app (var "stop") (rs x)) nothingE

    other = otherE . foldl' f rq
      where
        f :: Exp -> Token Field -> Exp
        f e x = infixApp e "&"
              . infixApp (x ^. tokenInput . to notationE) ".~"
              $ rs (x ^. tokenOutput . to notationE)

    check t ts = guardE (foldl' f (g t) ts) nothingE
      where
        f x = infixApp x "&&" . g
        g y = app (var "isNothing") $ rs (y ^. tokenOutput . to notationE)

    rq   = infixApp justE "$" (var "rq")
    rs x = infixApp (var "rs") (qop (getterN x)) x

getterN :: Exp -> Text
getterN e = if go e then "^?" else "^."
  where
    go = \case
        Exts.App x y                      -> go x || go y
        Exts.InfixApp x _ y               -> go x || go y
        Exts.Var (UnQual (Ident "_last")) -> True
        Exts.Var (UnQual (Ident "_Just")) -> True
        _                                 -> False

    -- FIXME: doesn't support Maybe fields currently.
notationE :: Notation Field -> Exp
notationE = \case
    NonEmptyText k        -> app (var "nonEmptyText") (label False k)
    NonEmptyList k        -> label False k
    Access      (k :| ks) -> labels k ks
    Choice       x y      -> appFun (var "choice") [branch x, branch y]
  where
    branch x = let e = notationE x in paren $ app (var (getterN e)) e

    labels k [] = label False k
    labels k ks = foldl' f (label True k) ks
      where
         f e x = infixApp e "." (label True x)

    label b = \case
        Key  f -> key b f
        Each f -> app (var "folding") . paren $ app (var "concatOf") (key False f)
        Last f -> infixApp (key False f) "." (var "_last")

    key False f = var (fieldLens f)
    key True  f
        | fieldMonoid f = key False f
        | fieldMaybe f  = infixApp (key False f) "." (var "_Just")
        | otherwise     = key False f

requestD :: HasMetadata a Identity
         => Config
         -> a
         -> HTTP
         -> (Ref, [Inst])
         -> (Ref, [Field])
         -> Decl
requestD c m h (a, as) (b, bs) = instD "AWSRequest" (identifier a)
    [ assocD (identifier a) "Rs" (typeId (identifier b))
    , funD "request"  (requestF c m h a as)
    , funD "response" (responseE (m ^. protocol) b bs)
    ]

responseE :: Protocol -> Ref -> [Field] -> Exp
responseE p r fs = app (responseF p r fs) bdy
  where
    n = r ^. to identifier
    s = r ^. refAnn . to extract

    bdy :: Exp
    bdy | null fs                      = var (ctorId n)
        | isShared s, all fieldBody fs = lam parseAll
        | otherwise                    = lam . ctorE n $ map parseField fs

    lam :: Exp -> Exp
    lam = lamE noLoc [pvar "s", pvar "h", pvar "x"]

    parseField :: Field -> Exp
    parseField x =
        case fieldLocation x of
            Just Headers        -> parseHeadersE p x
            Just Header         -> parseHeadersE p x
            Just StatusCode     -> parseStatusE    x
            Just Body    | body -> app pureE (var "x")
            Nothing      | body -> app pureE (var "x")
            _                   -> parseProto x

    parseProto :: Field -> Exp
    parseProto f =
        case p of
            _ | f ^. fieldPayload -> parseOne   f
            JSON                  -> parseJSONE p pJE pJEMay pJEDef f
            RestJSON              -> parseJSONE p pJE pJEMay pJEDef f
            APIGateway            -> parseJSONE p pJE pJEMay pJEDef f
            _                     -> parseXMLE  p f

    parseOne :: Field -> Exp
    parseOne f
        | fieldLit f =
             if fieldIsParam f
                 then app (var "pure") (var "x")
                 else app (var "pure") (paren (app (var "Just") (var "x")))
          -- ^ This ensures anything which is set as a payload,
          -- but is a primitive type is just consumed as a bytestring.
        | otherwise  = parseAll

    parseAll :: Exp
    parseAll = flip app (var "x") $
        case p of
            JSON       -> var "eitherParseJSON"
            RestJSON   -> var "eitherParseJSON"
            APIGateway -> var "eitherParseJSON"
            _          -> var "parseXML"

    body = any fieldStream fs

instanceD :: Protocol -> Id -> Inst -> Decl
instanceD p n = \case
    FromXML   fs   -> fromXMLD   p n fs
    FromJSON  fs   -> fromJSOND  p n fs
    ToElement ns e -> toElementD p n ns e
    ToXML     fs   -> toXMLD     p n fs
    ToJSON    fs   -> toJSOND    p n fs
    ToHeaders es   -> toHeadersD p n es
    ToPath    es   -> toPathD      n es
    ToQuery   es   -> toQueryD   p n es
    ToBody    f    -> toBodyD      n f
    IsHashable     -> hashableD    n
    IsNFData       -> nfDataD      n

hashableD, nfDataD :: Id -> Decl
hashableD n = instD "Hashable" n []
nfDataD   n = instD "NFData"   n []

-- FIXME: merge D + E constructors where possible
fromXMLD :: Protocol -> Id -> [Field] -> Decl
fromXMLD p n = decodeD "FromXML" n "parseXML" (ctorE n) . map (parseXMLE p)

fromJSOND :: Protocol -> Id -> [Field] -> Decl
fromJSOND p n fs = instD1 "FromJSON" n with
  where
    with = funD "parseJSON" $
        app (app (var "withObject") (str (typeId n)))
            (lamE noLoc [pvar "x"] es)

    es = ctorE n $ map (parseJSONE p pJ pJMay pJDef) fs

toElementD :: Protocol -> Id -> Maybe Text -> Either Text Field -> Decl
toElementD p n ns = instD1 "ToElement" n . funD "toElement" . toElementE p ns

toXMLD :: Protocol -> Id -> [Field] -> Decl
toXMLD p n = instD1 "ToXML" n
    . wildcardD n "toXML" enc memptyE
    . map (Right . toXMLE p)
  where
    enc = mconcatE . map (either id id)

toJSOND :: Protocol -> Id -> [Field] -> Decl
toJSOND p n = instD1 "ToJSON" n
    . wildcardD n "toJSON" enc (paren $ app (var "Object") memptyE)
    . map (Right . toJSONE p)
  where
    enc = app (var "object")
        . app (var "catMaybes")
        . listE
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
toPathD n = instD1 "ToPath" n . \case
    [Left t] -> funD "toPath" . app (var "const") $ str t
    es       -> wildcardD n "toPath" enc memptyE es
  where
    enc = mconcatE . map toPathE

toBodyD :: Id -> Field -> Decl
toBodyD n f = instD "ToBody" n [funD "toBody" (toBodyE f)]

wildcardD :: Id
          -> Text
          -> ([Either a b] -> Exp)
          -> Exp
          -> [Either a b]
          -> InstDecl
wildcardD n f enc xs = \case
    []                        -> constD f xs
    es | not (any isRight es) -> funD f $ app (var "const") (enc es)
       | otherwise            -> InsDecl (FunBind [match prec es])
  where
    match p es =
        Match noLoc (ident f) [p] Nothing (UnGuardedRhs (enc es)) noBinds

    prec = PRec (unqual (ctorId n)) [PFieldWildcard]

instD1 :: Text -> Id -> InstDecl -> Decl
instD1 c n = instD c n . (:[])

instD :: Text -> Id -> [InstDecl] -> Decl
instD c n = InstDecl noLoc Nothing [] [] (unqual c) [tycon (typeId n)]

funD :: Text -> Exp -> InstDecl
funD f = InsDecl . patBind noLoc (pvar f)

funArgsD :: Text -> [Text] -> Exp -> InstDecl
funArgsD f as e = InsDecl $
    sfun noLoc (ident f) (map ident as) (UnGuardedRhs e) noBinds

assocD :: Id -> Text -> Text -> InstDecl
assocD n x y = InsType noLoc (TyApp (tycon x) (tycon (typeId n))) (tycon y)

decodeD :: Text -> Id -> Text -> ([a] -> Exp) -> [a] -> Decl
decodeD c n f dec = instD1 c n . \case
    [] -> funD f . app (var "const") $ dec []
    es -> funArgsD f ["x"] (dec es)

constD :: Text -> Exp -> InstDecl
constD f = funArgsD f [] . app (var "const")

parseXMLE :: Protocol -> Field -> Exp
parseXMLE p f = case outputNames p f of
    NMap  mn e k v      -> unflatE mn pXMap   [str e, str k, str v]
    NList mn i
        | fieldMonoid f -> unflatE mn pXList  [str i]
        | otherwise     -> unflatE mn pXList1 [str i]
    NName n
        | req           -> decodeE x pX    n
        | otherwise     -> decodeE x pXMay n
  where
    unflatE Nothing  g xs
        | req       = appFun g (xs ++ [x])
        | otherwise = app (may (appFun g xs)) x

    unflatE (Just n) g xs =
        infixApp (defaultMonoidE x n pXMay pXDef) ">>=" $
            if req
                then appFun g xs
                else may (appFun g xs)

    may = app (var "may")
    x   = var "x"

    req = not (fieldMaybe f)

parseJSONE :: Protocol -> QOp -> QOp -> QOp -> Field -> Exp
parseJSONE p d dm dd f
    | fieldMonoid f = defaultMonoidE x n dm dd
    | fieldMaybe f  = decodeE x dm n
    | otherwise     = decodeE x d  n
  where
    n = memberName p Output f
    x = var "x"

parseHeadersE :: Protocol -> Field -> Exp
parseHeadersE p f
    | TMap {} <- typeOf f = appFun pHMap [str n, h]
    | fieldMaybe f        = decodeE h pHMay n
    | otherwise           = decodeE h pH    n
  where
    n = memberName p Output f
    h = var "h"

parseStatusE :: Field -> Exp
parseStatusE f
    | fieldMaybe f = app pureE (app justE v)
    | otherwise    = app pureE v
  where
    v = paren $ app (var "fromEnum") (var "s")

toXMLE :: Protocol -> Field -> Exp
toXMLE p f = toGenericE p opX "toXML" toXMap toXList f
  where
    opX | f ^. fieldRef . refXMLAttribute = toXAttr
        | otherwise                       = toX

toElementE :: Protocol -> Maybe Text -> Either Text Field -> Exp
toElementE p ns = either (`root` []) node
  where
    root n = appFun (var "mkElement") . (str (qual n) :)

    node f = root n [var ".", var (fieldAccessor f)]
      where
        n = memberName p Input f

    qual n | Just x <- ns = "{" <> x <> "}" <> n
           | otherwise    = n

toJSONE :: Protocol -> Field -> Exp
toJSONE p f
    | fieldMaybe f = infixApp (paren (app (str n) o)) "<$>" a
    | otherwise    = app (var "Just") (encodeE n toJ a)
  where
    n = memberName p Input f
    a = var (fieldAccessor f)
    o = var (Text.pack (Exts.prettyPrint toJ))

toHeadersE :: Protocol -> Either (Text, Text) Field -> Exp
toHeadersE p = either pair field
  where
    pair (k, v) = encodeE k toH $ impliesE v (var "ByteString")

    field f = encodeE (memberName p Input f) toH $ var (fieldAccessor f)

toQueryE :: Protocol -> Either (Text, Maybe Text) Field -> Exp
toQueryE p = either pair field
  where
    pair (k, Nothing) = str k
    pair (k, Just v)  = encodeE k toQ $ impliesE v (var "ByteString")

    field = toGenericE p toQ "toQuery" toQMap toQList

toPathE :: Either Text Field -> Exp
toPathE = either str (app (var "toBS") . var . fieldAccessor)

toBodyE :: Field -> Exp
toBodyE = infixApp (var "toBody") "." . var . fieldAccessor

toGenericE :: Protocol -> QOp -> Text -> Exp -> Exp -> Field -> Exp
toGenericE p toO toF toM toL f = case inputNames p f of
    NMap  mn e k v
        | fieldMaybe f -> flatE mn toO . app (var toF) $ appFun toM [str e, str k, str v, var "<$>", a]
        | otherwise    -> flatE mn toO $ appFun toM [str e, str k, str v, a]

    NList mn i
        | fieldMaybe f -> flatE mn toO . app (var toF) $ appFun toL [str i, var "<$>", a]
        | otherwise    -> flatE mn toO $ appFun toL [str i, a]

    NName n            -> encodeE n toO a
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
guardE x = GuardedRhs noLoc [Qualifier x]

ctorE :: Id -> [Exp] -> Exp
ctorE n = seqE (var (ctorId n)) . map paren

memptyE :: Exp
memptyE = var "mempty"

mconcatE :: [Exp] -> Exp
mconcatE = app (var "mconcat") . listE

seqE :: Exp -> [Exp] -> Exp
seqE l []     = app pureE l
seqE l (r:rs) = infixApp l "<$>" (infixE r "<*>" rs)

infixE :: Exp -> QOp -> [Exp] -> Exp
infixE l _ []     = l
infixE l o (r:rs) = infixE (infixApp l o r) o rs

impliesE :: Text -> Exp -> Exp
impliesE x y = paren (infixApp (str x) "::" y)

flatE :: Maybe Text -> QOp -> Exp -> Exp
flatE (Just n) o = encodeE n o
flatE Nothing  _ = id

defaultMonoidE :: Exp -> Text -> QOp -> QOp -> Exp
defaultMonoidE v n dm dd =
    infixApp (infixApp v dm (str n)) dd memptyE

encodeE :: Text -> QOp -> Exp -> Exp
encodeE n = infixApp (str n)

decodeE :: Exp -> QOp -> Text -> Exp
decodeE v o = infixApp v o . str

memberName :: Protocol -> Direction -> Field -> Text
memberName p d f = Proto.memberName p d (f ^. fieldId) (f ^. fieldRef)

inputNames, outputNames :: Protocol -> Field -> Names
inputNames  p f = Proto.nestedNames p Input  (f ^. fieldId) (f ^. fieldRef)
outputNames p f = Proto.nestedNames p Output (f ^. fieldId) (f ^. fieldRef)

requestF :: HasMetadata a Identity
         => Config
         -> a
         -> HTTP
         -> Ref
         -> [Inst]
         -> Exp
requestF c meta h r is = maybe e (foldr' plugin e) ps
  where
    plugin x = infixApp (var x) "."

    ps = Map.lookup (identifier r) (c ^. operationPlugins)

    e = app v (var n)

    v = var
      . mappend (methodToText m)
      . fromMaybe mempty
      . listToMaybe
      $ mapMaybe f is

    f = \case
        ToBody    {}  -> Just "Body"
        ToJSON    {}  -> Just "JSON"
        ToElement {}  -> Just "XML"
        _ | p == Query
          , m == POST -> Just "Query"
        _ | p == EC2
          , m == POST -> Just "Query"
        _             -> Nothing

    m = h    ^. method
    p = meta ^. protocol
    n = meta ^. serviceConfig

-- FIXME: take method into account for responses, such as HEAD etc, particuarly
-- when the body might be totally empty.
responseF :: Protocol -> RefF a -> [Field] -> Exp
responseF p r fs
    | null fs                         = var "receiveNull"
    | any fieldStream fs              = var "receiveBody"
    | any fieldLitPayload fs          = var "receiveJSON" -- Currently assumes JSON body literal.
    | Just x <- r ^. refResultWrapper = app (var (suf <> "Wrapper")) (str x)
    | all (not . fieldBody) fs        = var "receiveEmpty"
    | otherwise                       = var suf
  where
    suf = "receive" <> Proto.suffix p

waiterS :: Id -> Waiter a -> Decl
waiterS n w = TypeSig noLoc [ident c] $ TyApp (tycon "Wait") (tycon k)
  where
    k = w ^. waitOperation . to typeId
    c = smartCtorId n

waiterD :: Id -> Waiter Field -> Decl
waiterD n w = sfun noLoc (ident c) [] (UnGuardedRhs rhs) noBinds
  where
    c = smartCtorId n

    rhs = RecConstr (unqual "Wait")
        [ FieldUpdate (unqual "_waitName")      (str (memberId n))
        , FieldUpdate (unqual "_waitAttempts")  (w ^. waitAttempts . to intE)
        , FieldUpdate (unqual "_waitDelay")     (w ^. waitDelay    . to intE)
        , FieldUpdate (unqual "_waitAcceptors")
            . listE $ map match (w ^. waitAcceptors)
        ]

    match x =
        case (_acceptMatch x, _acceptArgument x) of
            (_, Just (NonEmptyList _)) -> appFun (var "matchNonEmpty") (expect x : criteria x : argument' x)
            (Path    , _) -> appFun (var "matchAll")    (expect x : criteria x : argument' x)
            (PathAll , _) -> appFun (var "matchAll")    (expect x : criteria x : argument' x)
            (PathAny , _) -> appFun (var "matchAny")    (expect x : criteria x : argument' x)
            (Status  , _) -> appFun (var "matchStatus") (expect x : criteria x : argument' x)
            (Error   , _) -> appFun (var "matchError")  (expect x : criteria x : argument' x)

    expect x =
        case _acceptExpect x of
            Status' i -> intE i
            Boolean b -> con . Text.pack $ show b
            Textual t -> str t

    criteria x =
        case _acceptCriteria x of
            Retry   -> var "AcceptRetry"
            Success -> var "AcceptSuccess"
            Failure -> var "AcceptFailure"

    argument' x = go <$> maybeToList (notationE <$> _acceptArgument x)
      where
        go = case _acceptExpect x of
            Textual {} -> \y -> infixApp y "." (app (var "to") (var "toTextCI"))
            _          -> id

signature :: HasMetadata a Identity => a -> TType -> Type
signature m = directed False m Nothing

internal, external :: HasMetadata a Identity => a -> Field -> Type
internal m f = directed True  m (_fieldDirection f) f
external m f = directed False m (_fieldDirection f) f

-- FIXME: split again into internal/external
directed :: (HasMetadata a Identity, TypeOf b)
         => Bool
         -> a
         -> Maybe Direction
         -> b
         -> Type
directed i m d (typeOf -> t) = case t of
    TType      x _ -> tycon x
    TLit       x   -> literal i (m ^. timestampFormat . _Identity) x
    TNatural       -> tycon nat
    TStream        -> tycon stream
    TSensitive x   -> sensitive (go x)
    TMaybe     x   -> may x
    TList      x   -> TyList (go x)
    TList1     x   -> list1  (go x)
    TMap       k v -> hmap k v
  where
    go = directed i m d

    nat | i         = "Nat"
        | otherwise = "Natural"

    sensitive
        | i         = TyApp (tycon "Sensitive")
        | otherwise = id

    may x@(TMap  {}) | not i = go x
    may x@(TList {}) | not i = go x
    may x                    = TyApp (tycon "Maybe") (go x)

    list1
        | i         = TyApp (tycon "List1")
        | otherwise = TyApp (tycon "NonEmpty")

    hmap k v
        | i         = TyApp (TyApp (tycon "Map")     (go k)) (go v)
        | otherwise = TyApp (TyApp (tycon "HashMap") (go k)) (go v)

    stream = case d of
        Nothing         -> "RsBody"
        Just Output     -> "RsBody"     -- ^ Response stream.
        Just Input
            | m ^. signatureVersion == S3
                        -> "RqBody"     -- ^ If the signer supports chunked encoding, both body types are accepted.
            | otherwise -> "HashedBody" -- ^ Otherwise only a pre-hashed body is accepted.

mapping :: TType -> Exp -> Exp
mapping t e = infixE e "." (go t)
  where
    go = \case
        TSensitive x            -> var "_Sensitive" : go x
        TMaybe     x@(TMap  {}) -> var "_Default"   : go x
        TMaybe     x@(TList {}) -> var "_Default"   : go x
        TMaybe     x            -> nest (go x)
        x                       -> maybeToList (iso x)

    nest []     = []
    nest (x:xs) = [app (var "mapping") (infixE x "." xs)]

iso :: TType -> Maybe Exp
iso = \case
    TLit Time     -> Just (var "_Time")
    TLit Blob     -> Just (var "_Base64")
    TNatural      -> Just (var "_Nat")
    TSensitive x  -> Just (infixE (var "_Sensitive") "." (maybeToList (iso x)))
    TList1     {} -> Just (var "_List1")
    TList      {} -> Just (var "_Coerce")
    TMap       {} -> Just (var "_Map")
    _             -> Nothing

literal :: Bool -> Timestamp -> Lit -> Type
literal i ts = \case
    Int              -> tycon "Int"
    Long             -> tycon "Integer"
    Double           -> tycon "Double"
    Text             -> tycon "Text"
    Blob | i         -> tycon "Base64"
         | otherwise -> tycon "ByteString"
    Bool             -> tycon "Bool"
    Time | i         -> tycon (tsToText ts)
         | otherwise -> tycon "UTCTime"
    Json             -> TyApp (TyApp (tycon "HashMap") (tycon "Text")) (tycon "Value")

strict :: Type -> Type
strict = TyBang BangedTy . \case
    t@TyApp{} -> TyParen t
    t         -> t

tyvar :: Text -> Type
tyvar = TyVar . ident

tycon :: Text -> Type
tycon = TyCon . unqual

con :: Text -> Exp
con = Con . unqual

qop :: Text -> QOp
qop = fromString . Text.unpack

str :: Text -> Exp
str = Exts.Lit . String . Text.unpack

pvar :: Text -> Pat
pvar = Exts.pvar . ident

var :: Text -> Exp
var = Exts.var . ident

param :: Int -> Name
param = Ident . mappend "p" . show

unqual :: Text -> QName
unqual = UnQual . ident

ident :: Text -> Name
ident = Ident . Text.unpack
