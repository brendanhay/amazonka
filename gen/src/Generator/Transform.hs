{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Generator.Transform
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Transform where

import           Control.Applicative
import           Control.Lens         hiding (indexed)
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Char
import           Data.Default
import qualified Data.HashMap.Strict  as Map
import           Data.List
import           Data.Maybe
import           Data.Monoid          hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Util
import           Debug.Trace
import           Generator.AST
import           Text.EDE.Filters

-- FIXME: When replacing/ignoring types via 'existing', it should actually merge
-- the request/response shenannigans so the correct instances are written for the type

-- FIXME: ResourceRecordSet/ResourceRecords is a list but the type ann it gets is Maybe [a]

-- FIXME: make endpoints overridable

-- FIXME: generate (maybe wrapped newtypes) types for base64/16 encoded bytestrings
-- ie: IAM types VirtualMFADevice

-- FIXME: remove error sum type record fields, and implement appropriate deserialisation

-- FIXME: what about target-prefix (from the model) and action for query services?

-- FIXME: possibly parameterise (Map k v) with an additional type to control the
-- prefixing used (specifically for metadata) unless metadata is the only usage
-- and ToHeaders can always add x-amz-meta-*

-- FIXME: Remove all the disparate request type classes in favour of case analysis
-- in templates

-- FIXME: Request shapes should take preference in the case of equality/ord
-- this ensures a minimum of overrides are needed for required fields.

-- FIXME: Make a way to override the endpoint when sending a request (for mocks etc)

-- FIXME: Fix ambiguous lens when using makeFields
-- Provide the 'length' of the prefix so lenses can be derived.

-- FIXME: Add documentation about where the type in the 'Types' module is used

-- FIXME: Add selected de/serialisation tests for the services

-- FIXME: Rewrite, so that each transformation step happens goddamn logically.

-- FIXME: Deal with result_wrapped=true in XML responses

-- FIXME: Route53 ResourceRecordSetRegion should be dropped in favour of Network.AWS.Types.Region
-- Likewise with RRType

-- FIXME: kinesis features "non_aggregate_keys" in pagination, what is it?

-- FIXME: kinesis pagination (DescribeStream) not correctly splitting python expressions
-- (with '.')

-- FIXME: add a way to correctly emit the usual deriving clauses for non-sum types
-- this is also needed so error types can have a show instance

transform :: [Service] -> [Service]
transform = map eval . sort . nub
  where
    eval x =
        let os  = evalState (mapM (operation x) (x^.svcOperations)) mempty
            y   = x & svcOperations .~ os
            z   = y & svcTypes      .~ serviceTypes y
            p o = o & opPagination  %~ fmap (pagination z o)
         in z & svcOperations %~ map p & svcError .~ serviceError z

current :: [Service] -> [Service]
current = mapMaybe latest . groupBy identical
  where
    identical x y = EQ == comparing _svcName x y

    latest [] = Nothing
    latest xs = Just . head $ sortBy (comparing _svcVersion) xs

operation :: Service -> Operation -> State (HashMap Text Text) Operation
operation svc@Service{..} o = do
    let x = o & opService            .~ _svcName
              & opNs                 .~ _svcNs
              & opNamespace          .~ operationNS svc o

    let y = x & opRequest.cmnName    .~ (x^.opName)
              & opRequest.cmnPrefix  .~ casedChars (x^.opName)
              & opResponse.cmnName   .~ (x^.opName <> "Response")
              & opResponse.cmnPrefix .~ casedChars (x^.opName) <> "r"

    rq <- uniquify svc (y^.opRequest.typShape)
    rs <- uniquify svc (y^.opResponse.typShape)
    es <- traverse (uniquify svc) (y^.opErrors)

    let z = y & opRequest.typShape   .~ rq
              & opRequest.rqType     .~ shapeType "operation:" True svc rq
              & opRequest.rqHttp     %~ http rq
              & opResponse.typShape  .~ rs
              & opResponse           %~ response svc
              & opErrors             .~ es

    return z

operationNS :: Service -> Operation -> NS
operationNS svc o = svc^.svcNs.nsRoot <> NS [o^.opName]

response :: Service -> Response -> Response
response svc@Service{..} rs = rs' & rsStyle.~style _svcType rs'
  where
    rs' = rs & rsType.~shapeType "response:" False svc (rs^.typShape)

uniquify :: Service -> Shape -> State (HashMap Text Text) Shape
uniquify svc (renameCommon svc -> s')
    | "Unknown" <- s'^.cmnName         = return s'
    | Just _    <- existingType svc s' = return s'
    | otherwise = case s' of
        SStruct s@Struct{..} -> do
            p  <- go (s^.cmnPrefix) (s^.cmnName) (s^.cmnDirection)
            fs <- traverse (\(k, v) -> (k,) <$> uniquify svc v) _sctFields
            return . SStruct $ s & cmnPrefix.~p & sctFields.~fs
        SList l@List{..} -> do
            i <- uniquify svc _lstItem
            return . SList $ l & lstItem.~i
        SMap m@Map{..} -> do
            k <- uniquify svc _mapKey
            v <- uniquify svc _mapValue
            return . SMap $ m & mapKey.~k & mapValue.~v
        _ -> return s'
  where
    go :: Text -> Text -> Direction -> State (HashMap Text Text) Text
    go cased n d = do
        m <- get

        let upd x = put (Map.insert x n m) >> return x

        if | Map.null m      -> upd cased
           | check n m cased -> upd cased
           | Just x <- resp d cased,   check n m x -> upd x
           | Just x <- firstAcronym n, check n m x -> upd x
           | otherwise       -> go (numericSuffix cased) n d

    resp DResponse x = Just (x <> "r")
    resp _         _ = Nothing

    check n m x =
        case Map.lookup x m of
            Just n' | n == n'   -> True
                    | otherwise -> False
            Nothing             -> True

http :: Shape -> HTTP -> HTTP
http p h = h & hPath %~ map f & hQuery %~ map g
  where
    f (PVar v) = PVar (prefixed p v)
    f c        = c

    g = qpVal %~ fmap (prefixed p)

style :: ServiceType -> Response -> Style
style t rs@Response{..} =
    case t of
        _ | fs == 0     -> SNullary

        Json            -> SJson
        RestJson        -> SJson

        _ | str, hs > 0 -> SBodyHeaders
          | str         -> SBody

          | hs == fs    -> SHeaders

        _ | hs > 0      -> SXmlHeaders
        _ | bdy         -> SXml
        _               -> SXmlCursor
  where
    str = maybe False (view cmnStreaming) (rs^.typPayload)

    bdy = isJust (rs^.typPayload)
    fs  = length (rs^.typFields)
    hs  = length (rs^.typHeaders)

pagination :: Service -> Operation -> Pagination -> Pagination
pagination svc o p = case p of
    More m t -> More (labeled (rs^.cmnName) m) (map token t)
    Next r t -> Next (labeled (rs^.cmnName) r) (token t)
  where
    token t = t
        & tokInput  %~ labeled (rq^.cmnName)
        & tokOutput %~ labeled (rs^.cmnName)

    rq = o^.opRequest.typShape
    rs = o^.opResponse.typShape

    types = shapeType "pagination:" True svc rq
          : shapeType "pagination:" False svc rs
          : _svcTypes svc

    labeled _ Empty        = Empty
    labeled x (Keyed  y)   = Keyed  (applied x y)
    labeled x (Index  y z) = Index  (applied x y) (labeled (indexed x y) z)
    labeled x (Apply  y z) = Apply  (applied x y) (labeled y z)
    labeled x (Choice y z) = Choice (labeled x y) (labeled x z)

    indexed x y =
        let t = getType x
            f = getField y (_typFields t)
          in Text.init . Text.tail . fst . typeOf $ f^.ann

    applied x y =
        let t = getType x
            f = getField y (_typFields t)
         in _fldPrefixed f

    getType x =
        fromMaybe (error $ "Missing type: " ++ show (x, map (view cmnName) types))
                  (find ((x ==) . view cmnName) types)

    getField y z =
        fromMaybe (error $ "Missing field: " ++ show y)
                  (find ((y ==) . _fldName) z)

serviceNamespaces :: Service -> [NS]
serviceNamespaces s = sort $ (s^.svcNs.nsTypes)
    : map _opNamespace (_svcOperations s)

shapeType :: Text -> Bool -> Service -> Shape -> Type'
shapeType frm rq svc@Service{..} s = defaultType shape
    & typAnn      .~ annOf rq svc shape
    & typPayload  .~ bdy
    & typFields   .~ fs
    & typHeaders  .~ hs
  where
    bdy = listToMaybe $ filter ((== LBody) . view cmnLocation) fs
    hs  = filter ((== LHeader) . view cmnLocation) fs
    fs  = map (requireField overrides . upd) (fieldsOf frm rq svc shape)

    overrides = fromMaybe [] $ Map.lookup (CI.mk name) (svc^.tRequired)

    upd f | f^.cmnLocation == LBody
          , f^.cmnStreaming = f & cmnRequired.~True & anRequired.~True
          | otherwise       = f

    shape = ignoreFields (svc^.fIgnored) s
    name  = s^.cmnName

fieldsOf :: Text -> Bool -> Service -> Shape -> [Field]
fieldsOf frm rq svc s = map f (shapesOf s)
  where
    f :: (Text, Shape) -> Field
    f (k, v) = Field k p (shapeType ("fieldsOf: " <> frm) rq svc val)
      where
        val | CI.mk k `elem` (svc^.fRequired) = v & cmnRequired.~True
            | otherwise = v

        p = prefixed s (fromMaybe k (prefix v))

        prefix (SList x) = x^.lstItem.cmnXmlName
        prefix x         = x^.cmnXmlName

shapesOf :: Shape -> [(Text, Shape)]
shapesOf s = case s of
    SStruct Struct{..} -> _sctFields
    SMap    Map{..}    -> [(_mapKey^.cmnName, _mapKey), (_mapValue^.cmnName, _mapValue)]
    SList   List{..}   -> [(_lstItem^.cmnName, _lstItem)]
    _                  -> []

prefixed :: HasCommon a => a -> Text -> Text
prefixed p = accessor . mappend (p^.cmnPrefix) . upperFirst

annOf :: Bool -> Service -> Shape -> Ann
annOf rq svc s = def
    & anRaw'     .~ isRaw
    & anCtor     .~ ctorOf s
    & anWrap     .~ isWrapped
    & anMonoid   .~ isMonoid s
    & anDefault  .~ isDefault s
    & anRequired .~ req
    & anStrict   .~ (isStrict s && s^.cmnRequired)
    & anDeriving .~ derivingOf s
  where
    (isRaw, isWrapped) = case s of
        _ | Just x <- renameName svc (s^.cmnName) -> (x, False)
        _ | Just x <- existingType svc s          -> (x, False)

        SStruct _ -> (name, False)

        SList l
            | l^.lstMinLength > 0
            -- This seems to be an incorrect assumption, that a list that
            -- is 'required' is equivalient to min_length > 0 || l^.cmnRequired
                -> let (r, w) = ann' (_lstItem l)
                    in ("List1 " <> parens w r, True)

        SList l -> ("[" <> raw (_lstItem l) <> "]", False)

        SMap  m ->
            let (kr, kw) = ann' (_mapKey m)
                (vr, vw) = ann' (_mapValue m)
             in ("Map " <> parens kw kr <> " " <> parens vw vr, True)

        SSum _
            | switch      -> ("Switch " <> name, True)
            | otherwise   -> (name, False)

        SPrim p
            | body, rq   -> ("RqBody", False)
            | body       -> ("RsBody", False)
            | otherwise  -> (formatPrim svc p, False)

    raw    = fst . ann'
    ann' x = let y = annOf rq svc x in (_anRaw' y, _anWrap y)

    switch = name `elem` switches

    name = s^.cmnName
    req  = body || s^.cmnRequired
    body = isBody s

derivingOf :: Shape -> [Derive]
derivingOf = sort . foldl' (flip delete) defaultDeriving . go
  where
    go x = nub . neq . nord $ concatMap (go . snd) (shapesOf x)
      where
        neq  = add (x^.cmnStreaming) [DEq, DOrd]
        nord = add (not (isn't _SMap x)) [DOrd]

        add True  = mappend
        add False = const id

typeOf :: Ann -> (Text, Text)
typeOf Ann{..}
    | _anMonoid       = (raw, _anRaw')
    | not _anRequired = let x = "Maybe " <> raw in (parens True x, x)
    | otherwise       = (raw, _anRaw')
  where
    raw = parens _anWrap _anRaw'

parens :: (IsString m, Monoid m) => Bool -> m -> m
parens True  x = "(" <> x <> ")"
parens False x = x

isStrict :: Shape -> Bool
isStrict (SPrim p) = _prmType p `elem` [PInteger, PDouble, PBool]
isStrict _         = False

isBody :: HasCommon a => a -> Bool
isBody s = s^.cmnLocation == LBody && s^.cmnStreaming

existingType :: HasCommon a => Service -> a -> Maybe Text
existingType svc x = Map.lookup (CI.mk (x^.cmnName)) (svc^.tExisting)

renameCommon :: HasCommon a => Service -> a -> a
renameCommon svc c = c &
    case (svc^.svcName, c^.cmnName) of
        -- FIXME: Special case for erroneous service model types
        ("EC2", "String") -> cmnName.~"VirtualizationType"
        (_,     n)        -> cmnName.~fromMaybe n (renameName svc n)

renameName :: Service -> Text -> Maybe Text
renameName svc x
    | x `elem` core = Just (x `Text.snoc` '\'')
    | otherwise     = Map.lookup (CI.mk x) (svc^.tRename)
  where
    core =
        [ "Source"
        , "Endpoint"
        , "Service"
        ]

formatPrim :: Service -> Prim -> Text
formatPrim Service{..} Prim{..} = Text.pack $
    case _prmType of
        PUTCTime               -> show _svcTimestamp
        PByteString
            | _svcType == Json -> "Base64"
        _                      -> drop 1 (show _prmType)

switches :: [Text]
switches =
    [ "BucketVersioningStatus"
    , "ExpirationStatus"
    , "MFADelete"
    , "MFADeleteStatus"
    ]

ctorOf :: Shape -> Ctor
ctorOf s =
    case s of
        SStruct Struct{..}
            | length _sctFields == 1     -> CNewtype
            | null _sctFields            -> CNullary
        SSum{}
            | (s^.cmnName) `elem` switches -> CSwitch
            | otherwise                      -> CSum
        _                                    -> CData

isDefault :: Shape -> Bool
isDefault s =
    case s of
        SStruct {} -> False
        SList   l  -> _lstMinLength l < 1
        SMap    {} -> False
        SSum    {} -> False
        SPrim   {} -> False

isMonoid :: Shape -> Bool
isMonoid s =
    case s of
        SStruct {} -> False
        SList   l  -> _lstMinLength l < 1
        SMap    {} -> True
        SSum    {} -> False
        SPrim   {} -> False

setDirection :: Direction -> Shape -> Shape
setDirection d s =
    case s of
        SStruct x@Struct{..} ->
            SStruct (dir x { _sctFields = map (second (setDirection d)) _sctFields })
        SList x@List{..} ->
            SList (dir x { _lstItem = dir _lstItem })
        SMap x@Map{..} ->
            SMap (dir x { _mapKey = dir _mapKey, _mapValue = dir _mapValue })
        SSum x ->
            SSum (dir x)
        SPrim x ->
            SPrim (dir x)
  where
    dir :: HasCommon a => a -> a
    dir = cmnDirection.~d

serviceTypes :: Service -> [Type']
serviceTypes svc@Service{..} = sort
    . Map.elems
    . types
    $ execState (shapes _svcOperations) mempty
  where
    types :: HashMap Text Shape -> HashMap Text Type'
    types = Map.map (override . shapeType "serviceTypes:" True svc)

    override :: Type' -> Type'
    override t
        | null candidates = t
        | otherwise       = t & typFields %~ map (requireField candidates)
      where
        candidates = fromMaybe [] $
            Map.lookup (CI.mk (t^.cmnName)) (svc^.tRequired)

    shapes :: [Operation] -> State (HashMap Text Shape) ()
    shapes = mapM_ mergeCommon . mapMaybe exclude . concatMap accessors

    exclude :: Shape -> Maybe Shape
    exclude x = maybe (Just x) (const Nothing) (existingType svc x)

    accessors :: Operation -> [Shape]
    accessors o =
           descend svc (_opRequest  o^.typShape)
        ++ descend svc (_opResponse o^.typShape)
        ++ concatMap (descend svc) (_opErrors o)

mergeCommon :: HasCommon a => a -> State (HashMap Text a) ()
mergeCommon x = modify (Map.insertWith merge (x^.cmnName) x)
  where
    merge b a = b
        & cmnXmlName       %~ (<|> a^.cmnXmlName)
        & cmnLocationName  %~ (<|> a^.cmnLocationName)
        & cmnDocumentation %~ (<|> a^.cmnDocumentation)
        & cmnDirection    <>~ (a^.cmnDirection)

descend :: Service -> Shape -> [Shape]
descend svc s' = case s' of
    SStruct Struct{..} -> concatMap (flat . snd) _sctFields
    _                  -> []
  where
    flat s@SStruct {}     = s : descend svc s
    flat (SList List{..}) = flat _lstItem
    flat (SMap Map{..})   = flat _mapKey ++ flat _mapValue
    flat (SSum x)         = [SSum (switch x)]
    flat _                = []

    switch s@Sum{..}
        | (s^.cmnName) `elem` switches = s { _sumValues = ss }
        | otherwise                    = s { _sumValues = es }
      where
        es = enumPairs svc (s^.cmnName) (Map.elems _sumValues)

        ss = Map.fromList . map f $ Map.toList _sumValues

        f (_, "Enabled") = ("Enabled",  "Enabled")
        f (_, v)         = ("Disabled", v)

enumPairs :: Service -> Text -> [Text] -> HashMap Text Text
enumPairs svc@Service{..} n =
    Map.fromList . map trans . filter (not . Text.null)
  where
    trans = first (mappend (reserve n) . rules) . join (,)

    reserve x = fromMaybe x $ Map.lookup (CI.mk x) (svc^.tPrefixed)

    rules x =
        let y  = Text.replace ":" ""
               . Text.replace "." " "
               . Text.replace "/" " "
               . Text.replace "(" " "
               . Text.replace ")" " "
               . Text.replace "_" " "
               $ Text.replace "-" " " x
            zs = Text.words y

         in if | length zs > 1      -> Text.concat (map Text.toTitle zs)
               | Text.all isUpper y -> Text.toTitle y
               | otherwise          -> upcase y

    upcase x
        | Text.null x = x
        | otherwise   = toUpper (Text.head x) `Text.cons` Text.tail x

serviceError :: Service -> Error
serviceError svc@Service{..} =
    Error (unAbbrev _svcName <> "Error") (es ++ cs) ts
  where
    ts = Map.fromList
         $ map (bimap (view cmnName) custom . join (,)) cs
        ++ map (bimap (view cmnName) (shapeType "serviceError:" True svc) . join (,)) es

    cs = [ except "Serializer" "String"
         , except "Client"     "HttpException"
         , except "Service"    "String"
         ]

    es = nub (concatMap _opErrors _svcOperations)

    custom s = shapeType "custom:" True svc s & anCtor.~CError

    except k v = SStruct (Struct [(v, field)] ctor)
      where
        field = SStruct . Struct mempty $ def
            & cmnName.~v
            & cmnRequired.~True

        ctor = def & cmnName.~unAbbrev _svcName <> k

requireField :: [CI Text] -> Field -> Field
requireField cs f
    | CI.mk (_fldName f) `notElem` cs = f
    | otherwise = f & cmnRequired.~True & anRequired.~True

ignoreFields :: [CI Text] -> Shape -> Shape
ignoreFields cs (SStruct s) =
    SStruct (s & sctFields %~ filter (\(k, _) -> CI.mk k `notElem` cs))
ignoreFields _ s = s
