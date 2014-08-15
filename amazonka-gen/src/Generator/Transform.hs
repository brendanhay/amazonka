{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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

import           Control.Lens         hiding (indexed)
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Char
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as Set
import           Data.List
import           Data.Maybe
import           Data.Monoid          hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Generator.AST
import           Text.EDE.Filters

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
        let os  = evalState (mapM (operation x) (_svcOperations x)) mempty
            y   = x & svcOperations .~ os
            z   = y & svcTypes .~ serviceTypes y
            p o = o & opPagination %~ fmap (pagination z o)
         in z & svcOperations %~ map p

current :: [Service] -> [Service]
current = mapMaybe latest . groupBy identical
  where
    identical x y = EQ == comparing _svcName x y

    latest [] = Nothing
    latest xs = Just . head $ sortBy (comparing _svcVersion) xs

operation :: Service -> Operation -> State (HashSet Text) Operation
operation svc@Service{..} o = do
    rq <- uniquify svc (o ^. opRequest  . rqShape)
    rs <- uniquify svc (o ^. opResponse . rsShape)

    let x = o & opService          .~ _svcName
              & opNamespace        .~ _svcVersionNamespace <> NS [_opName o]
              & opTypesNamespace   .~ _svcTypesNamespace
              & opVersionNamespace .~ _svcVersionNamespace
              & opRequestNamespace .~ "Network.AWS.Request" <> fromString (show _svcType)

        y = x & opRequest.rqShape  .~ rq
              & opRequest          %~ request svc x
              & opResponse.rsShape .~ rs
              & opResponse         %~ response svc x

    return y

uniquify :: Service -> Shape -> State (HashSet Text) Shape
uniquify svc (special svc -> s) = do
    x <- go (s ^. cmnPrefix) (s ^. cmnName)
    case s of
        SStruct c@Struct{..} -> do
            xs <- mapM (uniquify svc) (Map.elems _sctFields)
            let fs = Map.fromList (zip (Map.keys _sctFields) xs)
            return . SStruct $ (c & cmnPrefix .~ x) { _sctFields = fs }
        SList l@List{..} -> do
            i <- uniquify svc _lstItem
            return . SList $ (l & cmnPrefix .~ x) { _lstItem = i }
        SMap m@Map{..} -> do
            k <- uniquify svc _mapKey
            v <- uniquify svc _mapValue
            return . SMap $ (m & cmnPrefix .~ x) { _mapKey = k, _mapValue = v }
        _ -> return s
  where
    go :: Text -> Text -> State (HashSet Text) Text
    go x n = do
        p <- gets (Set.member x)
        if p
            then go (next x n) n
            else modify (Set.insert x) >> return x

    next x n
        | Text.null x = "_a"
        | "_" <- x    = "_a"
        | otherwise   = Text.init x <> succ' n (Text.last x)

    succ' n 'z' = Text.toLower (Text.pack [Text.head n, Text.last n])
    succ' _ c   = Text.singleton (succ c)

special :: Service -> Shape -> Shape
special svc s = f (_svcName svc) (s ^. cmnName) s
  where
    -- Special cases for erroneous service model types
    f "EC2" "String" = cmnName .~ "VirtualizationType"
    f _     _        = id

request :: Service -> Operation -> Request -> Request
request svc o rq = rq
    & rqName          .~ name
    & rqShape         .~ shape
    & rqDefault       .~ lowerFirst name
    & rqHttp          %~ http shape
    & rqFields        .~ fs
    & rqPayload       .~ bdy
    & rqRequired      .~ rs
    & rqHeaders       .~ hs
  where
    bdy = listToMaybe $ filter ((== LBody) . view cmnLocation) fs

    rs = filter (view cmnRequired) fs
    hs = filter ((== LHeader) . view cmnLocation) fs
    fs = map (requireField overrides . upd) . sort $ fields True svc shape

    overrides = fromMaybe [] (Map.lookup name (_svcRequired svc))

    upd f | f ^. cmnLocation == LBody
          , f ^. cmnStreaming         = f & cmnRequired .~ True
          | otherwise                 = f

    shape = rq ^. rqShape & cmnName .~ name
    name  = o ^. opName

http :: Shape -> HTTP -> HTTP
http p h = h & hPath %~ map f & hQuery %~ map g
  where
    f (PVar v) = PVar (prefixed p v)
    f c        = c

    g = qpVal %~ fmap (prefixed p)

response :: Service -> Operation -> Response -> Response
response svc@Service{..} o rs = rs' & rsType .~ responseType _svcType rs'
  where
    rs' = rs
        & rsName    .~ name
        & rsFields  .~ fs
        & rsPayload .~ bdy
        & rsHeaders .~ hs

    bdy = listToMaybe $ filter ((== LBody) . view cmnLocation) fs

    hs = filter ((== LHeader) . view cmnLocation) fs
    fs = map (requireField overrides) . sort $ fields False svc shape

    overrides = fromMaybe [] (Map.lookup name _svcRequired)

    shape = rs ^. rsShape & cmnName .~ name
    name  = o ^. opName <> "Response"

pagination :: Service -> Operation -> Pagination -> Pagination
pagination svc o p = case p of
    More m t -> More (labeled (rs ^. cmnName) m) (map token t)
    Next r t -> Next (labeled (rs ^. cmnName) r) (token t)
  where
    token t = t
        & tokInput  %~ labeled (rq ^. cmnName)
        & tokOutput %~ labeled (rs ^. cmnName)

    rq = o ^. opRequest.rqShape
    rs = o ^. opResponse.rsShape

    types = shapeType True  svc rq : shapeType False svc rs : _svcTypes svc

    labeled _ Empty        = Empty
    labeled x (Keyed  y)   = Keyed  (applied x y)
    labeled x (Index  y z) = Index  (applied x y) (labeled (indexed x y) z)
    labeled x (Apply  y z) = Apply  (applied x y) (labeled y z)
    labeled x (Choice y z) = Choice (labeled x y) (labeled x z)

    indexed x y =
        let t = getType x
            f = getField y (_typFields t)
          in Text.init . Text.tail . _anType $ _fldType f

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

rootNS :: NS -> NS
rootNS (NS []) = NS []
rootNS (NS xs) = NS (init xs)

typeNS :: NS -> NS
typeNS = (<> "Types")

serviceNamespaces :: Service -> [NS]
serviceNamespaces s = sort $
    _svcTypesNamespace s : map _opNamespace (_svcOperations s)

shapeEnums :: Text -> [Text] -> HashMap Text Text
shapeEnums n = Map.fromList . map trans . filter (not . Text.null)
  where
    trans = first (mappend (reserve n) . rules) . join (,)

    reserve x
        | x `elem` unprefixed = ""
        | otherwise           = x

    unprefixed =
        [ "InstanceType"
        ]

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

fields :: Bool -> Service -> Shape -> [Field]
fields rq svc s = case s of
    SStruct Struct{..} -> map f (Map.toList _sctFields)
    _                  -> []
  where
    f :: (Text, Shape) -> Field
    f (k, v) =
        let fld = Field (typeof rq svc v) k (prefixed s k) (v ^. common)
         in if k == "IsTruncated"
                then fld & cmnRequired .~ True & fldType %~ (anRequired_ .~ True)
                else fld

prefixed :: HasCommon a => a -> Text -> Text
prefixed p = mappend (p ^. cmnPrefix) . upperFirst

shapeType :: Bool -> Service -> Shape -> Type
shapeType rq svc s = Type s (typeof rq svc s) (ctorof s) (fields rq svc s)

typeof :: Bool -> Service -> Shape -> Ann
typeof rq svc s = Ann req (defaults s) (monoids s) typ
  where
    typ = case s of
        _ | Just x <- remapType svc s -> x

        SStruct _ -> name
        SList   l -> "[" <> ann (_lstItem l) <> "]"
        SMap    m -> "Map " <> ann (_mapKey m) <> " " <> ann (_mapValue m)

        SSum _ | switch, req -> "Switch " <> name
               | switch      -> "(Switch " <> name <> ")"
               | otherwise   -> name

        SPrim p | body, rq   -> "RqBody"
                | body       -> "RsBody"
                | otherwise  -> formatPrim svc p

    req    = body || s ^. cmnRequired
    switch = name `elem` switches

    name   = s ^. cmnName
    body   = isBody s

    ann    = _anType . typeof rq svc

isBody :: HasCommon a => a -> Bool
isBody s = s ^. cmnLocation == LBody && s ^. cmnStreaming

remapType :: HasCommon a => Service -> a -> Maybe Text
remapType Service{..} x = lookup (x ^. cmnName) $
    case _svcName of
        "S3" ->
            [ ("BucketName",      "BucketName")
            , ("ObjectKey",       "ObjectKey")
            , ("ObjectVersionId", "ObjectVersionId")
            , ("ETag",            "ETag")
            , ("Delimiter",       "Char")
            ]
        "Route53" ->
            [ ("RRType",                  "RecordType")
            , ("ResourceRecordSetRegion", "Region")
            ]
        _ -> []

formatPrim :: Service -> Prim -> Text
formatPrim Service{..} Prim{..} = Text.pack $
    case _prmType of
        PUTCTime -> show _svcTimestamp
        PByteString
            | _svcType == Json -> "Base64"
           -- _svcName `elem` ["Kinesis", "DynamoDB"] -> "Base64"
        _        -> drop 1 (show _prmType)

switches :: [Text]
switches =
    [ "BucketVersioningStatus"
    , "ExpirationStatus"
    , "MFADelete"
    , "MFADeleteStatus"
    ]

ctorof :: Shape -> Ctor
ctorof s =
    case s of
        SStruct Struct{..}
            | Map.size _sctFields == 1       -> CNewtype
            | Map.null _sctFields            -> CNullary
        SSum{}
            | (s ^. cmnName) `elem` switches -> CSwitch
            | otherwise                      -> CSum
        _                                    -> CData

defaults :: Shape -> Bool
defaults s =
    case s of
        SStruct {} -> False
        SList   l  -> _lstMinLength l < 1
        SMap    {} -> False
        SSum    {} -> False
        SPrim   {} -> False

monoids :: Shape -> Bool
monoids s =
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
            SStruct (dir x { _sctFields = Map.map (setDirection d) _sctFields })
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
    dir = cmnDirection .~ d

serviceTypes :: Service -> [Type]
serviceTypes svc@Service{..} = map override
    . sort
    . Map.elems
    . (`execState` mempty)
    . mapM (uniq . shapeType True svc . snd)
    . mapMaybe exclude
    $ concatMap opfields _svcOperations
  where
    override t
        | null candidates = t
        | otherwise       = t & typFields %~ map (requireField candidates)
      where
        candidates = fromMaybe [] (Map.lookup (t ^. cmnName) _svcRequired)

    exclude :: (a, Shape) -> Maybe (a, Shape)
    exclude x = maybe (Just x) (const Nothing) (remapType svc (snd x))

    uniq :: Type -> State (HashMap Text Type) ()
    uniq x = modify $ \m ->
        let n = x ^. cmnName
            y = Map.lookup n m
            z = maybe x (cmnDirection <>~ (x ^. cmnDirection)) y
         in Map.insert n z m

    opfields o =
           descend (_rqShape $ _opRequest  o)
        ++ descend (_rsShape $ _opResponse o)
        ++ concatMap descend (_opErrors o)

    descend (SStruct Struct{..}) = concatMap (uncurry flat) (Map.toList _sctFields)
    descend _                    = []

    flat p s@SStruct {}         = (p, s) : descend s
    flat _ s@(SList  List {..}) = flat (s ^. cmnName) _lstItem
    flat _ s@(SMap   Map  {..}) = flat (s ^. cmnName) _mapKey ++ flat (s ^. cmnName) _mapValue
    flat p (SSum     x)         = [(p, SSum $ rename x)]
    flat _ _                    = []

    rename s
        | (s ^. cmnName) `notElem` switches = s
        | otherwise = s { _sumValues        = f (_sumValues s) }
      where
        f = Map.fromList . map g . Map.toList

        g (_, "Enabled") = ("Enabled", "Enabled")
        g (_, v)         = ("Disabled", v)

serviceError :: Abbrev -> [Operation] -> Error
serviceError a os = Error (unAbbrev a <> "Error") (es ++ cs) ts
  where
    ts = Map.fromList
         $ map (bimap (view cmnName) custom . join (,)) cs
        ++ map (bimap (view cmnName) (shapeType True svc) . join (,)) es

    cs = [ except "Serializer" "String"
         , except "Client"     "HttpException"
         , except "Service"    "String"
         ]

    es = nub (concatMap _opErrors os)

    custom s = Type s (typeof True svc s) CError (fields True svc s)

    svc = defaultService a

    except k v = SStruct (Struct (Map.fromList [("", field)]) ctor)
      where
        field = SStruct . Struct mempty $ def
            & cmnName .~ v
            & cmnRequired .~ True

        ctor = def & cmnName .~ (unAbbrev a <> k)

requireField :: [CI Text] -> Field -> Field
requireField cs f
    | CI.mk (_fldName f) `notElem` cs = f
    | otherwise = f & cmnRequired .~ True & fldType.anRequired_ .~ True
