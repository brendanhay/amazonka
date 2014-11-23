{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.Transform
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Transform (transformS1ToS2) where

import           Control.Applicative        ((<$>), (<*>), (<|>), pure)
import           Control.Arrow              ((&&&))
import           Control.Error
import           Control.Lens               hiding (op, ignored, filtered, indexed)
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.CaseInsensitive       as CI
import           Data.Char
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set
import           Data.List                  (find, sort, group)
import           Data.Monoid                hiding (Product)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Manipulate
import           Gen.Names
import qualified Gen.Stage1                 as S1
import           Gen.Stage1                 hiding (Operation)
import           Gen.Stage2
import           Gen.Types

transformS1ToS2 :: Model -> Stage1 -> Stage2
transformS1ToS2 m s1 = Stage2 cabal service ops types
  where
    cabal = Cabal
        { _cName         = name
        , _cUrl          = url
        , _cLibrary      = overrides ^. oLibrary
        , _cVersion      = overrides ^. oVersion
        , _cDescription  = help
        , _cProtocol     = protocol
        , _cExposed      = sort $
            service ^. svNamespace : typesNamespace : operationNamespaces
        , _cOther        = sort $
            (overrides ^. oOperationsModules) ++ (overrides ^. oTypesModules)
        }

    service = Service
        { _svName           = name
        , _svUrl            = url
        , _svAbbrev         = abbrev
        , _svNamespace      = namespace [unAbbrev abbrev]
        , _svImports        = sort (typesNamespace : operationNamespaces)
        , _svVersion        = version
        , _svDocumentation  = help
        , _svProtocol       = protocol
        , _svEndpoint       = endpoint
        , _svEndpointPrefix = endpointPrefix
        , _svSignature      = s1 ^. mSignatureVersion
        , _svChecksum       = checksum
        , _svXmlNamespace   = xmlNamespace
        , _svTargetPrefix   = s1 ^. mTargetPrefix
        , _svJsonVersion    = s1 ^. mJsonVersion
        , _svError          = errorType protocol abbrev
        }

    types = Types
        { _tNamespace = typesNamespace
        , _tImports   = overrides ^. oTypesModules
        , _tTypes     = filter (not . isVoid) (Map.elems ts)
        , _tShared    = share
        }

    typesNamespace = typesNS abbrev

    operationNamespaces = sort (map (view opNamespace) ops)

    name = "Amazon " <> stripAWS (s1 ^. mServiceFullName)

    url = overrides ^. oUrl

    abbrev   = s1 ^. mServiceAbbreviation
    protocol = s1 ^. mProtocol
    version  = s1 ^. mApiVersion

    (ops, ts, share) = dataTypes overrides abbrev s1

    help = Doc (s1 ^. s1Documentation)

    endpointPrefix = s1 ^. mEndpointPrefix

    overrides = m ^. mOverrides
    endpoint  = maybe Regional (const Global) (s1 ^. mGlobalEndpoint)
    checksum  = fromMaybe SHA256 (s1 ^. mChecksumFormat)

    xmlNamespace
        | Just x <- s1 ^. mXmlNamespace         = Just x
        | protocol `elem` [Query, Xml, RestXml] = Just $
               "http://"
            <> endpointPrefix
            <> ".amazonaws.com/doc/"
            <> version
            <> "/"
        | otherwise                             = Nothing

dataTypes :: Overrides
          -> Abbrev
          -> Stage1
          -> ([Operation], HashMap Text Data, HashSet Text)
dataTypes o a s1 = res (runState run ds)
  where
    res (x, y) = (sort (Map.elems x), y, ss)

    run = Map.traverseWithKey
        (operation a proto url (o ^. oOperationsModules) ss (s1 ^. s1Pagination))
        (s1' ^. s1Operations)

    (s1', prefixed -> ds) = runState (requests s1 ss) datas

    ss = evalState share datas

    share = shared s1
    datas = overriden overrides (shapes proto (s1 ^. s1Shapes))

    proto     = s1 ^. mProtocol
    url       = o ^. oOperationUrl
    overrides = o ^. oOverrides

-- | Insert a new request datatype for any shared input, and update
-- the operations accordingly.
requests :: Stage1 -> HashSet Text -> State (HashMap Text Data) Stage1
requests s1 ss = do
    os <- Map.traverseWithKey go (s1 ^. s1Operations)
    return $! s1 & s1Operations .~ os
  where
    go :: Text -> S1.Operation -> State (HashMap Text Data) S1.Operation
    go n o = do
        rq <- update n (o ^. oInput)
        rs <- update (n <> "Response") (o ^. oOutput)
        return $! o
            & oInput  .~ rq
            & oOutput .~ rs

    update :: Text -> Maybe Ref -> State (HashMap Text Data) (Maybe Ref)
    update _ Nothing            = return Nothing
    update n (Just r) = do
        let k = r ^. refShape
        md <- gets (Map.lookup k)
        case md of
            Just d | Set.member k ss -> copy n r d
            Just d                   -> move n k r d
            Nothing                  -> return Nothing

    move :: Text
         -> Text
         -> Ref
         -> Data
         -> State (HashMap Text Data) (Maybe Ref)
    move n k r d = modify (Map.delete k) >> copy n r d

    copy :: Text
         -> Ref
         -> Data
         -> State (HashMap Text Data) (Maybe Ref)
    copy k r d = do
        modify (Map.insert k (dataRename k d))
        return (Just (r & refShape .~ k))

operation :: Abbrev
          -> Protocol
          -> Text
          -> [NS]
          -> HashSet Text
          -> HashMap Text (Pager ())
          -> Text
          -> S1.Operation
          -> State (HashMap Text Data) Operation
operation a proto base ns ss pgs n o = do
    inp <- request  (o ^. oInput)
    out <- response (o ^. oOutput)
    op inp out <$> pager inp out (Map.lookup n pgs)
  where
    op rq rs pg = Operation
        { _opName             = n
        , _opUrl              = base <> n <> ".html"
        , _opService          = a
        , _opProtocol         = proto
        , _opNamespace        = operationNS a n
        , _opImports          = requestNS proto : typesNS a : ns
        , _opDocumentation    = documentation (o ^. oDocumentation)
        , _opDocumentationUrl = o ^. oDocumentationUrl
        , _opMethod           = o ^. oHttp.hMethod
        , _opRequest          = rq
        , _opResponse         = rs
        , _opPager            = pg
        }

    request = go (\x k s d -> Request proto (prefixURI x d) k s d) True

    prefixURI k d = o ^. oHttp.hRequestUri & uriSegments %~ f
      where
        f (Seg x)                     = Seg x
        f (Var x)
            | Just y <- x `lookup` ls = Var y
            | otherwise               = Var (k <> x)

        ls = fieldLocations d

    response r = go (const (Response proto w k)) False r
      where
        w = fromMaybe False (join (_refWrapper <$> r))
        k = join (_refResultWrapper <$> r)

    rqName = n
    rsName = n <> "Response"

    go :: (Text -> Text -> Bool -> Data -> a)
       -> Bool
       -> Maybe Ref
       -> State (HashMap Text Data) a
    go c rq Nothing  = return $! placeholder c rq
    go c rq (Just x) = do
        let k = x ^. refShape
        m <- gets (^. at k)
        case m of
            Nothing   -> return $! placeholder c rq
            Just Void -> return $! placeholder c rq
            Just d    -> do
                let d' = setStreaming rq d
                    t  = fromMaybe "" (fieldPrefix d')
                    p  = Set.member k ss
                unless p $
                    modify (Map.delete k)
                return $! c t k p d'

    placeholder c True  = c "" rqName False (Empty rqName)
    placeholder c False = c "" rsName False (Empty rsName)

-- | Prefix the nested field access and index notation of a pager by introspecting
-- the state fields.
pager :: Request
      -> Response
      -> Maybe (Pager ())
      -> State (HashMap Text Data) (Maybe (Pager Type))
pager _   _   Nothing   = return Nothing
pager inp out (Just pg) = get >>= go
  where
    go ds = return . Just $!
        case pg of
            More m t -> More (label rs m) (map token t)
            Next r t -> Next (label rs r) (token t)
      where
        ts = Map.fromList [(rq, _rqData inp), (rs, _rsData out)] <> ds

        token (Token _ _ i o) =
            let x = label rq i
                y = label rs o
             in Token (require (_rqData inp) x) (require (_rsData out) y) x y

        rq = _rqName inp
        rs = _rsName out

        require d k
            | Just n <- key k
            , Just f <- find ((n ==) . _fName) (toListOf dataFields d)
                = _fType f
            | otherwise
                = TType "Unknown"

        key (Key    n)   = Just n
        key (Index  n _) = Just n
        key (Apply  n _) = Just n
        key _            = Nothing

        label _ NoKey        = NoKey
        label x (Key    n)   = Key    (applied x n)
        label x (Index  n k) = Index  (applied x n) (label (indexed x n) k)
        label x (Apply  n k) = Apply  (applied x n) (label n k)
        label x (Choice n k) = Choice (label x n)   (label x k)

        applied x n
            | Just d <- Map.lookup x ts
            , Just f <- field n d = _fName f
            | otherwise           = error $
                "Unable to apply field "
                    ++ show n
                    ++ " in datatype "
                    ++ show x
                    ++ "\n"
                    ++ show (Map.keys ts)

        indexed x n
            | Just d         <- Map.lookup x ts
            , Just f         <- field n d
            , Just (TType l) <- listElement (f ^. typeOf) = l
            | otherwise           = error $
                "Unable to index field "
                    ++ show n
                    ++ " in datatype "
                    ++ show x
                    ++ "\n"
                    ++ show (Map.keys ts)

        field :: Text -> Data -> Maybe Field
        field (CI.mk -> k) = find f . toListOf dataFields
          where
            f = (k ==) . CI.mk . Text.dropWhile (not . isUpper) . _fName

-- | Find any datatypes that are shared as operation inputs/outputs.
shared :: Stage1 -> State (HashMap Text Data) (HashSet Text)
shared s1 = do
    xs <- forM ops $ \o ->
        (++) <$> ins (o ^. oInput)
             <*> ins (o ^. oOutput)
    return $! occur (freq (concat xs))
  where
    ops = Map.elems (s1 ^. s1Operations)

    occur = Set.fromList . mapMaybe snd . filter ((> 1) . fst)
    freq  = map (length &&& headMay) . group . sort

    ins :: Maybe Ref -> State (HashMap Text Data) [Text]
    ins Nothing  = return []
    ins (Just r) = do
        let k = r ^. refShape
        md <- gets (Map.lookup k)
        return $! maybe [] (nested k) md

    nested :: Text -> Data -> [Text]
    nested k Nullary{} = []
    nested k d = k : mapMaybe name (toListOf (dataFields . typesOf) d)

    name :: Type -> Maybe Text
    name (TType k) = Just k
    name _         = Nothing

prefixed :: HashMap Text Data -> HashMap Text Data
prefixed m = Map.fromList $ evalState (mapM run (Map.toList m)) mempty
  where
    run (k, x) = (k,) <$> go k (prefix k) x

    prefix k = Text.toLower (fromMaybe def (toAcronym (suffix k)))
      where
        def | Text.length k <= 3 = k
            | otherwise          = Text.take 1 k

    suffix k = fromMaybe k ("Request" `Text.stripSuffix` k)

    names = Set.fromList (Map.keys m)

    go :: Text -> Text -> Data -> State (HashSet Text) Data
    go n k v1
        | Nullary{} <- v1 = do
            let v2 = mapFieldNames (enumName False "") v1
                v3 = mapFieldNames (enumName True  k)  v2
                v4 = mapFieldNames (enumName False n)  v1

                f1 = Set.fromList (fieldNames v2)
                f2 = Set.fromList (fieldNames v3)
                f3 = Set.fromList (fieldNames v4)

            s <- get

            let p1 = Set.null (Set.intersection f1 s)
                p2 = Set.null (Set.intersection f2 s)
                p3 = Set.null (Set.intersection f3 s)

                d1 = Set.null (Set.intersection f1 names)
                d2 = Set.null (Set.intersection f2 names)
                d3 = Set.null (Set.intersection f3 names)

            if | p1, d1    -> modify (mappend f1) >> return v2
               | p2, d2    -> modify (mappend f2) >> return v3
               | p3, d3    -> modify (mappend f3) >> return v4
               | otherwise ->
                   error $ "Unabled to generate enum fields for: " ++ show n

        | otherwise       = do
            let v2 = mapFieldNames (mappend k . upperHead) v1
                fs = Set.fromList (fieldNames v2)

            p <- gets (Set.null . Set.intersection fs)

            if | p         -> modify (mappend fs) >> return v2
               | otherwise -> go n (numericSuffix k) v1

overriden :: HashMap Text Override -> HashMap Text Data -> HashMap Text Data
overriden = flip (Map.foldlWithKey' run)
  where
    run :: HashMap Text Data -- ^ acc
        -> Text              -- ^ key
        -> Override          -- ^ val
        -> HashMap Text Data
    run r k o =
          renameTo   k (o ^. oRenameTo)
        . replacedBy k (o ^. oReplacedBy)
        . sumPrefix  k (o ^. oSumPrefix)
        . Map.adjust (dataFields %~ field) k
        $ r
      where
        field = required (o ^. oRequired)
              . renamed  (o ^. oRenamed)

    -- Types:

    renameTo :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    renameTo _ Nothing  m = m
    renameTo x (Just y) m = replaced x y $
        case Map.lookup x m of
            Nothing -> m
            Just z  -> Map.insert y (ren z) (Map.delete x m)
      where
        ren = \case
            Newtype _ f  -> Newtype y f
            Record  _ fs -> Record  y fs
            Product _ fs -> Product y fs
            Nullary _ m' -> Nullary y m'
            Empty   _    -> Empty   y
            Void         -> Void

    replacedBy :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    replacedBy _ Nothing  = id
    replacedBy x (Just y) = Map.filterWithKey (const . (/= x)) . replaced x y

    replaced :: Text -> Text -> HashMap Text Data -> HashMap Text Data
    replaced x y = Map.map (dataFields . typeOf %~ retype)
      where
        retype :: Type -> Type
        retype = \case
            TPrim      z    -> TPrim      z
            TMaybe     z    -> TMaybe     (retype z)
            TFlatten   z    -> TFlatten   (retype z)
            TSensitive z    -> TSensitive (retype z)
            TCase      z    -> TCase      (retype z)
            TList  e   z    -> TList    e (retype z)
            TList1 e   z    -> TList1   e (retype z)
            TMap   e k v    -> TMap     e (retype k) (retype v)
            THashMap k v    -> THashMap   (retype k) (retype v)

            TType z
                | z == x    -> TType y
                | otherwise -> TType z

    sumPrefix :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    sumPrefix _ Nothing  = id
    sumPrefix k (Just y) = Map.adjust f k
      where
        f x@Nullary{} = mapFieldNames (enumName False y) x
        f x           = x

    required s f
        | Set.member (nameCI f) s
        , TMaybe t <- f ^. typeOf = f & typeOf .~ t
        | otherwise               = f

    renamed m = nameOf %~ (\n -> fromMaybe n (Map.lookup (CI.mk n) m))

shapes :: Protocol -> HashMap Text S1.Shape -> HashMap Text Data
shapes proto m = evalState (Map.traverseWithKey solve $ Map.filter skip m) mempty
  where
    skip (Struct' x)
        | Just True <- x ^. scException = False
        | Just True <- x ^. scFault     = False
    skip _                              = True

    solve :: Text -> S1.Shape -> State (HashMap Text Type) Data
    solve k = \case
        Struct' x -> go <$> mapM (field pay req) (ordMap (x ^. scMembers))
          where
            pay = x ^. scPayload
            req = fromMaybe [] (x ^. scRequired)

        String' x
            | Just xs <- x ^. strEnum ->
                return $! Nullary k (Map.fromList (map (join (,)) xs))

        _         -> return Void
      where
        go []  = Void
        go [f] = Newtype k f
        go fs  = Record  k fs

    field :: Maybe Text
          -> [Text]
          -> (Text, Ref)
          -> State (HashMap Text Type) Field
    field pay req (fld, r) = do
        t <- require req fld <$> ref fld r
        return $ Field
            { _fName          = fld
            , _fShape         = r ^. refShape
            , _fType          = t
            , _fLocation      = location proto (r ^. refStreaming) (r ^. refLocation)
            , _fLocationName  = fromMaybe fld (r ^. refLocationName)
            , _fPayload       = Just fld == pay
            , _fStream        = fromMaybe False (r ^. refStreaming)
            , _fDocumentation = Doc <$> (r ^. refDocumentation)
            }

    require :: [Text] -> Text -> Type -> Type
    require req fld x
        | fld `elem` req = x
        | otherwise      =
            case x of
                TPrim      {} -> TMaybe x
                TType      {} -> TMaybe x
                TSensitive {} -> TMaybe x
                _             -> x

    ref :: Text -> Ref -> State (HashMap Text Type) Type
    ref fld r = do
        let k = r ^. refShape
            t = TType k
        x <- gets (Map.lookup k)
        maybe (maybe (insert k t >> return t)
                     (prop fld r k)
                     (Map.lookup k m))
              return
              x

    prop :: Text -> Ref -> Text -> S1.Shape -> State (HashMap Text Type) Type
    prop fld r k s =  do
        x <- gets (Map.lookup k)
        maybe (go >>= insert k)
              return
              x
      where
        go = case s of
            Struct' _ -> pure (TType k)
            Double' _ -> pure (TPrim PDouble)
            Bool'   _ -> pure (TPrim PBool)
            Time'   x -> pure (TPrim . PTime $ defaultTS (x ^. tsTimestampFormat))
            Blob'   _ -> pure (TPrim PBlob)

            List'   x -> list x <$> ref fld (x ^. lstMember)
            Map'    x -> hmap x <$> ref fld (x ^. mapKey) <*> ref fld (x ^. mapValue)

            String' x
                | Just _    <- x ^. strEnum      -> pure (TType k)
                | Just True <- x ^. strSensitive -> pure (TSensitive (TPrim PText))
                | otherwise                      -> pure (TPrim PText)

            Int' x
                | isNatural x -> pure (TPrim PNatural)
                | otherwise   -> pure (TPrim PInt)

            Long' x
                | isNatural x -> pure (TPrim PNatural)
                | otherwise   -> pure (TPrim PInteger)

        hmap x k' v'
            | r ^. refLocation == Just Headers = THashMap (TCase k') v'
            | proto == Json                    = THashMap k' v'
            | proto == RestJson                = THashMap k' v'
            | otherwise =
                flat flatten (TMap (ann, key, val) k' v')
          where
            ann | fromMaybe False flatten = ent
                | otherwise               = "entry"

            ent = fromMaybe "entry" (r ^. refLocationName)
            key = fromMaybe "key"   (x ^. mapKey   . refLocationName)
            val = fromMaybe "value" (x ^. mapValue . refLocationName)

            flatten = x ^. mapFlattened

        list x = flat flatten . typ ann
          where
            typ | fromMaybe 0 (_lstMin x) > 0 = TList1
                | otherwise                   = TList

            flatten = x ^. lstFlattened
                  <|> x ^. lstMember . refFlattened

            ann = fromMaybe fld $
                    x ^. lstMember . refLocationName
                <|> r ^. refLocationName

    flat :: Maybe Bool -> Type -> Type
    flat (Just True) = TFlatten
    flat _           = id

    insert :: Text -> Type -> State (HashMap Text Type) Type
    insert k t = modify (Map.insert k t) >> return t

errorType :: Protocol -> Abbrev -> Text
errorType p a =
    case (p, a) of
        (_,        Abbrev "EC2") -> unAbbrev a <> "Error"
        (Json,     _)            -> "JSONError"
        (RestJson, _)            -> "JSONError"
        _                        -> "RESTError"
