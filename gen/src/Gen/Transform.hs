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

import           Control.Applicative        ((<$>), (<*>), pure)
import           Control.Error
import           Control.Lens               hiding (op, ignored, filtered)
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Bifunctor
import qualified Data.CaseInsensitive       as CI
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set
import           Data.List                  (sort)
import           Data.Monoid                hiding (Product)
import           Data.SemVer                (initial)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Manipulate
import           Debug.Trace
import           Gen.Names
import qualified Gen.Stage1              as S1
import           Gen.Stage1              hiding (Operation)
import           Gen.Stage2
import           Gen.Types

transformS1ToS2 :: Model -> Stage1 -> Stage2
transformS1ToS2 m s1 = Stage2 cabal service ops types
  where
    cabal = Cabal
        { _cLibrary      = overrides ^. oLibrary
        , _cVersion      = initial
        , _cSynopsis     = ""
        , _cDescription  = ""
        , _cDependencies = []
        , _cExposed      = sort $
            service ^. svNamespace : typesNamespace : operationNamespaces
        , _cOther        = sort $
            (overrides ^. oOperationsModules) ++ (overrides ^. oTypesModules)
        }

    service = Service
        { _svName           = s1 ^. mServiceFullName
        , _svAbbrev         = abbrev
        , _svNamespace      = namespace [unAbbrev abbrev]
        , _svImports        = sort (typesNamespace : operationNamespaces)
        , _svVersion        = version
        , _svDocumentation  = Doc (s1 ^. s1Documentation)
        , _svProtocol       = s1 ^. mProtocol
        , _svEndpoint       = endpoint
        , _svEndpointPrefix = endpointPrefix
        , _svSignature      = s1 ^. mSignatureVersion
        , _svChecksum       = checksum
        , _svXmlNamespace   = fromMaybe xmlNamespace (s1 ^. mXmlNamespace)
        , _svTargetPrefix   = s1 ^. mTargetPrefix
        , _svError          = unAbbrev abbrev <> "Error"
        }

    types = Types
        { _tService   = service
        , _tNamespace = typesNamespace
        , _tImports   = overrides ^. oTypesModules
        , _tTypes     = filter (not . isVoid) (Map.elems ts)
        }

    typesNamespace = typesNS abbrev

    operationNamespaces = sort (map (view opNamespace) ops)

    abbrev = maybeAbbrev (s1 ^. mServiceFullName) (s1 ^. mServiceAbbreviation)

    (ops, ts) = dataTypes overrides abbrev s1

    overrides = m ^. mOverrides

    version = s1 ^. mApiVersion

    endpointPrefix = s1 ^. mEndpointPrefix

    endpoint = maybe Regional (const Global) (s1 ^. mGlobalEndpoint)

    checksum = fromMaybe SHA256 (s1 ^. mChecksumFormat)

    xmlNamespace = "https://"
        <> endpointPrefix
        <> ".amazonaws.com/doc/"
        <> version
        <> "/"

-- errors :: Abbrev -> Stage1 -> HashMap Text Data -> Error
-- errors (unAbbrev -> a) s1 ts = Error (a <> "Error") ds
--   where
--     ds = Map.fromList
--          [ p "Service"    [TType "Status", TType (a <> "Message")]
--          , p "Http"       [TType "HttpException"]
--          , p "Serializer" [TType "String"]
--          ]

--     p k xs = let k' = a <> k in (k', Product k' xs)

    -- es = Map.filterWithKey (const . (`elem` rs)) ts
    -- rs = map _refShape
    --      . concatMap (fromMaybe [] . _oErrors)
    --      . Map.elems
    --      $ s1 ^. s1Operations

dataTypes :: Overrides
          -> Abbrev
          -> Stage1
          -> ([Operation], HashMap Text Data)
dataTypes o a s1 = (sort . Map.elems) `first` runState run s
  where
    run = Map.traverseWithKey f (s1 ^. s1Operations)

    f = operation a (s1 ^. mProtocol) (o ^. oOperationsModules)

    s = prefixes
        . filtered (o ^. oOverrides)
        $ datas (s1 ^. mProtocol) (s1 ^. s1Shapes)

prefixes :: HashMap Text Data -> HashMap Text Data
prefixes m = Map.fromList $ evalState (mapM run (Map.toList m)) mempty
  where
    run (k, x) = (k,) <$> go (prefix k) x

    prefix k = Text.toLower (fromMaybe (Text.take 1 k) (toAcronym k))

    go :: Text -> Data -> State (HashSet Text) Data
    go k v1
        | Nullary{} <- v1 = do
            let v2  = mapFieldNames (enumName "") v1
                v3  = mapFieldNames (enumName k)  v2
                fs1 = Set.fromList (fieldNames v2)
                fs2 = Set.fromList (fieldNames v3)

            p1 <- gets (Set.null . Set.intersection fs1)
            p2 <- gets (Set.null . Set.intersection fs2)

            if | p1        -> modify (mappend fs1) >> return v2
               | p2        -> modify (mappend fs2) >> return v3
               | otherwise -> go (numericSuffix k) v3

        | otherwise       = do
            let v2 = mapFieldNames (mappend k . upperHead) v1
                fs = Set.fromList (fieldNames v2)

            p <- gets (Set.null . Set.intersection fs)

            if | p         -> modify (mappend fs) >> return v2
               | otherwise -> go (numericSuffix k) v1

operation :: Abbrev
          -> Protocol
          -> [NS]
          -> Text
          -> S1.Operation
          -> State (HashMap Text Data) Operation
operation a p ns n o = op <$> request (o ^. oInput) <*> response (o ^. oOutput)
  where
    op rq rs = Operation
        { _opName             = n
        , _opService          = a
        , _opProtocol         = p
        , _opNamespace        = operationNS a (o ^. oName)
        , _opImports          = requestNS p : typesNS a : ns
        , _opDocumentation    = documentation (o ^. oDocumentation)
        , _opDocumentationUrl = o ^. oDocumentationUrl
        , _opMethod           = o ^. oHttp.hMethod
        , _opRequest          = rq
        , _opResponse         = rs
        }

    prefixURI x = o ^. oHttp.hRequestUri & uriSegments.segVars %~ mappend x

    request = go (\x -> Request (prefixURI x)) True

    response r = go (const (Response w k)) False r
      where
        w = fromMaybe False (join (_refWrapper <$> r))
        k = join (_refResultWrapper <$> r)

    go c rq Nothing  = return (type' c rq)
    go c rq (Just x) = do
        let k = x ^. refShape
        m <- gets (^. at k)
        case m of
            Nothing -> return (type' c rq)
            Just d  -> do
                let d' = setStreaming rq d
                    k' = operationName k
                    t  = fromMaybe "" (fieldPrefix d')
                modify (Map.delete k)
                return $! c t k' (renamed k' d')

    type' c True  = c "" n (Empty n)
    type' c False = let k = n <> "Response" in c "" k (Empty k)

    renamed k = \case
        Newtype _ f  -> Newtype k f
        Record  _ fs -> Record  k fs
        x            -> x

filtered :: HashMap Text Override -> HashMap Text Data -> HashMap Text Data
filtered = flip (Map.foldlWithKey' run)
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
              -- . ignored  (o ^. oIgnored)

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
    replaced x y = Map.map (\d -> let p = exists d in d & dataFields %~ go p)
      where
        exists = any (== TType x) . nestedTypes

        go True  = typeOf %~ retype
        go False = id

        retype :: Type -> Type
        retype (TMaybe _) = TMaybe z
        retype (TList  _) = TList  z
        retype (TList1 _) = TList1 z
        retype (TType  _) = z
        retype (TPrim  _) = z
        retype e          = error $ "Unsupported retyping of: " ++ show (e, y)

        z = TType y

    sumPrefix :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    sumPrefix _ Nothing  = id
    sumPrefix k (Just y) = Map.adjust f k
      where
        f x@Nullary{} = mapFieldNames (mappend y) x
        f x           = x

    -- Fields:

--    required :: HashSet (CI Text) -> Ann a -> Ann a
    required s f
        | Set.member (nameCI f) s
        , TMaybe t <- f ^. typeOf = f & typeOf .~ t
        | otherwise               = f

--    ignored :: HashSet (CI Text) -> Named a -> Maybe (Named a)
    ignored s n
        | Set.member (nameCI n) s = Nothing
        | otherwise               = Just n

--    renamed :: HashMap (CI Text) Text -> Named a -> Named a
    renamed m = nameOf %~ (\n -> fromMaybe n (Map.lookup (CI.mk n) m))

datas :: Protocol -> HashMap Text S1.Shape -> HashMap Text Data
datas p m = evalState (Map.traverseWithKey solve m) mempty
  where
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

        -- wrapped l n = return
        --     . Newtype
        --     . Named k
        --     . (TPrim PText)
        --     $ Field l n False False
        -- String' x -> wrapped Nothing (x ^. strLocationName)

    field :: Maybe Text
          -> [Text]
          -> (Text, Ref)
          -> State (HashMap Text Type) Field
    field _ req (k, r) = do
        t <- require req k <$> ref r

        let l = r ^. refLocation
            n = r ^. refLocationName
            d = r ^. refDocumentation
            s = fromMaybe False (r ^. refStreaming)
--            p = pay == Just k

        return $ Field
            { _fName          = k
            , _fShape         = r ^. refShape
            , _fType          = t
            , _fLocation      = location p s l
            , _fLocationName  = fromMaybe k n
            , _fDocumentation = Doc <$> d
            }

    require :: [Text] -> Text -> Type -> Type
    require req k x
        | k `elem` req = x
        | otherwise    =
            case x of
                TPrim      {} -> TMaybe x
                TType      {} -> TMaybe x
                TSensitive {} -> TMaybe x
                _             -> x

    ref :: Ref -> State (HashMap Text Type) Type
    ref r = do
        let k = r ^. refShape
            t = TType k
        x <- gets (Map.lookup k)
        maybe (maybe (insert k t >> return t)
                     (prop k)
                     (Map.lookup k m))
              return
              x

    prop :: Text -> S1.Shape -> State (HashMap Text Type) Type
    prop k s =  do
        x <- gets (Map.lookup k)
        maybe (go >>= insert k)
              return
              x
      where
        go = case s of
            Struct' _ -> pure (TType k)
            List'   x -> list x <$> ref (x ^. lstMember)
            Map'    x -> TMap   <$> ref (x ^. mapKey) <*> ref (x ^. mapValue)
            Int'    _ -> pure (TPrim PInt)
            Long'   _ -> pure (TPrim PInteger)
            Double' _ -> pure (TPrim PDouble)
            Bool'   _ -> pure (TPrim PBool)
            Time'   x -> pure (TPrim . PTime $ defaultTS (x ^. tsTimestampFormat))
            Blob'   _ -> pure (TPrim PBlob)
            String' x
                | fromMaybe False (x ^. strSensitive)
                            -> pure (TSensitive (TPrim PText))
                | otherwise -> pure (TPrim PText)

        list SList{..}
            | fromMaybe 0 _lstMin > 0 = TList1
            | otherwise               = TList

    insert :: Text -> Type -> State (HashMap Text Type) Type
    insert k t = modify (Map.insert k t) >> return t
