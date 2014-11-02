{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.V2.Transform
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Transform (transformS1ToS2) where

import           Control.Applicative        ((<$>), (<*>), pure)
import           Control.Error
import           Control.Lens               hiding (op, ignored)
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set
import           Data.List                  (sort)
import           Data.Monoid
import           Data.SemVer                (initial)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Manipulate
import           Debug.Trace
import           Gen.V2.Names
import qualified Gen.V2.Stage1              as S1
import           Gen.V2.Stage1              hiding (Operation)
import           Gen.V2.Stage2
import           Gen.V2.Types

transformS1ToS2 :: Model -> Stage1 -> Stage2
transformS1ToS2 m s1 = Stage2 cabal service ops types
  where
    cabal = Cabal
        { _cLibrary      = s1 ^. s1Library
        , _cVersion      = initial
        , _cSynopsis     = ""
        , _cDescription  = ""
        , _cDependencies = []
        , _cModules      = sort $
            service ^. svNamespace : typesNamespace : operationNamespaces
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
        , _tTypes     = ts
        }

    typesNamespace = typesNS abbrev

    operationNamespaces = sort (map (view opNamespace) ops)

    abbrev = maybeAbbrev (s1 ^. mServiceFullName) (s1 ^. mServiceAbbreviation)

    (ops, filter (/= Empty) -> ts) = dataTypes (m ^. mOverrides) abbrev s1

    version = s1 ^. mApiVersion

    endpointPrefix = s1 ^. mEndpointPrefix

    endpoint = maybe Regional (const Global) (s1 ^. mGlobalEndpoint)

    checksum = fromMaybe SHA256 (s1 ^. mChecksumFormat)

    xmlNamespace = "https://"
        <> endpointPrefix
        <> ".amazonaws.com/doc/"
        <> version
        <> "/"

dataTypes :: HashMap Text Override -> Abbrev -> Stage1 -> ([Operation], [Data])
dataTypes os a s1 = bimap sort (sort . Map.elems) (runState run s)
  where
    run = Map.elems <$> Map.traverseWithKey f (s1 ^. s1Operations)

    f = operation a (s1 ^. mProtocol)
    s = prefixes (overrides os (datas (s1 ^. mProtocol) (s1 ^. s1Shapes)))

prefixes :: HashMap Text Data -> HashMap Text Data
prefixes m = Map.fromList $ evalState (mapM run (Map.toList m)) mempty
  where
    run (k, x) = (k,) <$> go (prefix k) x

    prefix k = Text.toLower (fromMaybe (Text.take 1 k) (toAcronym k))

    go :: Text -> Data -> State (HashSet Text) Data
    go k v1
        | Nullary{} <- v1 = do
            let v2  = mapNames (enumName "") v1
                v3  = mapNames (enumName k)  v2
                fs1 = Set.fromList (fields v2)
                fs2 = Set.fromList (fields v3)

            p1 <- gets (Set.null . Set.intersection fs1)
            p2 <- gets (Set.null . Set.intersection fs2)

            if | p1        -> modify (mappend fs1) >> return v2
               | p2        -> modify (mappend fs2) >> return v3
               | otherwise -> go (numericSuffix k) v3

        | otherwise       = do
            let v2 = mapNames (mappend k) v1
                fs = Set.fromList (fields v2)

            p <- gets (Set.null . Set.intersection fs)

            if | p         -> modify (mappend fs) >> return v2
               | otherwise -> go (numericSuffix k) v1

    fields (Newtype _ f)  = [f ^. nameOf]
    fields (Record  _ fs) = map (view nameOf) fs
    fields (Nullary _ bs) = Map.keys bs
    fields Empty          = []

operation :: Abbrev
          -> Protocol
          -> Text
          -> S1.Operation
          -> State (HashMap Text Data) Operation
operation a p n o = op <$> request (o ^. oInput) <*> response (o ^. oOutput)
  where
    op rq rs = Operation
        { _opName             = n
        , _opService          = a
        , _opProtocol         = p
        , _opNamespace        = operationNS a (o ^. oName)
        , _opImports          = [requestNS p, typesNS a]
        , _opDocumentation    = documentation (o ^. oDocumentation)
        , _opDocumentationUrl = o ^. oDocumentationUrl
        , _opMethod           = o ^. oHttp.hMethod
        , _opUri              = o ^. oHttp.hRequestUri
        , _opRequest          = rq
        , _opResponse         = rs
        }

    request = go Request True

    response r = go (Response w k) False r
      where
        w = fromMaybe False (join (_refWrapper <$> r))
        k = join (_refResultWrapper <$> r)

    go c _  Nothing  = return (c "Empty" Empty)
    go c rq (Just x) = do
        let k = x ^. refShape
        m <- gets (^. at k)
        case m of
            Nothing -> return (c k Empty)
            Just d  -> do
                let d' = setStreaming rq d
                modify (Map.delete k)
                return (c k d')

overrides :: HashMap Text Override -> HashMap Text Data -> HashMap Text Data
overrides = flip (Map.foldlWithKey' run)
  where
    run :: HashMap Text Data -- ^ acc
        -> Text              -- ^ key
        -> Override          -- ^ val
        -> HashMap Text Data
    run r k o =
          renameTo   k (o ^. oRenameTo)
        . replacedBy k (o ^. oReplacedBy)
        . sumPrefix  k (o ^. oSumPrefix)
        . Map.adjust (mapFields field) k
        $ r
      where
        field = required (o ^. oRequired)
              . renamed  (o ^. oRenamed)
              -- . ignored  (o ^. oIgnored)

    -- Types:

    renameTo :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    renameTo _ Nothing  m = m
    renameTo x (Just y) m = replaced x y $
        maybe m (\z -> Map.delete x (Map.insert y z m))
                (Map.lookup x m)

    replacedBy :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    replacedBy _ Nothing  = id
    replacedBy x (Just y) = Map.filterWithKey (const . (/= y)) . replaced x y

    replaced :: Text -> Text -> HashMap Text Data -> HashMap Text Data
    replaced x y = Map.map (mapFields go)
      where
        go :: Field -> Field
        go f | f ^. fShape == x = f & typeOf %~ retype
             | otherwise        = f

        retype :: Type -> Type
        retype (TMaybe _) = TMaybe (TType y)
        retype (TList  _) = TList  (TType y)
        retype (TList1 _) = TList1 (TType y)
        retype (TType  _) = TType y
        retype (TPrim  _) = TType y
        retype z          = error $ "Unsupported retyping of: " ++ show (z, y)

    sumPrefix :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    sumPrefix _ Nothing  = id
    sumPrefix k (Just y) = Map.adjust f k
      where
        f x@Nullary{} = mapNames (mappend y) x
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

        _         -> return Empty
      where
        go []  = Empty
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
