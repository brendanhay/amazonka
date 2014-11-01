{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
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
transformS1ToS2 m s1 = Stage2 cabal serviceModule ops typesModule
  where
    cabal = Cabal
        { _cLibrary      = s1 ^. s1Library
        , _cVersion      = initial
        , _cSynopsis     = ""
        , _cDescription  = ""
        , _cDependencies = []
        , _cModules      = sort $
              serviceModule ^. mNamespace
            : typesModule   ^. mNamespace
            : operationNamespaces
        }

    serviceModule = Mod
        { _mModule    = service
        , _mNamespace = namespace [unAbbrev abbrev]
        , _mImports   = sort $
              typesModule ^. mNamespace
            : operationNamespaces
        }

    service = Service
        { _svName           = s1 ^. mServiceFullName
        , _svAbbrev         = abbrev
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

    typesModule = Mod
        { _mModule    = ts
        , _mNamespace = typesNS abbrev
        , _mImports   = []
        }

    operationNamespaces = sort (map (view mNamespace) ops)

    abbrev = maybeAbbrev (s1 ^. mServiceFullName) (s1 ^. mServiceAbbreviation)

    (ops, Map.filter (/= Empty) -> ts) = types (m ^. mOverrides) abbrev s1

    version = s1 ^. mApiVersion

    endpointPrefix = s1 ^. mEndpointPrefix

    endpoint = maybe Regional (const Global) (s1 ^. mGlobalEndpoint)

    checksum = fromMaybe SHA256 (s1 ^. mChecksumFormat)

    xmlNamespace = "https://"
        <> endpointPrefix
        <> ".amazonaws.com/doc/"
        <> version
        <> "/"

types :: HashMap Text Override
      -> Abbrev
      -> Stage1
      -> ([Mod (Named Operation)], HashMap Text Data)
types os a s1 = runState run s
  where
    run = Map.elems <$> Map.traverseWithKey f (s1 ^. s1Operations)

    f = operation a (s1 ^. mProtocol)
    s = prefixes (overrides os (datas (s1 ^. s1Shapes)))

prefixes :: HashMap Text Data -> HashMap Text Data
prefixes m = evalState (Map.fromList <$> mapM run (Map.toList m)) mempty
  where
    run (k, x) = (k,) <$> go (prefix k) x

    prefix k = Text.toLower (fromMaybe (Text.take 1 k) (toAcronym k))

    go :: MonadState (HashSet Text) m => Text -> Data -> m Data
    go k v1
        | Nullary{} <- v1 = return $! mapNames (enumName k) v1
        | otherwise       = do
            let v2 = mapNames (mappend k) v1
                fs = Set.fromList (fields v2)
            p <- gets (Set.null . Set.intersection fs)
            if p
                then modify (mappend fs) >> return v2
                else go (numericSuffix k) v1

    fields (Newtype f)  = [f ^. nameOf]
    fields (Record  fs) = map (view nameOf) fs
    fields (Nullary fs) = map (view nameOf) fs
    fields Empty        = []

operation :: Abbrev
          -> Protocol
          -> Text
          -> S1.Operation
          -> State (HashMap Text Data) (Mod (Named Operation))
operation a p n o = op <$> request (o ^. oInput) <*> response (o ^. oOutput)
  where
    op rq rs = Mod
        { _mModule = Named n $ Operation
            { _opDocumentation    = documentation (o ^. oDocumentation)
            , _opDocumentationUrl = o ^. oDocumentationUrl
            , _opMethod           = o ^. oHttp.hMethod
            , _opUri              = o ^. oHttp.hRequestUri
            , _opRequest          = rq
            , _opResponse         = rs
            }
        , _mNamespace = operationNS a (o ^. oName)
        , _mImports   = [requestNS p, typesNS a]
        }

    request = fmap (fmap Request) . go True

    response r = fmap (Response w k) <$> go False r
      where
        w = fromMaybe False (join (_refWrapper <$> r))
        k = join (_refResultWrapper <$> r)

    go _  Nothing  = return (Named "Empty" Empty)
    go rq (Just x) = do
        let k = x ^. refShape
        m <- gets (^. at k)
        case m of
            Nothing -> return (Named k Empty)
            Just d  -> do
                modify (Map.delete k)
                return (Named k (setStreaming rq d))

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
--              . ignored  (o ^. oIgnored)
              . renamed  (o ^. oRenamed)

    -- Types:

    renameTo :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    renameTo _ Nothing  m = m
    renameTo x (Just y) m = replaced x (Just y) $
        maybe m (\z -> Map.delete x (Map.insert y z m))
                (Map.lookup x m)

    replacedBy :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    replacedBy x y = Map.filterWithKey (const . (/= x)) . replaced x y

    replaced :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    replaced _ Nothing  = id
    replaced x (Just y) = Map.map (mapFields (typeOf %~ transform go))
      where
        go :: Type -> Type
        go (TType z)
            | z == y = TType x
        go z         = z

    sumPrefix :: Text -> Maybe Text -> HashMap Text Data -> HashMap Text Data
    sumPrefix _ Nothing  = id
    sumPrefix x (Just y) = Map.adjust (mapFields (nameOf %~ mappend y)) x

    -- Fields:

    required :: HashSet (CI Text) -> Ann a -> Ann a
    required s f
        | Set.member (nameCI f) s
        , TMaybe t <- f ^. typeOf = f & typeOf .~ t
        | otherwise               = f

    ignored :: HashSet (CI Text) -> Named a -> Maybe (Named a)
    ignored s n
        | Set.member (nameCI n) s = Nothing
        | otherwise               = Just n

    renamed :: HashMap (CI Text) Text -> Named a -> Named a
    renamed m = nameOf %~ (\n -> fromMaybe n (Map.lookup (CI.mk n) m))

datas :: HashMap Text S1.Shape -> HashMap Text Data
datas m = evalState (Map.traverseWithKey solve m) mempty
  where
    solve :: Text -> S1.Shape -> State (HashMap Text Type) Data
    solve k = \case
        Struct' x -> go <$> mapM (field pay req) (ordMap (x ^. scMembers))
          where
            pay = x ^. scPayload
            req = fromMaybe [] (x ^. scRequired)

        String' x
            | Just xs <- x ^. strEnum ->
                return $! Nullary (map (\t -> Named t (Branch t)) xs)

        _         -> return Empty
      where
        go []       = Empty
        go [(n, f)] = Newtype (Named n f)
        go fs       = Record  (map (uncurry Named) fs)

        -- wrapped l n = return
        --     . Newtype
        --     . Named k
        --     . Typed (TPrim PText)
        --     $ Field l n False False
        -- String' x -> wrapped Nothing (x ^. strLocationName)


    field :: Maybe Text
          -> [Text]
          -> (Text, Ref)
          -> State (HashMap Text Type) (Text, Typed Field)
    field pay req (k, r) = do
        t <- require req k <$> ref r

        let l = r ^. refLocation
            n = r ^. refLocationName
            d = r ^. refDocumentation
            p = pay == Just k
            s = fromMaybe False (r ^. refStreaming)

        return (k, Typed t (Field l n p s (Doc <$> d)))

    require :: [Text] -> Text -> Type -> Type
    require req k x
        | k `elem` req = x
        | otherwise    =
            case x of
                TType{} -> TMaybe x
                TPrim{} -> TMaybe x
                _       -> x

    ref :: Ref -> State (HashMap Text Type) Type
    ref r = do
        let k = r ^. refShape
            t = TType k
        x <- gets (Map.lookup k)
        maybe (maybe (insert k t >> return t)
                     (prop . Named k)
                     (Map.lookup k m))
              return
              x

    prop :: Named S1.Shape -> State (HashMap Text Type) Type
    prop (Named k s) =  do
        x <- gets (Map.lookup k)
        maybe (go >>= insert k)
              return
              x
      where
        go = case s of
            Struct' _ -> pure (TType k)
            List'   x -> list x <$> ref (x ^. lstMember)
            Map'    x -> TMap   <$> ref (x ^. mapKey) <*> ref (x ^. mapValue)
            String' _ -> pure (TPrim PText)
            Int'    _ -> pure (TPrim PInt)
            Long'   _ -> pure (TPrim PInteger)
            Double' _ -> pure (TPrim PDouble)
            Bool'   _ -> pure (TPrim PBool)
            Time'   x -> pure (TPrim . PTime $ defaultTS (x ^. tsTimestampFormat))
            Blob'   _ -> pure (TPrim PBlob)

        list SList{..}
            | fromMaybe 0 _lstMin > 0 = TList1
            | otherwise               = TList

    insert :: Text -> Type -> State (HashMap Text Type) Type
    insert k t = modify (Map.insert k t) >> return t
