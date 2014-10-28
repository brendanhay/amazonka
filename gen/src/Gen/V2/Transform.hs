{-# LANGUAGE DataKinds         #-}
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

module Gen.V2.Transform where

import           Control.Applicative        ((<$>), (<*>), pure)
import           Control.Error
import           Control.Lens               hiding (transform, op)
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import           Data.SemVer                (initial)
import           Data.Text                  (Text)
import qualified Gen.V2.Stage1              as S1
import           Gen.V2.Stage1              hiding (Operation)
import           Gen.V2.Stage2
import           Gen.V2.Types

transformS1ToS2 :: Stage1 -> Stage2
transformS1ToS2 s1 = Stage2 cabal serviceModule ops typesModule
  where
    cabal = Cabal
        { _cLibrary      = endpointPrefix
        , _cVersion      = initial
        , _cSynopsis     = ""
        , _cDescription  = ""
        , _cDependencies = []
        , _cModules      =
              serviceModule ^. mNamespace
            : typesModule   ^. mNamespace
            : operationNamespaces
        }

    serviceModule = Mod
        { _mModule    = service
        , _mNamespace = namespace [unAbbrev abbrev]
        , _mImports   =
              typesModule ^. mNamespace
            : operationNamespaces
        }

    service = Service
        { _svName           = s1 ^. mServiceFullName
        , _svAbbrev         = abbrev
        , _svVersion        = version
        , _svDocumentation  = documentation (s1 ^. s1Documentation)
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

    operationNamespaces = map (view mNamespace) (Map.elems ops)

    abbrev = maybeAbbrev (s1 ^. mServiceFullName) (s1 ^. mServiceAbbreviation)

    (ops, ts) = transform abbrev s1

    version = s1 ^. mApiVersion

    endpointPrefix = s1 ^. mEndpointPrefix

    endpoint = maybe Regional (const Global) (s1 ^. mGlobalEndpoint)

    checksum = fromMaybe SHA256 (s1 ^. mChecksumFormat)

    xmlNamespace = "https://"
        <> endpointPrefix
        <> ".amazonaws.com/doc/"
        <> version
        <> "/"

transform :: Abbrev
          -> Stage1
          -> (HashMap Text (Mod Operation), HashMap Text Data)
transform a s1 = runState (Map.traverseWithKey (const f) (s1 ^. s1Operations)) s
  where
    f = operation a (s1 ^. mProtocol)
    s = dataTypes (s1 ^. s1Shapes)

operation :: Abbrev
          -> Protocol
          -> S1.Operation
          -> State (HashMap Text Data) (Mod Operation)
operation a p o = op <$> request (o ^. oInput) <*> response (o ^. oOutput)
  where
    op rq rs = Mod
        { _mModule = Operation
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

    request :: Maybe Ref -> State (HashMap Text Data) (Named Request)
    request = fmap (fmap Request) . go

    response :: Maybe Ref -> State (HashMap Text Data) (Named Response)
    response r = fmap (Response w k) <$> go r
      where
        w = fromMaybe False (join (_refWrapper <$> r))
        k = join (_refResultWrapper <$> r)

    go Nothing  = return (Named "Empty" Empty)
    go (Just x) = do
        let k = x ^. refShape
        m <- gets (^. at k)
        case m of
            Nothing -> return (Named k Empty)
            Just d  -> do
                modify (Map.delete k)
                return (Named k d)

dataTypes :: HashMap Text S1.Shape -> HashMap Text Data
dataTypes m = evalState run mempty
  where
    run = Map.fromList . catMaybes <$> mapM (uncurry struct) (Map.toList m)

    struct k = \case
        Struct' s -> Just . (k,) <$> solve s
        _         -> return Nothing

    solve SStruct{..} = do
        fs <- forM (ordMap _scMembers) $ \(k, r) -> do
            t <- required _scRequired k <$> ref r
            return (k, Typed t (Field (r ^. refLocation) (r ^. refLocationName)))

        return $ case fs of
            []       -> Empty
            [(n, f)] -> Newtype (Named n f)
            _        -> Record  (map (uncurry Named) fs)

    required (fromMaybe [] -> rs) k x =
        let f = if k `elem` rs then id else TMaybe
         in case x of
                TType{} -> f x
                TPrim{} -> f x
                _       -> x

    prop :: Named S1.Shape -> State (HashMap Text Type) Type
    prop (Named k s) =  do
        x <- gets (Map.lookup k)
        maybe (go >>= ins k)
              return
              x
      where
        go = case s of
            Struct' _ -> pure (TType k)
            List'   x -> list x <$> ref (x ^. lstMember)
            Map'    x -> TMap   <$> ref (x ^. mapKey) <*> ref (x ^. mapValue)
            String' x -> pure (TPrim PText)
            Int'    x -> pure (TPrim PInt)
            Long'   x -> pure (TPrim PInteger)
            Double' x -> pure (TPrim PDouble)
            Bool'   _ -> pure (TPrim PBool)
            Time'   x -> pure (TPrim . PTime $ defaultTS (x ^. tsTimestampFormat))
            Blob'   x
                -- | Just True <- x ^. refStreaming
                --             -> pure (TBody True)
                | otherwise -> pure (TPrim PBlob)

        list SList{..}
            | fromMaybe 0 _lstMin > 0 = TList1
            | otherwise               = TList

    ref :: Ref -> State (HashMap Text Type) Type
    ref r = do
        let k = r ^. refShape
            t = TType k
        x <- gets (Map.lookup k)
        maybe (maybe (ins k t >> return t)
                     (prop . Named k)
                     (Map.lookup k m))
              return
              x

    ins :: Text -> Type -> State (HashMap Text Type) Type
    ins k t = modify (Map.insert k t) >> return t

trimS2 :: Stage2 -> Stage2
trimS2 = id
