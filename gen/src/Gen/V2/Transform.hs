{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

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

import           Control.Applicative        ((<$>))
import           Control.Error
import           Control.Lens               hiding (transform)
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Bifunctor
import qualified Data.ByteString.Lazy       as LBS
import           Data.Function              (on)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Jason                 (eitherDecode')
import           Data.Jason.Types           hiding (object)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.SemVer                (initial)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Gen.V2.Log
import           Gen.V2.Naming
import qualified Gen.V2.Stage1              as S1
import           Gen.V2.Stage1              hiding (Operation, Shape(..))
import           Gen.V2.Stage2
import           Gen.V2.Types
import           System.Directory
import           System.FilePath

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
            : map (view mNamespace) (Map.elems ops)
        }

    serviceModule = Mod
        { _mModule    = service
        , _mNamespace = namespace [abbrev]
        , _mImports   = []
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
        , _svError          = abbrev <> "Error"
        }

    typesModule = Mod
        { _mModule    = ts
        , _mNamespace = namespace [abbrev, "Types"]
        , _mImports   = []
        }

    (ops, ts) = transform abbrev s1

    abbrev = stripAWS $
        fromMaybe (s1 ^. mServiceFullName)
                  (s1 ^. mServiceAbbreviation)

    version = s1 ^. mApiVersion

    endpointPrefix = s1 ^. mEndpointPrefix

    endpoint  = maybe Regional (const Global) (s1 ^. mGlobalEndpoint)

    timestamp = fromMaybe RFC822 (s1 ^. mTimestampFormat)
    checksum  = fromMaybe SHA256 (s1 ^. mChecksumFormat)

    xmlNamespace = "https://"
        <> endpointPrefix
        <> ".amazonaws.com/doc/"
        <> version
        <> "/"

transform :: Text
          -> Stage1
          -> (HashMap Text (Mod Operation), HashMap Text Data)
transform abbrev s1 = runState f s
  where
    f = Map.traverseWithKey (const (operation abbrev)) (s1 ^. s1Operations)
    s = types (s1 ^. s1Shapes)

operation :: Text
          -> S1.Operation
          -> State (HashMap Text Data) (Mod Operation)
operation abbrev o = do
    rq <- go (o ^. oInput)
    rs <- go (o ^. oOutput)
    return $ Mod
        { _mModule = Operation
            { _opDocumentation    = documentation (o ^. oDocumentation)
            , _opDocumentationUrl = o ^. oDocumentationUrl
            , _opMethod           = o ^. oHttp.hMethod
            , _opUri              = o ^. oHttp.hRequestUri
            , _opRequest          = rq
            , _opResponse         = rs
            }
        , _mNamespace = namespace [abbrev, o ^. oName]
        , _mImports   = []
        }
  where
    go Nothing  = return (Named "Empty" Empty)
    go (Just x) = do
        let k = x ^. refShape
        m <- gets (^. at k)
        case m of
            Nothing -> return (Named k Empty)
            Just y  -> do
                modify (Map.delete k)
                return (Named k y)

types :: HashMap Text S1.Shape -> HashMap Text Data
types = Map.fromList . mapMaybe (uncurry go) . Map.toList
  where
    go k = \case
         S1.Structure{..} ->
             let fs = map (second field) (ordMap _shpMembers)
              in Just . (k,) $ case fs of
                     [(n, f)] -> Newtype (Named n f)
                     _        -> Record  (OrdMap fs)

         _ -> Nothing

    field Ref{..} =
         Field { _fType         = Type _refShape
               , _fLocation     = fromMaybe Unknown _refLocation
               , _fLocationName = fromMaybe _refShape _refLocationName
               }


    -- | Map
    --   { _shpKey             :: Ref
    --   , _shpValue           :: Ref
    --   , _shpDocumentation   :: Maybe Text
    --   , _shpMin             :: Maybe Int
    --   , _shpMax             :: Maybe Int
    --   }

    -- | String
    --   { _shpMin             :: Maybe Int
    --   , _shpMax             :: Maybe Int
    --   , _shpDocumentation   :: Maybe Text
    --   , _shpPattern         :: Maybe Text
    --   , _shpEnum            :: Maybe [Text]
    --   , _shpXmlAttribute    :: Maybe Bool
    --   , _shpLocationName    :: Maybe Text
    --   , _shpSensitive       :: Maybe Bool
    --   }

    -- | Integer
    --   { _shpMin             :: Maybe Int
    --   , _shpMax             :: Maybe Int
    --   , _shpDocumentation   :: Maybe Text
    --   , _shpBox             :: Maybe Bool
    --   }

    -- | Long
    --   { _shpMin             :: Maybe Int
    --   , _shpMax             :: Maybe Int
    --   , _shpDocumentation   :: Maybe Text
    --   , _shpBox             :: Maybe Bool
    --   }

    -- | Double
    --   { _shpMin             :: Maybe Int
    --   , _shpMax             :: Maybe Int
    --   , _shpDocumentation   :: Maybe Text
    --   , _shpBox             :: Maybe Bool
    --   }

    -- | Float
    --   { _shpMin             :: Maybe Int
    --   , _shpMax             :: Maybe Int
    --   , _shpDocumentation   :: Maybe Text
    --   , _shpBox             :: Maybe Bool
    --   }

    -- | Boolean
    --   { _shpDocumentation   :: Maybe Text
    --   , _shpBox             :: Maybe Bool
    --   }

    -- | Timestamp
    --   { _shpTimestampFormat :: Maybe Timestamp
    --   , _shpDocumentation   :: Maybe Text
    --   }

    -- | Blob
    --   { _shpSensitive       :: Maybe Bool
    --   , _shpDocumentation   :: Maybe Text
    --   }

    --      Map {..} ->

    --      String{} ->
        

trimS2 :: Stage2 -> Stage2
trimS2 = id
