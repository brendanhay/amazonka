{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Applicative  ((<$>))
import           Control.Error
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import           Data.Function        (on)
import           Data.Jason           (eitherDecode')
import           Data.Jason.Types     hiding (object)
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.SemVer          (initial)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Gen.V2.Log
import           Gen.V2.Naming
import           Gen.V2.Stage1
import qualified Gen.V2.Stage1        as S1
import           Gen.V2.Stage2
import           Gen.V2.Types
import           System.Directory
import           System.FilePath

transformS1ToS2 :: Stage1 -> Either String Stage2
transformS1ToS2 s1 = do
    return (Stage2 cabal serviceModule [] typesModule)
  where
    cabal = Cabal
        { _cLibrary      = endpointPrefix
        , _cVersion      = initial
        , _cSynopsis     = ""
        , _cDescription  = ""
        , _cModules      = [serviceModule ^. mNamespace]
        , _cDependencies = []
        }

    serviceModule = Mod
        { _mModule    = service
        , _mNamespace = namespace [abbrev]
        , _mImports   = []
        }

    service = Service
        { _svName           = s1 ^. mServiceFullName
        , _svAbbrev         = abbrev
        , _svVersion        = s1 ^. mApiVersion
        , _svDocumentation  = documentation (s1 ^. s1Documentation)
        , _svProtocol       = s1 ^. mProtocol
        , _svEndpoint       = endpoint
        , _svEndpointPrefix = endpointPrefix
        , _svSignature      = s1 ^. mSignatureVersion
        , _svTimestamp      = timestamp
        , _svChecksum       = checksum
        , _svXmlNamespace   = s1 ^. mXmlNamespace
        , _svTargetPrefix   = s1 ^. mTargetPrefix
        , _svError          = abbrev <> "Error"
        }

    typesModule = Mod
        { _mModule    = []
        , _mNamespace = namespace [abbrev, "Types"]
        , _mImports   = []
        }

    abbrev = stripAWS $
        fromMaybe (s1 ^. mServiceFullName)
                  (s1 ^. mServiceAbbreviation)

    endpointPrefix = s1 ^. mEndpointPrefix

    endpoint  = maybe Regional (const Global) (s1 ^. mGlobalEndpoint)

    timestamp = fromMaybe RFC822 (s1 ^. mTimestampFormat)

    checksum  = fromMaybe SHA256 (s1 ^. mChecksumFormat)

trimS2 :: Stage2 -> Stage2
trimS2 = id
