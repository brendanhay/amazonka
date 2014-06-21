{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Generator.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict        (HashMap)
import Data.String.CaseConversion
import Data.Text                  (Text)
import GHC.Generics
import System.FilePath

data Model = Model
    { modPath    :: FilePath
    , modVersion :: String
    } deriving (Show)

modelFromPath :: FilePath -> String -> Model
modelFromPath d f = Model (d </> f) (fst $ break (== '.') f)

newtype Abbrev = Abbrev { unAbbrev :: Text }
    deriving (Eq, Show, Generic)

newtype NS = NS { unNS :: Text }
    deriving (Eq, Show, Generic)

newtype Version = Version { unVersion :: Text }
    deriving (Eq, Show, Generic)

newtype Doc = Doc { unDoc :: Text }
    deriving (Eq, Show, Generic)

data Time
    = RFC822
    | ISO8601
      deriving (Eq, Show, Generic)

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show, Generic)

data ServiceType
    = RestXML
    | RestJSON
    | RestS3
    | JSON
    | Query
      deriving (Show, Generic)

data Signature
    = V2
    | V3
    | V4
      deriving (Show, Generic)

data Service = Service
    { svcName           :: Abbrev
    , svcNamespace      :: NS
    , svcFullName       :: Text
    , svcVersion        :: Version
    , svcType           :: ServiceType
    , svcWrapped        :: Bool
    , svcSignature      :: Signature
    , svcDocumentation  :: Doc
    , svcEndpointPrefix :: Text
    , svcGlobalEndpoint :: Maybe Text
    , svcXmlNamespace   :: Maybe Text
    , svcTimestamp      :: Maybe Time
    , svcChecksum       :: Maybe Checksum
    , svcJSONVersion    :: Text
    , svcTargetPrefix   :: Maybe Text
    , svcOperations     :: [Operation]
    } deriving (Show)

data Operation = Operation
    { opName          :: Text
    , opAlias         :: Maybe Text
    , opNamespace     :: NS
    , opDocumentation :: Doc
    , opUrl           :: Maybe Text
    , opInput         :: Request
    , opOutput        :: Response
    , opErrors        :: [Shape]
    , opPagination    :: Maybe Pagination
    } deriving (Show)

newtype Request = Request { unRequest :: Shape }
    deriving (Eq, Show)

newtype Response = Response { unResponse :: Shape }
    deriving (Eq, Show)

data Location
    = LHeader
    | LUri
    | LQuery
    | LBody
      deriving (Eq, Show, Generic)
--    , cPayload       :: Bool -- Mix payload into the location?

data Common = Common
    { cmnName          :: Text
    , cmnXmlName       :: Text
    , cmnLocation      :: Location
    , cmnLocationName  :: Text
    , cmnRequired      :: Bool
    , cmnDocumentation :: Doc
    , cmnStreaming     :: Bool
    } deriving (Eq, Show)

data Shape
    = SStruct
      { shpCommon    :: Common
      , shpFields    :: HashMap Text Shape
      , shpOrder     :: [Text]
      }

    | SList
      { shpCommon    :: Common
      , shpItem      :: Shape
      , shpFlattened :: Bool
      , shpLength    :: Int
      }

    | SMap
      { shpCommon    :: Common
      , shpKey       :: Shape
      , shpValue     :: Shape
      }

    | SEnum
      { shpCommon    :: Common
      , shpValues    :: HashMap Text Text
      }

    | SPrim
      { shpCommon    :: Common
      , shpType      :: Prim
      , shpMinLength :: Int
      , shpMaxLength :: Int
      , shpPattern   :: Text
      }

      deriving (Eq, Show, Generic)

data Prim
    = PText
    | PInteger
    | PDouble
    | PBool
    | PByteString
    | PUTCTime
      deriving (Eq, Show, Generic)

data HTTP = HTTP
    { hMethod :: Text
    , hUri    :: [Part]
    , hQuery  :: HashMap Text (Maybe Text)
    } deriving (Eq, Show, Generic)

data Part
    = T Text
    | I Text
      deriving (Eq, Show)

data Pagination = Pagination
    { pgMoreKey     :: Maybe Text
    , pgLimitKey    :: Maybe Text
    , pgInputToken  :: Text
    , pgOutputToken :: Text
    , pgResultKeys  :: Text
    } deriving (Show, Generic)
