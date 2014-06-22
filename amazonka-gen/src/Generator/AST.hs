{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Generator.AST
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.AST where

import           Data.Default
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Util
import           GHC.Generics

newtype Abbrev = Abbrev { unAbbrev :: Text }
    deriving (Eq, Show, Generic)

abbrev :: Text -> Abbrev
abbrev = Abbrev . mconcat . Text.words . strip "AWS" . strip "Amazon"

newtype NS = NS { unNS :: [Text] }
    deriving (Eq, Show, Generic)

instance Monoid NS where
    mempty      = NS []
    mappend a b = NS (on mappend unNS a b)

instance Default NS where
    def = mempty

namespace :: Abbrev -> Version -> NS
namespace a v = NS
    [ "Network"
    , "AWS"
    , unAbbrev a
    , unVersion v
    ]

newtype Version = Version { unVersion :: Text }
    deriving (Eq, Show, Generic)

version :: Text -> Version
version = Version . mappend "V" . Text.replace "-" "_"

newtype Doc = Doc { unDoc :: Text }
    deriving (Eq, Show, Generic)

documentation :: Text -> Doc
documentation = Doc

instance Default Doc where
    def = Doc mempty

data Time
    = RFC822
    | ISO8601
      deriving (Eq, Show, Generic)

instance Default Time where
    def = ISO8601

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show, Generic)

instance Default Checksum where
    def = SHA256

data ServiceType
    = RestXml
    | RestJson
    | RestS3
    | Json
    | Query
      deriving (Eq, Show, Generic)

instance Default ServiceType where
    def = Query

data Signature
    = V2
    | V3
    | V4
      deriving (Eq, Show, Generic)

newtype JSONV = JSONV { unJSONV :: Text }
    deriving (Eq, Show)

instance Default JSONV where
    def = JSONV "1.0"

data Service = Service
    { svcName           :: Abbrev
    , svcFullName       :: Text
    , svcNamespace      :: NS
    , svcVersion        :: Version
    , svcType           :: ServiceType
    , svcWrapped        :: Bool
    , svcSignature      :: Signature
    , svcDocumentation  :: Doc
    , svcEndpointPrefix :: Text
    , svcGlobalEndpoint :: Maybe Text
    , svcXmlNamespace   :: Maybe Text
    , svcTimestamp      :: Time
    , svcChecksum       :: Checksum
    , svcJSONVersion    :: JSONV
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

      deriving (Eq, Show)

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
    , hPath   :: [PathPart]
    , hQuery  :: [QueryPart]
    } deriving (Eq, Show)

data PathPart
    = PConst Text
    | PVar   Text
      deriving (Eq, Show)

data QueryPart = QueryPart
    { qpKey :: Text
    , kpVal :: Maybe Text
    } deriving (Eq, Show)

data Pagination = Pagination
    { pgMoreKey     :: Maybe Text
    , pgLimitKey    :: Maybe Text
    , pgInputToken  :: Text
    , pgOutputToken :: Text
    , pgResultKeys  :: Text
    } deriving (Show, Generic)
