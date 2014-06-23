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
import           Data.HashMap.Strict       (HashMap)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Util
import           GHC.Generics
import           Network.HTTP.Types.Method

newtype Abbrev = Abbrev { unAbbrev :: Text }
    deriving (Eq, Ord, Show, Generic)

abbrev :: Text -> Abbrev
abbrev = Abbrev . mconcat . Text.words . strip "AWS" . strip "Amazon"

newtype NS = NS { unNS :: [Text] }
    deriving (Eq, Ord, Show, Generic)

instance Monoid NS where
    mempty      = NS []
    mappend a b = NS (on mappend unNS a b)

instance Default NS where
    def = mempty

instance IsString NS where
    fromString = NS . filter (/= "") . Text.split (== '.') . Text.pack

namespace :: Abbrev -> Version -> NS
namespace a v = NS
    [ "Network"
    , "AWS"
    , unAbbrev a
    , unVersion v
    ]

rootNS :: NS -> NS
rootNS (NS []) = NS []
rootNS (NS xs) = NS (init xs)

typeNS :: NS -> NS
typeNS = (<> "Types")

newtype Version = Version { unVersion :: Text }
    deriving (Eq, Ord, Show, Generic)

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

newtype Cabal = Cabal [Service]
    deriving (Show)

data Service = Service
    { svcName             :: Abbrev
    , svcFullName         :: Text
    , svcNamespace        :: NS
    , svcTypesNamespace   :: NS
    , svcVersionNamespace :: NS
    , svcVersion          :: Version
    , svcType             :: ServiceType
    , svcWrapped          :: Bool
    , svcSignature        :: Signature
    , svcDocumentation    :: Doc
    , svcEndpointPrefix   :: Text
    , svcGlobalEndpoint   :: Maybe Text
    , svcXmlNamespace     :: Maybe Text
    , svcTimestamp        :: Time
    , svcChecksum         :: Checksum
    , svcJsonVersion      :: JSONV
    , svcTargetPrefix     :: Maybe Text
    , svcOperations       :: [Operation]
    } deriving (Eq, Show, Generic)

instance Ord Service where
    compare a b = f svcNamespace <> f svcVersion
      where
        f :: Ord a => (Service -> a) -> Ordering
        f g = compare (g a) (g b)

current :: [Service] -> [Service]
current = mapMaybe latest . groupBy identical
  where
    identical x y = EQ == comparing svcName x y

    latest [] = Nothing
    latest xs = Just . head $ sortBy (comparing svcVersion) xs

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
    } deriving (Eq, Show)

data Request = Request
    { rqShape :: Shape
    , rqHttp  :: HTTP
    } deriving (Eq, Show)

newtype Response = Response { unResponse :: Shape }
    deriving (Eq, Show)

data Location
    = LHeader
    | LUri
    | LQuery
    | LBody
      deriving (Eq, Show, Generic)
--    , cPayload       :: Bool -- Mix payload into the location?

instance Default Location where
    def = LBody

data Common = Common
    { cmnName          :: Maybe Text
    , cmnXmlName       :: Maybe Text
    , cmnLocation      :: Location
    , cmnLocationName  :: Maybe Text
    , cmnRequired      :: Bool
    , cmnDocumentation :: Doc
    , cmnStreaming     :: Bool
    } deriving (Eq, Show)

instance Default Common where
    def = Common Nothing Nothing def Nothing False def False

data Shape
    = SStruct
      { shpFields    :: [Shape]
      , shpCommon    :: Common
      }

    | SList
      { shpItem      :: Shape
      , shpFlattened :: Bool
      , shpMinLength :: Int
      , shpMaxLength :: Int
      , shpCommon    :: Common
      }

    | SMap
      { shpKey       :: Shape
      , shpValue     :: Shape
      , shpCommon    :: Common
      }

    | SEnum
      { shpValues    :: HashMap Text Text
      , shpCommon    :: Common
      }

    | SPrim
      { shpType      :: Prim
      , shpMinLength :: Int
      , shpMaxLength :: Int
      , shpPattern   :: Maybe Text
      , shpCommon    :: Common
      }

      deriving (Eq, Show)

instance Default Shape where
    def = SPrim PText 0 0 Nothing def

data Prim
    = PText
    | PInteger
    | PDouble
    | PBool
    | PByteString
    | PUTCTime
      deriving (Eq, Show, Generic)

data HTTP = HTTP
    { hMethod :: !StdMethod
    , hPath   :: [PathPart]
    , hQuery  :: [QueryPart]
    } deriving (Eq, Show)

instance Default HTTP where
    def = HTTP GET [] []

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
    } deriving (Eq, Show, Generic)
