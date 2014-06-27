{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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

import           Control.Lens
import           Data.Default
import           Data.Function
import           Data.HashMap.Strict       (HashMap)
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

instance IsString Abbrev where
    fromString = abbrev . Text.pack

instance Default Abbrev where
    def = Abbrev "AWS"

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
    { _svcName             :: Abbrev
    , _svcFullName         :: Text
    , _svcNamespace        :: NS
    , _svcVersionNamespace :: NS
    , _svcTypesNamespace   :: NS
    , _svcVersion          :: Version
    , _svcRawVersion       :: Text
    , _svcType             :: ServiceType
    , _svcError            :: Error
    , _svcWrapped          :: Bool
    , _svcSignature        :: Signature
    , _svcDocumentation    :: Doc
    , _svcEndpointPrefix   :: Text
    , _svcGlobalEndpoint   :: Maybe Text
    , _svcXmlNamespace     :: Maybe Text
    , _svcTimestamp        :: Time
    , _svcChecksum         :: Checksum
    , _svcJsonVersion      :: JSONV
    , _svcTargetPrefix     :: Maybe Text
    , _svcOperations       :: [Operation]
    } deriving (Eq, Show, Generic)

instance Ord Service where
    compare a b = f _svcNamespace <> f _svcVersion
      where
        f :: Ord a => (Service -> a) -> Ordering
        f g = compare (g a) (g b)

data Error = Error
    { _erName   :: Text
    , _erShapes :: [Shape]
    , _erCtors  :: HashMap Text Type
    } deriving (Eq, Show, Generic)

data Operation = Operation
    { _opName          :: Text
    , _opService       :: Abbrev
    , _opAlias         :: Maybe Text
    , _opNamespace     :: NS
    , _opImports       :: [NS]
    , _opDocumentation :: Doc
    , _opUrl           :: Maybe Text
    , _opRequest       :: Request
    , _opResponse      :: Response
    , _opErrors        :: [Shape]
    , _opPagination    :: Maybe Pagination
    } deriving (Eq, Show, Generic)

data Request = Request
    { _rqName     :: Text
    , _rqDefault  :: Text
    , _rqPayload  :: Maybe Field
    , _rqFields   :: [Field]
    , _rqRequired :: [Field]
    , _rqHeaders  :: [Field]
    , _rqShape    :: Shape
    , _rqHttp     :: HTTP
    } deriving (Eq, Show, Generic)

data Response = Response
    { _rsName   :: Text
    , _rsFields :: [Field]
    , _rsShape  :: Shape
    } deriving (Eq, Show, Generic)

data Location
    = LUri
    | LQuery
    | LHeader
    | LBody
      deriving (Eq, Ord, Show, Generic)

instance Default Location where
    def = LBody

data Common = Common
    { _cmnName          :: Maybe Text
    , _cmnXmlName       :: Maybe Text
    , _cmnLocation      :: Location
    , _cmnLocationName  :: Maybe Text
    , _cmnRequired      :: Bool
    , _cmnDocumentation :: Maybe Doc
    , _cmnStreaming     :: Bool
    } deriving (Eq, Show, Generic)

instance Ord Common where
    compare a b =
        if _cmnLocation a == LBody
            then GT
            else comparing _cmnLocation a b <> comparing _cmnName a b

instance Default Common where
    def = Common Nothing Nothing def Nothing False Nothing False

data Shape
    = SStruct Struct
    | SList   List
    | SMap    Map
    | SEnum   Enum
    | SPrim   Prim
      deriving (Eq, Show)

instance Default Shape where
    def = SPrim def

-- instance Ord Shape where
--     compare a b =
--         case (a, b) of
--             (SEnum{}, SEnum{}) -> on compare (_cmnName . _shpCommon) a b
--             (SEnum{}, _)       -> LT
--             (_,       SEnum{}) -> GT
--             _                  -> on compare (_cmnName . _shpCommon) a b

data Struct = Struct
    { shpFields    :: HashMap Text Shape
    , _shpCommon   :: Common
    } deriving (Eq, Show, Generic)

data List = List
    { shpItem      :: Shape
    , shpFlattened :: Bool
    , shpMinLength :: Int
    , shpMaxLength :: Int
    , _shpCommon   :: Common
    } deriving (Eq, Show, Generic)

data Map = Map
    { shpKey       :: Shape
    , shpValue     :: Shape
    , _shpCommon   :: Common
    } deriving (Eq, Show, Generic)

data Enum = Enum
    { shpValues    :: HashMap Text Text
    , _shpCommon   :: Common
    } deriving (Eq, Show, Generic)

data Prim = Prim
    { shpType      :: Primitive
    , shpMinLength :: Int
    , shpMaxLength :: Int
    , shpPattern   :: Maybe Text
    , _shpCommon   :: Common
    } deriving (Eq, Show, Generic)

instance Default Prim where
    def = def 0 0 Nothing def

data Primitive
    = PText
    | PInteger
    | PDouble
    | PBool
    | PByteString
    | PUTCTime
      deriving (Eq, Show, Generic)

instance Default Primitive where
    def = PText

data Ann = Ann
   { anRequired :: !Bool
   , anDefault  :: !Bool
   , anType     :: Text
   } deriving (Eq, Show, Generic)

data Field = Field
    { fldType     :: Ann
    , fldPrefixed :: Text
    , fldCommon   :: Common
    } deriving (Eq, Show)

instance Ord Field where
    compare = compare `on` fldCommon

data Ctor
    = CEnum
    | CNewtype
    | CData
    | CNullary
      deriving (Eq, Show, Generic)

data Type = Type
    { typShape  :: Shape
    , typType   :: Ann
    , typCtor   :: Ctor
    , typFields :: [Field]
    } deriving (Show, Generic)

instance Eq Type where
    (==) = (==) `on` (_cmnName . _shpCommon . typShape)

instance Ord Type where
    compare = compare `on` typShape

data HTTP = HTTP
    { _hMethod :: !StdMethod
    , _hPath   :: [PathPart]
    , _hQuery  :: [QueryPart]
    } deriving (Eq, Show, Generic)

instance Default HTTP where
    def = HTTP GET [] []

data PathPart
    = PConst Text
    | PVar   Text
      deriving (Eq, Show)

data QueryPart = QueryPart
    { qpKey :: Text
    , qpVal :: Maybe Text
    } deriving (Eq, Show, Generic)

data Pagination = Pagination
    { pgMoreKey     :: Maybe Text
    , pgLimitKey    :: Maybe Text
    , pgInputToken  :: Text
    , pgOutputToken :: Text
    , pgResultKeys  :: Text
    } deriving (Eq, Show, Generic)

makeLenses ''Service
makeLenses ''Operation
makeLenses ''Request
makeLenses ''Response
makeLenses ''Error
makeLenses ''Common
makeLenses ''Shape
makeLenses ''HTTP
