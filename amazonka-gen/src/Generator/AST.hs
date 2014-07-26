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

import           Control.Applicative
import           Control.Lens
import           Data.Default
import           Data.Function
import           Data.HashMap.Strict       (HashMap)
import           Data.Monoid               hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Util
import           GHC.Generics
import           Network.HTTP.Types.Method

-- FIXME: switch sum type nested access to prisms

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

data Location
    = LUri
    | LQuery
    | LHeader
    | LBody
      deriving (Eq, Ord, Show, Generic)

instance Default Location where
    def = LBody

data Direction
    = DBoth
    | DRequest
    | DResponse
      deriving (Eq, Show, Generic)

instance Default Direction where
    def = mempty

instance Monoid Direction where
    mempty                      = DBoth
    mappend DBoth      _        = DBoth
    mappend _         DBoth     = DBoth
    mappend DRequest  DResponse = DBoth
    mappend DResponse DRequest  = DBoth
    mappend _         b         = b

data Common = Common
    { _cmnName          :: Maybe Text
    , _cmnPrefix        :: Text
    , _cmnXmlName       :: Maybe Text
    , _cmnLocation      :: Location
    , _cmnLocationName  :: Maybe Text
    , _cmnRequired      :: Bool
    , _cmnDocumentation :: Maybe Doc
    , _cmnStreaming     :: Bool
    , _cmnDirection     :: Direction
    } deriving (Eq, Show, Generic)

instance Ord Common where
    compare a b =
           comparing (Down . _cmnRequired) a b
        <> comparing (Down . _cmnLocation) a b
        <> comparing _cmnName a b

instance Default Common where
    def = Common Nothing "_" Nothing def Nothing False Nothing False def

makeClassy ''Common

data Struct = Struct
    { _sctFields    :: HashMap Text Shape
    , _sctCommon    :: Common
    } deriving (Eq, Show, Generic)

instance HasCommon Struct where
    common f x = (\y -> x { _sctCommon = y }) <$> f (_sctCommon x)

data List = List
    { _lstItem      :: Shape
    , _lstFlattened :: Bool
    , _lstMinLength :: Int
    , _lstMaxLength :: Int
    , _lstCommon    :: Common
    } deriving (Eq, Show, Generic)

instance HasCommon List where
    common f x = (\y -> x { _lstCommon = y }) <$> f (_lstCommon x)

data Map = Map
    { _mapKey       :: Shape
    , _mapValue     :: Shape
    , _mapCommon    :: Common
    } deriving (Eq, Show, Generic)

instance HasCommon Map where
    common f x = (\y -> x { _mapCommon = y }) <$> f (_mapCommon x)

data Sum = Sum
    { _sumValues    :: HashMap Text Text
    , _sumCommon    :: Common
    } deriving (Eq, Show, Generic)

instance HasCommon Sum where
    common f x = (\y -> x { _sumCommon = y }) <$> f (_sumCommon x)

data Prim = Prim
    { _prmType      :: Primitive
    , _prmMinLength :: Int
    , _prmMaxLength :: Int
    , _prmPattern   :: Maybe Text
    , _prmCommon    :: Common
    } deriving (Eq, Show, Generic)

instance HasCommon Prim where
    common f x = (\y -> x { _prmCommon = y }) <$> f (_prmCommon x)

instance Default Prim where
    def = Prim def 0 0 Nothing def

data Primitive
    = PText
    | PInteger
    | PDouble
    | PBool
    | PByteString
    | PSource
    | PUTCTime
      deriving (Eq, Show, Generic)

instance Default Primitive where
    def = PText

data Shape
    = SStruct Struct
    | SList   List
    | SMap    Map
    | SSum    Sum
    | SPrim   Prim
      deriving (Eq, Show, Generic)

instance Default Shape where
    def = SPrim def

instance HasCommon Shape where
    common f x = case x of
        SStruct y -> SStruct <$> common f y
        SList   y -> SList   <$> common f y
        SMap    y -> SMap    <$> common f y
        SSum    y -> SSum    <$> common f y
        SPrim   y -> SPrim   <$> common f y

instance Ord Shape where
    compare a b =
        case (a, b) of
            (SSum{}, SSum{}) -> on compare (view cmnName) a b
            (SSum{}, _)      -> LT
            (_,      SSum{}) -> GT
            _                -> on compare (view cmnName) a b

isPrim :: Shape -> Bool
isPrim (SPrim _) = True
isPrim _         = False

data Ann = Ann
   { _anRequired_ :: !Bool
   , _anDefault   :: !Bool
   , _anMonoid    :: !Bool
   , _anType      :: Text
   } deriving (Eq, Show, Generic)

makeLenses ''Ann

data Field = Field
    { _fldType     :: Ann
    , _fldPrefixed :: Text
    , _fldCommon   :: Common
    } deriving (Eq, Show)

makeLenses ''Field

instance Ord Field where
    compare = compare `on` _fldCommon

instance HasCommon Field where
    common = fldCommon

data Ctor
    = CWitness
    | CSwitch
    | CSum
    | CNullary
    | CNewtype
    | CData
      deriving (Eq, Ord, Show, Generic)

data Type = Type
    { _typShape  :: Shape
    , _typType   :: Ann
    , _typCtor   :: Ctor
    , _typFields :: [Field]
    } deriving (Show, Generic)

makeLenses ''Type

instance HasCommon Type where
    common = typShape . common

instance Eq Type where
    (==) = (==) `on` (view cmnName)

instance Ord Type where
    compare a b = on compare _typCtor a b <> on compare _typShape a b

data Error = Error
    { _erName   :: Text
    , _erShapes :: [Shape]
    , _erCtors  :: HashMap Text Type
    } deriving (Eq, Show, Generic)

makeLenses ''Error

data PathPart
    = PConst Text
    | PVar   Text
      deriving (Eq, Show)

data QueryPart = QueryPart
    { qpKey :: Text
    , qpVal :: Maybe Text
    } deriving (Eq, Show, Generic)

data Token = Token
    { _tokInput  :: Text
    , _tokOutput :: Text
    } deriving (Eq, Show, Generic)

makeLenses ''Token

data Pagination = Pagination
    { _pgTokens :: [Token]
    , _pgMore   :: Maybe Text
    } deriving (Eq, Show, Generic)

makeLenses ''Pagination

data HTTP = HTTP
    { _hMethod :: !StdMethod
    , _hPath   :: [PathPart]
    , _hQuery  :: [QueryPart]
    } deriving (Eq, Show, Generic)

instance Default HTTP where
    def = HTTP GET [] []

makeLenses ''HTTP

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

makeLenses ''Request

data RespType
    = RHeaders
    | RXmlMix
    | RXml
    | RBody
      deriving (Eq, Show, Generic)

instance Default RespType where
    def = RXmlMix

data Response = Response
    { _rsName    :: Text
    , _rsType    :: RespType
    , _rsPayload :: Maybe Field
    , _rsFields  :: [Field]
    , _rsHeaders :: [Field]
    , _rsShape   :: Shape
    } deriving (Eq, Show, Generic)

makeLenses ''Response

data Operation = Operation
    { _opName             :: Text
    , _opService          :: Abbrev
    , _opAlias            :: Maybe Text
    , _opNamespace        :: NS
    , _opTypesNamespace   :: NS
    , _opVersionNamespace :: NS
    , _opRequestNamespace :: NS
    , _opDocumentation    :: Doc
    , _opUrl              :: Maybe Text
    , _opRequest          :: Request
    , _opResponse         :: Response
    , _opErrors           :: [Shape]
    , _opPagination       :: Maybe Pagination
    } deriving (Eq, Show, Generic)

makeLenses ''Operation

newtype Cabal = Cabal [Service]
    deriving (Show)

data Service = Service
    { _svcName             :: Abbrev
    , _svcFullName         :: Text
    , _svcNamespace        :: NS
    , _svcVersionNamespace :: NS
    , _svcTypesNamespace   :: NS
    , _svcLensNamespace    :: NS
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

makeLenses ''Service
