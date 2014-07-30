{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           GHC.Generics
import           Network.HTTP.Types.Method

newtype Abbrev = Abbrev { unAbbrev :: Text }
    deriving (Eq, Ord, Show, Generic)

instance IsString Abbrev where
    fromString = Abbrev . Text.pack

instance Default Abbrev where
    def = Abbrev "AWS"

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

newtype Doc = Doc { unDoc :: Maybe Text }
    deriving (Eq, Show, Generic)

documentation :: Text -> Doc
documentation "" = Doc Nothing
documentation x  = Doc (Just x)

instance Default Doc where
    def = Doc Nothing

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

instance Default Signature where
    def = V4

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
    { _cmnName          :: Text
    , _cmnPrefix        :: Text
    , _cmnXmlName       :: Text
    , _cmnLocation      :: Location
    , _cmnLocationName  :: Maybe Text
    , _cmnRequired      :: Bool
    , _cmnDocumentation :: Maybe Doc
    , _cmnStreaming     :: Bool
    , _cmnDirection     :: Direction
    } deriving (Eq, Show, Generic)

defName :: Text
defName = "Unknown"

instance Ord Common where
    compare a b =
           comparing (Down . _cmnRequired) a b
        <> comparing (Down . _cmnLocation) a b
        <> comparing _cmnName a b

instance Default Common where
    def = Common defName "_" defName def Nothing False Nothing False def

makeClassy ''Common

data Struct = Struct
    { _sctFields    :: HashMap Text Shape
    , _sctCommon    :: Common
    } deriving (Eq, Show, Generic)

data List = List
    { _lstItem      :: Shape
    , _lstFlattened :: Bool
    , _lstMinLength :: Int
    , _lstMaxLength :: Int
    , _lstCommon    :: Common
    } deriving (Eq, Show, Generic)

data Map = Map
    { _mapKey       :: Shape
    , _mapValue     :: Shape
    , _mapCommon    :: Common
    } deriving (Eq, Show, Generic)

data Sum = Sum
    { _sumValues    :: HashMap Text Text
    , _sumCommon    :: Common
    } deriving (Eq, Show, Generic)

data Prim = Prim
    { _prmType      :: Primitive
    , _prmMinLength :: Int
    , _prmMaxLength :: Int
    , _prmPattern   :: Maybe Text
    , _prmCommon    :: Common
    } deriving (Eq, Show, Generic)

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

data Field = Field
    { _fldType     :: Ann
    , _fldPrefixed :: Text
    , _fldCommon   :: Common
    } deriving (Eq, Show)

instance Ord Field where
    compare = compare `on` _fldCommon

data Ctor
    = CWitness
    | CSwitch
    | CSum
    | CNullary
    | CNewtype
    | CData
    | CError
      deriving (Eq, Ord, Show, Generic)

data Type = Type
    { _typShape  :: Shape
    , _typType   :: Ann
    , _typCtor   :: Ctor
    , _typFields :: [Field]
    } deriving (Show, Generic)

instance Eq Type where
    (==) = (==) `on` (view cmnName)

instance Ord Type where
    compare a b = on compare _typCtor a b <> on compare _typShape a b

data Error = Error
    { _erName   :: Text
    , _erShapes :: [Shape]
    , _erCtors  :: HashMap Text Type
    } deriving (Eq, Show, Generic)

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

data Pagination
    = More Text [Token]
    | Next Text Token
      deriving (Eq, Show, Generic)

data HTTP = HTTP
    { _hMethod :: !StdMethod
    , _hPath   :: [PathPart]
    , _hQuery  :: [QueryPart]
    } deriving (Eq, Show, Generic)

instance Default HTTP where
    def = HTTP GET [] []

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
    , _svcOverrides        :: [Shape]
    } deriving (Show, Generic)

instance Eq Service where
    (==) a b = f _svcVersionNamespace
      where
        f :: Eq a => (Service -> a) -> Bool
        f g = g a == g b

instance Ord Service where
    compare a b = f _svcVersionNamespace
      where
        f :: Ord a => (Service -> a) -> Ordering
        f g = compare (Down $ g a) (Down $ g b)

defaultService :: Abbrev -> Service
defaultService a = Service
    { _svcName             = a
    , _svcFullName         = unAbbrev a
    , _svcNamespace        = def
    , _svcVersionNamespace = def
    , _svcTypesNamespace   = def
    , _svcLensNamespace    = def
    , _svcVersion          = Version mempty
    , _svcRawVersion       = mempty
    , _svcType             = def
    , _svcError            = Error (unAbbrev a) [] mempty
    , _svcWrapped          = False
    , _svcSignature        = def
    , _svcDocumentation    = def
    , _svcEndpointPrefix   = mempty
    , _svcGlobalEndpoint   = def
    , _svcXmlNamespace     = def
    , _svcTimestamp        = def
    , _svcChecksum         = def
    , _svcJsonVersion      = def
    , _svcTargetPrefix     = def
    , _svcOperations       = []
    , _svcOverrides        = []
    }

makeLenses ''Request
makeLenses ''Ann
makeLenses ''Field
makeLenses ''Type
makeLenses ''Error
makeLenses ''Token
makeLenses ''HTTP
makeLenses ''Response
makeLenses ''Operation
makeLenses ''Service
makeLenses ''Struct
makeLenses ''List
makeLenses ''Map
makeLenses ''Sum
makeLenses ''Prim

instance HasCommon Shape where
    common f x = case x of
        SStruct y -> SStruct <$> common f y
        SList   y -> SList   <$> common f y
        SMap    y -> SMap    <$> common f y
        SSum    y -> SSum    <$> common f y
        SPrim   y -> SPrim   <$> common f y

instance HasCommon Struct where
    common f x = (\y -> x { _sctCommon = y }) <$> f (_sctCommon x)

instance HasCommon List where
    common f x = (\y -> x { _lstCommon = y }) <$> f (_lstCommon x)

instance HasCommon Map where
    common f x = (\y -> x { _mapCommon = y }) <$> f (_mapCommon x)

instance HasCommon Sum where
    common f x = (\y -> x { _sumCommon = y }) <$> f (_sumCommon x)

instance HasCommon Prim where
    common f x = (\y -> x { _prmCommon = y }) <$> f (_prmCommon x)

instance HasCommon Field where
    common = fldCommon

instance HasCommon Type where
    common = typShape . common
