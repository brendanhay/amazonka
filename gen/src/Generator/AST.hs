{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

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
import           Data.CaseInsensitive      (CI)
import           Data.Default
import           Data.Function
import qualified Data.HashMap.Strict       as Map
import           Data.Maybe
import           Data.Monoid               hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           GHC.Generics
import           Network.HTTP.Types.Method

type HashMap = Map.HashMap

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
    fromString = namespaceFromText . Text.pack

namespaceFromText :: Text -> NS
namespaceFromText = NS . filter (/= "") . Text.split (== '.')

namespace :: Abbrev -> NS
namespace a = NS ["Network", "AWS", unAbbrev a]

newtype Version = Version { unVersion :: Text }
    deriving (Eq, Ord, Show, Generic)

version :: Text -> Version
version = Version . mappend "V" . Text.replace "-" "_"

cabalVersion :: Version
cabalVersion = Version "0.0.0"

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
    | POSIX
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
      deriving (Eq, Generic)

instance Show ServiceType where
    show t = case t of
        RestXml  -> "RestXML"
        RestJson -> "RestJSON"
        RestS3   -> "RestS3"
        Json     -> "JSON"
        Query    -> "Query"

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
    def = Common defName "" defName def Nothing False Nothing False def

makeClassy ''Common

data Struct = Struct
    { _sctFields    :: [(Text, Shape)]
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

data Ctor
    = CWitness
    | CSwitch
    | CSum
    | CNullary
    | CNewtype
    | CData
    | CError
      deriving (Eq, Ord, Show, Generic)

instance Default Ctor where
    def = CData

data Ann = Ann
   { _anRaw'     :: !Text
   , _anCtor     :: !Ctor
   , _anWrap     :: !Bool
   , _anMonoid   :: !Bool
   , _anDefault  :: !Bool
   , _anRequired :: !Bool
   } deriving (Eq, Show, Generic)

instance Default Ann where
    def = Ann "Default" def False False False False

data Field = Field
    { _fldAnn      :: !Ann
    , _fldName     :: Text
    , _fldPrefixed :: Text
    , _fldCommon   :: Common
    } deriving (Eq, Show)

instance Ord Field where
    compare = compare `on` _fldCommon

data Type' = Type
    { _typShape    :: Shape
    , _typAnn      :: !Ann
    , _typPayload  :: Maybe Field
    , _typFields   :: [Field]
    , _typHeaders  :: [Field]
    } deriving (Show, Generic)

instance Eq Type' where
    (==) = (==) `on` view cmnName

instance Ord Type' where
    compare a b = on compare (_anCtor . _typAnn) a b <> on compare _typShape a b

makeClassy ''Type'

defaultType :: Shape -> Type'
defaultType s = Type
    { _typShape   = s
    , _typAnn     = def
    , _typPayload = Nothing
    , _typFields  = []
    , _typHeaders = []
    }

data Error = Error
    { _erName   :: Text
    , _erShapes :: [Shape]
    , _erCtors  :: HashMap Text Type'
    } deriving (Eq, Show, Generic)

data Python
    = Empty
    | Keyed  Text
    | Index  Text Python
    | Apply  Text Python
    | Choice Python Python
      deriving (Eq, Show, Generic)

data Token = Token
    { _tokInput  :: Python
    , _tokOutput :: Python
    } deriving (Eq, Show, Generic)

data Pagination
    = More Python [Token]
    | Next Python Token
      deriving (Eq, Show, Generic)

data PathPart
    = PConst Text
    | PVar   Text
      deriving (Eq, Show)

data QueryPart = QueryPart
    { _qpKey :: Text
    , _qpVal :: Maybe Text
    } deriving (Eq, Show, Generic)

data HTTP = HTTP
    { _hMethod :: !StdMethod
    , _hPath   :: [PathPart]
    , _hQuery  :: [QueryPart]
    } deriving (Eq, Show, Generic)

instance Default HTTP where
    def = HTTP GET [] []

data Request = Request
    { _rqType :: Type'
    , _rqHttp :: HTTP
    } deriving (Eq, Show, Generic)

instance HasType' Request where
    type' f x = (\y -> x { _rqType = y }) <$> f (_rqType x)

data Style
    = SHeaders
    | SXml
    | SXmlHeaders
    | SXmlCursor
    | SJson
    | SBody
    | SBodyHeaders
    | SNullary
      deriving (Eq, Show, Generic)

instance Default Style where
    def = SXml

data Response = Response
    { _rsType  :: Type'
    , _rsStyle :: Style
    } deriving (Eq, Show, Generic)

instance HasType' Response where
    type' f x = (\y -> x { _rsType = y }) <$> f (_rsType x)

data Operation = Operation
    { _opName             :: Text
    , _opService          :: Abbrev
    , _opNs               :: Namespaces
    , _opAlias            :: Maybe Text
    , _opNamespace        :: NS
    , _opDocumentation    :: Doc
    , _opUrl              :: Maybe Text
    , _opRequest          :: Request
    , _opResponse         :: Response
    , _opErrors           :: [Shape]
    , _opPagination       :: Maybe Pagination
    } deriving (Eq, Show, Generic)

newtype Library = Library Text
    deriving (Show, Generic)

library :: Abbrev -> Library
library = Library . mappend "amazonka-" . Text.toLower . unAbbrev

data Cabal = Cabal
    { _cblVersion       :: Version
    , _cblSynopsis      :: Text
    , _cblDocumentation :: Doc
    } deriving (Show, Generic)

cabal :: Version -> Maybe Text -> Maybe Doc -> Text -> Doc -> Cabal
cabal v ms md s d = Cabal v (fromMaybe (s <> " SDK") ms) (fromMaybe d md)

data TypeOverride = TypeOverride
    { _tRequired   :: HashMap Text [CI Text]
    , _tExisting   :: HashMap Text Text
    , _tRename     :: HashMap Text Text
    , _tUnprefixed :: [Text]
    } deriving (Show, Generic)

instance Default TypeOverride where
    def = TypeOverride mempty mempty mempty mempty

data FieldOverride = FieldOverride
    { _fRequired :: [CI Text]
    , _fIgnored  :: [CI Text]
    } deriving (Show, Generic)

instance Default FieldOverride where
    def = FieldOverride mempty mempty

data Namespaces = Namespaces
    { _nsRoot    :: !NS
    , _nsMonadic :: !NS
    , _nsTypes   :: !NS
    , _nsRequest :: !NS
    } deriving (Eq, Show, Generic)

namespacesFromAbbrev :: Abbrev -> ServiceType -> Namespaces
namespacesFromAbbrev a t = Namespaces
    { _nsRoot    = root
    , _nsMonadic = root <> "Monadic"
    , _nsTypes   = root <> "Types"
    , _nsRequest = NS ["Network", "AWS", "Request", fromString (show t)]
    }
  where
    root = NS ["Network", "AWS", unAbbrev a]

data Service = Service
    { _svcName           :: Abbrev
    , _svcLibrary        :: Library
    , _svcFullName       :: Text
    , _svcNs             :: Namespaces
    , _svcVersion        :: Version
    , _svcRawVersion     :: Text
    , _svcType           :: ServiceType
    , _svcError          :: Error
    , _svcWrapped        :: Bool
    , _svcSignature      :: Signature
    , _svcDocumentation  :: Doc
    , _svcEndpointPrefix :: Text
    , _svcGlobalEndpoint :: Maybe Text
    , _svcXmlNamespace   :: Maybe Text
    , _svcTimestamp      :: Time
    , _svcChecksum       :: Checksum
    , _svcJsonVersion    :: JSONV
    , _svcTargetPrefix   :: Maybe Text
    , _svcOperations     :: [Operation]
    , _svcTypes          :: [Type']
    , _svcCabal          :: Cabal
    , _svcStatic         :: [NS]
    , _svcTypeOverride   :: TypeOverride
    , _svcFieldOverride  :: FieldOverride
    } deriving (Show, Generic)

instance Eq Service where
    (==) a b = f (_nsRoot . _svcNs)
      where
        f :: Eq a => (Service -> a) -> Bool
        f g = g a == g b

instance Ord Service where
    compare a b = f (_nsRoot . _svcNs)
      where
        f :: Ord a => (Service -> a) -> Ordering
        f g = compare (Down $ g a) (Down $ g b)

defaultService :: Abbrev -> Service
defaultService a = Service
    { _svcName               = a
    , _svcLibrary            = library a
    , _svcFullName           = unAbbrev a
    , _svcNs                 = namespacesFromAbbrev a def
    , _svcVersion            = Version mempty
    , _svcRawVersion         = mempty
    , _svcType               = def
    , _svcError              = Error (unAbbrev a) mempty mempty
    , _svcWrapped            = False
    , _svcSignature          = def
    , _svcDocumentation      = def
    , _svcEndpointPrefix     = mempty
    , _svcGlobalEndpoint     = def
    , _svcXmlNamespace       = def
    , _svcTimestamp          = def
    , _svcChecksum           = def
    , _svcJsonVersion        = def
    , _svcTargetPrefix       = def
    , _svcOperations         = mempty
    , _svcTypes              = mempty
    , _svcCabal              = Cabal cabalVersion (unAbbrev a) def
    , _svcStatic             = mempty
    , _svcTypeOverride       = def
    , _svcFieldOverride      = def
    }

makeLenses ''Request
makeLenses ''Ann
makeLenses ''Field
makeLenses ''Error
makeLenses ''Token
makeLenses ''HTTP
makeLenses ''Response
makeLenses ''Operation
makeLenses ''Cabal
makeLenses ''Namespaces
makeLenses ''Service
makeLenses ''Struct
makeLenses ''List
makeLenses ''Map
makeLenses ''Sum
makeLenses ''Prim
makeLenses ''QueryPart

makeClassy ''TypeOverride
makeClassy ''FieldOverride

instance HasTypeOverride Service where
    typeOverride = svcTypeOverride

instance HasFieldOverride Service where
    fieldOverride = svcFieldOverride

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

instance HasCommon Type' where
    common = typShape . common

instance HasCommon Request where
    common = rqType . common

instance HasCommon Response where
    common = rsType . common
