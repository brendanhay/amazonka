{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}

-- Module      : Gen.Model
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Model
   ( module Gen.Model
   , module Model
   ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.CaseInsensitive (CI)
import           Data.HashSet         (HashSet)
import           Data.Jason
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Debug.Trace
import           Gen.Documentation
import           Gen.Model.Paginator  as Model
import           Gen.Model.Retrier    as Model
import           Gen.Model.URI        as Model
import           Gen.Model.Waiter     as Model
import           Gen.OrdMap           (OrdMap)
import qualified Gen.OrdMap           as OrdMap
import           Gen.Text
import           Gen.TH
import           Gen.Types
import           Prelude              hiding (Enum)

data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
      deriving (Eq, Show)

deriveFromJSON upper ''Method

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq, Show)

deriveFromJSON lower ''Signature

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq, Show)

deriveFromJSON spinal ''Protocol

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show)

deriveFromJSON lower ''Checksum

data Location
    = Headers
    | Header
    | Uri
    | Querystring
    | StatusCode
    | Body
      deriving (Eq, Show)

deriveFromJSON camel ''Location

data XMLNS = XMLNS
    { _xnsPrefix :: !Text
    , _xnsUri    :: !Text
    } deriving (Eq, Show)

makeLenses ''XMLNS
deriveFromJSON camel ''XMLNS

-- | A reference to a 'Shape', plus any additional annotations
-- specific to the point at which the type is de/serialised.
data Ref a = Ref
    { _refAnn           :: a
    , _refShape         :: !Text
    , _refDocumentation :: Maybe Doc
    , _refLocation      :: Maybe Location
    , _refLocationName  :: Maybe Text
    , _refStreaming     :: !Bool
    , _refResultWrapper :: Maybe Text
    , _refWrapper       :: !Bool
    , _refFlattened     :: !Bool
    , _refException     :: !Bool
    , _refFault         :: !Bool
    } deriving (Eq, Show)

makeLenses ''Ref

instance FromJSON (Ref ()) where
    parseJSON = withObject "ref" $ \o -> Ref ()
        <$> o .:  "shape"
        <*> o .:? "documentation"
        <*> o .:? "location"
        <*> o .:? "locationName"
        <*> o .:? "streaming" .!= False
        <*> o .:? "resultWrapper"
        <*> o .:? "wrapper"   .!= False
        <*> o .:? "flattened" .!= False
        <*> o .:? "exception" .!= False
        <*> o .:? "fault"     .!= False

data List a = List
    { _listDocumentation :: Maybe Doc
    , _listMember        :: Ref a
    , _listMin           :: Int
    , _listMax           :: Maybe Int
    , _listFlattened     :: Bool
    , _listLocationName  :: Maybe Text
    } deriving (Eq, Show)

instance FromJSON (List ()) where
    parseJSON = withObject "list" $ \o -> List
        <$> o .:? "documentation"
        <*> o .:  "member"
        <*> o .:? "min"       .!= 0
        <*> o .:? "max"
        <*> o .:? "flattened" .!= False
        <*> o .:? "locationName"

data Map a = Map
    { _mapDocumentation :: Maybe Doc
    , _mapKey           :: Ref a
    , _mapValue         :: Ref a
    , _mapMin           :: Int
    , _mapMax           :: Maybe Int
    , _mapFlattened     :: Bool
    } deriving (Eq, Show)

instance FromJSON (Map ()) where
    parseJSON = withObject "map" $ \o -> Map
        <$> o .:? "documentation"
        <*> o .:  "key"
        <*> o .:  "value"
        <*> o .:? "min"       .!= 0
        <*> o .:? "max"
        <*> o .:? "flattened" .!= False

data Struct a = Struct
    { _structDocumentation :: Maybe Doc
    , _structRequired      :: HashSet (CI Text)
    , _structPayload       :: Maybe Text
    , _structXmlNamespace  :: Maybe XMLNS
    , _structException     :: Bool
    , _structFault         :: Bool
    , _structMembers       :: OrdMap Member (Ref a)
    } deriving (Eq, Show)

instance FromJSON (Struct ()) where
    parseJSON = withObject "structure" $ \o -> Struct
        <$> o .:? "documentation"
        <*> o .:? "required"  .!= mempty
        <*> o .:? "payload"
        <*> o .:? "xmlNamespace"
        <*> o .:? "exception" .!= False
        <*> o .:? "fault"     .!= False
        <*> (first member <$> o .:? "members" .!= mempty)

data Chars = Chars
    { _charsDocumentation :: Maybe Doc
    , _charsMin           :: Maybe Int
    , _charsMax           :: Maybe Int
    , _charsPattern       :: Maybe Text
    , _charsXmlAttribute  :: Maybe Bool
    , _charsLocationName  :: Maybe Text
    , _charsSensitive     :: Maybe Bool
    } deriving (Eq, Show)

data Enum = Enum
    { _enumDocumentation :: Maybe Doc
    , _enumLocationName  :: Maybe Text
    , _enumValues        :: OrdMap Member Text
    } deriving (Eq, Show)

instance FromJSON Enum where
    parseJSON = withObject "enum" $ \o -> Enum
        <$> o .:? "documentation"
        <*> o .:? "locationName"
        <*> (omap <$> o .: "enum")
      where
        omap = OrdMap.fromList
             . map (first (member . constructor) . join (,))

data Blob = Blob
    { _blobDocumentation :: Maybe Doc
    , _blobSensitive     :: Maybe Bool
    , _blobStreaming     :: Maybe Bool
    } deriving (Eq, Show)

data Boolean = Boolean
    { _boolDocumentation :: Maybe Doc
    , _boolBox           :: Maybe Bool
    } deriving (Eq, Show)

data Time = Time
    { _timeDocumentation   :: Maybe Doc
    , _timeTimestampFormat :: Maybe Timestamp
    } deriving (Eq, Show)

data Number a = Number
    { _numDocumentation :: Maybe Doc
    , _numMin           :: Maybe a
    , _numMax           :: Maybe a
    , _numBox           :: Maybe Bool
    } deriving (Eq, Show)

deriveFromJSON defaults ''Chars
deriveFromJSON defaults ''Blob
deriveFromJSON defaults ''Boolean
deriveFromJSON defaults ''Time
deriveFromJSON defaults ''Number

makeLenses ''List
makeLenses ''Map
makeLenses ''Struct
makeLenses ''Chars
makeLenses ''Enum
makeLenses ''Blob
makeLenses ''Boolean
makeLenses ''Time
makeLenses ''Number

-- | The sum of all possible types.
data Shape a
    = SList   (List   a)
    | SMap    (Map    a)
    | SStruct (Struct a)
    | SString Chars
    | SEnum   Enum
    | SBlob   Blob
    | SBool   Boolean
    | STime   Time
    | SInt    (Number Int)
    | SDouble (Number Double)
    | SLong   (Number Integer)
      deriving (Eq, Show)

makePrisms ''Shape

instance FromJSON (Shape ()) where
    parseJSON = withObject "shape" $ \o -> do
        let f g = g <$> parseJSON (Object o)
        o .: "type" >>= \case
            "list"      -> f SList
            "map"       -> f SMap
            "structure" -> f SStruct
            "string"    -> f SEnum <|> f SString
            "blob"      -> f SBlob
            "boolean"   -> f SBool
            "timestamp" -> f STime
            "integer"   -> f SInt
            "float"     -> f SDouble
            "double"    -> f SDouble
            "long"      -> f SLong
            e           -> fail ("Unknown Shape type: " ++ Text.unpack e)

references :: Traversal (Shape a) (Shape b) (Ref a) (Ref b)
references f = \case
    SList   x -> list   x <$> f (_listMember x)
    SMap    x -> hmap   x <$> f (_mapKey x) <*> f (_mapValue x)
    SStruct x -> struct x <$> traverse g (_structMembers x)
    SString x -> pure (SString x)
    SEnum   x -> pure (SEnum   x)
    SBlob   x -> pure (SBlob   x)
    SBool   x -> pure (SBool   x)
    STime   x -> pure (STime   x)
    SInt    x -> pure (SInt    x)
    SDouble x -> pure (SDouble x)
    SLong   x -> pure (SLong   x)
  where
    g x = f x -- trace (show (_refShape x)) (f x)

    list   x m   = SList   $ x { _listMember = m }
    hmap   x k v = SMap    $ x { _mapKey = k, _mapValue = v}
    struct x ms  = SStruct $ x { _structMembers = ms }

documentation :: Lens' (Shape a) (Maybe Doc)
documentation f = \case
    SList   x -> SList   <$> listDocumentation   f x
    SMap    x -> SMap    <$> mapDocumentation    f x
    SStruct x -> SStruct <$> structDocumentation f x
    SString x -> SString <$> charsDocumentation  f x
    SEnum   x -> SEnum   <$> enumDocumentation   f x
    SBlob   x -> SBlob   <$> blobDocumentation   f x
    SBool   x -> SBool   <$> boolDocumentation   f x
    STime   x -> STime   <$> timeDocumentation   f x
    SInt    x -> SInt    <$> numDocumentation    f x
    SDouble x -> SDouble <$> numDocumentation    f x
    SLong   x -> SLong   <$> numDocumentation    f x

constraints :: Shape a -> HashSet Constraint
constraints = \case
    SString _ -> [CEq, COrd, CRead, CShow, CGeneric, CIsString]
    SEnum   _ -> [CEq, COrd, CEnum, CRead, CShow, CGeneric]
    SBlob   _ -> [CGeneric]
    SBool   _ -> [CEq, COrd, CEnum, CRead, CShow, CGeneric]
    STime   _ -> [CEq, COrd, CRead, CShow, CGeneric]
    SInt    _ -> [CEq, COrd, CEnum, CRead, CShow, CNum, CIntegral, CReal]
    SDouble _ -> [CEq, COrd, CEnum, CRead, CShow, CNum, CReal, CRealFrac, CRealFloat]
    SLong   _ -> [CEq, COrd, CEnum, CRead, CShow, CNum, CIntegral, CReal]
    _         -> mempty

-- | Applicable HTTP components for an operation.
data HTTP = HTTP
    { _httpMethod     :: !Method
    , _httpRequestUri :: !URI
    , _httpStatus     :: Maybe Int
    } deriving (Eq, Show)

deriveFromJSON camel ''HTTP

-- | An individual service opereration.
data Operation a = Operation
    { _operName             :: !Text
    , _operDocumentation    :: Maybe Doc
    , _operDocumentationUrl :: Maybe Text
    , _operHttp             :: !HTTP
    , _operInput            :: Maybe a
    , _operOutput           :: Maybe a
    , _operErrors           :: [a]
    } deriving (Eq, Show)

makeLenses ''Operation

instance FromJSON (Operation (Ref ())) where
    parseJSON = withObject "operation" $ \o -> Operation
        <$> o .:  "name"
        <*> o .:? "documentation"
        <*> o .:? "documentationUrl"
        <*> o .:  "http"
        <*> o .:? "input"
        <*> o .:? "output"
        <*> o .:? "errors" .!= mempty

newtype Name = Name { nameToText :: Text }
    deriving (Eq, Show)

instance FromJSON Name where
    parseJSON = withText "name" (pure . Name . f)
      where
        f = ("Amazon " <>)
          . (<> " Service")
          . stripPrefix "Amazon"
          . stripPrefix "AWS"
          . stripSuffix "Service"

newtype Abbrev = Abbrev { abbrevToText :: Text }
    deriving (Eq, Show)

instance FromJSON Abbrev where
    parseJSON = withText "abbrev" (pure . Abbrev)

-- | Top-level service metadata.
data Metadata = Metadata
    { _metaServiceFullName     :: !Name
    , _metaServiceAbbreviation :: !Abbrev
    , _metaApiVersion          :: !Text
    , _metaEndpointPrefix      :: !Text
    , _metaGlobalEndpoint      :: Maybe Text
    , _metaSignatureVersion    :: !Signature
    , _metaXmlNamespace        :: Maybe Text
    , _metaTargetPrefix        :: Maybe Text
    , _metaJsonVersion         :: Maybe Text
    , _metaTimestampFormat     :: Maybe Timestamp
    , _metaChecksumFormat      :: Maybe Checksum
    , _metaProtocol            :: !Protocol
    } deriving (Eq, Show)

makeClassy ''Metadata
deriveFromJSON camel ''Metadata

svcName, svcAbbrev :: HasMetadata a => Getter a Text
svcName   = metaServiceFullName     . to nameToText
svcAbbrev = metaServiceAbbreviation . to abbrevToText

data Service a b = Service
    { _svcMetadata         :: !Metadata
    , _svcLibrary          :: !Text
    , _svcDocumentation    :: !Doc
    , _svcDocumentationUrl :: !Text
    , _svcOperations       :: TextMap (Operation b)
    , _svcShapes           :: TextMap a
    , _svcOverride         :: !Override
    } deriving (Eq, Show)

makeClassy ''Service

instance HasMetadata (Service a b) where metadata = svcMetadata
instance HasOverride (Service a b) where override = svcOverride

instance FromJSON (Service (Untyped Shape) (Untyped Ref)) where
    parseJSON = withObject "service" $ \o -> Service
        <$> o .:  "metadata"
        <*> o .:  "library"
        <*> o .:  "documentation"
        <*> o .:? "documentationUrl" .!= mempty -- FIXME: temporarily defaulted
        <*> o .:  "operations"
        <*> o .:  "shapes"
        <*> parseJSON (Object o)
