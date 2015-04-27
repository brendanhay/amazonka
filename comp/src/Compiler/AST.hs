{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}

-- Module      : Compiler.AST
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST where

import           Compiler.AST.URI
import           Compiler.TH
import           Compiler.Types
import           Control.Lens
import           Data.Aeson           (ToJSON (..))
import qualified Data.Aeson           as A
import           Data.CaseInsensitive (CI)
import           Data.Jason           hiding (Bool, ToJSON (..))
import           Data.Monoid
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics         (Generic)
import           Numeric.Natural

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq, Show, Generic)

instance FromJSON Signature where
    parseJSON = gParseJSON' lower

instance ToJSON Signature where
    toJSON = gToJSON' lower

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq, Show, Generic)

instance FromJSON Protocol where
    parseJSON = gParseJSON' spinal

instance ToJSON Protocol where
    toJSON = gToJSON' spinal

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Show, Generic)

instance FromJSON Timestamp where
    parseJSON = withText "timestamp" $ \case
        "rfc822"        -> pure RFC822
        "iso8601"       -> pure ISO8601
        "unixTimestamp" -> pure POSIX
        e               -> fail ("Unknown Timestamp: " ++ Text.unpack e)

instance ToJSON Timestamp where
    toJSON = gToJSON' lower

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show, Generic)

instance FromJSON Checksum where
    parseJSON = gParseJSON' lower

instance ToJSON Checksum where
    toJSON = gToJSON' lower

data Location
    = Headers
    | Header
    | URI
    | Querystring
    | StatusCode
    | Body
      deriving (Eq, Show, Generic)

instance FromJSON Location where
    parseJSON = gParseJSON' camel

data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
      deriving (Eq, Show, Generic)

instance FromJSON Method where
    parseJSON = gParseJSON' upper

newtype NS = NS [Text]
    deriving (Eq, Ord, Show)

instance IsString NS where
    fromString = namespace . fromString

instance Monoid NS where
    mempty                  = NS []
    mappend (NS xs) (NS ys) = NS (xs <> ys)

instance FromJSON NS where
    parseJSON = withText "namespace" (pure . namespace)

instance ToJSON NS where
    toJSON (NS xs) = toJSON (Text.intercalate "." xs)

namespace :: Text -> NS
namespace = NS . Text.splitOn "."

data XML = XML'
    { _xmlPrefix :: Text
    , _xmlUri    :: Text
    } deriving (Eq, Show, Generic)

makeClassy ''XML

instance FromJSON XML where
    parseJSON = gParseJSON' $ camel { lenses = True }

data Ref f = Ref
    { _refShape         :: Text
    , _refDocumentation :: f Text
    , _refLocation      :: f Location
    , _refLocationName  :: f Text
    , _refQueryName     :: f Text
    , _refStreaming     :: !Bool
    , _refWrapper       :: !Bool
    , _refXMLAttribute  :: !Bool
    , _refXMLNamespace  :: f XML
    } deriving (Generic)

makeLenses ''Ref

instance FromJSON (Ref Maybe) where
    parseJSON = withObject "ref" $ \o -> Ref
        <$> o .:  "shape"
        <*> o .:? "documentation"
        <*> o .:? "location"
        <*> o .:? "locationName"
        <*> o .:? "queryName"
        <*> o .:? "streaming"    .!= False
        <*> o .:? "wrapper"      .!= False
        <*> o .:? "xmlAttribute" .!= False
        <*> o .:? "xmlnamespace"

instance HasXML (Ref Identity) where
    xML = refXMLNamespace . _Wrapped

data Info f = Info
    { _infoDocumentation :: f Text
    , _infoMin           :: !Natural
    , _infoMax           :: Maybe Natural
    , _infoFlattened     :: !Bool
    , _infoSensitive     :: !Bool
    , _infoStreaming     :: !Bool
    , _infoException     :: !Bool
    } deriving (Generic)

makeClassy ''Info

instance FromJSON (Info Maybe) where
    parseJSON = withObject "info" $ \o -> Info
        <$> o .:? "documentation"
        <*> o .:? "min"       .!= 0
        <*> o .:? "max"
        <*> o .:? "flattened" .!= False
        <*> o .:? "sensitive" .!= False
        <*> o .:? "streaming" .!= False
        <*> o .:? "exception" .!= False

data Lit
    = Int
    | Long
    | Double
    | Text
    | Blob
    | Time
    | Bool

class HasRefs f a | a -> f where
    references :: Traversal' a (Ref f)

data Struct f = Struct'
    { _members  :: Map Text (Ref f)
    , _required :: Set (CI Text)
    , _payload  :: Maybe (CI Text)
    }

makeLenses ''Struct

instance HasRefs f (Struct f) where
    references = members . each

instance FromJSON (Struct Maybe) where
    parseJSON = withObject "struct" $ \o -> Struct'
        <$> o .:  "members"
        <*> o .:? "required" .!= mempty
        <*> o .:? "payload"

data Shape f
    = List   (Info f) (Ref f)
    | Map    (Info f) (Ref f) (Ref f)
    | Struct (Info f) (Struct f)
    | Enum   (Info f) (Map Text Text)
    | Lit    (Info f) Lit

makePrisms ''Shape

instance HasRefs f (Shape f) where
    references f = \case
        List   i e   -> List   i <$> f e
        Map    i k v -> Map    i <$> f k <*> f v
        Struct i s   -> Struct i <$> references f s
        s            -> pure s

fields :: Traversal' (Shape f) (Text, Ref f)
fields = _Struct . _2 . members . traverseKV

values :: Traversal' (Shape f) (Text, Text)
values = _Enum . _2 . traverseKV

instance HasInfo (Shape f) f where
    info = lens f (flip g)
      where
        f = \case
            List   i _   -> i
            Map    i _ _ -> i
            Struct i _   -> i
            Enum   i _   -> i
            Lit    i _   -> i

        g i = \case
            List   _ e   -> List   i e
            Map    _ k v -> Map    i k v
            Struct _ s   -> Struct i s
            Enum   _ m   -> Enum   i m
            Lit    _ l   -> Lit    i l

instance FromJSON (Shape Maybe) where
    parseJSON = withObject "shape" $ \o -> do
        i <- parseJSON (Object o)
        t <- o .:  "type"
        m <- o .:? "enum"
        case t of
            "list"      -> List   i <$> o .: "member"
            "map"       -> Map    i <$> o .: "key" <*> o .: "value"
            "structure" -> Struct i <$> parseJSON (Object o)
            "integer"   -> pure (Lit i Int)
            "long"      -> pure (Lit i Long)
            "double"    -> pure (Lit i Double)
            "float"     -> pure (Lit i Double)
            "blob"      -> pure (Lit i Blob)
            "boolean"   -> pure (Lit i Bool)
            "timestamp" -> pure (Lit i Time)
            "string"
                | Just v <- m -> pure . Enum i $ joinKV v
                | otherwise   -> pure (Lit i Text)
            _           -> fail $ "Unknown Shape type: " ++ Text.unpack t

data HTTP f = HTTP
    { _method       :: !Method
    , _requestURI   :: URI
    , _responseCode :: f Int
    } deriving (Generic)

makeClassy ''HTTP

instance FromJSON (HTTP Maybe) where
    parseJSON = gParseJSON' camel

-- data RQRS f a = RQRS (Ref f a) (Shape f)

data Operation f a = Operation
    { _opName          :: Text
    , _opDocumentation :: f Text
    , _opHTTP          :: HTTP f
    , _opInput         :: f (a f)
    , _opOutput        :: f (a f)
    }

makeLenses ''Operation

requestName, responseName :: Getter (Operation f a) Text
requestName  = to _opName
responseName = to ((<> "Response") . _opName)

instance FromJSON (Operation Maybe Ref) where
    parseJSON = withObject "operation" $ \o -> Operation
        <$> o .:  "name"
        <*> o .:? "documentation"
        <*> o .:  "http"
        <*> o .:? "input"
        <*> o .:? "output"

instance HasHTTP (Operation f a) f where
    hTTP = opHTTP

data Metadata f = Metadata
    { _protocol            :: !Protocol
    , _serviceAbbreviation :: Text
    , _serviceFullName     :: Text
    , _apiVersion          :: Text
    , _signatureVersion    :: !Signature
    , _endpointPrefix      :: Text
    , _timestampFormat     :: f Timestamp
    , _checksumFormat      :: f Checksum
    , _jsonVersion         :: Text
    , _targetPrefix        :: Maybe Text
    } deriving (Generic)

makeClassy ''Metadata

instance FromJSON (Metadata Maybe) where
    parseJSON = withObject "meta" $ \o -> Metadata
        <$> o .:  "protocol"
        <*> o .:  "serviceAbbreviation"
        <*> o .:  "serviceFullName"
        <*> o .:  "apiVersion"
        <*> o .:  "signatureVersion"
        <*> o .:  "endpointPrefix"
        <*> o .:? "timestampFormat"
        <*> o .:? "checksumFormat"
        <*> o .:? "jsonVersion"     .!= "1.0"
        <*> o .:? "targetPrefix"

instance ToJSON (Metadata Identity) where
    toJSON = gToJSON' camel

data Override = Override
    { _renamedTo      :: Maybe Text         -- ^ Rename type
    , _replacedBy     :: Maybe Text         -- ^ Existing type that supplants this type
    , _enumPrefix     :: Maybe Text         -- ^ Enum constructor prefix
    , _requiredFields :: Set (CI Text)      -- ^ Required fields
    , _optionalFields :: Set (CI Text)      -- ^ Optional fields
    , _renamedFields  :: Map (CI Text) Text -- ^ Rename fields
    } deriving (Eq, Show)

makeLenses ''Override

instance FromJSON Override where
    parseJSON = withObject "override" $ \o -> Override
        <$> o .:? "renamedTo"
        <*> o .:? "replacedBy"
        <*> o .:? "enumPrefix"
        <*> o .:? "requiredFields" .!= mempty
        <*> o .:? "optionalFields" .!= mempty
        <*> o .:? "renamedFields"  .!= mempty

-- FIXME: An Operation should end up referring to a Shape,
-- similarly to a Ref.

data API f a = API
    { _metadata'        :: Metadata f
    , _referenceUrl     :: Text
    , _operationUrl     :: Text
    , _description      :: Text
    , _operations       :: Map Text (Operation f a)
    , _shapes           :: Map Text (Shape f)
    , _libraryName      :: Text
    , _operationImports :: [NS]
    , _typeImports      :: [NS]
    , _typeOverrides    :: Map Text Override
    , _ignoredWaiters   :: Set (CI Text)
    } deriving (Generic)

makeClassy ''API

instance HasMetadata (API f a) f where
    metadata = metadata'

instance FromJSON (API Maybe Ref) where
    parseJSON = withObject "api" $ \o -> API
        <$> o .:  "metadata"
        <*> o .:  "referenceUrl"
        <*> o .:  "operationUrl"
        <*> o .:  "description"
        <*> o .:  "operations"
        <*> o .:  "shapes"
        <*> o .:  "libraryName"
        <*> o .:? "operationImports" .!= mempty
        <*> o .:? "typeImports"      .!= mempty
        <*> o .:? "typeOverrides"    .!= mempty
        <*> o .:? "ignoredWaiters"   .!= mempty

data Package = Package
    { _api'           :: API Identity Shape
    , _libraryVersion :: SemVer
    , _exposedModules :: [NS]
    , _otherModules   :: [NS]
    } deriving (Generic)

makeLenses ''Package

instance HasMetadata Package Identity where
    metadata = api' . metadata'

instance HasAPI Package Identity Shape where
    aPI = api'

instance ToJSON Package where
    toJSON p@Package{..} = A.Object (x <> y)
      where
        A.Object y = A.toJSON (p ^. metadata)
        A.Object x = A.object
            [ "referenceUrl"   A..= (p ^. referenceUrl)
            , "operationUrl"   A..= (p ^. operationUrl)
            , "description"    A..= (p ^. description)
            , "libraryName"    A..= (p ^. libraryName)
            , "libraryVersion" A..= (p ^. libraryVersion)
            , "exposedModules" A..= (p ^. exposedModules)
            , "otherModules"   A..= (p ^. otherModules)
            ]
