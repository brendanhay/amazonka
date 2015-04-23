{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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

import           Compiler.TH
import           Compiler.Types
import           Control.Lens
import qualified Data.Aeson      as A
import           Data.Jason      hiding (Bool)
import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Numeric.Natural

-- a Ref can circularly point to a Shape via a stable/unique identifer
-- which is indexed somewhere to point to the correct Shape.
--
-- This means all Shape Refs need to be updated to correctly point
-- to the destination Shapes.
--
-- This way a Ref can transparently 'have' Info with an unsafe lookup
-- since the initial assignment of identifiers can be considered safe.
-- e.g.:
--   instance HasInfo (StateT (IntMap Shape) Ref)

-- Maybe constraints and types can be solved similarly, via separate indexes
-- rather than trying to transform the Shapes in place?
--   * how would the above be serialised to JSON for rendering?
--   * should another attempt be made to use haskell-src-exts
--     to pre-render the type class instances?

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq, Show)

deriveJSON lower ''Signature

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq, Show)

deriveJSON spinal ''Protocol

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Show)

instance FromJSON Timestamp where
    parseJSON = withText "timestamp" $ \case
        "rfc822"        -> pure RFC822
        "iso8601"       -> pure ISO8601
        "unixTimestamp" -> pure POSIX
        e               -> fail ("Unknown Timestamp: " ++ Text.unpack e)

deriveToJSON lower ''Timestamp

defaultTimestamp :: Protocol -> Timestamp
defaultTimestamp = \case
    JSON     -> POSIX
    RestJSON -> POSIX
    XML      -> ISO8601
    RestXML  -> ISO8601
    Query    -> ISO8601
    EC2      -> ISO8601

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show)

deriveJSON lower ''Checksum

data Location
    = Headers
    | Header
    | URI
    | QueryString
    | StatusCode
    | Body
      deriving (Eq, Show)

deriveJSON lower ''Location

data NS = NS
    { _xmlPrefix :: Text
    , _xmlUri    :: Text
    } deriving (Eq, Show)

makeClassy ''NS
deriveJSON (camel { lenses = True }) ''NS

data Ref = Ref
    { _refShape         :: Text
    , _refDocumentation :: Maybe Text
    , _refLocation      :: !Location
    , _refLocationName  :: Text
    , _refQueryName     :: Maybe Text
    , _refStreaming     :: !Bool
    , _refWrapper       :: !Bool
    , _refXMLAttribute  :: !Bool
    , _refXMLNamespace  :: NS
    } deriving (Eq, Show)

makeLenses ''Ref
deriveJSON (camel { lenses = True }) ''Ref

instance HasNS Ref where
    nS = refXMLNamespace

data Info = Info
    { _infoDocumentation :: Maybe Text
    , _infoMin           :: !Natural
    , _infoMax           :: Maybe Natural
    , _infoFlattened     :: !Bool
    , _infoSensitive     :: !Bool
    , _infoStreaming     :: !Bool
    , _infoException     :: !Bool
    } deriving (Eq, Show)

makeClassy ''Info

instance FromJSON Info where
    parseJSON = withObject "info" $ \o -> Info
        <$> o .:? "documentation"
        <*> o .:? "min"       .!= 0
        <*> o .:? "max"
        <*> o .:? "flattened" .!= False
        <*> o .:? "sensitive" .!= False
        <*> o .:? "streaming" .!= False
        <*> o .:? "exception" .!= False

deriveToJSON (camel { lenses = True }) ''Info

data Lit
    = Int
    | Long
    | Double
    | Text
    | Blob
    | Time
    | Bool
      deriving (Eq, Show)

literal :: Text -> Either String Lit
literal = \case
    "integer"   -> Right Int
    "long"      -> Right Long
    "double"    -> Right Double
    "float"     -> Right Double
    "string"    -> Right Text
    "blob"      -> Right Blob
    "boolean"   -> Right Bool
    "timestamp" -> Right Time
    e           -> Left $ "Unknown Lit type: " ++ Text.unpack e

deriveToJSON lower ''Lit

data Shape
    = List   Info Ref
    | Map    Info Ref Ref
    | Struct Info (Map Text Ref) [Text] (Maybe Text)
    | Enum   Info (Map Text Text)
    | Lit    Info Lit
      deriving (Eq, Show)

instance HasInfo Shape where
    info = lens f (flip g)
      where
         f = \case
             List   i _      -> i
             Map    i _ _    -> i
             Struct i _ _ _  -> i
             Enum   i _      -> i
             Lit    i _      -> i

         g i = \case
             List   _ e      -> List   i e
             Map    _ k v    -> Map    i k v
             Struct _ ms r p -> Struct i ms r p
             Enum   _ m      -> Enum   i m
             Lit    _ l      -> Lit    i l

instance FromJSON Shape where
    parseJSON = withObject "shape" $ \o -> do
        i  <- parseJSON (Object o)
        t  <- o .:  "type"
        mv <- o .:? "enum"
        case t of
            "list"   -> List   i <$> o .: "member"
            "map"    -> Map    i <$> o .: "key" <*> o .: "value"
            "struct" -> Struct i <$> o .: "members" <*> o .: "required" <*> o .:? "payload"
            "string"
                | Just v <- mv
                     -> pure . Enum i $ joinMap v
            _        -> either fail (return . Lit i) (literal t)

data Metadata = Metadata
    { _protocol            :: !Protocol
    , _serviceAbbreviation :: Text
    , _serviceFullName     :: Text
    , _apiVersion          :: Text
    , _signatureVersion    :: !Signature
    , _endpointPrefix      :: Text
    , _timestampFormat     :: !Timestamp
    , _checksumFormat      :: !Checksum
    , _jsonVersion         :: Text
    , _targetPrefix        :: Maybe Text
    } deriving (Eq, Show)

makeClassy ''Metadata

instance FromJSON Metadata where
    parseJSON = withObject "metadata" $ \o -> do
        p <- o .: "protocol"
        Metadata p
            <$> o .:  "serviceAbbreviation"
            <*> o .:  "serviceFullName"
            <*> o .:  "apiVersion"
            <*> o .:  "signatureVersion"
            <*> o .:  "endpointPrefix"
            <*> o .:? "timestampFormat" .!= defaultTimestamp p
            <*> o .:? "checksumFormat"  .!= SHA256
            <*> o .:? "jsonVersion"     .!= "1.0"
            <*> o .:? "targetPrefix"

deriveToJSON camel ''Metadata

data API = API
    { _metadata'    :: Metadata
    , _referenceUrl :: Text
    , _operationUrl :: Text
    , _description  :: Text
    -- , Operations   :: Map Text Operation
    , _shapes       :: Map Text Shape
    , _libraryName  :: Text
    } deriving (Eq, Show)

makeClassy ''API

instance HasMetadata API where
    metadata = metadata'

instance FromJSON API where
    parseJSON = withObject "api" $ \o ->
         API <$> o .: "metadata"
             <*> o .: "referenceUrl"
             <*> o .: "operationUrl"
             <*> o .: "description"
             <*> o .: "shapes"
             <*> o .: "libraryName"

data Package = Package
    { _api'           :: API
    , _libraryVersion :: SemVer
    , _exposedModules :: [Text]
    , _otherModules   :: [Text]
    } deriving (Eq, Show)

makeLenses ''Package

instance HasMetadata Package where
    metadata = api' . metadata'

instance HasAPI Package where
    aPI = api'

instance A.ToJSON Package where
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
