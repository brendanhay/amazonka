{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -ddump-splices        #-}

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
      deriving (Eq)

deriveJSON lower ''Signature

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq)

deriveJSON spinal ''Protocol

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq)

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
      deriving (Eq)

deriveJSON lower ''Checksum

data Location
    = Headers
    | Header
    | URI
    | Querystring
    | StatusCode
    | Body
      deriving (Eq)

deriveJSON camel ''Location

data NS = NS
    { _xmlPrefix :: Text
    , _xmlUri    :: Text
    }

makeClassy ''NS
deriveJSON (camel { lenses = True }) ''NS

data Ref f = Ref
    { _refShape         :: Text
    , _refDocumentation :: f Text
    , _refLocation      :: f Location
    , _refLocationName  :: f Text
    , _refQueryName     :: f Text
    , _refStreaming     :: !Bool
    , _refWrapper       :: !Bool
    , _refXMLAttribute  :: !Bool
    , _refXMLNamespace  :: f NS
    }

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

--deriveToJSON (camel { lenses = True }) ''Ref

-- instance HasNS Ref where
--     nS = refXMLNamespace

data Info f = Info
    { _infoDocumentation :: f Text
    , _infoMin           :: !Natural
    , _infoMax           :: Maybe Natural
    , _infoFlattened     :: !Bool
    , _infoSensitive     :: !Bool
    , _infoStreaming     :: !Bool
    , _infoException     :: !Bool
    }

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

-- deriveToJSON (camel { lenses = True }) ''Info

data Lit
    = Int
    | Long
    | Double
    | Text
    | Blob
    | Time
    | Bool

deriveToJSON lower ''Lit

data Shape f
    = List   (Info f) (Ref f)
    | Map    (Info f) (Ref f) (Ref f)
    | Struct (Info f) (Map Text (Ref f)) [Text] (Maybe Text)
    | Enum   (Info f) (Map Text Text)
    | Lit    (Info f) Lit

instance HasInfo (Shape f) f where
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

instance FromJSON (Shape Maybe) where
    parseJSON = withObject "shape" $ \o -> do
        i <- parseJSON (Object o)
        t <- o .:  "type"
        m <- o .:? "enum"
        case t of
            "list"      -> List i <$> o .: "member"
            "map"       -> Map  i <$> o .: "key" <*> o .: "value"

            "structure" -> Struct i
                <$> o .:  "members"
                <*> o .:? "required" .!= mempty
                <*> o .:? "payload"

            "string"
                | Just v <- m -> pure . Enum i $ joinMap v
                | otherwise   -> pure (Lit i Text)

            "integer"         -> pure (Lit i Int)
            "long"            -> pure (Lit i Long)
            "double"          -> pure (Lit i Double)
            "float"           -> pure (Lit i Double)
            "blob"            -> pure (Lit i Blob)
            "boolean"         -> pure (Lit i Bool)
            "timestamp"       -> pure (Lit i Time)
            _                 -> fail $ "Unknown Shape type: " ++ Text.unpack t

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
    }

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

data API f = API
    { _metadata'    :: Metadata
    , _referenceUrl :: Text
    , _operationUrl :: Text
    , _description  :: Text
    -- , Operations   :: Map Text Operation
    , _shapes       :: Map Text (Shape f)
    , _libraryName  :: Text
    }

makeClassy ''API

instance HasMetadata (API f) where
    metadata = metadata'

instance FromJSON (API Maybe) where
    parseJSON = withObject "api" $ \o ->
         API <$> o .: "metadata"
             <*> o .: "referenceUrl"
             <*> o .: "operationUrl"
             <*> o .: "description"
             <*> o .: "shapes"
             <*> o .: "libraryName"

data Package f = Package
    { _api'           :: API f
    , _libraryVersion :: SemVer
    , _exposedModules :: [Text]
    , _otherModules   :: [Text]
    }

makeLenses ''Package

instance HasMetadata (Package f) where
    metadata = api' . metadata'

instance HasAPI (Package f) f where
    aPI = api'

instance A.ToJSON (Package Identity) where
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
