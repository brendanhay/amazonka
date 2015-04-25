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

import           Compiler.TH
import           Compiler.Types
import           Control.Lens
import           Data.Aeson      (ToJSON (..))
import qualified Data.Aeson      as A
import           Data.Jason      hiding (Bool, ToJSON (..))
import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           GHC.Generics
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

newtype Id = Id Int deriving (Eq, Ord, Show)

makePrisms ''Id

newtype Name = Name Text deriving (Eq, Show)

instance FromJSON Name where
    parseJSON = fmap Name . parseJSON

makePrisms ''Name

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

data NS = NS
    { _xmlPrefix :: Text
    , _xmlUri    :: Text
    } deriving (Eq, Show, Generic)

makeClassy ''NS

instance FromJSON NS where
    parseJSON = gParseJSON' $ camel { lenses = True }

data Ref f a = Ref
    { _refShape         :: !a
    , _refDocumentation :: f Text
    , _refLocation      :: f Location
    , _refLocationName  :: f Text
    , _refQueryName     :: f Text
    , _refStreaming     :: !Bool
    , _refWrapper       :: !Bool
    , _refXMLAttribute  :: !Bool
    , _refXMLNamespace  :: f NS
    } deriving (Generic)

makeLenses ''Ref

instance FromJSON (Ref Maybe Name) where
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

instance HasNS (Ref Identity a) where
    nS = refXMLNamespace . _Wrapped

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

data Shape f a
    = List   (Info f) (Ref f a)
    | Map    (Info f) (Ref f a) (Ref f a)
    | Struct (Info f) (Map Text (Ref f a)) [Text] (Maybe Text)
    | Enum   (Info f) (Map Text Text)
    | Lit    (Info f) Lit

makePrisms ''Shape

instance HasInfo (Shape f a) f where
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

-- references :: Traversal (Shape a) (Shape b) (Ref a) (Ref b)
-- references f = \case
--     SList   x -> list   x <$> f (_listMember x)
--     SMap    x -> hmap   x <$> f (_mapKey x) <*> f (_mapValue x)
--     SStruct x -> struct x <$> traverse g (_structMembers x)
--     SString x -> pure (SString x)
--     SEnum   x -> pure (SEnum   x)
--     SBlob   x -> pure (SBlob   x)
--     SBool   x -> pure (SBool   x)
--     STime   x -> pure (STime   x)
--     SInt    x -> pure (SInt    x)
--     SDouble x -> pure (SDouble x)
--     SLong   x -> pure (SLong   x)
--   where
--     g x = f x -- trace (show (_refShape x)) (f x)

--     list   x m   = SList   $ x { _listMember = m }
--     hmap   x k v = SMap    $ x { _mapKey = k, _mapValue = v}
--     struct x ms  = SStruct $ x { _structMembers = ms }

instance FromJSON (Shape Maybe Name) where
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

data Meta f = Meta
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

makeClassy ''Meta

instance FromJSON (Meta Maybe) where
    parseJSON = withObject "meta" $ \o -> Meta
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

instance ToJSON (Meta Identity) where
    toJSON = gToJSON' camel

data API f a = API
    { _metadata     :: Meta f
    , _referenceUrl :: Text
    , _operationUrl :: Text
    , _description  :: Text
    -- , Operations   :: Map Text Operation
    , _shapes       :: Map Text (Shape f a)
    , _libraryName  :: Text
    } deriving (Generic)

makeClassy ''API

instance HasMeta (API f a) f where
    meta = metadata

instance FromJSON (API Maybe Name) where
    parseJSON = withObject "api" $ \o -> API
         <$> o .: "metadata"
         <*> o .: "referenceUrl"
         <*> o .: "operationUrl"
         <*> o .: "description"
         <*> o .: "shapes"
         <*> o .: "libraryName"

data Package = Package
    { _api            :: API Identity Id
    , _libraryVersion :: SemVer
    , _exposedModules :: [Text]
    , _otherModules   :: [Text]
    } deriving (Generic)

makeLenses ''Package

instance HasMeta Package Identity where
    meta = api . metadata

instance HasAPI Package Identity Id where
    aPI = api

instance ToJSON Package where
    toJSON p@Package{..} = A.Object (x <> y)
      where
        A.Object y = A.toJSON (p ^. meta)
        A.Object x = A.object
            [ "referenceUrl"   A..= (p ^. referenceUrl)
            , "operationUrl"   A..= (p ^. operationUrl)
            , "description"    A..= (p ^. description)
            , "libraryName"    A..= (p ^. libraryName)
            , "libraryVersion" A..= (p ^. libraryVersion)
            , "exposedModules" A..= (p ^. exposedModules)
            , "otherModules"   A..= (p ^. otherModules)
            ]
