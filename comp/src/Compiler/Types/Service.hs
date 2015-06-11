{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeOperators          #-}

-- Module      : Compiler.Types.Service
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Service where

import           Compiler.Text
import           Compiler.TH
import           Compiler.Types.Ann
import           Compiler.Types.Help
import           Compiler.Types.Id
import           Compiler.Types.Map
import           Compiler.Types.NS
import           Compiler.Types.Orphans ()
import           Compiler.Types.URI
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Lens           hiding ((.=))
import           Data.Aeson             hiding (Bool)
import           Data.Bifunctor
import qualified Data.HashMap.Strict    as Map
import           Data.List              (nub)
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           GHC.Generics           (Generic)
import           Numeric.Natural

makePrisms ''Identity

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
    toJSON V2 = String "V2"
    toJSON _  = String "V4"

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Show, Generic)

tsToText :: Timestamp -> Text
tsToText = Text.pack . show

instance FromJSON Timestamp where
    parseJSON = withText "timestamp" $ \case
        "rfc822"        -> pure RFC822
        "iso8601"       -> pure ISO8601
        "unixTimestamp" -> pure POSIX
        e               -> fail ("Unknown Timestamp: " ++ Text.unpack e)

instance ToJSON Timestamp where
    toJSON = toJSON . tsToText

data Protocol
    = JSON
    | RestJSON
    | RestXML
    | Query
    | EC2
      deriving (Eq, Show, Generic)

instance FromJSON Protocol where
    parseJSON = gParseJSON' spinal

instance ToJSON Protocol where
    toJSON = String . \case
        JSON     -> "JSON"
        RestJSON -> "JSON"
        RestXML  -> "XML"
        Query    -> "Query"
        EC2      -> "Query"

timestamp :: Protocol -> Timestamp
timestamp = \case
    JSON     -> POSIX
    RestJSON -> POSIX
    RestXML  -> ISO8601
    Query    -> ISO8601
    EC2      -> ISO8601

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

instance ToJSON Location where
    toJSON = gToJSON' camel

data XML = XML'
    { _xmlPrefix :: Maybe Text
    , _xmlUri    :: Text
    } deriving (Eq, Show, Generic)

makeLenses ''XML

instance FromJSON XML where
    parseJSON = gParseJSON' (camel & lenses .~ True)

data RefF a = RefF
    { _refAnn           :: a
    , _refShape         :: Id
    , _refDocumentation :: Maybe Help
    , _refLocation      :: Maybe Location
    , _refLocationName  :: Maybe Text
    , _refResultWrapper :: Maybe Text
    , _refQueryName     :: Maybe Text
    , _refStreaming     :: !Bool
    , _refXMLAttribute  :: !Bool
    , _refXMLNamespace  :: Maybe XML
    } deriving (Show, Functor, Foldable, Traversable, Generic)

makeLenses ''RefF

instance HasId (RefF a) where
    identifier = identifier . _refShape

instance FromJSON (RefF ()) where
    parseJSON = withObject "ref" $ \o -> RefF ()
        <$> o .:  "shape"
        <*> o .:? "documentation"
        <*> o .:? "location"
        <*> o .:? "locationName"
        <*> o .:? "resultWrapper"
        <*> o .:? "queryName"
        <*> o .:? "streaming"    .!= False
        <*> o .:? "xmlAttribute" .!= False
        <*> o .:? "xmlNamespace"

class HasRefs f where
     references :: Traversal (f a) (f b) (RefF a) (RefF b)

data Info = Info
    { _infoDocumentation :: Maybe Help
    , _infoMin           :: Maybe Natural
    , _infoMax           :: Maybe Natural
    , _infoFlattened     :: !Bool
    , _infoSensitive     :: !Bool
    , _infoStreaming     :: !Bool
    , _infoException     :: !Bool
    } deriving (Show, Generic)

makeClassy ''Info

instance FromJSON Info where
    parseJSON = withObject "info" $ \o -> Info
        <$> o .:? "documentation"
        <*> o .:? "min"
        <*> o .:? "max"
        <*> o .:? "flattened" .!= False
        <*> o .:? "sensitive" .!= False
        <*> o .:? "streaming" .!= False
        <*> o .:? "exception" .!= False

nonEmpty :: HasInfo a => a -> Bool
nonEmpty = (> Just 0) . view infoMin

data ListF a = ListF
    { _listInfo :: Info
    , _listItem :: RefF a
    } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''ListF

instance HasInfo (ListF a) where
    info = listInfo

instance HasRefs ListF where
    references = listItem

instance FromJSON (Info -> ListF ()) where
    parseJSON = withObject "list" $ \o -> flip ListF
        <$> o .: "member"

data MapF a = MapF
    { _mapInfo  :: Info
    , _mapKey   :: RefF a
    , _mapValue :: RefF a
    } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''MapF

instance HasInfo (MapF a) where
    info = mapInfo

instance HasRefs MapF where
    references f (MapF i k v) = MapF i <$> f k <*> f v

instance FromJSON (Info -> MapF ()) where
    parseJSON = withObject "map" $ \o -> do
        k <- o .: "key"
        v <- o .: "value"
        return $! \i -> MapF i k v

data StructF a = StructF
    { _structInfo :: Info
    , _members    :: Map Id (RefF a)
    , _required'  :: [Id] -- ^ List so it can be used for ordering.
    , _payload    :: Maybe Id
    } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''StructF

instance HasInfo (StructF a) where
    info = structInfo

instance HasRefs StructF where
    references = traverseOf (members . each)

instance FromJSON (Info -> StructF ()) where
    parseJSON = withObject "struct" $ \o -> do
        ms <- o .:  "members"
        r  <- o .:? "required" .!= mempty
        p  <- o .:? "payload"
        return $! \i -> StructF i (body p ms) r p
      where
        -- This ensure that the field referenced by a possible
        -- "payload":<id> has a location set.
        body :: Maybe Id -> Map Id (RefF a) -> Map Id (RefF a)
        body Nothing  = id
        body (Just p) = Map.mapWithKey f
          where
            f n r | p == n    = r & refLocation ?~ Body
                  | otherwise = r

data ShapeF a
    = Ptr    Info (Set Derive)
    | List   (ListF   a)
    | Map    (MapF    a)
    | Struct (StructF a)
    | Enum   Info (Map Id Text)
    | Lit    Info Lit
      deriving (Show, Functor, Foldable, Traversable)

makePrisms ''ShapeF

instance HasInfo (ShapeF a) where
    info f = \case
        Ptr    i ds -> (`Ptr`  ds) <$> f i
        List   l    -> List        <$> info f l
        Map    m    -> Map         <$> info f m
        Struct s    -> Struct      <$> info f s
        Enum   i vs -> (`Enum` vs) <$> f i
        Lit    i l  -> (`Lit`  l)  <$> f i

instance HasInfo (Cofree ShapeF a) where
    info = lens unwrap go . info
      where
        go s a = extract s :< a

instance HasRefs ShapeF where
    references f = \case
        Ptr  i ds -> pure (Ptr  i ds)
        List l    -> List   <$> references f l
        Map  m    -> Map    <$> references f m
        Struct s  -> Struct <$> references f s
        Enum i vs -> pure (Enum i vs)
        Lit  i l  -> pure (Lit  i l)

instance FromJSON (ShapeF ()) where
    parseJSON = withObject "shape" $ \o -> do
        i <- parseJSON (Object o)
        t <- o .:  "type"
        m <- o .:? "enum"
        case t of
            "list"      -> List   . ($ i) <$> parseJSON (Object o)
            "map"       -> Map    . ($ i) <$> parseJSON (Object o)
            "structure" -> Struct . ($ i) <$> parseJSON (Object o)
            "integer"   -> pure (Lit i Int)
            "long"      -> pure (Lit i Long)
            "double"    -> pure (Lit i Double)
            "float"     -> pure (Lit i Double)
            "blob"      -> pure (Lit i Blob)
            "boolean"   -> pure (Lit i Bool)
            "timestamp" -> pure (Lit i Time)
            "string"    -> pure (maybe (Lit i Text) f m)
              where
                f = Enum i . Map.fromList . map (first mkId . renameBranch)
            _           -> fail $ "Unknown Shape type: " ++ Text.unpack t

data Operation f a = Operation
    { _opName          :: Id
    , _opDocumentation :: f Help
    , _opDeprecated    :: !Bool
    , _opHTTP          :: HTTP f
    , _opInput         :: f a
    , _opOutput        :: f a
    }

makeLenses ''Operation

operationNS :: NS -> Getter (Operation f a) NS
operationNS ns = opName . typeId . to (mappend ns . mkNS)

inputName, outputName :: HasId a => Getter (Operation Identity a) Id
inputName  = opInput  . _Identity . to identifier
outputName = opOutput . _Identity . to identifier

instance HasHTTP (Operation f a) f where
    hTTP = opHTTP

instance FromJSON (Operation Maybe (RefF ())) where
    parseJSON = withObject "operation" $ \o -> Operation
        <$> o .:  "name"
        <*> o .:? "documentation"
        <*> o .:? "deprecated" .!= False
        <*> o .:  "http"
        <*> o .:? "input"
        <*> o .:? "output"

instance ToJSON a => ToJSON (Operation Identity a) where
    toJSON o = object
        [ "name"          .= (o ^. opName)
        , "documentation" .= (o ^. opDocumentation)
        , "input"         .= (o ^. opInput)
        , "output"        .= (o ^. opOutput)
        ]

data Metadata f = Metadata
    { _protocol         :: !Protocol
    , _serviceAbbrev    :: Text
    , _serviceFullName  :: Text
    , _apiVersion       :: Text
    , _signatureVersion :: !Signature
    , _endpointPrefix   :: Text
    , _timestampFormat  :: f Timestamp
    , _checksumFormat   :: f Checksum
    , _xmlNamespace     :: Maybe Text
    , _jsonVersion      :: Maybe Text
    , _targetPrefix     :: Maybe Text
    } deriving (Generic)

makeClassy ''Metadata

serviceError :: HasMetadata a f => Getter a (Text, Text)
serviceError = protocol . to f
  where
    f = \case
        JSON     -> ("JSONError", "jsonError")
        RestJSON -> ("JSONError", "jsonError")
        EC2      -> ("EC2Error",  "restError")
        _        -> ("RESTError", "restError")

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
        <*> o .:? "xmlNamespace"
        <*> o .:? "jsonVersion"
        <*> o .:? "targetPrefix"

instance ToJSON (Metadata Identity) where
    toJSON m = Object (x <> y)
      where
        Object x = gToJSON' camel m
        Object y = object
            [ "serviceError"         .= e
            , "serviceErrorFunction" .= f
            ]

        (e, f) = m ^. serviceError

data Service f a b = Service
    { _metadata'     :: Metadata f
    , _documentation :: Help
    , _operations    :: Map Id (Operation f a)
    , _shapes        :: Map Id b
    } deriving (Generic)

makeClassy ''Service

instance HasMetadata (Service f a b) f where
    metadata = metadata'

instance FromJSON (Service Maybe (RefF ()) (ShapeF ())) where
    parseJSON = gParseJSON' lower

type Shape = Cofree ShapeF
type Ref   = RefF (Shape Solved)

class IsStreaming a where
    streaming :: a -> Bool

    default streaming :: HasInfo a => a -> Bool
    streaming = view infoStreaming

instance IsStreaming Info
instance IsStreaming (StructF a)
instance IsStreaming (ShapeF  a)
instance IsStreaming (Shape   a)

instance IsStreaming a => IsStreaming (RefF a) where
    streaming r = _refStreaming r || streaming (_refAnn r)

instance IsStreaming TType where
    streaming TStream = True
    streaming _       = False

setRequired :: ([Id] -> [Id]) -> ShapeF a -> ShapeF a
setRequired f = _Struct . required' %~ nub . f

getRequired :: StructF (Shape a) -> [Id]
getRequired s = nub $ _required' s <> concatMap f (Map.toList (s ^. members))
  where
    f (n, r)
        | streaming r                            = [n]
        | Just i <- r ^. refAnn . infoMin, i > 0 = [n]
        | otherwise                              = []
