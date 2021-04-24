{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.Types.Service
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>44
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Service where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Lens ((%~), (&), (.~), (<&>), (?~), (^.))
import Control.Lens qualified as Lens
import Control.Lens.TH qualified as TH
import Data.Aeson (FromJSON, ToJSON, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as JSON
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as Map
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Gen.TH
import Gen.Text
import Gen.Types.Ann
import Gen.Types.Help
import Gen.Types.Id
import Gen.Types.Map
import Gen.Types.NS
import Gen.Types.Pager
import Gen.Types.Retry
import Gen.Types.URI
import Gen.Types.Waiter
import Text.Show.Deriving qualified as TH (deriveShow, deriveShow1)

$(TH.makePrisms ''Identity)

data Signature
  = V2
  | V3
  | V3HTTPS
  | V4
  | S3
  deriving (Eq, Show, Generic)

sigToText :: Signature -> Text
sigToText V2 = "v2"
sigToText _ = "v4"

instance FromJSON Signature where
  parseJSON = gParseJSON' lower

instance ToJSON Signature where
  toJSON = JSON.String . sigToText

data Timestamp
  = RFC822
  | ISO8601
  | POSIX
  deriving (Eq, Show, Generic)

tsToText :: Timestamp -> Text
tsToText = Text.pack . show

instance FromJSON Timestamp where
  parseJSON = JSON.withText "timestamp" $ \case
    "rfc822" -> pure RFC822
    "iso8601" -> pure ISO8601
    "unixTimestamp" -> pure POSIX
    e -> fail ("Unknown Timestamp: " ++ Text.unpack e)

instance ToJSON Timestamp where
  toJSON = JSON.toJSON . tsToText

data Protocol
  = JSON
  | RestJSON
  | RestXML
  | Query
  | EC2
  | APIGateway
  deriving (Eq, Show, Generic)

instance FromJSON Protocol where
  parseJSON = gParseJSON' spinal

instance ToJSON Protocol where
  toJSON =
    JSON.String . \case
      JSON -> "JSON"
      RestJSON -> "JSON"
      RestXML -> "XML"
      Query -> "Query"
      EC2 -> "Query"
      APIGateway -> "APIGateway"

timestamp :: Protocol -> Timestamp
timestamp = \case
  JSON -> POSIX
  RestJSON -> POSIX
  RestXML -> ISO8601
  Query -> ISO8601
  EC2 -> ISO8601
  APIGateway -> POSIX

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
  { _xmlPrefix :: Maybe Text,
    _xmlUri :: Text
  }
  deriving (Eq, Show, Generic)

$(TH.makeLenses ''XML)

instance FromJSON XML where
  parseJSON = gParseJSON' (camel & lenses .~ True)

data RefF a = RefF
  { _refAnn :: a,
    _refShape :: Id,
    _refDocumentation :: Maybe Help,
    _refLocation :: Maybe Location,
    _refLocationName :: Maybe Text,
    _refResultWrapper :: Maybe Text,
    _refQueryName :: Maybe Text,
    _refStreaming :: !Bool,
    _refXMLAttribute :: !Bool,
    _refXMLNamespace :: Maybe XML
  }
  deriving (Functor, Foldable, Traversable, Generic)

$(TH.deriveShow1 ''RefF)
$(TH.deriveShow ''RefF)
$(TH.makeLenses ''RefF)

instance HasId (RefF a) where
  identifier = identifier . _refShape

instance FromJSON (RefF ()) where
  parseJSON = JSON.withObject "ref" $ \o ->
    RefF ()
      <$> o .: "shape"
      <*> o .:? "documentation"
      <*> o .:? "location"
      <*> o .:? "locationName"
      <*> o .:? "resultWrapper"
      <*> o .:? "queryName"
      <*> o .:? "streaming" .!= False
      <*> o .:? "xmlAttribute" .!= False
      <*> o .:? "xmlNamespace"

class HasRefs f where
  references :: Lens.Traversal (f a) (f b) (RefF a) (RefF b)

data ErrorInfo = ErrorInfo
  { _errCode :: Maybe Text,
    _errStatus :: !Int,
    _errSenderFault :: !Bool
  }
  deriving (Show, Generic)

$(TH.makeLenses ''ErrorInfo)

instance FromJSON ErrorInfo where
  parseJSON = JSON.withObject "error" $ \o ->
    ErrorInfo
      <$> o .:? "code"
      <*> (o .: "httpStatusCode" <&> parseStatusCode)
      <*> o .:? "senderFault" .!= False

data Info = Info
  { _infoDocumentation :: Maybe Help,
    _infoMin :: Maybe Scientific,
    _infoMax :: Maybe Scientific,
    _infoFlattened :: !Bool,
    _infoSensitive :: !Bool,
    _infoStreaming :: !Bool,
    _infoException :: !Bool,
    _infoError :: Maybe ErrorInfo
  }
  deriving (Show, Generic)

$(TH.makeClassy ''Info)

instance FromJSON Info where
  parseJSON = JSON.withObject "info" $ \o ->
    Info
      <$> o .:? "documentation"
      <*> o .:? "min"
      <*> o .:? "max"
      <*> o .:? "flattened" .!= False
      <*> o .:? "sensitive" .!= False
      <*> o .:? "streaming" .!= False
      <*> o .:? "exception" .!= False
      <*> o .:? "error"

nonEmpty :: HasInfo a => a -> Bool
nonEmpty = (> Just 0) . Lens.view infoMin

data ListF a = ListF
  { _listInfo :: Info,
    _listItem :: RefF a
  }
  deriving (Functor, Foldable, Traversable)

$(TH.deriveShow1 ''ListF)
$(TH.deriveShow ''ListF)
$(TH.makeLenses ''ListF)

instance HasInfo (ListF a) where
  info = listInfo

instance HasRefs ListF where
  references = listItem

instance FromJSON (Info -> ListF ()) where
  parseJSON = JSON.withObject "list" $ \o ->
    flip ListF
      <$> o .: "member"

data MapF a = MapF
  { _mapInfo :: Info,
    _mapKey :: RefF a,
    _mapValue :: RefF a
  }
  deriving (Functor, Foldable, Traversable)

$(TH.deriveShow1 ''MapF)
$(TH.deriveShow ''MapF)
$(TH.makeLenses ''MapF)

instance HasInfo (MapF a) where
  info = mapInfo

instance HasRefs MapF where
  references f (MapF i k v) = MapF i <$> f k <*> f v

instance FromJSON (Info -> MapF ()) where
  parseJSON = JSON.withObject "map" $ \o -> do
    k <- o .: "key"
    v <- o .: "value"
    return $ \i -> MapF i k v

data StructF a = StructF
  { _structInfo :: Info,
    _members :: Map Id (RefF a),
    -- | List so it can be used for ordering.
    _required' :: [Id],
    _payload :: Maybe Id
  }
  deriving (Functor, Foldable, Traversable)

$(TH.deriveShow1 ''StructF)
$(TH.deriveShow ''StructF)
$(TH.makeLenses ''StructF)

instance HasInfo (StructF a) where
  info = structInfo

instance HasRefs StructF where
  references = Lens.traverseOf (members . Lens.each)

instance FromJSON (Info -> StructF ()) where
  parseJSON = JSON.withObject "struct" $ \o -> do
    ms <- o .: "members"
    r <- o .:? "required" .!= mempty
    p <- o .:? "payload"
    return $ \i -> StructF i (body p ms) r p
    where
      -- This ensure that the field referenced by a possible
      -- "payload":<id> has a location set.
      body :: Maybe Id -> Map Id (RefF a) -> Map Id (RefF a)
      body Nothing = id
      body (Just p) = Map.mapWithKey f
        where
          f n r
            | p == n = r & refLocation ?~ Body
            | otherwise = r

data ShapeF a
  = Ptr Info TType
  | List (ListF a)
  | Map (MapF a)
  | Struct (StructF a)
  | Enum Info (Map Id Text)
  | Lit Info Lit
  deriving (Functor, Foldable, Traversable)

$(TH.deriveShow1 ''ShapeF)
$(TH.deriveShow ''ShapeF)
$(TH.makePrisms ''ShapeF)

instance HasInfo (ShapeF a) where
  info f = \case
    Ptr i ds -> (`Ptr` ds) <$> f i
    List l -> List <$> info f l
    Map m -> Map <$> info f m
    Struct s -> Struct <$> info f s
    Enum i vs -> (`Enum` vs) <$> f i
    Lit i l -> (`Lit` l) <$> f i

instance HasInfo (Cofree ShapeF a) where
  info = Lens.lens unwrap go . info
    where
      go s a = extract s :< a

instance HasRefs ShapeF where
  references f = \case
    Ptr i ds -> pure (Ptr i ds)
    List l -> List <$> references f l
    Map m -> Map <$> references f m
    Struct s -> Struct <$> references f s
    Enum i vs -> pure (Enum i vs)
    Lit i l -> pure (Lit i l)

instance FromJSON (ShapeF ()) where
  parseJSON = JSON.withObject "shape" $ \o -> do
    i <- JSON.parseJSON (JSON.Object o)
    t <- o .: "type"
    m <- o .:? "enum"

    case t of
      "list" -> List . ($ i) <$> JSON.parseJSON (JSON.Object o)
      "map" -> Map . ($ i) <$> JSON.parseJSON (JSON.Object o)
      "structure" -> Struct . ($ i) <$> JSON.parseJSON (JSON.Object o)
      "integer" -> pure (Lit i Int)
      "long" -> pure (Lit i Long)
      "double" -> pure (Lit i Double)
      "float" -> pure (Lit i Double)
      "blob" -> pure (Lit i Base64)
      "boolean" -> pure (Lit i Bool)
      "timestamp" -> pure (Lit i Time)
      "json" -> pure (Lit i Json)
      "string" -> pure (maybe (Lit i Text) f m)
        where
          f = Enum i . Map.fromList . map (first mkId . renameBranch)
      _ -> fail $ "Unknown Shape type: " ++ Text.unpack t

data Operation f a b = Operation
  { _opName :: Id,
    _opDocumentation :: f Help,
    _opDeprecated :: !Bool,
    _opHTTP :: !HTTP,
    _opInput :: f a,
    _opOutput :: f a,
    _opPager :: Maybe b
  }

$(TH.makeLenses ''Operation)

operationNS :: NS -> Id -> NS
operationNS ns = mappend ns . mkNS . typeId

inputName, outputName :: HasId a => Operation Identity a b -> Id
inputName = identifier . Lens.view (opInput . _Identity)
outputName = identifier . Lens.view (opOutput . _Identity)

instance HasHTTP (Operation f a b) where
  hTTP = opHTTP

instance FromJSON (Operation Maybe (RefF ()) ()) where
  parseJSON = JSON.withObject "operation" $ \o ->
    Operation
      <$> (o .: "name" <&> mkId . renameOperation)
      <*> o .:? "documentation"
      <*> o .:? "deprecated" .!= False
      <*> o .: "http"
      <*> o .:? "input"
      <*> o .:? "output"
      <*> pure Nothing

instance ToJSON a => ToJSON (Operation Identity a b) where
  toJSON o =
    JSON.object
      [ "name" .= (o ^. opName),
        "documentation" .= (o ^. opDocumentation),
        "input" .= (o ^. opInput),
        "output" .= (o ^. opOutput),
        "pager" .= (o ^. opPager . Lens.to isJust)
      ]

data Metadata f = Metadata
  { _protocol :: !Protocol,
    _serviceAbbrev :: Text,
    _serviceConfig :: Text,
    _serviceFullName :: Text,
    _apiVersion :: Text,
    _signatureVersion :: !Signature,
    _endpointPrefix :: Text,
    _timestampFormat :: f Timestamp,
    _checksumFormat :: f Checksum,
    _xmlNamespace :: Maybe Text,
    _jsonVersion :: Maybe Text,
    _targetPrefix :: Maybe Text
  }
  deriving (Generic)

deriving instance Show (Metadata Maybe)

deriving instance Show (Metadata Identity)

$(TH.makeClassy ''Metadata)

instance FromJSON (Metadata Maybe) where
  parseJSON = JSON.withObject "meta" $ \o ->
    Metadata
      <$> o .: "protocol"
      <*> o .: "serviceAbbreviation"
      <*> (o .: "serviceAbbreviation" <&> serviceFunction)
      <*> (o .: "serviceFullName" <&> renameService)
      <*> o .: "apiVersion"
      <*> o .: "signatureVersion"
      <*> o .: "endpointPrefix"
      <*> o .:? "timestampFormat"
      <*> o .:? "checksumFormat"
      <*> o .:? "xmlNamespace"
      <*> o .:? "jsonVersion"
      <*> o .:? "targetPrefix"

instance ToJSON (Metadata Identity) where
  toJSON = gToJSON' camel

serviceError :: HasMetadata a f => a -> Text
serviceError m =
  case m ^. protocol of
    JSON -> "parseJSONError"
    RestJSON -> "parseJSONError"
    RestXML -> "parseXMLError"
    Query -> "parseXMLError"
    EC2 -> "parseXMLError"
    APIGateway -> "parseJSONError"

data Service f a b c = Service
  { _metadata' :: Metadata f,
    _documentation :: Help,
    _operations :: Map Id (Operation f a (Pager Id)),
    _shapes :: Map Id b,
    _waiters :: Map Id c,
    _retry :: Retry
  }
  deriving (Generic)

$(TH.makeClassy ''Service)

instance HasMetadata (Service f a b c) f where
  metadata = metadata'

instance FromJSON (Service Maybe (RefF ()) (ShapeF ()) (Waiter Id)) where
  parseJSON = JSON.withObject "service" $ \o -> do
    m <- o .: "metadata"
    p <- o .:? "pagination" .!= mempty
    Service m
      <$> o .: "documentation"
      <*> (o .: "operations" <&> Map.map (pager p))
      <*> o .: "shapes"
      <*> o .:? "waiters" .!= mempty
      <*> parseRetry (m ^. serviceAbbrev) o
    where
      pager ::
        Map Id (Pager Id) ->
        Operation f a () ->
        Operation f a (Pager Id)
      pager ps o = o & opPager .~ Map.lookup (o ^. opName) ps

type Shape = Cofree ShapeF

type Ref = RefF (Shape Solved)

class IsStreaming a where
  isStreaming :: a -> Bool
  default isStreaming :: HasInfo a => a -> Bool
  isStreaming = Lens.view infoStreaming

instance IsStreaming Info

instance IsStreaming (StructF a)

instance IsStreaming (ShapeF a)

instance IsStreaming (Shape a)

instance IsStreaming a => IsStreaming (RefF a) where
  isStreaming r = _refStreaming r || isStreaming (_refAnn r)

instance IsStreaming TType where
  isStreaming TStream = True
  isStreaming _ = False

setRequired :: ([Id] -> [Id]) -> ShapeF a -> ShapeF a
setRequired f = _Struct . required' %~ nub . f

getRequired :: Lens.Fold (StructF a) Id
getRequired = required' . Lens.each
