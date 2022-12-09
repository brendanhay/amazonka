{-# LANGUAGE TemplateHaskell #-}

module Gen.Types.Service where

import qualified Control.Comonad as Comonad
import qualified Control.Comonad.Cofree as Cofree
import qualified Control.Lens as Lens
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Text as Text
import Gen.Prelude
import Gen.TH
import Gen.Text
import Gen.Types.Ann
import Gen.Types.Help
import Gen.Types.Id
import Gen.Types.NS
import Gen.Types.Pager
import Gen.Types.Retry
import Gen.Types.URI
import Gen.Types.Waiter
import qualified Text.Show.Deriving as Deriving

$(Lens.makePrisms ''Identity)

data Signature
  = V2
  | V3
  | V3HTTPS
  | V4
  | S3
  | S3V4
  deriving (Eq, Show, Generic)

sigToText :: Signature -> Text
sigToText = \case
  V2 -> "v2"
  _ -> "v4"

instance FromJSON Signature where
  parseJSON = gParseJSON' lower

instance ToJSON Signature where
  toJSON = Aeson.String . sigToText

data Timestamp
  = RFC822
  | ISO8601
  | POSIX
  deriving (Eq, Show, Generic)

tsToText :: Timestamp -> Text
tsToText = Text.pack . show

instance FromJSON Timestamp where
  parseJSON = Aeson.withText "timestamp" $ \case
    "rfc822" -> pure RFC822
    "iso8601" -> pure ISO8601
    "unixTimestamp" -> pure POSIX
    e -> fail ("Unknown Timestamp: " ++ Text.unpack e)

instance ToJSON Timestamp where
  toJSON = Aeson.toJSON . tsToText

data Protocol
  = JSON
  | RestJSON
  | RestXML
  | Query
  | EC2
  | APIGateway
  deriving (Eq, Show, Generic)

instance FromJSON Protocol where
  parseJSON = Aeson.withText "protocol" $ \case
    "json" -> pure JSON
    "rest-json" -> pure RestJSON
    "rest-xml" -> pure RestXML
    "query" -> pure Query
    "ec2" -> pure EC2
    "api-gateway" -> pure APIGateway
    other -> fail $ "Failed to parse protocol from " ++ show other

instance ToJSON Protocol where
  toJSON =
    Aeson.String . \case
      JSON -> "json"
      RestJSON -> "rest-json"
      RestXML -> "rest-xml"
      Query -> "query"
      EC2 -> "ec2"
      APIGateway -> "api-gateway"

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
  | Uri
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

$(Lens.makeLenses ''XML)

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
    _refStreaming :: Bool,
    _refXMLAttribute :: Bool,
    _refXMLNamespace :: Maybe XML
  }
  deriving (Functor, Foldable, Traversable, Generic)

$(Deriving.deriveShow1 ''RefF)
$(Deriving.deriveShow ''RefF)
$(Lens.makeLenses ''RefF)

instance HasId (RefF a) where
  identifier = identifier . _refShape

instance FromJSON (RefF ()) where
  parseJSON = Aeson.withObject "ref" $ \o ->
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
    _errStatus :: Int,
    _errSenderFault :: Bool
  }
  deriving (Show, Generic)

$(Lens.makeLenses ''ErrorInfo)

instance FromJSON ErrorInfo where
  parseJSON = Aeson.withObject "error" $ \o ->
    ErrorInfo
      <$> o .:? "code"
      <*> (o .: "httpStatusCode" <&> parseStatusCode)
      <*> o .:? "senderFault" .!= False

data Info = Info
  { _infoDocumentation :: Maybe Help,
    _infoMin :: Maybe Scientific,
    _infoMax :: Maybe Scientific,
    _infoFlattened :: Bool,
    _infoSensitive :: Bool,
    _infoStreaming :: Bool,
    _infoException :: Bool,
    _infoError :: Maybe ErrorInfo
  }
  deriving (Show, Generic)

$(Lens.makeClassy ''Info)

instance FromJSON Info where
  parseJSON = Aeson.withObject "info" $ \o ->
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

$(Deriving.deriveShow1 ''ListF)
$(Deriving.deriveShow ''ListF)
$(Lens.makeLenses ''ListF)

instance HasInfo (ListF a) where
  info = listInfo

instance HasRefs ListF where
  references = listItem

instance FromJSON (Info -> ListF ()) where
  parseJSON = Aeson.withObject "list" $ \o ->
    flip ListF
      <$> o .: "member"

data MapF a = MapF
  { _mapInfo :: Info,
    _mapKey :: RefF a,
    _mapValue :: RefF a
  }
  deriving (Functor, Foldable, Traversable)

$(Deriving.deriveShow1 ''MapF)
$(Deriving.deriveShow ''MapF)
$(Lens.makeLenses ''MapF)

instance HasInfo (MapF a) where
  info = mapInfo

instance HasRefs MapF where
  references f (MapF i k v) = MapF i <$> f k <*> f v

instance FromJSON (Info -> MapF ()) where
  parseJSON = Aeson.withObject "map" $ \o -> do
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

$(Deriving.deriveShow1 ''StructF)
$(Deriving.deriveShow ''StructF)
$(Lens.makeLenses ''StructF)

instance HasInfo (StructF a) where
  info = structInfo

instance HasRefs StructF where
  references = Lens.traverseOf (members . Lens.each)

instance FromJSON (Info -> StructF ()) where
  parseJSON = Aeson.withObject "struct" $ \o -> do
    ms <- o .: "members"
    r <- o .:? "required" .!= mempty
    p <- o .:? "payload"
    return $ \i -> StructF i (body p ms) r p
    where
      -- This ensures that the field referenced by a possible
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

$(Deriving.deriveShow1 ''ShapeF)
$(Deriving.deriveShow ''ShapeF)
$(Lens.makePrisms ''ShapeF)

instance HasInfo (ShapeF a) where
  info f = \case
    Ptr i ds -> (`Ptr` ds) <$> f i
    List l -> List <$> info f l
    Map m -> Map <$> info f m
    Struct s -> Struct <$> info f s
    Enum i vs -> (`Enum` vs) <$> f i
    Lit i l -> (`Lit` l) <$> f i

instance HasInfo (Cofree ShapeF a) where
  info = Lens.lens Cofree.unwrap go . info
    where
      go s a = Comonad.extract s :< a

instance HasRefs ShapeF where
  references f = \case
    Ptr i ds -> pure (Ptr i ds)
    List l -> List <$> references f l
    Map m -> Map <$> references f m
    Struct s -> Struct <$> references f s
    Enum i vs -> pure (Enum i vs)
    Lit i l -> pure (Lit i l)

instance FromJSON (ShapeF ()) where
  parseJSON = Aeson.withObject "shape" $ \o -> do
    i <- Aeson.parseJSON (Aeson.Object o)
    t <- o .: "type"
    m <- o .:? "enum"

    case t of
      "list" -> List . ($ i) <$> Aeson.parseJSON (Aeson.Object o)
      "map" -> Map . ($ i) <$> Aeson.parseJSON (Aeson.Object o)
      "structure" -> Struct . ($ i) <$> Aeson.parseJSON (Aeson.Object o)
      "integer" -> pure (Lit i Int)
      "long" -> pure (Lit i Long)
      "double" -> pure (Lit i Double)
      "float" -> pure (Lit i Double)
      "blob" -> pure (Lit i Base64)
      "bytes" -> pure (Lit i Bytes)
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
    _opDeprecated :: Bool,
    _opHttp :: HTTP,
    _opInput :: f a,
    _opOutput :: f a,
    _opPager :: Maybe b
  }

$(Lens.makeLenses ''Operation)

operationNS :: NS -> Id -> NS
operationNS ns = mappend ns . mkNS . typeId

inputName, outputName :: HasId a => Operation Identity a b -> Id
inputName = identifier . Lens.view (opInput . _Identity)
outputName = identifier . Lens.view (opOutput . _Identity)

instance HasHTTP (Operation f a b) where
  hTTP = opHttp

instance FromJSON (Operation Maybe (RefF ()) ()) where
  parseJSON = Aeson.withObject "operation" $ \o ->
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
    Aeson.object
      [ "name" .= (o ^. opName),
        "documentation" .= (o ^. opDocumentation),
        "input" .= (o ^. opInput),
        "output" .= (o ^. opOutput),
        "pager" .= (o ^. opPager . Lens.to isJust)
      ]

data Metadata f = Metadata
  { _protocol :: Protocol,
    _serviceAbbrev :: Text,
    _serviceConfig :: Text,
    _serviceFullName :: Text,
    _signingName :: Text,
    _apiVersion :: Text,
    _signatureVersion :: Signature,
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

$(Lens.makeClassy ''Metadata)

instance FromJSON (Metadata Maybe) where
  parseJSON = Aeson.withObject "meta" $ \o ->
    Metadata
      <$> o .: "protocol"
      <*> o .: "serviceAbbreviation"
      <*> (o .: "serviceAbbreviation" <&> renameServiceFunction)
      <*> (o .: "serviceFullName" <&> renameService)
      <*> (o .: "signingName" <|> o .: "endpointPrefix")
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

$(Lens.makeClassy ''Service)

instance HasMetadata (Service f a b c) f where
  metadata = metadata'

instance FromJSON (Service Maybe (RefF ()) (ShapeF ()) (Waiter Id)) where
  parseJSON =
    Aeson.withObject "service" $ \o -> do
      m <- o .: "metadata"
      p <- o .:? "pagination" .!= mempty
      Service m
        <$> o .: "documentation"
        <*> (o .: "operations" <&> fmap (pager p))
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
  isStreaming = \case
    TStream -> True
    _ -> False

setRequired :: ([Id] -> [Id]) -> ShapeF a -> ShapeF a
setRequired f = _Struct . required' %~ List.nub . f

getRequired :: Lens.Fold (StructF a) Id
getRequired = required' . Lens.each
