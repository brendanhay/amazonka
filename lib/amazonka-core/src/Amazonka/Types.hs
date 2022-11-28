-- |
-- Module      : Amazonka.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Types
  ( -- * Authentication

    -- ** Credentials
    AccessKey (..),
    SecretKey (..),
    SessionToken (..),

    -- ** Environment
    Auth (..),
    withAuth,
    AuthEnv (..),

    -- * Logging
    LogLevel (..),
    Logger,

    -- * Signing
    Algorithm,
    Meta (..),
    Signer (..),
    Signed (..),

    -- * Service
    Abbrev,
    Service (..),
    S3AddressingStyle (..),

    -- * Requests
    AWSRequest (..),
    Request (..),
    requestSign,
    requestPresign,
    requestUnsigned,

    -- * Retries
    Retry (..),

    -- * Errors
    AsError (..),
    Error (..),

    -- ** HTTP Errors
    Client.HttpException,

    -- ** Serialize Errors
    SerializeError (..),

    -- ** Service Errors
    ServiceError (..),

    -- ** Error Types
    ErrorCode (..),
    newErrorCode,
    ErrorMessage (..),
    RequestId (..),

    -- * Regions
    Region
      ( NorthVirginia,
        Ohio,
        NorthCalifornia,
        Oregon,
        GovCloudWest,
        GovCloudEast,
        Montreal,
        SaoPaulo,
        Frankfurt,
        Ireland,
        London,
        Milan,
        Paris,
        Stockholm,
        Bahrain,
        CapeTown,
        Beijing,
        Ningxia,
        HongKong,
        Tokyo,
        Seoul,
        Osaka,
        Singapore,
        Sydney,
        Mumbai,
        ..
      ),

    -- * Endpoints
    Endpoint (..),

    -- * HTTP
    ClientRequest,
    ClientResponse,
    ClientBody,
    newClientRequest,

    -- ** Seconds
    Seconds (..),
    toSeconds,
    toMicroseconds,
  )
where

import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Data
import Amazonka.Prelude hiding (error)
import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit (ConduitM)
import Data.IORef (IORef, readIORef)
import qualified Data.Text as Text
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types.Method (StdMethod)
import Network.HTTP.Types.Status (Status)

-- | A convenience alias to avoid type ambiguity.
type ClientRequest = Client.Request

-- | Construct a 'ClientRequest' using common parameters such as TLS and prevent
-- throwing errors when receiving erroneous status codes in respones.
newClientRequest :: Endpoint -> Maybe Seconds -> ClientRequest
newClientRequest Endpoint {host, secure, port} timeout =
  Client.defaultRequest
    { Client.secure = secure,
      Client.host = host,
      Client.port = port,
      Client.redirectCount = 0,
      Client.responseTimeout =
        case timeout of
          Nothing -> Client.responseTimeoutNone
          Just n -> Client.responseTimeoutMicro (toMicroseconds n)
    }

-- | A convenience alias encapsulating the common 'Response'.
type ClientResponse = Client.Response

-- | A convenience alias encapsulating the common 'Response' body.
type ClientBody = ConduitM () ByteString (ResourceT IO) ()

-- | Abbreviated service name.
newtype Abbrev = Abbrev {fromAbbrev :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromXML, FromJSON, FromText, ToText, ToLog)

newtype ErrorCode = ErrorCode Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToText, ToLog)

instance IsString ErrorCode where
  fromString = newErrorCode . fromString

instance FromJSON ErrorCode where
  parseJSON = parseJSONText "ErrorCode"

instance FromXML ErrorCode where
  parseXML = parseXMLText "ErrorCode"

instance FromText ErrorCode where
  fromText = pure . newErrorCode

-- | Construct an 'ErrorCode'.
newErrorCode :: Text -> ErrorCode
newErrorCode = ErrorCode . strip . unnamespace
  where
    -- Common suffixes are stripped since the service definitions are ambigiuous
    -- as to whether the error shape's name, or the error code is present
    -- in the response.
    strip x =
      fromMaybe x $
        Text.stripSuffix "Exception" x <|> Text.stripSuffix "Fault" x

    -- Removing the (potential) leading ...# namespace.
    unnamespace x =
      case Text.break (== '#') x of
        (ns, e)
          | Text.null e -> ns
          | otherwise -> Text.drop 1 e

newtype ErrorMessage = ErrorMessage {fromErrorMessage :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromXML, FromJSON, FromText, ToText, ToLog)

newtype RequestId = RequestId {fromRequestId :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromXML, FromJSON, FromText, ToText, ToLog)

-- | An error type representing errors that can be attributed to this library.
data Error
  = TransportError Client.HttpException
  | SerializeError SerializeError
  | ServiceError ServiceError
  deriving stock (Show, Generic)

instance Exception Error

instance ToLog Error where
  build = \case
    TransportError e -> build e
    SerializeError e -> build e
    ServiceError e -> build e

data SerializeError = SerializeError'
  { abbrev :: Abbrev,
    status :: Status,
    -- | The response body, if the response was not streaming.
    body :: Maybe ByteStringLazy,
    message :: String
  }
  deriving stock (Eq, Show, Generic)

instance ToLog SerializeError where
  build SerializeError' {..} =
    buildLines
      [ "[SerializeError] {",
        "  service = " <> build abbrev,
        "  status  = " <> build status,
        "  message = " <> build message,
        "  body    = " <> build body,
        "}"
      ]

data ServiceError = ServiceError'
  { abbrev :: Abbrev,
    status :: Status,
    headers :: [Header],
    code :: ErrorCode,
    message :: Maybe ErrorMessage,
    requestId :: Maybe RequestId
  }
  deriving stock (Eq, Show, Generic)

instance ToLog ServiceError where
  build ServiceError' {..} =
    buildLines
      [ "[ServiceError] {",
        "  service    = " <> build abbrev,
        "  status     = " <> build status,
        "  code       = " <> build code,
        "  message    = " <> build message,
        "  request-id = " <> build requestId,
        "}"
      ]

class AsError a where
  -- | A general Amazonka error.
  _Error :: Prism' a Error

  {-# MINIMAL _Error #-}

  -- | An error occured while communicating over HTTP with a remote service.
  _TransportError :: Prism' a Client.HttpException

  -- | A serialisation error occured when attempting to deserialise a response.
  _SerializeError :: Prism' a SerializeError

  -- | A service specific error returned by the remote service.
  _ServiceError :: Prism' a ServiceError

  _TransportError = _Error . _TransportError
  _SerializeError = _Error . _SerializeError
  _ServiceError = _Error . _ServiceError

instance AsError SomeException where
  _Error = Lens.exception

instance AsError Error where
  _Error = id

  _TransportError = Lens.prism TransportError $ \case
    TransportError e -> Right e
    x -> Left x

  _SerializeError = Lens.prism SerializeError $ \case
    SerializeError e -> Right e
    x -> Left x

  _ServiceError = Lens.prism ServiceError $ \case
    ServiceError e -> Right e
    x -> Left x

data Endpoint = Endpoint
  { host :: ByteString,
    secure :: Bool,
    port :: Int,
    scope :: ByteString
  }
  deriving stock (Eq, Show)

data LogLevel
  = -- | Info messages supplied by the user - this level is not emitted by the library.
    Info
  | -- | Error messages only.
    Error
  | -- | Useful debug information + info + error levels.
    Debug
  | -- | Includes potentially sensitive signing metadata, and non-streaming response bodies.
    Trace
  deriving stock (Eq, Ord, Enum, Show, Generic)

instance FromText LogLevel where
  fromText = \case
    "info" -> pure Info
    "error" -> pure Error
    "debug" -> pure Debug
    "trace" -> pure Trace
    other -> Left ("Failure parsing LogLevel from " ++ show other)

instance ToText LogLevel where
  toText = \case
    Info -> "info"
    Error -> "error"
    Debug -> "debug"
    Trace -> "trace"

instance ToByteString LogLevel

-- | A function threaded through various request and serialisation routines
-- to log informational and debug messages.
type Logger = LogLevel -> ByteStringBuilder -> IO ()

-- | Constants and predicates used to create a 'RetryPolicy'.
data Retry = Exponential
  { base :: Double,
    growth :: Int,
    attempts :: Int,
    -- | Returns a descriptive name for logging
    -- if the request should be retried.
    check :: ServiceError -> Maybe Text
  }
  deriving stock (Generic)

-- | Signing algorithm specific metadata.
data Meta where
  Meta :: ToLog a => a -> Meta

instance ToLog Meta where
  build (Meta m) = build m

-- | A signed 'ClientRequest' and associated metadata specific
-- to the signing algorithm, tagged with the initial request type
-- to be able to obtain the associated response, @'AWSResponse' a@.
data Signed a = Signed
  { signedMeta :: Meta,
    signedRequest :: ClientRequest
  }

type Algorithm a = Request a -> AuthEnv -> Region -> UTCTime -> Signed a

data Signer = Signer
  { sign :: forall a. Algorithm a,
    presign :: forall a. Seconds -> Algorithm a
  }

-- | Attributes and functions specific to an AWS service.
data Service = Service
  { abbrev :: Abbrev,
    signer :: Signer,
    signingName :: ByteString,
    version :: ByteString,
    -- | Only service bindings using the s3vhost request plugin
    -- (configured in the generator) will care about this field. It is
    -- ignored otherwise.
    s3AddressingStyle :: S3AddressingStyle,
    endpointPrefix :: ByteString,
    endpoint :: (Region -> Endpoint),
    timeout :: (Maybe Seconds),
    check :: (Status -> Bool),
    error :: (Status -> [Header] -> ByteStringLazy -> Error),
    retry :: Retry
  }
  deriving stock (Generic)

-- | When to rewrite S3 requests into /virtual-hosted style/.
--
-- Requests to S3 can be rewritten to access buckets by setting the
-- @Host:@ header, which allows you to point a @CNAME@ record at an
-- Amazon S3 Bucket.
--
-- Non-S3 object stores usually do not support this, which is usually
-- the only time you'll need to change this.
--
-- /See:/ [Virtual hosting of buckets](https://docs.aws.amazon.com/AmazonS3/latest/userguide/VirtualHosting.html)
-- in the Amazon S3 User Guide.
--
-- /See:/ [Changing the Addressing Style](https://boto3.amazonaws.com/v1/documentation/api/1.9.42/guide/s3.html#changing-the-addressing-style)
-- for the corresponding option in Boto 3.
data S3AddressingStyle
  = -- | Rewrite S3 request paths only if they can be expressed
    -- as a DNS label. This is the default.
    S3AddressingStyleAuto
  | -- | Do not ever rewrite S3 request paths.
    S3AddressingStylePath
  | -- | Force virtual hosted style rewrites without checking the
    -- bucket name.
    S3AddressingStyleVirtual
  deriving stock (Eq, Show, Generic)

-- | An unsigned request.
data Request a = Request
  { service :: Service,
    method :: StdMethod,
    path :: RawPath,
    query :: QueryString,
    headers :: [Header],
    body :: RequestBody
  }
  deriving stock (Generic)

requestSign :: Algorithm a
requestSign rq@Request {service = Service {signer = Signer {sign}}} = sign rq

requestPresign :: Seconds -> Algorithm a
requestPresign ex rq@Request {service = Service {signer = Signer {presign}}} =
  presign ex rq

-- | Create an unsigned 'ClientRequest'. You will almost never need to do this.
requestUnsigned :: Request a -> Region -> ClientRequest
requestUnsigned Request {service = Service {..}, ..} r =
  (newClientRequest end timeout)
    { Client.method = toBS method,
      Client.path = toBS (escapePath path),
      Client.queryString = toBS query,
      Client.requestHeaders = headers,
      Client.requestBody = toRequestBody body
    }
  where
    end = endpoint r

-- | Specify how a request can be de/serialised.
class AWSRequest a where
  -- | The successful, expected response associated with a request.
  type AWSResponse a :: *

  request ::
    -- | Overrides applied to the default 'Service'.
    (Service -> Service) ->
    a ->
    Request a

  response ::
    MonadResource m =>
    Logger ->
    Service ->
    Proxy a ->
    ClientResponse ClientBody ->
    m (Either Error (ClientResponse (AWSResponse a)))

-- | An access key ID.
--
-- For example: @AKIAIOSFODNN7EXAMPLE@
--
-- /See:/ <http://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html Understanding and Getting Your Security Credentials>.
newtype AccessKey = AccessKey ByteString
  deriving stock (Eq, Show, Read, Generic)
  deriving newtype
    ( IsString,
      ToText,
      FromText,
      ToLog,
      ToByteString,
      ToQuery,
      FromXML,
      ToXML,
      Hashable,
      NFData
    )

instance ToJSON AccessKey where
  toJSON = toJSONText

instance FromJSON AccessKey where
  parseJSON = parseJSONText "AccessKey"

-- | Secret access key credential.
--
-- For example: @wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKE@
--
-- /See:/ <http://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html Understanding and Getting Your Security Credentials>.
newtype SecretKey = SecretKey ByteString
  deriving stock (Eq, Generic)
  deriving newtype
    ( IsString,
      ToText,
      FromText,
      ToByteString,
      FromXML,
      ToXML,
      Hashable,
      NFData
    )

instance ToJSON SecretKey where
  toJSON = toJSONText

instance FromJSON SecretKey where
  parseJSON = parseJSONText "SecretKey"

-- | A session token used by STS to temporarily authorise access to
-- an AWS resource.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials>.
newtype SessionToken = SessionToken ByteString
  deriving stock (Eq, Generic)
  deriving newtype
    ( IsString,
      ToText,
      FromText,
      ToByteString,
      FromXML,
      ToXML,
      Hashable,
      NFData
    )

instance ToJSON SessionToken where
  toJSON = toJSONText

instance FromJSON SessionToken where
  parseJSON = parseJSONText "SessionToken"

-- | The AuthN/AuthZ credential environment.
data AuthEnv = AuthEnv
  { accessKeyId :: AccessKey,
    secretAccessKey :: (Sensitive SecretKey),
    sessionToken :: Maybe (Sensitive SessionToken),
    expiration :: Maybe ISO8601
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToLog AuthEnv where
  build AuthEnv {..} =
    buildLines
      [ "[Amazonka Auth] {",
        "  access key id     = " <> build accessKeyId,
        "  secret access key = " <> build secretAccessKey,
        "  session token     = " <> build sessionToken,
        "  expiration        = " <> build (fmap (Lens.view _Time) expiration),
        "}"
      ]

instance FromJSON AuthEnv where
  parseJSON = withObject "AuthEnv" $ \o ->
    AuthEnv
      <$> o .: "AccessKeyId"
      <*> o .: "SecretAccessKey"
      <*> o .:? "Token"
      <*> o .:? "Expiration"

instance FromXML AuthEnv where
  parseXML x =
    AuthEnv
      <$> x .@ "AccessKeyId"
      <*> x .@ "SecretAccessKey"
      <*> x .@? "SessionToken"
      <*> x .@? "Expiration"

-- | An authorisation environment containing AWS credentials, and potentially
-- a reference which can be refreshed out-of-band as temporary credentials expire.
data Auth
  = Ref ThreadId (IORef AuthEnv)
  | Auth AuthEnv

instance ToLog Auth where
  build (Ref t _) = "[Amazonka Auth] { <thread:" <> build (show t) <> "> }"
  build (Auth e) = build e

withAuth :: MonadIO m => Auth -> (AuthEnv -> m a) -> m a
withAuth (Ref _ r) f = liftIO (readIORef r) >>= f
withAuth (Auth e) f = f e

-- | The available AWS regions.
newtype Region = Region' {fromRegion :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype
    ( IsString,
      Hashable,
      NFData,
      ToText,
      FromText,
      ToQuery,
      ToXML,
      FromXML,
      ToJSON,
      FromJSON,
      ToByteString,
      ToLog
    )

-- North America

pattern NorthVirginia :: Region
pattern NorthVirginia = Region' "us-east-1"

pattern Ohio :: Region
pattern Ohio = Region' "us-east-2"

pattern NorthCalifornia :: Region
pattern NorthCalifornia = Region' "us-west-1"

pattern Oregon :: Region
pattern Oregon = Region' "us-west-2"

pattern GovCloudWest :: Region
pattern GovCloudWest = Region' "us-gov-west-1"

pattern GovCloudEast :: Region
pattern GovCloudEast = Region' "us-gov-east-1"

pattern Montreal :: Region
pattern Montreal = Region' "ca-central-1"

-- South America

pattern SaoPaulo :: Region
pattern SaoPaulo = Region' "sa-east-1"

-- Europe

pattern Frankfurt :: Region
pattern Frankfurt = Region' "eu-central-1"

pattern Ireland :: Region
pattern Ireland = Region' "eu-west-1"

pattern London :: Region
pattern London = Region' "eu-west-2"

pattern Milan :: Region
pattern Milan = Region' "eu-south-1"

pattern Paris :: Region
pattern Paris = Region' "eu-west-3"

pattern Stockholm :: Region
pattern Stockholm = Region' "eu-north-1"

-- Middle East

pattern Bahrain :: Region
pattern Bahrain = Region' "me-south-1"

-- Africa

pattern CapeTown :: Region
pattern CapeTown = Region' "af-south-1"

-- Asia Pacific

pattern Beijing :: Region
pattern Beijing = Region' "cn-north-1"

pattern Ningxia :: Region
pattern Ningxia = Region' "cn-northwest-1"

pattern HongKong :: Region
pattern HongKong = Region' "ap-east-1"

pattern Tokyo :: Region
pattern Tokyo = Region' "ap-northeast-1"

pattern Seoul :: Region
pattern Seoul = Region' "ap-northeast-2"

pattern Osaka :: Region
pattern Osaka = Region' "ap-northeast-3"

pattern Singapore :: Region
pattern Singapore = Region' "ap-southeast-1"

pattern Sydney :: Region
pattern Sydney = Region' "ap-southeast-2"

pattern Mumbai :: Region
pattern Mumbai = Region' "ap-south-1"

{-# COMPLETE
  NorthVirginia,
  Ohio,
  NorthCalifornia,
  Oregon,
  GovCloudWest,
  GovCloudEast,
  Montreal,
  SaoPaulo,
  Frankfurt,
  Ireland,
  London,
  Milan,
  Paris,
  Stockholm,
  Bahrain,
  CapeTown,
  Beijing,
  Ningxia,
  HongKong,
  Tokyo,
  Seoul,
  Osaka,
  Singapore,
  Sydney,
  Mumbai,
  Region'
  #-}

-- | A numeric value representing seconds.
newtype Seconds = Seconds DiffTime
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (Enum, Num, Real, NFData)

instance Hashable Seconds where
  hashWithSalt salt = hashWithSalt salt . toRational . toSeconds

instance FromText Seconds where
  fromText t =
    maybe (Left err) (Right . Seconds) $
      parseTimeM False defaultTimeLocale diffTimeFormatString str
    where
      str = Text.unpack t
      err =
        "Seconds value failed to parse as expected format ("
          <> diffTimeFormatString
          <> "): "
          <> str

instance ToText Seconds where
  toText =
    Text.pack . formatTime defaultTimeLocale diffTimeFormatString . toSeconds

-- | Format string used in parse/format options
--
-- Currently @%Es@, which is "total seconds, with decimal point and up to
-- <width> (default 12) decimal places, without trailing zeros. For a whole
-- number of seconds, %Es omits the decimal point unless padding is specified."
--
-- We also use 'defaultTimeLocale', which means @0.1@ and not @0,1@.
diffTimeFormatString :: String
diffTimeFormatString = "%Es"

instance ToByteString Seconds

instance ToQuery Seconds

instance ToLog Seconds where
  build s = build (toText s) <> "s"

toSeconds :: Seconds -> DiffTime
toSeconds (Seconds n)
  | n < 0 = 0
  | otherwise = n

toMicroseconds :: Seconds -> Int
toMicroseconds = round . (1000000 *) . toSeconds
