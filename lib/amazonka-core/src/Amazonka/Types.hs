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

    -- * Requests
    AWSRequest (..),
    Request (..),

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
    serviceCodeL,

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

import Amazonka.Data
import qualified Amazonka.Lens as Lens
import Amazonka.Prelude
import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit (ConduitM)
import Data.IORef (IORef, readIORef)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types.Method (StdMethod)
import Network.HTTP.Types.Status (Status)

-- | A convenience alias to avoid type ambiguity.
type ClientRequest = Client.Request

-- | Construct a 'ClientRequest' using common parameters such as TLS and prevent
-- throwing errors when receiving erroneous status codes in respones.
newClientRequest :: Endpoint -> Maybe Seconds -> ClientRequest
newClientRequest endpoint timeout =
  Client.defaultRequest
    { Client.secure = endpointSecure endpoint,
      Client.host = endpointHost endpoint,
      Client.port = endpointPort endpoint,
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
  deriving stock (Eq, Ord, Show, Generic)
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
  { serializeErrorAbbrev :: Abbrev,
    serializeErrorStatus :: Status,
    -- | The response body, if the response was not streaming.
    serializeErrorBody :: Maybe ByteStringLazy,
    serializeErrorMessage :: String
  }
  deriving stock (Eq, Show, Generic)

instance ToLog SerializeError where
  build SerializeError' {..} =
    buildLines
      [ "[SerializeError] {",
        "  service = " <> build serializeErrorAbbrev,
        "  status  = " <> build serializeErrorStatus,
        "  message = " <> build serializeErrorMessage,
        "  body    = " <> build serializeErrorBody,
        "}"
      ]

data ServiceError = ServiceError'
  { serviceErrorAbbrev :: Abbrev,
    serviceErrorStatus :: Status,
    serviceErrorHeaders :: [Header],
    serviceErrorCode :: ErrorCode,
    serviceErrorMessage :: Maybe ErrorMessage,
    serviceErrorRequestId :: Maybe RequestId
  }
  deriving stock (Eq, Show, Generic)

instance ToLog ServiceError where
  build ServiceError' {..} =
    buildLines
      [ "[ServiceError] {",
        "  service    = " <> build serviceErrorAbbrev,
        "  status     = " <> build serviceErrorStatus,
        "  code       = " <> build serviceErrorCode,
        "  message    = " <> build serviceErrorMessage,
        "  request-id = " <> build serviceErrorRequestId,
        "}"
      ]

serviceCodeL :: Lens' ServiceError ErrorCode
serviceCodeL = Lens.lens serviceErrorCode (\s a -> s {serviceErrorCode = a})

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
  { endpointHost :: ByteString,
    endpointSecure :: Bool,
    endpointPort :: Int,
    endpointScope :: ByteString
  }
  deriving stock (Eq, Show, Generic)


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
  { retryBase :: Double,
    retryGrowth :: Int,
    retryAttempts :: Int,
    -- | Returns a descriptive name for logging
    -- if the request should be retried.
    retryCheck :: ServiceError -> Maybe Text
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
  { signerSign :: forall a. Algorithm a,
    signerPresign :: forall a. Seconds -> Algorithm a
  }

-- | Attributes and functions specific to an AWS service.
data Service = Service
  { serviceAbbrev :: Abbrev,
    serviceSigner :: Signer,
    serviceSigningName :: ByteString,
    serviceVersion :: ByteString,
    serviceEndpointPrefix :: ByteString,
    serviceEndpoint :: (Region -> Endpoint),
    serviceTimeout :: (Maybe Seconds),
    serviceCheck :: (Status -> Bool),
    serviceError :: (Status -> [Header] -> ByteStringLazy -> Error),
    serviceRetry :: Retry
  }
  deriving stock (Generic)

-- | An unsigned request.
data Request a = Request
  { requestService :: Service,
    requestMethod :: StdMethod,
    requestPath :: RawPath,
    requestQuery :: QueryString,
    requestHeaders :: [Header],
    requestBody :: RequestBody
  }
  deriving stock (Generic)

-- | Create an unsigned 'ClientRequest'. You will almost never need to do this.
requestUnsigned :: Request a -> Region -> ClientRequest
requestUnsigned Request {..} r =
  (newClientRequest end serviceTimeout)
    { Client.method = toBS requestMethod,
      Client.path = toBS (escapePath requestPath),
      Client.queryString = toBS requestQuery,
      Client.requestHeaders = requestHeaders,
      Client.requestBody = toRequestBody requestBody
    }
  where
    end = serviceEndpoint r
    Service {..} = requestService

-- | Specify how a request can be de/serialised.
class AWSRequest a where
  -- | The successful, expected response associated with a request.
  type AWSResponse a :: *

  request :: a -> Request a
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
  { authAccessKeyId :: AccessKey,
    authSecretAccessKey :: (Sensitive SecretKey),
    authSessionToken :: Maybe (Sensitive SessionToken),
    authExpiration :: Maybe ISO8601
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToLog AuthEnv where
  build AuthEnv {..} =
    buildLines
      [ "[Amazonka Auth] {",
        "  access key id     = " <> build authAccessKeyId,
        "  secret access key = " <> build authSecretAccessKey,
        "  session token     = " <> build authSessionToken,
        "  expiration        = " <> build (fmap (Lens.view _Time) authExpiration),
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

-- | An integral value representing seconds.
newtype Seconds = Seconds Int
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype
    ( Enum,
      Num,
      Bounded,
      Integral,
      Real,
      ToQuery,
      ToByteString,
      ToText,
      FromText,
      Hashable,
      NFData
    )

instance ToLog Seconds where
  build s = build (toSeconds s) <> "s"

toSeconds :: Seconds -> Int
toSeconds (Seconds n)
  | n < 0 = 0
  | otherwise = n

toMicroseconds :: Seconds -> Int
toMicroseconds = (1000000 *) . toSeconds
