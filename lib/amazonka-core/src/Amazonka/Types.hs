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
    authAccessKeyId,
    authSecretAccessKey,
    authSessionToken,
    authExpiration,

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
    serviceSigner,
    serviceEndpoint,
    serviceTimeout,
    serviceCheck,
    serviceRetry,

    -- * Requests
    AWSRequest (..),
    Request (..),
    requestService,
    requestMethod,
    requestHeaders,
    requestPath,
    requestQuery,
    requestBody,
    requestSign,
    requestPresign,
    requestUnsigned,

    -- * Retries
    Retry (..),
    exponentBase,
    exponentGrowth,
    retryAttempts,
    retryCheck,

    -- * Errors
    AsError (..),
    Error (..),

    -- ** HTTP Errors
    Client.HttpException,

    -- ** Serialize Errors
    SerializeError (..),
    serializeAbbrev,
    serializeStatus,
    serializeMessage,

    -- ** Service Errors
    ServiceError (..),
    serviceAbbrev,
    serviceStatus,
    serviceHeaders,
    serviceCode,
    serviceMessage,
    serviceRequestId,

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
    endpointHost,
    endpointPort,
    endpointSecure,
    endpointScope,

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
    { Client.secure = _endpointSecure endpoint,
      Client.host = _endpointHost endpoint,
      Client.port = _endpointPort endpoint,
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
  { _serializeErrorAbbrev :: Abbrev,
    _serializeErrorStatus :: Status,
    -- | The response body, if the response was not streaming.
    _serializeErrorBody :: Maybe ByteStringLazy,
    _serializeErrorMessage :: String
  }
  deriving stock (Eq, Show, Generic)

instance ToLog SerializeError where
  build SerializeError' {..} =
    buildLines
      [ "[SerializeError] {",
        "  service = " <> build _serializeErrorAbbrev,
        "  status  = " <> build _serializeErrorStatus,
        "  message = " <> build _serializeErrorMessage,
        "  body    = " <> build _serializeErrorBody,
        "}"
      ]

serializeAbbrev :: Lens' SerializeError Abbrev
serializeAbbrev = Lens.lens _serializeErrorAbbrev (\s a -> s {_serializeErrorAbbrev = a})

serializeStatus :: Lens' SerializeError Status
serializeStatus = Lens.lens _serializeErrorStatus (\s a -> s {_serializeErrorStatus = a})

serializeMessage :: Lens' SerializeError String
serializeMessage = Lens.lens _serializeErrorMessage (\s a -> s {_serializeErrorMessage = a})

data ServiceError = ServiceError'
  { _serviceErrorAbbrev :: Abbrev,
    _serviceErrorStatus :: Status,
    _serviceErrorHeaders :: [Header],
    _serviceErrorCode :: ErrorCode,
    _serviceErrorMessage :: Maybe ErrorMessage,
    _serviceErrorRequestId :: Maybe RequestId
  }
  deriving stock (Eq, Show, Generic)

instance ToLog ServiceError where
  build ServiceError' {..} =
    buildLines
      [ "[ServiceError] {",
        "  service    = " <> build _serviceErrorAbbrev,
        "  status     = " <> build _serviceErrorStatus,
        "  code       = " <> build _serviceErrorCode,
        "  message    = " <> build _serviceErrorMessage,
        "  request-id = " <> build _serviceErrorRequestId,
        "}"
      ]

serviceAbbrev :: Lens' ServiceError Abbrev
serviceAbbrev = Lens.lens _serviceErrorAbbrev (\s a -> s {_serviceErrorAbbrev = a})

serviceStatus :: Lens' ServiceError Status
serviceStatus = Lens.lens _serviceErrorStatus (\s a -> s {_serviceErrorStatus = a})

serviceHeaders :: Lens' ServiceError [Header]
serviceHeaders = Lens.lens _serviceErrorHeaders (\s a -> s {_serviceErrorHeaders = a})

serviceCode :: Lens' ServiceError ErrorCode
serviceCode = Lens.lens _serviceErrorCode (\s a -> s {_serviceErrorCode = a})

serviceMessage :: Lens' ServiceError (Maybe ErrorMessage)
serviceMessage = Lens.lens _serviceErrorMessage (\s a -> s {_serviceErrorMessage = a})

serviceRequestId :: Lens' ServiceError (Maybe RequestId)
serviceRequestId = Lens.lens _serviceErrorRequestId (\s a -> s {_serviceErrorRequestId = a})

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
  { _endpointHost :: ByteString,
    _endpointSecure :: Bool,
    _endpointPort :: Int,
    _endpointScope :: ByteString
  }
  deriving stock (Eq, Show)

endpointHost :: Lens' Endpoint ByteString
endpointHost = Lens.lens _endpointHost (\s a -> s {_endpointHost = a})

endpointSecure :: Lens' Endpoint Bool
endpointSecure = Lens.lens _endpointSecure (\s a -> s {_endpointSecure = a})

endpointPort :: Lens' Endpoint Int
endpointPort = Lens.lens _endpointPort (\s a -> s {_endpointPort = a})

endpointScope :: Lens' Endpoint ByteString
endpointScope = Lens.lens _endpointScope (\s a -> s {_endpointScope = a})

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
  { _retryBase :: Double,
    _retryGrowth :: Int,
    _retryAttempts :: Int,
    -- | Returns a descriptive name for logging
    -- if the request should be retried.
    _retryCheck :: ServiceError -> Maybe Text
  }
  deriving stock (Generic)

exponentBase :: Lens' Retry Double
exponentBase = Lens.lens _retryBase (\s a -> s {_retryBase = a})

exponentGrowth :: Lens' Retry Int
exponentGrowth = Lens.lens _retryGrowth (\s a -> s {_retryGrowth = a})

retryAttempts :: Lens' Retry Int
retryAttempts = Lens.lens _retryAttempts (\s a -> s {_retryAttempts = a})

retryCheck :: Lens' Retry (ServiceError -> Maybe Text)
retryCheck = Lens.lens _retryCheck (\s a -> s {_retryCheck = a})

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
  { _serviceAbbrev :: Abbrev,
    _serviceSigner :: Signer,
    _serviceSigningName :: ByteString,
    _serviceVersion :: ByteString,
    _serviceEndpointPrefix :: ByteString,
    _serviceEndpoint :: (Region -> Endpoint),
    _serviceTimeout :: (Maybe Seconds),
    _serviceCheck :: (Status -> Bool),
    _serviceError :: (Status -> [Header] -> ByteStringLazy -> Error),
    _serviceRetry :: Retry
  }
  deriving stock (Generic)

serviceSigner :: Lens' Service Signer
serviceSigner = Lens.lens _serviceSigner (\s a -> s {_serviceSigner = a})

serviceEndpoint :: Setter' Service Endpoint
serviceEndpoint = Lens.sets (\f s -> s {_serviceEndpoint = \r -> f (_serviceEndpoint s r)})

serviceTimeout :: Lens' Service (Maybe Seconds)
serviceTimeout = Lens.lens _serviceTimeout (\s a -> s {_serviceTimeout = a})

serviceCheck :: Lens' Service (Status -> Bool)
serviceCheck = Lens.lens _serviceCheck (\s a -> s {_serviceCheck = a})

serviceRetry :: Lens' Service Retry
serviceRetry = Lens.lens _serviceRetry (\s a -> s {_serviceRetry = a})

-- | An unsigned request.
data Request a = Request
  { _requestService :: Service,
    _requestMethod :: StdMethod,
    _requestPath :: RawPath,
    _requestQuery :: QueryString,
    _requestHeaders :: [Header],
    _requestBody :: RequestBody
  }
  deriving stock (Generic)

requestService :: Lens' (Request a) Service
requestService = Lens.lens _requestService (\s a -> s {_requestService = a})

requestBody :: Lens' (Request a) RequestBody
requestBody = Lens.lens _requestBody (\s a -> s {_requestBody = a})

requestHeaders :: Lens' (Request a) [Header]
requestHeaders = Lens.lens _requestHeaders (\s a -> s {_requestHeaders = a})

requestMethod :: Lens' (Request a) StdMethod
requestMethod = Lens.lens _requestMethod (\s a -> s {_requestMethod = a})

requestPath :: Lens' (Request a) RawPath
requestPath = Lens.lens _requestPath (\s a -> s {_requestPath = a})

requestQuery :: Lens' (Request a) QueryString
requestQuery = Lens.lens _requestQuery (\s a -> s {_requestQuery = a})

requestSign :: Algorithm a
requestSign x = signerSign (_serviceSigner (_requestService x)) x

requestPresign :: Seconds -> Algorithm a
requestPresign ex x = signerPresign (_serviceSigner (_requestService x)) ex x

-- | Create an unsigned 'ClientRequest'. You will almost never need to do this.
requestUnsigned :: Request a -> Region -> ClientRequest
requestUnsigned Request {..} r =
  (newClientRequest end _serviceTimeout)
    { Client.method = toBS _requestMethod,
      Client.path = toBS (escapePath _requestPath),
      Client.queryString = toBS _requestQuery,
      Client.requestHeaders = _requestHeaders,
      Client.requestBody = toRequestBody _requestBody
    }
  where
    end = _serviceEndpoint r
    Service {..} = _requestService

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
  { _authAccessKeyId :: AccessKey,
    _authSecretAccessKey :: (Sensitive SecretKey),
    _authSessionToken :: Maybe (Sensitive SessionToken),
    _authExpiration :: Maybe ISO8601
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToLog AuthEnv where
  build AuthEnv {..} =
    buildLines
      [ "[Amazonka Auth] {",
        "  access key id     = " <> build _authAccessKeyId,
        "  secret access key = " <> build _authSecretAccessKey,
        "  session token     = " <> build _authSessionToken,
        "  expiration        = " <> build (fmap (Lens.view _Time) _authExpiration),
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

-- | The access key ID that identifies the temporary security credentials.
authAccessKeyId :: Lens' AuthEnv AccessKey
authAccessKeyId =
  Lens.lens _authAccessKeyId (\s a -> s {_authAccessKeyId = a})

-- | The secret access key that can be used to sign requests.
authSecretAccessKey :: Lens' AuthEnv SecretKey
authSecretAccessKey =
  Lens.lens _authSecretAccessKey (\s a -> s {_authSecretAccessKey = a})
    . _Sensitive

-- | The token that users must pass to the service API to use the temporary
-- credentials.
authSessionToken :: Lens' AuthEnv (Maybe SessionToken)
authSessionToken =
  Lens.lens _authSessionToken (\s a -> s {_authSessionToken = a})
    . Lens.mapping _Sensitive

-- | The date on which the current credentials expire.
authExpiration :: Lens' AuthEnv (Maybe UTCTime)
authExpiration =
  Lens.lens _authExpiration (\s a -> s {_authExpiration = a})
    . Lens.mapping _Time

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
