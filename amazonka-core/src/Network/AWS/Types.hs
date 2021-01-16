-- |
-- Module      : Network.AWS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Types
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
    SigningAlgorithm,
    Signer (..),

    -- * Service
    Abbrev,
    Service (..),

    -- * Requests
    Request (..),
    Response,
    AWSRequest (..),

    -- * Retries
    Retry (..),

    -- * Errors
    AsError (..),
    Error (..),

    -- ** HTTP Errors
    Client.HttpException,

    -- ** Serialization Errors
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
        CapeTown,
        Montreal,
        Frankfurt,
        Ireland,
        London,
        Milan,
        Paris,
        Stockholm,
        HongKong,
        Tokyo,
        Seoul,
        Osaka,
        Singapore,
        Sydney,
        Mumbai,
        Bahrain,
        SaoPaulo,
        GovCloudWest,
        GovCloudEast,
        Beijing,
        Ningxia,
        Region'
      ),

    -- * Endpoints
    Endpoint (..),

    -- * HTTP
    ClientResponse,
    ClientRequest,
    newClientRequest,

    -- * Seconds
    Seconds (..),
    seconds,
    microseconds,
  )
where

import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit (ConduitM)
import Data.Dynamic (Dynamic)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Network.AWS.Data
import qualified Network.AWS.Lens as Lens
import Network.AWS.Prelude
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP

-- | A convenience alias encapsulating the common 'Response'.
type ClientResponse = Client.Response ResponseBody

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
          Just n -> Client.responseTimeoutMicro (microseconds n)
    }

-- | Abbreviated service name.
newtype Abbrev = Abbrev {fromAbbrev :: Text}
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
      FromJSON
    )

newtype ErrorCode = ErrorCode {fromErrorCode :: Text}
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
      FromJSON
    )

-- | Construct an 'ErrorCode'.
newErrorCode :: Text -> ErrorCode
newErrorCode = ErrorCode . strip . unNamespace
  where
    -- Common suffixes are stripped since the service definitions are ambigiuous
    -- as to whether the error shape's name, or the error code is present
    -- in the response.
    strip x =
      Maybe.fromMaybe x $
        Text.stripSuffix "Exception" x <|> Text.stripSuffix "Fault" x

    -- Removing the (potential) leading ...# namespace.
    unNamespace x =
      case Text.break (== '#') x of
        (ns, e)
          | Text.null e -> ns
          | otherwise -> Text.drop 1 e

newtype ErrorMessage = ErrorMessage {fromErrorMessage :: Text}
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
      FromJSON
    )

newtype RequestId = RequestId {fromRequestId :: Text}
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
      FromJSON
    )

-- | An error type representing errors that can be attributed to this library.
data Error
  = TransportError Client.HttpException
  | SerializeError SerializeError
  | ServiceError ServiceError
  deriving stock (Show, Generic)

instance Exception Error

-- instance ToLog Error where
--   build = \case
--     TransportError e -> build e
--     SerializeError e -> build e
--     ServiceError e -> build e

data SerializeError = SerializeError'
  { serializeErrorAbbrev :: Abbrev,
    serializeErrorStatus :: HTTP.Status,
    -- | The response body, if the response was not streaming.
    serializeErrorBody :: Maybe ByteStringLazy,
    serializeErrorMessage :: String
  }
  deriving stock (Show, Eq, Generic)

-- instance ToLog SerializeError where
--   build SerializeError' {..} =
--     buildLines
--       [ "[SerializeError] {",
--         "  service = " <> build serializeAbbrev,
--         "  status  = " <> build serializeStatus,
--         "  message = " <> build serializeMessage,
--         "  body    = " <> build serializeBody,
--         "}"
--       ]

data ServiceError = ServiceError'
  { serviceErrorAbbrev :: Abbrev,
    serviceErrorStatus :: HTTP.Status,
    serviceErrorHeaders :: [Header],
    serviceErrorCode :: ErrorCode,
    serviceErrorMessage :: Maybe ErrorMessage,
    serviceErrorRequestId :: Maybe RequestId
  }
  deriving stock (Show, Eq, Generic)

-- instance ToLog ServiceError where
--   build ServiceError' {..} =
--     buildLines
--       [ "[ServiceError] {",
--         "  service    = " <> build _serviceAbbrev,
--         "  status     = " <> build _serviceStatus,
--         "  code       = " <> build _serviceCode,
--         "  message    = " <> build _serviceMessage,
--         "  request-id = " <> build _serviceRequestId,
--         "}"
--       ]

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
  deriving stock (Show, Read, Eq)

data LogLevel
  = -- | Info messages supplied by the user - this level is not emitted by the library.
    Info
  | -- | Error messages only.
    Error
  | -- | Useful debug information + info + error levels.
    Debug
  | -- | Includes potentially sensitive signing metadata, and non-streaming response bodies.
    Trace
  deriving stock (Show, Eq, Ord, Enum)

instance FromText LogLevel where
  parseText = \case
    "info" -> pure Info
    "error" -> pure Error
    "debug" -> pure Debug
    "trace" -> pure Trace
    other -> Left ("failure parsing LogLevel from: " <> other)

instance ToText LogLevel where
  toText = \case
    Info -> "info"
    Error -> "error"
    Debug -> "debug"
    Trace -> "trace"

-- instance ToByteString LogLevel

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

-- | Attributes and functions specific to an AWS service.
data Service = Service
  { serviceAbbrev :: Abbrev,
    serviceSigner :: Signer,
    servicePrefix :: ByteString,
    serviceVersion :: ByteString,
    serviceEndpoint :: Region -> Endpoint,
    serviceTimeout :: Maybe Seconds,
    serviceCheck :: HTTP.Status -> Bool,
    serviceError :: HTTP.Status -> [Header] -> ByteStringLazy -> Error,
    serviceRetry :: Retry
  }
  deriving stock (Generic)

-- -- | A signed 'ClientRequest' tagged with the initial request type.
-- newtype SignedRequest request = SignedRequest
--   { fromSignedRequest :: ClientRequest
--   }
--   deriving stock (Generic)

-- Note: The signing metadata is used for testing and debugging.
-- Additional information such as a V4 string-to-sign or V2 headers can be
-- retrieved from the dynamic payload or the client request.
type SigningAlgorithm request =
  Request request ->
  AuthEnv ->
  Region ->
  UTCTime ->
  (ClientRequest, Dynamic)

data Signer = Signer
  { runSigner :: forall request. SigningAlgorithm request,
    runPresigner :: forall request. Seconds -> SigningAlgorithm request
  }

-- | An unsigned request.
data Request request = Request
  { requestService :: Service,
    requestMethod :: HTTP.StdMethod,
    requestPath :: ByteString,
    requestQuery :: QueryBuilder,
    requestHeaders :: Headers,
    requestBody :: RqBody
  }
  deriving stock (Generic)

data Response response = Response
  { responseStatus :: HTTP.Status,
    responsePayload :: response
  }
  deriving stock (Functor, Foldable, Traversable, Generic)

-- | Specify how a request can be de/serialised.
class AWSRequest request where
  type AWSResponse request :: Type

  toRequest ::
    request ->
    Request request

  parseResponse ::
    Monad m =>
    Logger ->
    Service ->
    Proxy request ->
    ClientResponse ->
    m (Either Error (Response (AWSResponse request)))

-- | An access key ID.
--
-- For example: @AKIAIOSFODNN7EXAMPLE@
--
-- /See:/ <http://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html Understanding and Getting Your Security Credentials>.
newtype AccessKey = AccessKey {fromAccessKey :: Text}
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
      FromJSON
    )

-- | Secret access key credential.
--
-- For example: @wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKE@
--
-- /See:/ <http://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html Understanding and Getting Your Security Credentials>.
newtype SecretKey = SecretKey {fromSecretKey :: Text}
  deriving stock (Eq, Ord, Generic)
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
      FromJSON
    )

instance Show SecretKey where
  showsPrec _ _ =
    showString "SecretKey {fromSecretKey = \"*****\"}"

-- | A session token used by STS to temporarily authorise access to
-- an AWS resource.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials>.
newtype SessionToken = SessionToken {fromSessionToken :: Text}
  deriving stock (Eq, Ord, Generic)
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
      FromJSON
    )

instance Show SessionToken where
  showsPrec _ _ =
    showString "SessionToken {fromSessionToken = \"*****\"}"

-- | The AuthN/AuthZ credential environment.
data AuthEnv = AuthEnv
  { -- | The access key ID that identifies the temporary security credentials.
    authAccessKeyId :: AccessKey,
    -- | The secret access key that can be used to sign requests.
    authSecretAccessKey :: SecretKey,
    -- | The token that users must pass to the service API to use the temporary
    authSessionToken :: Maybe SessionToken,
    -- | The expiry time of the credentials.
    authExpiration :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- instance ToLog AuthEnv where
--   build AuthEnv {..} =
--     buildLines
--       [ "[Amazonka Auth] {",
--         "  access key id     = " <> build _authAccess,
--         "  secret access key = " <> build _authSecret,
--         "  session token     = " <> build _authToken,
--         "  expiration        = " <> build _authExpiry,
--         "}"
--       ]

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

-- instance ToLog Auth where
--   build (Ref t _) = "[Amazonka Auth] { <thread:" <> build (show t) <> "> }"
--   build (Auth e) = build e

withAuth :: MonadIO m => Auth -> (AuthEnv -> m a) -> m a
withAuth (Ref _ r) f = liftIO (IORef.readIORef r) >>= f
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
      FromJSON
    )

pattern NorthVirginia :: Region
pattern NorthVirginia = Region' "us-east-1"

pattern Ohio :: Region
pattern Ohio = Region' "us-east-2"

pattern NorthCalifornia :: Region
pattern NorthCalifornia = Region' "us-west-1"

pattern Oregon :: Region
pattern Oregon = Region' "us-west-2"

pattern CapeTown :: Region
pattern CapeTown = Region' "af-south-1"

pattern Montreal :: Region
pattern Montreal = Region' "ca-central-1"

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

pattern Bahrain :: Region
pattern Bahrain = Region' "me-south-1"

pattern SaoPaulo :: Region
pattern SaoPaulo = Region' "sa-east-1"

pattern GovCloudWest :: Region
pattern GovCloudWest = Region' "us-gov-west-1"

pattern GovCloudEast :: Region
pattern GovCloudEast = Region' "us-gov-east-1"

pattern Beijing :: Region
pattern Beijing = Region' "cn-north-1"

pattern Ningxia :: Region
pattern Ningxia = Region' "cn-northwest-1"

{-# COMPLETE
  NorthVirginia,
  Ohio,
  NorthCalifornia,
  Oregon,
  CapeTown,
  Montreal,
  Frankfurt,
  Ireland,
  London,
  Milan,
  Paris,
  Stockholm,
  HongKong,
  Tokyo,
  Seoul,
  Osaka,
  Singapore,
  Sydney,
  Mumbai,
  Bahrain,
  SaoPaulo,
  GovCloudWest,
  GovCloudEast,
  Beijing,
  Ningxia,
  Region'
  #-}

-- | An integral value representing seconds.
newtype Seconds = Seconds Int
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype
    ( Enum,
      Num,
      Bounded,
      Integral,
      Real,
      Hashable,
      NFData,
      ToText,
      FromText,
      ToQuery,
      ToXML,
      FromXML,
      ToJSON,
      FromJSON
    )

seconds :: Seconds -> Int
seconds (Seconds n)
  | n < 0 = 0
  | otherwise = n

microseconds :: Seconds -> Int
microseconds = (1000000 *) . seconds
