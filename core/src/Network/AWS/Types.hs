-- |
-- Module      : Network.AWS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    rqService,
    rqMethod,
    rqHeaders,
    rqPath,
    rqQuery,
    rqBody,
    rqSign,
    rqPresign,

    -- * Responses
    Response,

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
    ResponseBody,
    clientRequest,

    -- ** Seconds
    Seconds (..),
    toSeconds,
    toMicroseconds,

    -- * Isomorphisms
    _Coerce,
  )
where

import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Coerce (coerce)
import Data.Conduit (ConduitM)
import Data.IORef (IORef, readIORef)
import qualified Data.Text as Text
import Network.AWS.Data
import qualified Network.AWS.Lens as Lens
import Network.AWS.Prelude
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types.Method (StdMethod)
import Network.HTTP.Types.Status (Status)

-- | A convenience alias to avoid type ambiguity.
type ClientRequest = Client.Request

-- | A convenience alias encapsulating the common 'Response'.
type ClientResponse = Client.Response ResponseBody

-- | A convenience alias encapsulating the common 'Response' body.
type ResponseBody = ConduitM () ByteString (ResourceT IO) ()

-- | Abbreviated service name.
newtype Abbrev = Abbrev Text
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

newtype ErrorMessage = ErrorMessage Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromXML, FromJSON, FromText, ToText, ToLog)

newtype RequestId = RequestId Text
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
  { _serializeAbbrev :: Abbrev,
    _serializeStatus :: Status,
    -- | The response body, if the response was not streaming.
    _serializeBody :: Maybe ByteStringLazy,
    _serializeMessage :: String
  }
  deriving stock (Eq, Show, Generic)

instance ToLog SerializeError where
  build SerializeError' {..} =
    buildLines
      [ "[SerializeError] {",
        "  service = " <> build _serializeAbbrev,
        "  status  = " <> build _serializeStatus,
        "  message = " <> build _serializeMessage,
        "  body    = " <> build _serializeBody,
        "}"
      ]

serializeAbbrev :: Lens' SerializeError Abbrev
serializeAbbrev = Lens.lens _serializeAbbrev (\s a -> s {_serializeAbbrev = a})

serializeStatus :: Lens' SerializeError Status
serializeStatus = Lens.lens _serializeStatus (\s a -> s {_serializeStatus = a})

serializeMessage :: Lens' SerializeError String
serializeMessage = Lens.lens _serializeMessage (\s a -> s {_serializeMessage = a})

data ServiceError = ServiceError'
  { _serviceAbbrev :: Abbrev,
    _serviceStatus :: Status,
    _serviceHeaders :: [Header],
    _serviceCode :: ErrorCode,
    _serviceMessage :: Maybe ErrorMessage,
    _serviceRequestId :: Maybe RequestId
  }
  deriving stock (Eq, Show, Generic)

instance ToLog ServiceError where
  build ServiceError' {..} =
    buildLines
      [ "[ServiceError] {",
        "  service    = " <> build _serviceAbbrev,
        "  status     = " <> build _serviceStatus,
        "  code       = " <> build _serviceCode,
        "  message    = " <> build _serviceMessage,
        "  request-id = " <> build _serviceRequestId,
        "}"
      ]

serviceAbbrev :: Lens' ServiceError Abbrev
serviceAbbrev = Lens.lens _serviceAbbrev (\s a -> s {_serviceAbbrev = a})

serviceStatus :: Lens' ServiceError Status
serviceStatus = Lens.lens _serviceStatus (\s a -> s {_serviceStatus = a})

serviceHeaders :: Lens' ServiceError [Header]
serviceHeaders = Lens.lens _serviceHeaders (\s a -> s {_serviceHeaders = a})

serviceCode :: Lens' ServiceError ErrorCode
serviceCode = Lens.lens _serviceCode (\s a -> s {_serviceCode = a})

serviceMessage :: Lens' ServiceError (Maybe ErrorMessage)
serviceMessage = Lens.lens _serviceMessage (\s a -> s {_serviceMessage = a})

serviceRequestId :: Lens' ServiceError (Maybe RequestId)
serviceRequestId = Lens.lens _serviceRequestId (\s a -> s {_serviceRequestId = a})

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
-- to be able to obtain the associated response, 'Rs a'.
data Signed a = Signed
  { sgMeta :: Meta,
    sgRequest :: ClientRequest
  }

type Algorithm a = Request a -> AuthEnv -> Region -> UTCTime -> Signed a

data Signer = Signer
  { sgSign :: forall a. Algorithm a,
    sgPresign :: forall a. Seconds -> Algorithm a
  }

-- | Attributes and functions specific to an AWS service.
data Service = Service
  { _svcAbbrev :: Abbrev,
    _svcSigner :: Signer,
    _svcEndpointPrefix :: ByteString,
    _svcSigningName :: ByteString,
    _svcVersion :: ByteString,
    _svcEndpoint :: (Region -> Endpoint),
    _svcTimeout :: (Maybe Seconds),
    _svcCheck :: (Status -> Bool),
    _svcError :: (Status -> [Header] -> ByteStringLazy -> Error),
    _svcRetry :: Retry
  }
  deriving stock (Generic)

serviceSigner :: Lens' Service Signer
serviceSigner = Lens.lens _svcSigner (\s a -> s {_svcSigner = a})

serviceEndpoint :: Setter' Service Endpoint
serviceEndpoint = Lens.sets (\f s -> s {_svcEndpoint = \r -> f (_svcEndpoint s r)})

serviceTimeout :: Lens' Service (Maybe Seconds)
serviceTimeout = Lens.lens _svcTimeout (\s a -> s {_svcTimeout = a})

serviceCheck :: Lens' Service (Status -> Bool)
serviceCheck = Lens.lens _svcCheck (\s a -> s {_svcCheck = a})

serviceRetry :: Lens' Service Retry
serviceRetry = Lens.lens _svcRetry (\s a -> s {_svcRetry = a})

-- | Construct a 'ClientRequest' using common parameters such as TLS and prevent
-- throwing errors when receiving erroneous status codes in respones.
clientRequest :: Endpoint -> Maybe Seconds -> ClientRequest
clientRequest e t =
  Client.defaultRequest
    { Client.secure = _endpointSecure e,
      Client.host = _endpointHost e,
      Client.port = _endpointPort e,
      Client.redirectCount = 0,
      Client.responseTimeout =
        case t of
          Nothing -> Client.responseTimeoutNone
          Just x -> Client.responseTimeoutMicro (toMicroseconds x)
    }

-- | An unsigned request.
data Request a = Request
  { _rqService :: Service,
    _rqMethod :: StdMethod,
    _rqPath :: RawPath,
    _rqQuery :: QueryString,
    _rqHeaders :: [Header],
    _rqBody :: RqBody
  }
  deriving stock (Generic)

rqService :: Lens' (Request a) Service
rqService = Lens.lens _rqService (\s a -> s {_rqService = a})

rqBody :: Lens' (Request a) RqBody
rqBody = Lens.lens _rqBody (\s a -> s {_rqBody = a})

rqHeaders :: Lens' (Request a) [Header]
rqHeaders = Lens.lens _rqHeaders (\s a -> s {_rqHeaders = a})

rqMethod :: Lens' (Request a) StdMethod
rqMethod = Lens.lens _rqMethod (\s a -> s {_rqMethod = a})

rqPath :: Lens' (Request a) RawPath
rqPath = Lens.lens _rqPath (\s a -> s {_rqPath = a})

rqQuery :: Lens' (Request a) QueryString
rqQuery = Lens.lens _rqQuery (\s a -> s {_rqQuery = a})

rqSign :: Algorithm a
rqSign x = sgSign (_svcSigner (_rqService x)) x

rqPresign :: Seconds -> Algorithm a
rqPresign ex x = sgPresign (_svcSigner (_rqService x)) ex x

type Response a = (Status, Rs a)

-- | Specify how a request can be de/serialised.
class AWSRequest a where
  -- | The successful, expected response associated with a request.
  type Rs a :: *

  request :: a -> Request a
  response ::
    MonadResource m =>
    Logger ->
    Service ->
    Proxy a -> -- For injectivity reasons.
    ClientResponse ->
    m (Response a)

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

_Coerce :: (Coercible a b, Coercible b a) => Iso' a b
_Coerce = Lens.iso coerce coerce
