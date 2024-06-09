-- |
-- Module      : Amazonka.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- *** Optics
    _AccessKey,
    _SecretKey,
    _SessionToken,

    -- ** Environment
    Auth (..),
    withAuth,
    AuthEnv (..),

    -- *** Lenses
    authEnv_accessKeyId,
    authEnv_secretAccessKey,
    authEnv_sessionToken,
    authEnv_expiration,

    -- * Signing
    Algorithm,
    Meta (..),
    Signer (..),
    Signed (..),

    -- ** Lenses
    signed_signedMeta,
    signed_signedRequest,

    -- * Service
    Abbrev,
    Service (..),
    S3AddressingStyle (..),

    -- ** Optics
    _Abbrev,
    service_abbrev,
    service_signer,
    service_signingName,
    service_version,
    service_s3AddressingStyle,
    service_endpointPrefix,
    service_endpoint,
    service_timeout,
    service_check,
    service_error,
    service_retry,

    -- * Requests
    AWSRequest (..),
    Request (..),
    requestSign,
    requestPresign,
    requestUnsigned,

    -- ** Lenses
    request_service,
    request_method,
    request_path,
    request_query,
    request_headers,
    request_body,

    -- * Retries
    Retry (..),

    -- ** Lenses
    retry_base,
    retry_growth,
    retry_attempts,
    retry_check,

    -- * Errors
    AsError (..),
    Error (..),

    -- ** HTTP Errors
    Client.HttpException,

    -- ** Serialize Errors
    SerializeError (..),

    -- *** Lenses
    serializeError_abbrev,
    serializeError_status,
    serializeError_body,
    serializeError_message,

    -- ** Service Errors
    ServiceError (..),

    -- *** Lenses
    serviceError_abbrev,
    serviceError_status,
    serviceError_headers,
    serviceError_code,
    serviceError_message,
    serviceError_requestId,

    -- ** Error Types
    ErrorCode (..),
    newErrorCode,
    ErrorMessage (..),
    RequestId (..),

    -- *** Optics
    _ErrorCode,
    _ErrorMessage,
    _RequestId,

    -- * Regions
    Region
      ( Ohio,
        NorthVirginia,
        NorthCalifornia,
        Oregon,
        CapeTown,
        HongKong,
        Hyderabad,
        Jakarta,
        Melbourne,
        Mumbai,
        Osaka,
        Seoul,
        Singapore,
        Sydney,
        Tokyo,
        Montreal,
        Frankfurt,
        Ireland,
        London,
        Milan,
        Paris,
        Spain,
        Stockholm,
        Zurich,
        Bahrain,
        UAE,
        SaoPaulo,
        GovCloudEast,
        GovCloudWest,
        Beijing,
        Ningxia,
        ..
      ),

    -- * Endpoints
    Endpoint (..),

    -- ** Lenses
    endpoint_host,
    endpoint_basePath,
    endpoint_secure,
    endpoint_port,
    endpoint_scope,

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
import Data.Typeable (Typeable)
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

{-# INLINE _Abbrev #-}
_Abbrev :: Iso' Abbrev Text
_Abbrev = Lens.coerced

newtype ErrorCode = ErrorCode Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToText, ToLog)

{-# INLINE _ErrorCode #-}
_ErrorCode :: Iso' ErrorCode Text
_ErrorCode = Lens.coerced

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

{-# INLINE _ErrorMessage #-}
_ErrorMessage :: Iso' ErrorMessage Text
_ErrorMessage = Lens.coerced

newtype RequestId = RequestId {fromRequestId :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromXML, FromJSON, FromText, ToText, ToLog)

{-# INLINE _RequestId #-}
_RequestId :: Iso' RequestId Text
_RequestId = Lens.coerced

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

{-# INLINE serializeError_abbrev #-}
serializeError_abbrev :: Lens' SerializeError Abbrev
serializeError_abbrev f e@SerializeError' {abbrev} = f abbrev <&> \abbrev' -> (e :: SerializeError) {abbrev = abbrev'}

{-# INLINE serializeError_status #-}
serializeError_status :: Lens' SerializeError Status
serializeError_status f e@SerializeError' {status} = f status <&> \status' -> (e :: SerializeError) {status = status'}

{-# INLINE serializeError_body #-}
serializeError_body :: Lens' SerializeError (Maybe ByteStringLazy)
serializeError_body f e@SerializeError' {body} = f body <&> \body' -> (e :: SerializeError) {body = body'}

{-# INLINE serializeError_message #-}
serializeError_message :: Lens' SerializeError String
serializeError_message f e@SerializeError' {message} = f message <&> \message' -> (e :: SerializeError) {message = message'}

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

{-# INLINE serviceError_abbrev #-}
serviceError_abbrev :: Lens' ServiceError Abbrev
serviceError_abbrev f e@ServiceError' {abbrev} = f abbrev <&> \abbrev' -> (e :: ServiceError) {abbrev = abbrev'}

{-# INLINE serviceError_status #-}
serviceError_status :: Lens' ServiceError Status
serviceError_status f e@ServiceError' {status} = f status <&> \status' -> (e :: ServiceError) {status = status'}

{-# INLINE serviceError_headers #-}
serviceError_headers :: Lens' ServiceError [Header]
serviceError_headers f e@ServiceError' {headers} = f headers <&> \headers' -> (e :: ServiceError) {headers = headers'}

{-# INLINE serviceError_code #-}
serviceError_code :: Lens' ServiceError ErrorCode
serviceError_code f e@ServiceError' {code} = f code <&> \code' -> e {code = code'}

{-# INLINE serviceError_message #-}
serviceError_message :: Lens' ServiceError (Maybe ErrorMessage)
serviceError_message f e@ServiceError' {message} = f message <&> \message' -> (e :: ServiceError) {message = message'}

{-# INLINE serviceError_requestId #-}
serviceError_requestId :: Lens' ServiceError (Maybe RequestId)
serviceError_requestId f e@ServiceError' {requestId} = f requestId <&> \requestId' -> (e :: ServiceError) {requestId = requestId'}

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
  { -- | The host to make requests to. Usually something like
    -- @s3.us-east-1.amazonaws.com@.
    host :: ByteString,
    -- | Path segment prepended to the request path of any request
    -- made to this endpoint. This is useful if you want to use the
    -- AWS API Gateway Management API, which requires you to override
    -- the client endpoint including a leading path segment (either
    -- the stage or, on a custom domain, the mapped base path).
    basePath :: RawPath,
    secure :: Bool,
    port :: Int,
    -- | Signing scope, usually a region like @us-east-1@.
    scope :: ByteString
  }
  deriving stock (Eq, Show, Generic)

{-# INLINE endpoint_host #-}
endpoint_host :: Lens' Endpoint ByteString
endpoint_host f e@Endpoint {host} = f host <&> \host' -> e {host = host'}

{-# INLINE endpoint_basePath #-}
endpoint_basePath :: Lens' Endpoint RawPath
endpoint_basePath f e@Endpoint {basePath} = f basePath <&> \basePath' -> e {basePath = basePath'}

{-# INLINE endpoint_secure #-}
endpoint_secure :: Lens' Endpoint Bool
endpoint_secure f e@Endpoint {secure} = f secure <&> \secure' -> e {secure = secure'}

{-# INLINE endpoint_port #-}
endpoint_port :: Lens' Endpoint Int
endpoint_port f e@Endpoint {port} = f port <&> \port' -> e {port = port'}

{-# INLINE endpoint_scope #-}
endpoint_scope :: Lens' Endpoint ByteString
endpoint_scope f e@Endpoint {scope} = f scope <&> \scope' -> e {scope = scope'}

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

{-# INLINE retry_base #-}
retry_base :: Lens' Retry Double
retry_base f r@Exponential {base} = f base <&> \base' -> r {base = base'}

{-# INLINE retry_growth #-}
retry_growth :: Lens' Retry Int
retry_growth f r@Exponential {growth} = f growth <&> \growth' -> r {growth = growth'}

{-# INLINE retry_attempts #-}
retry_attempts :: Lens' Retry Int
retry_attempts f r@Exponential {attempts} = f attempts <&> \attempts' -> r {attempts = attempts'}

{-# INLINE retry_check #-}
retry_check :: Lens' Retry (ServiceError -> Maybe Text)
retry_check f r@Exponential {check} = f check <&> \check' -> (r :: Retry) {check = check'}

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

{-# INLINE signed_signedMeta #-}
signed_signedMeta :: Lens' (Signed a) Meta
signed_signedMeta f s@Signed {signedMeta} = f signedMeta <&> \signedMeta' -> s {signedMeta = signedMeta'}

{-# INLINE signed_signedRequest #-}
signed_signedRequest :: Lens' (Signed a) ClientRequest
signed_signedRequest f s@Signed {signedRequest} = f signedRequest <&> \signedRequest' -> s {signedRequest = signedRequest'}

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
    endpoint :: Region -> Endpoint,
    timeout :: Maybe Seconds,
    check :: Status -> Bool,
    error :: Status -> [Header] -> ByteStringLazy -> Error,
    retry :: Retry
  }
  deriving stock (Generic)

{-# INLINE service_abbrev #-}
service_abbrev :: Lens' Service Abbrev
service_abbrev f s@Service {abbrev} = f abbrev <&> \abbrev' -> (s :: Service) {abbrev = abbrev'}

{-# INLINE service_signer #-}
service_signer :: Lens' Service Signer
service_signer f s@Service {signer} = f signer <&> \signer' -> (s :: Service) {signer = signer'}

{-# INLINE service_signingName #-}
service_signingName :: Lens' Service ByteString
service_signingName f s@Service {signingName} = f signingName <&> \signingName' -> s {signingName = signingName'}

{-# INLINE service_version #-}
service_version :: Lens' Service ByteString
service_version f s@Service {version} = f version <&> \version' -> s {version = version'}

{-# INLINE service_s3AddressingStyle #-}
service_s3AddressingStyle :: Lens' Service S3AddressingStyle
service_s3AddressingStyle f s@Service {s3AddressingStyle} = f s3AddressingStyle <&> \s3AddressingStyle' -> s {s3AddressingStyle = s3AddressingStyle'}

{-# INLINE service_endpointPrefix #-}
service_endpointPrefix :: Lens' Service ByteString
service_endpointPrefix f s@Service {endpointPrefix} = f endpointPrefix <&> \endpointPrefix' -> s {endpointPrefix = endpointPrefix'}

{-# INLINE service_endpoint #-}
service_endpoint :: Lens' Service (Region -> Endpoint)
service_endpoint f s@Service {endpoint} = f endpoint <&> \endpoint' -> s {endpoint = endpoint'}

{-# INLINE service_timeout #-}
service_timeout :: Lens' Service (Maybe Seconds)
service_timeout f s@Service {timeout} = f timeout <&> \timeout' -> s {timeout = timeout'}

{-# INLINE service_check #-}
service_check :: Lens' Service (Status -> Bool)
service_check f s@Service {check} = f check <&> \check' -> (s :: Service) {check = check'}

{-# INLINE service_error #-}
service_error :: Lens' Service (Status -> [Header] -> ByteStringLazy -> Error)
service_error f s@Service {error} = f error <&> \error' -> (s :: Service) {error = error'}

{-# INLINE service_retry #-}
service_retry :: Lens' Service Retry
service_retry f s@Service {retry} = f retry <&> \retry' -> (s :: Service) {retry = retry'}

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

{-# INLINE request_service #-}
request_service :: Lens' (Request a) Service
request_service f rq@Request {service} = f service <&> \service' -> rq {service = service'}

{-# INLINE request_method #-}
request_method :: Lens' (Request a) StdMethod
request_method f rq@Request {method} = f method <&> \method' -> rq {method = method'}

{-# INLINE request_path #-}
request_path :: Lens' (Request a) RawPath
request_path f rq@Request {path} = f path <&> \path' -> rq {path = path'}

{-# INLINE request_query #-}
request_query :: Lens' (Request a) QueryString
request_query f rq@Request {query} = f query <&> \query' -> rq {query = query'}

{-# INLINE request_headers #-}
request_headers :: forall a. Lens' (Request a) [Header]
request_headers f rq@Request {headers} = f headers <&> \headers' -> (rq :: Request a) {headers = headers'}

{-# INLINE request_body #-}
request_body :: forall a. Lens' (Request a) RequestBody
request_body f rq@Request {body} = f body <&> \body' -> (rq :: Request a) {body = body'}

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
class (Typeable a, Typeable (AWSResponse a)) => AWSRequest a where
  -- | The successful, expected response associated with a request.
  type AWSResponse a :: Type

  request ::
    -- | Overrides applied to the default 'Service'.
    (Service -> Service) ->
    a ->
    Request a

  response ::
    MonadResource m =>
    -- | Raw response body hook.
    (ByteStringLazy -> IO ByteStringLazy) ->
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

_AccessKey :: Iso' AccessKey ByteString
_AccessKey = Lens.coerced

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

_SecretKey :: Iso' SecretKey ByteString
_SecretKey = Lens.coerced

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

_SessionToken :: Iso' SessionToken ByteString
_SessionToken = Lens.coerced

-- | The AuthN/AuthZ credential environment.
data AuthEnv = AuthEnv
  { accessKeyId :: AccessKey,
    secretAccessKey :: Sensitive SecretKey,
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

{-# INLINE authEnv_accessKeyId #-}
authEnv_accessKeyId :: Lens' AuthEnv AccessKey
authEnv_accessKeyId f a@AuthEnv {accessKeyId} = f accessKeyId <&> \accessKeyId' -> a {accessKeyId = accessKeyId'}

{-# INLINE authEnv_secretAccessKey #-}
authEnv_secretAccessKey :: Lens' AuthEnv (Sensitive SecretKey)
authEnv_secretAccessKey f a@AuthEnv {secretAccessKey} = f secretAccessKey <&> \secretAccessKey' -> a {secretAccessKey = secretAccessKey'}

{-# INLINE authEnv_sessionToken #-}
authEnv_sessionToken :: Lens' AuthEnv (Maybe (Sensitive SessionToken))
authEnv_sessionToken f a@AuthEnv {sessionToken} = f sessionToken <&> \sessionToken' -> a {sessionToken = sessionToken'}

{-# INLINE authEnv_expiration #-}
authEnv_expiration :: Lens' AuthEnv (Maybe ISO8601)
authEnv_expiration f a@AuthEnv {expiration} = f expiration <&> \expiration' -> a {expiration = expiration'}

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

{-# INLINE _Region #-}
_Region :: Iso' Region Text
_Region = Lens.coerced

-- Patterns for Regions - keep in sync with
-- https://docs.aws.amazon.com/general/latest/gr/rande.html#regional-endpoints

-- United States

pattern Ohio :: Region
pattern Ohio = Region' "us-east-2"

pattern NorthVirginia :: Region
pattern NorthVirginia = Region' "us-east-1"

pattern NorthCalifornia :: Region
pattern NorthCalifornia = Region' "us-west-1"

pattern Oregon :: Region
pattern Oregon = Region' "us-west-2"

-- Africa

pattern CapeTown :: Region
pattern CapeTown = Region' "af-south-1"

-- Asia Pacific

pattern HongKong :: Region
pattern HongKong = Region' "ap-east-1"

pattern Hyderabad :: Region
pattern Hyderabad = Region' "ap-south-2"

pattern Jakarta :: Region
pattern Jakarta = Region' "ap-southeast-3"

pattern Melbourne :: Region
pattern Melbourne = Region' "ap-southeast-4"

pattern Mumbai :: Region
pattern Mumbai = Region' "ap-south-1"

pattern Osaka :: Region
pattern Osaka = Region' "ap-northeast-3"

pattern Seoul :: Region
pattern Seoul = Region' "ap-northeast-2"

pattern Singapore :: Region
pattern Singapore = Region' "ap-southeast-1"

pattern Sydney :: Region
pattern Sydney = Region' "ap-southeast-2"

pattern Tokyo :: Region
pattern Tokyo = Region' "ap-northeast-1"

-- Canada

pattern Montreal :: Region
pattern Montreal = Region' "ca-central-1"

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

pattern Spain :: Region
pattern Spain = Region' "eu-south-2"

pattern Stockholm :: Region
pattern Stockholm = Region' "eu-north-1"

pattern Zurich :: Region
pattern Zurich = Region' "eu-central-2"

-- Middle East

pattern Bahrain :: Region
pattern Bahrain = Region' "me-south-1"

pattern UAE :: Region
pattern UAE = Region' "me-central-1"

-- South America

pattern SaoPaulo :: Region
pattern SaoPaulo = Region' "sa-east-1"

-- GovCloud

pattern GovCloudEast :: Region
pattern GovCloudEast = Region' "us-gov-east-1"

pattern GovCloudWest :: Region
pattern GovCloudWest = Region' "us-gov-west-1"

-- China

pattern Beijing :: Region
pattern Beijing = Region' "cn-north-1"

pattern Ningxia :: Region
pattern Ningxia = Region' "cn-northwest-1"

{-# COMPLETE
  Ohio,
  NorthVirginia,
  NorthCalifornia,
  Oregon,
  CapeTown,
  HongKong,
  Hyderabad,
  Jakarta,
  Melbourne,
  Mumbai,
  Osaka,
  Seoul,
  Singapore,
  Sydney,
  Tokyo,
  Montreal,
  Frankfurt,
  Ireland,
  London,
  Milan,
  Paris,
  Spain,
  Stockholm,
  Zurich,
  Bahrain,
  UAE,
  SaoPaulo,
  GovCloudEast,
  GovCloudWest,
  Beijing,
  Ningxia,
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

_Seconds :: Iso' Seconds DiffTime
_Seconds = Lens.coerced

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
