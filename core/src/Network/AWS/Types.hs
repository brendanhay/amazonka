{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE CPP                        #-}

-- |
-- Module      : Network.AWS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Types
    (
    -- * Authentication
    -- ** Credentials
      AccessKey      (..)
    , SecretKey      (..)
    , SessionToken   (..)
    -- ** Environment
    , Auth           (..)
    , withAuth

    , AuthEnv        (..)
    , authAccessKeyId
    , authSecretAccessKey
    , authSessionToken
    , authExpiration

    -- * Logging
    , LogLevel       (..)
    , Logger

    -- * Signing
    , Algorithm
    , Meta           (..)
    , Signer         (..)
    , Signed         (..)

    -- * Service
    , Abbrev
    , Service        (..)
    , serviceSigner
    , serviceEndpoint
    , serviceTimeout
    , serviceCheck
    , serviceRetry

    -- * Requests
    , AWSRequest     (..)
    , Request        (..)
    , rqService
    , rqMethod
    , rqHeaders
    , rqPath
    , rqQuery
    , rqBody
    , rqSign
    , rqPresign

    -- * Responses
    , Response

    -- * Retries
    , Retry          (..)
    , exponentBase
    , exponentGrowth
    , retryAttempts
    , retryCheck

    -- * Errors
    , AsError        (..)
    , Error          (..)
    -- ** HTTP Errors
    , HttpException
    -- ** Serialize Errors
    , SerializeError (..)
    , serializeAbbrev
    , serializeStatus
    , serializeMessage
    -- ** Service Errors
    , ServiceError   (..)
    , serviceAbbrev
    , serviceStatus
    , serviceHeaders
    , serviceCode
    , serviceMessage
    , serviceRequestId
    -- ** Error Types
    , ErrorCode      (..)
    , newErrorCode
    , ErrorMessage   (..)
    , RequestId      (..)

    -- * Regions
    , Region         (..)

    -- * Endpoints
    , Endpoint       (..)
    , endpointHost
    , endpointPort
    , endpointSecure
    , endpointScope

    -- * HTTP
    , ClientRequest
    , ClientResponse
    , ResponseBody
    , clientRequest

    -- ** Seconds
    , Seconds         (..)
    , toSeconds
    , toMicroseconds

    -- * Isomorphisms
    , _Coerce
    ) where

import Control.Applicative
import Control.Concurrent           (ThreadId)
import Control.DeepSeq
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.Aeson              hiding (Error)
import Data.ByteString.Builder (Builder)
import Data.Coerce
import Data.Conduit
import Data.Data               (Data, Typeable)
import Data.Hashable
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String
import Data.Time

import GHC.Generics (Generic)

import Network.AWS.Data.Body
import Network.AWS.Data.ByteString
import Network.AWS.Data.JSON
import Network.AWS.Data.Log
import Network.AWS.Data.Path
import Network.AWS.Data.Query
import Network.AWS.Data.Sensitive  (Sensitive, _Sensitive)
import Network.AWS.Data.Text
import Network.AWS.Data.Time       (ISO8601, _Time)
import Network.AWS.Data.XML
import Network.AWS.Lens            (Iso', Lens', Prism', Setter')
import Network.AWS.Lens            (exception, iso, lens, mapping, prism, sets,
                                    view)
import Network.HTTP.Conduit        hiding (Proxy, Request, Response)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status   (Status)

import qualified Data.Text            as Text
import qualified Network.HTTP.Conduit as Client

#if ! MIN_VERSION_http_client(0,4,30)
import           Text.XML                     (def)
#endif

-- | A convenience alias to avoid type ambiguity.
type ClientRequest = Client.Request

-- | A convenience alias encapsulating the common 'Response'.
type ClientResponse = Client.Response ResponseBody

-- | A convenience alias encapsulating the common 'Response' body.
type ResponseBody = ConduitM () ByteString (ResourceT IO) ()

-- | Abbreviated service name.
newtype Abbrev = Abbrev Text
    deriving (Eq, Ord, Show, IsString, FromXML, FromJSON, FromText, ToText, ToLog)

newtype ErrorCode = ErrorCode Text
    deriving (Eq, Ord, Show, ToText, ToLog)

instance IsString ErrorCode where fromString = newErrorCode . fromString
instance FromJSON ErrorCode where parseJSON  = parseJSONText "ErrorCode"
instance FromXML  ErrorCode where parseXML   = parseXMLText  "ErrorCode"

instance FromText ErrorCode where
    parser = newErrorCode <$> parser

-- | Construct an 'ErrorCode'.
newErrorCode :: Text -> ErrorCode
newErrorCode = ErrorCode . strip . unnamespace
  where
    -- Common suffixes are stripped since the service definitions are ambigiuous
    -- as to whether the error shape's name, or the error code is present
    -- in the response.
    strip x = fromMaybe x $
        Text.stripSuffix "Exception" x <|> Text.stripSuffix "Fault" x

    -- Removing the (potential) leading ...# namespace.
    unnamespace x =
        case Text.break (== '#') x of
            (ns, e) | Text.null e -> ns
                    | otherwise   -> Text.drop 1 e

newtype ErrorMessage = ErrorMessage Text
    deriving (Eq, Ord, Show, IsString, FromXML, FromJSON, FromText, ToText, ToLog)

newtype RequestId = RequestId Text
    deriving (Eq, Ord, Show, IsString, FromXML, FromJSON, FromText, ToText, ToLog)

-- | An error type representing errors that can be attributed to this library.
data Error
    = TransportError HttpException
    | SerializeError SerializeError
    | ServiceError   ServiceError
      deriving (Show, Typeable)

instance Exception Error

instance ToLog Error where
    build = \case
        TransportError e -> build e
        SerializeError e -> build e
        ServiceError   e -> build e

data SerializeError = SerializeError'
    { _serializeAbbrev  :: !Abbrev
    , _serializeStatus  :: !Status
    , _serializeBody    :: Maybe LazyByteString
      -- ^ The response body, if the response was not streaming.
    , _serializeMessage :: String
    } deriving (Eq, Show, Typeable)

instance ToLog SerializeError where
    build SerializeError'{..} = buildLines
        [ "[SerializeError] {"
        , "  service = " <> build _serializeAbbrev
        , "  status  = " <> build _serializeStatus
        , "  message = " <> build _serializeMessage
        , "  body    = " <> build _serializeBody
        , "}"
        ]

serializeAbbrev :: Lens' SerializeError Abbrev
serializeAbbrev = lens _serializeAbbrev (\s a -> s { _serializeAbbrev = a })

serializeStatus :: Lens' SerializeError Status
serializeStatus = lens _serializeStatus (\s a -> s { _serializeStatus = a })

serializeMessage :: Lens' SerializeError String
serializeMessage = lens _serializeMessage (\s a -> s { _serializeMessage = a })

data ServiceError = ServiceError'
    { _serviceAbbrev    :: !Abbrev
    , _serviceStatus    :: !Status
    , _serviceHeaders   :: [Header]
    , _serviceCode      :: !ErrorCode
    , _serviceMessage   :: Maybe ErrorMessage
    , _serviceRequestId :: Maybe RequestId
    } deriving (Eq, Show, Typeable)

instance ToLog ServiceError where
    build ServiceError'{..} = buildLines
        [ "[ServiceError] {"
        , "  service    = " <> build _serviceAbbrev
        , "  status     = " <> build _serviceStatus
        , "  code       = " <> build _serviceCode
        , "  message    = " <> build _serviceMessage
        , "  request-id = " <> build _serviceRequestId
        , "}"
        ]

serviceAbbrev :: Lens' ServiceError Abbrev
serviceAbbrev = lens _serviceAbbrev (\s a -> s { _serviceAbbrev = a })

serviceStatus :: Lens' ServiceError Status
serviceStatus = lens _serviceStatus (\s a -> s { _serviceStatus = a })

serviceHeaders :: Lens' ServiceError [Header]
serviceHeaders = lens _serviceHeaders (\s a -> s { _serviceHeaders = a })

serviceCode :: Lens' ServiceError ErrorCode
serviceCode = lens _serviceCode (\s a -> s { _serviceCode = a })

serviceMessage :: Lens' ServiceError (Maybe ErrorMessage)
serviceMessage = lens _serviceMessage (\s a -> s { _serviceMessage = a })

serviceRequestId :: Lens' ServiceError (Maybe RequestId)
serviceRequestId = lens _serviceRequestId (\s a -> s { _serviceRequestId = a })

class AsError a where
    -- | A general Amazonka error.
    _Error          :: Prism' a Error
    {-# MINIMAL _Error #-}

    -- | An error occured while communicating over HTTP with a remote service.
    _TransportError :: Prism' a HttpException

    -- | A serialisation error occured when attempting to deserialise a response.
    _SerializeError :: Prism' a SerializeError

    -- | A service specific error returned by the remote service.
    _ServiceError   :: Prism' a ServiceError

    _TransportError = _Error . _TransportError
    _SerializeError = _Error . _SerializeError
    _ServiceError   = _Error . _ServiceError

instance AsError SomeException where
    _Error = exception

instance AsError Error where
    _Error = id

    _TransportError = prism TransportError $ \case
        TransportError e -> Right e
        x                -> Left x

    _SerializeError = prism SerializeError $ \case
        SerializeError e -> Right e
        x                -> Left  x

    _ServiceError = prism ServiceError $ \case
        ServiceError e -> Right e
        x              -> Left  x

data Endpoint = Endpoint
    { _endpointHost   :: ByteString
    , _endpointSecure :: !Bool
    , _endpointPort   :: !Int
    , _endpointScope  :: ByteString
    } deriving (Eq, Show, Data, Typeable)

endpointHost :: Lens' Endpoint ByteString
endpointHost = lens _endpointHost (\s a -> s { _endpointHost = a })

endpointSecure :: Lens' Endpoint Bool
endpointSecure = lens _endpointSecure (\s a -> s { _endpointSecure = a })

endpointPort :: Lens' Endpoint Int
endpointPort = lens _endpointPort (\s a -> s { _endpointPort = a })

endpointScope :: Lens' Endpoint ByteString
endpointScope = lens _endpointScope (\s a -> s { _endpointScope = a })

data LogLevel
    = Info  -- ^ Info messages supplied by the user - this level is not emitted by the library.
    | Error -- ^ Error messages only.
    | Debug -- ^ Useful debug information + info + error levels.
    | Trace -- ^ Includes potentially sensitive signing metadata, and non-streaming response bodies.
      deriving (Eq, Ord, Enum, Show, Data, Typeable)

instance FromText LogLevel where
    parser = takeLowerText >>= \case
        "info"  -> pure Info
        "error" -> pure Error
        "debug" -> pure Debug
        "trace" -> pure Trace
        e       -> fromTextError $ "Failure parsing LogLevel from " <> e

instance ToText LogLevel where
    toText = \case
        Info  -> "info"
        Error -> "error"
        Debug -> "debug"
        Trace -> "trace"

instance ToByteString LogLevel

-- | A function threaded through various request and serialisation routines
-- to log informational and debug messages.
type Logger = LogLevel -> Builder -> IO ()

-- | Constants and predicates used to create a 'RetryPolicy'.
data Retry = Exponential
    { _retryBase     :: !Double
    , _retryGrowth   :: !Int
    , _retryAttempts :: !Int
    , _retryCheck    :: ServiceError -> Maybe Text
      -- ^ Returns a descriptive name for logging
      -- if the request should be retried.
    }

exponentBase :: Lens' Retry Double
exponentBase = lens _retryBase (\s a -> s { _retryBase = a })

exponentGrowth :: Lens' Retry Int
exponentGrowth = lens _retryGrowth (\s a -> s { _retryGrowth = a })

retryAttempts :: Lens' Retry Int
retryAttempts = lens _retryAttempts (\s a -> s { _retryAttempts = a })

retryCheck :: Lens' Retry (ServiceError -> Maybe Text)
retryCheck = lens _retryCheck (\s a -> s { _retryCheck = a })

-- | Signing algorithm specific metadata.
data Meta where
    Meta :: ToLog a => a -> Meta

instance ToLog Meta where
   build (Meta m) = build m

-- | A signed 'ClientRequest' and associated metadata specific
-- to the signing algorithm, tagged with the initial request type
-- to be able to obtain the associated response, 'Rs a'.
data Signed a = Signed
    { sgMeta    :: !Meta
    , sgRequest :: !ClientRequest
    }

type Algorithm a = Request a -> AuthEnv -> Region -> UTCTime -> Signed a

data Signer = Signer
    { sgSign    :: forall a. Algorithm a
    , sgPresign :: forall a. Seconds -> Algorithm a
    }

-- | Attributes and functions specific to an AWS service.
data Service = Service
    { _svcAbbrev   :: !Abbrev
    , _svcSigner   :: !Signer
    , _svcPrefix   :: !ByteString
    , _svcVersion  :: !ByteString
    , _svcEndpoint :: !(Region -> Endpoint)
    , _svcTimeout  :: !(Maybe Seconds)
    , _svcCheck    :: !(Status -> Bool)
    , _svcError    :: !(Status -> [Header] -> LazyByteString -> Error)
    , _svcRetry    :: !Retry
    }

serviceSigner :: Lens' Service Signer
serviceSigner = lens _svcSigner (\s a -> s { _svcSigner = a })

serviceEndpoint :: Setter' Service Endpoint
serviceEndpoint = sets (\f s -> s { _svcEndpoint = \r -> f (_svcEndpoint s r) })

serviceTimeout :: Lens' Service (Maybe Seconds)
serviceTimeout = lens _svcTimeout (\s a -> s { _svcTimeout = a })

serviceCheck :: Lens' Service (Status -> Bool)
serviceCheck = lens _svcCheck (\s a -> s { _svcCheck = a })

serviceRetry :: Lens' Service Retry
serviceRetry = lens _svcRetry (\s a -> s { _svcRetry = a })

-- | Construct a 'ClientRequest' using common parameters such as TLS and prevent
-- throwing errors when receiving erroneous status codes in respones.
clientRequest :: Endpoint -> Maybe Seconds -> ClientRequest
clientRequest e t =
#if MIN_VERSION_http_client(0,4,30)
  Client.defaultRequest
#else
  def
#endif
    { Client.secure          = _endpointSecure e
    , Client.host            = _endpointHost   e
    , Client.port            = _endpointPort   e
    , Client.redirectCount   = 0
    , Client.responseTimeout =
#if MIN_VERSION_http_client(0,5,0)
        case t of
            Nothing -> Client.responseTimeoutNone
            Just x  -> Client.responseTimeoutMicro (toMicroseconds x)
#else
        microseconds <$> t
#endif
    }

-- | An unsigned request.
data Request a = Request
    { _rqService :: !Service
    , _rqMethod  :: !StdMethod
    , _rqPath    :: !RawPath
    , _rqQuery   :: !QueryString
    , _rqHeaders :: ![Header]
    , _rqBody    :: !RqBody
    }

rqService :: Lens' (Request a) Service
rqService = lens _rqService (\s a -> s { _rqService = a })

rqBody :: Lens' (Request a) RqBody
rqBody = lens _rqBody (\s a -> s { _rqBody = a })

rqHeaders :: Lens' (Request a) [Header]
rqHeaders = lens _rqHeaders (\s a -> s { _rqHeaders = a })

rqMethod :: Lens' (Request a) StdMethod
rqMethod = lens _rqMethod (\s a -> s { _rqMethod = a })

rqPath :: Lens' (Request a) RawPath
rqPath = lens _rqPath (\s a -> s { _rqPath = a })

rqQuery :: Lens' (Request a) QueryString
rqQuery = lens _rqQuery (\s a -> s { _rqQuery = a })

rqSign :: Algorithm a
rqSign x = sgSign (_svcSigner (_rqService x)) x

rqPresign :: Seconds -> Algorithm a
rqPresign ex x = sgPresign (_svcSigner (_rqService x)) ex x

type Response a = (Status, Rs a)

-- | Specify how a request can be de/serialised.
class AWSRequest a where
    -- | The successful, expected response associated with a request.
    type Rs a :: *

    request  :: a -> Request a
    response :: (MonadResource m, MonadThrow m)
             => Logger
             -> Service
             -> Proxy a -- For injectivity reasons.
             -> ClientResponse
             -> m (Response a)

-- | An access key ID.
--
-- For example: @AKIAIOSFODNN7EXAMPLE@
--
-- /See:/ <http://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html Understanding and Getting Your Security Credentials>.
newtype AccessKey = AccessKey ByteString
    deriving
        ( Eq
        , Show
        , Read
        , Data
        , Typeable
        , IsString
        , ToText
        , FromText
        , ToByteString
        , ToLog
        , FromXML
        , ToXML
        , ToQuery
        , Hashable
        , NFData
        )

instance ToJSON   AccessKey where toJSON    = toJSONText
instance FromJSON AccessKey where parseJSON = parseJSONText "AccessKey"

-- | Secret access key credential.
--
-- For example: @wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKE@
--
-- /See:/ <http://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html Understanding and Getting Your Security Credentials>.
newtype SecretKey = SecretKey ByteString
    deriving
        ( Eq
        , Data
        , Typeable
        , IsString
        , ToText
        , FromText
        , ToByteString
        , FromXML
        , ToXML
        , Hashable
        , NFData
        )

instance ToJSON   SecretKey where toJSON    = toJSONText
instance FromJSON SecretKey where parseJSON = parseJSONText "SecretKey"

-- | A session token used by STS to temporarily authorise access to
-- an AWS resource.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials>.
newtype SessionToken = SessionToken ByteString
    deriving
        ( Eq
        , Data
        , Typeable
        , IsString
        , ToText
        , FromText
        , ToByteString
        , FromXML
        , ToXML
        , Hashable
        , NFData
        )

instance ToJSON   SessionToken where toJSON    = toJSONText
instance FromJSON SessionToken where parseJSON = parseJSONText "SessionToken"

-- | The AuthN/AuthZ credential environment.
data AuthEnv = AuthEnv
    { _authAccessKeyId :: !AccessKey
    , _authSecretAccessKey :: !(Sensitive SecretKey)
    , _authSessionToken  :: Maybe (Sensitive SessionToken)
    , _authExpiration :: Maybe ISO8601
    } deriving (Eq, Show, Data, Typeable, Generic)

instance NFData AuthEnv

instance ToLog AuthEnv where
    build AuthEnv{..} = buildLines
        [ "[Amazonka Auth] {"
        , "  access key id     = " <> build _authAccessKeyId
        , "  secret access key = " <> build _authSecretAccessKey
        , "  session token     = " <> build _authSessionToken
        , "  expiration        = " <> build (fmap (view _Time) _authExpiration)
        , "}"
        ]

instance FromJSON AuthEnv where
    parseJSON = withObject "AuthEnv" $ \o -> AuthEnv
        <$> o .:  "AccessKeyId"
        <*> o .:  "SecretAccessKey"
        <*> o .:? "Token"
        <*> o .:? "Expiration"

instance FromXML AuthEnv where
    parseXML x = AuthEnv
        <$> x .@  "AccessKeyId"
        <*> x .@  "SecretAccessKey"
        <*> x .@? "SessionToken"
        <*> x .@? "Expiration"

-- | The access key ID that identifies the temporary security credentials.
authAccessKeyId :: Lens' AuthEnv AccessKey
authAccessKeyId = lens _authAccessKeyId (\s a -> s{ _authAccessKeyId = a })

-- | The secret access key that can be used to sign requests.
authSecretAccessKey :: Lens' AuthEnv SecretKey
authSecretAccessKey =
    lens _authSecretAccessKey (\s a -> s { _authSecretAccessKey = a })
        . _Sensitive

-- | The token that users must pass to the service API to use the temporary
-- credentials.
authSessionToken :: Lens' AuthEnv (Maybe SessionToken)
authSessionToken =
    lens _authSessionToken (\s a -> s { _authSessionToken = a })
        . mapping _Sensitive

-- | The date on which the current credentials expire.
authExpiration :: Lens' AuthEnv (Maybe UTCTime)
authExpiration =
    lens _authExpiration (\s a -> s { _authExpiration = a })
        . mapping _Time

-- | An authorisation environment containing AWS credentials, and potentially
-- a reference which can be refreshed out-of-band as temporary credentials expire.
data Auth
    = Ref  ThreadId (IORef AuthEnv)
    | Auth AuthEnv

instance ToLog Auth where
    build (Ref t _) = "[Amazonka Auth] { <thread:" <> build (show t) <> "> }"
    build (Auth  e) = build e

withAuth :: MonadIO m => Auth -> (AuthEnv -> m a) -> m a
withAuth (Ref _ r) f = liftIO (readIORef r) >>= f
withAuth (Auth  e) f = f e

-- | The available AWS regions.
data Region
    = NorthVirginia   -- ^ US East ('us-east-1').
    | Ohio            -- ^ US East ('us-east-2').
    | NorthCalifornia -- ^ US West ('us-west-1').
    | Oregon          -- ^ US West ('us-west-2').
    | Montreal        -- ^ Canada ('ca-central-1').
    | HongKong        -- ^ Asia Pacific ('ap-east-1').
    | Tokyo           -- ^ Asia Pacific ('ap-northeast-1').
    | Seoul           -- ^ Asia Pacific ('ap-northeast-2').
    | Osaka           -- ^ Asia Pacific ('ap-northeast-2').
    | Mumbai          -- ^ Asia Pacific ('ap-south-1').
    | Singapore       -- ^ Asia Pacific ('ap-southeast-1').
    | Sydney          -- ^ Asia Pacific ('ap-southeast-2').
    | SaoPaulo        -- ^ South America ('sa-east-1').
    | Ireland         -- ^ EU ('eu-west-1').
    | London          -- ^ EU ('eu-west-2').
    | Paris           -- ^ EU ('eu-west-3').
    | Frankfurt       -- ^ EU ('eu-central-1').
    | Stockholm       -- ^ EU ('eu-north-1').
    | GovCloud        -- ^ US GovCloud ('us-gov-west-1').
    | GovCloudFIPS    -- ^ US GovCloud FIPS (S3 Only, 'fips-us-gov-west-1').
    | GovCloudEast    -- ^ US GovCloud ('us-gov-east-1').
    | Beijing         -- ^ China ('cn-north-1').
    | Ningxia         -- ^ China ('cn-northwest-1').
      deriving (Eq, Ord, Read, Enum, Bounded, Show, Data, Typeable, Generic)

instance Hashable Region
instance NFData   Region

instance FromText Region where
    parser = takeLowerText >>= \case
        "us-east-1"          -> pure NorthVirginia
        "us-east-2"          -> pure Ohio
        "us-west-1"          -> pure NorthCalifornia
        "us-west-2"          -> pure Oregon
        "ca-central-1"       -> pure Montreal
        "ap-east-1"          -> pure HongKong
        "ap-northeast-1"     -> pure Tokyo
        "ap-northeast-2"     -> pure Seoul
        "ap-northeast-3"     -> pure Osaka
        "ap-south-1"         -> pure Mumbai
        "ap-southeast-1"     -> pure Singapore
        "ap-southeast-2"     -> pure Sydney
        "sa-east-1"          -> pure SaoPaulo
        "eu-west-1"          -> pure Ireland
        "eu-west-2"          -> pure London
        "eu-west-3"          -> pure Paris
        "eu-central-1"       -> pure Frankfurt
        "eu-north-1"         -> pure Stockholm
        "us-gov-west-1"      -> pure GovCloud
        "fips-us-gov-west-1" -> pure GovCloudFIPS
        "us-gov-east-1"      -> pure GovCloudEast
        "cn-north-1"         -> pure Beijing
        "cn-northwest-1"     -> pure Ningxia
        e                    ->
            fromTextError $ "Failure parsing Region from " <> e

instance ToText Region where
    toText = \case
        NorthVirginia   -> "us-east-1"
        Ohio            -> "us-east-2"
        NorthCalifornia -> "us-west-1"
        Oregon          -> "us-west-2"
        Montreal        -> "ca-central-1"
        HongKong        -> "ap-east-1"
        Tokyo           -> "ap-northeast-1"
        Seoul           -> "ap-northeast-2"
        Osaka           -> "ap-northeast-3"
        Mumbai          -> "ap-south-1"
        Singapore       -> "ap-southeast-1"
        Sydney          -> "ap-southeast-2"
        SaoPaulo        -> "sa-east-1"
        Ireland         -> "eu-west-1"
        London          -> "eu-west-2"
        Paris           -> "eu-west-3"
        Frankfurt       -> "eu-central-1"
        Stockholm       -> "eu-north-1"
        GovCloudEast    -> "us-gov-east-1"
        GovCloud        -> "us-gov-west-1"
        GovCloudFIPS    -> "fips-us-gov-west-1"
        Beijing         -> "cn-north-1"
        Ningxia         -> "cn-northwest-1"

instance ToByteString Region

instance ToLog Region where
    build = build . toBS

instance FromXML  Region where parseXML  = parseXMLText "Region"
instance ToXML    Region where toXML     = toXMLText
instance FromJSON Region where parseJSON = parseJSONText "Region"
instance ToJSON   Region where toJSON    = toJSONText

-- | An integral value representing seconds.
newtype Seconds = Seconds Int
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Enum
        , Num
        , Bounded
        , Integral
        , Real
        , Data
        , Typeable
        , Generic
        , ToQuery
        , ToByteString
        , ToText
        , FromText
        )

instance Hashable Seconds
instance NFData   Seconds

instance ToLog Seconds where
    build s = build (toSeconds s) <> "s"

toSeconds :: Seconds -> Int
toSeconds (Seconds n)
    | n < 0     = 0
    | otherwise = n

toMicroseconds :: Seconds -> Int
toMicroseconds =  (1000000 *) . toSeconds

_Coerce :: (Coercible a b, Coercible b a) => Iso' a b
_Coerce = iso coerce coerce
