{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- Module      : Network.AWS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Types
    (
    -- * Authentication
    -- ** Credentials
      AccessKey     (..)
    , SecretKey     (..)
    , SecurityToken (..)
    -- ** Environment
    , AuthEnv       (..)
    , Auth          (..)
    , withAuth

    -- * Services
    , AWSService    (..)
    , Abbrev
    , Service       (..)
    , serviceOf

    -- * Retries
    , Retry         (..)

    -- * Endpoints
    , Endpoint      (..)
    , endpoint

    -- * Errors
    , ServiceError  (..)
    , _HttpError
    , _SerializerError
    , _ServiceError
    , _Errors

    -- * Signing
    , AWSSigner     (..)
    , AWSPresigner  (..)
    , Signed        (..)
    , Meta
    , sgMeta
    , sgRequest

    -- * Requests
    , AWSRequest    (..)
    , AWSPager      (..)
    , Request       (..)
    , rqMethod
    , rqHeaders
    , rqPath
    , rqQuery
    , rqBody

    -- * Responses
    , Response
    , Response'
    , Empty         (..)

    -- * Logging
    , LogLevel      (..)
    , Logger

    -- * Regions
    , Region        (..)

    -- * Query Actions
    , Action        (..)

    -- * Convenience
    , ClientRequest
    , ClientResponse
    , ResponseBody
    , clientRequest
    ) where

import           Control.Applicative
import           Control.Concurrent           (ThreadId)
import           Control.Exception            (Exception)
import           Control.Lens                 hiding (Action)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson                   hiding (Error)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import qualified Data.CaseInsensitive         as CI
import           Data.Conduit
import           Data.Default.Class
import qualified Data.HashSet                 as Set
import           Data.Hashable
import           Data.IORef
import           Data.List                    (intersperse)
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as Text
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           Network.AWS.Data
import qualified Network.HTTP.Client          as Client
import           Network.HTTP.Client          hiding (Request, Response)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status    (Status)

-- | Abbreviated service name.
type Abbrev = Text

-- | An error type representing the subset of errors that can be directly
-- attributed to this library.
data ServiceError a
    = HttpError        HttpException
    | SerializerError  Abbrev String
    | ServiceError     Abbrev Status a
    | Errors           [ServiceError a]
      deriving (Show, Typeable)

instance (Show a, Typeable a) => Exception (ServiceError a)

instance Monoid (ServiceError a) where
    mempty      = Errors []
    mappend a b = Errors (f a <> f b)
      where
        f (Errors xs) = xs
        f x           = [x]

-- | The properties (such as endpoint) for a service, as well as it's
-- associated signing algorithm and error types.
class (AWSSigner (Sg a), Show (Er a)) => AWSService a where
    -- | Signing algorithm supported by the service.
    type Sg a :: *

    -- | The general service error.
    type Er a :: *

    service :: Service a

serviceOf :: AWSService (Sv a) => Request a -> Service (Sv a)
serviceOf = const service

-- | An alias for the common response 'Either' containing a service error in the
-- 'Left' case, or the expected response in the 'Right'.
type Response  a = Either (ServiceError (Er (Sv a))) (Rs a)

type Response' a = Either (ServiceError (Er (Sv a))) (Status, Rs a)

data LogLevel
    = Info  -- ^ Informational messages supplied by the user, not used by the library.
    | Debug -- ^ Info level + debug messages + non-streaming response bodies.
    | Trace -- ^ Debug level + potentially sensitive signing metadata.
      deriving (Eq, Ord, Enum, Show)

type Logger = LogLevel -> Builder -> IO ()

-- | Specify how a request can be de/serialised.
class (AWSService (Sv a), AWSSigner (Sg (Sv a))) => AWSRequest a where
    -- | The service definition for a request.
    type Sv a :: *

    -- | The successful, expected response associated with a request.
    type Rs a :: *

    request  :: a -> Request a
    response :: MonadResource m
             => Logger
             -> Request a
             -> Either HttpException ClientResponse
             -> m (Response' a)

-- | Specify how an 'AWSRequest' and it's associated 'Rs' response can generate
-- a subsequent request, if available.
class AWSRequest a => AWSPager a where
    page :: a -> Rs a -> Maybe a

-- | Signing metadata data specific to a signing algorithm.
--
-- /Note:/ this is used for test and debug purposes, or is otherwise ignored.
data family Meta v :: *

-- | A signed 'ClientRequest' and associated metadata specific to the signing
-- algorithm that was used.
data Signed a v where
    Signed :: ToBuilder (Meta v)
           => { _sgMeta    :: Meta v
              , _sgRequest :: ClientRequest
              }
           -> Signed a v

sgMeta :: Lens' (Signed a v) (Meta v)
sgMeta f (Signed m rq) = f m <&> \y -> Signed y rq

sgRequest :: Lens' (Signed a v) ClientRequest
sgRequest f (Signed m rq) = f rq <&> \y -> Signed m y

class AWSSigner v where
    signed :: (AWSService (Sv a), v ~ Sg (Sv a))
           => AuthEnv
           -> Region
           -> Request a
           -> UTCTime
           -> Signed a v

class AWSPresigner v where
    presigned :: (AWSService (Sv a), v ~ Sg (Sv a))
              => AuthEnv
              -> Region
              -> Request a
              -> UTCTime
              -> Integer
              -> Signed a v

-- | Access key credential.
newtype AccessKey = AccessKey ByteString
    deriving (Eq, Show, IsString, ToText, ToByteString, ToBuilder)

-- | Secret key credential.
newtype SecretKey = SecretKey ByteString
    deriving (Eq, IsString, ToText, ToByteString)

-- | A security token used by STS to temporarily authorise access to
-- an AWS resource.
newtype SecurityToken = SecurityToken ByteString
    deriving (Eq, IsString, ToText, ToByteString)

-- | The authorisation environment.
data AuthEnv = AuthEnv
    { _authAccess :: !AccessKey
    , _authSecret :: !SecretKey
    , _authToken  :: Maybe SecurityToken
    , _authExpiry :: Maybe UTCTime
    }

instance FromJSON AuthEnv where
    parseJSON = withObject "AuthEnv" $ \o -> AuthEnv
        <$> f AccessKey (o .: "AccessKeyId")
        <*> f SecretKey (o .: "SecretAccessKey")
        <*> fmap (f SecurityToken) (o .:? "Token")
        <*> o .:? "Expiration"
      where
        f g = fmap (g . Text.encodeUtf8)

instance ToBuilder AuthEnv where
    build AuthEnv{..} = mconcat $ intersperse "\n"
        [ "[Amazonka Auth] {"
        , "  access key     = " <> build _authAccess
        , "  secret key     = ****"
        , "  security token = " <> maybe "Nothing" (const "Just ****") _authToken
        , "  expiry         = " <> build _authExpiry
        , "}"
        ]

-- | An authorisation environment containing AWS credentials, and potentially
-- a reference which can be refreshed out-of-band as temporary credentials expire.
data Auth
    = Ref  ThreadId (IORef AuthEnv)
    | Auth AuthEnv

instance ToBuilder Auth where
    build (Ref t _) = "[Amazonka Auth] { <thread:" <> build (show t) <> "> }"
    build (Auth  e) = build e

withAuth :: MonadIO m => Auth -> (AuthEnv -> m a) -> m a
withAuth (Auth  e) f = f e
withAuth (Ref _ r) f = liftIO (readIORef r) >>= f

data Endpoint = Endpoint
    { _endpointHost  :: ByteString
    , _endpointScope :: ByteString
    } deriving (Eq, Show)

-- | Determine the full host address and credential scope for a 'Service' within
-- the specified 'Region'.
endpoint :: Service a -> Region -> Endpoint
endpoint Service{..} r = go (CI.mk _svcPrefix)
  where
    go = \case
        "iam"
            | china     -> region "iam.cn-north-1.amazonaws.com.cn"
            | govcloud  -> region "iam.us-gov.amazonaws.com"
            | otherwise -> global "iam.amazonaws.com"

        "sdb"
            | virginia  -> region "sdb.amazonaws.com"

        "sts"
            | china     -> region "sts.cn-north-1.amazonaws.com.cn"
            | govcloud  -> region ("sts." <> reg <> ".amazonaws.com")
            | otherwise -> global "sts.amazonaws.com"

        "s3"
            | virginia  -> global "s3.amazonaws.com"
            | china     -> region ("s3." <> reg <> ".amazonaws.com.cn")
            | s3        -> region ("s3-" <> reg <> ".amazonaws.com")

        "rds"
            | virginia  -> global "rds.amazonaws.com"

        "route53"
            | not china -> global "route53.amazonaws.com"

        "emr"
            | virginia  -> global "elasticmapreduce.us-east-1.amazonaws.com"
            | otherwise -> region (reg <> ".elasticmapreduce.amazonaws.com")

        "sqs"
            | virginia  -> global "queue.amazonaws.com"
            | china     -> region (reg <> ".queue.amazonaws.com.cn")

        "importexport"
            | not china -> region "importexport.amazonaws.com"

        "cloudfront"
            | not china -> global "cloudfront.amazonaws.com"

        _   | china     -> region (_svcPrefix <> "." <> reg <> ".amazonaws.com.cn")
            | otherwise -> region (_svcPrefix <> "." <> reg <> ".amazonaws.com")

    virginia = r == NorthVirginia

    s3 = r `Set.member` except

    govcloud = "us-gov" `BS.isPrefixOf` reg
    china    = "cn-"    `BS.isPrefixOf` reg

    region h = Endpoint { _endpointHost = h, _endpointScope = reg }
    global h = Endpoint { _endpointHost = h, _endpointScope = "us-east-1" }

    reg = toBS r

    except = Set.fromList
        [ GovCloud
        , GovCloudFIPS
        , Ireland
        , NorthCalifornia
        , NorthVirginia
        , Oregon
        , SaoPaulo
        , Singapore
        , Sydney
        , Tokyo
        ]

-- | Attributes specific to an AWS service.
data Service a = Service
    { _svcAbbrev       :: !Text
    , _svcPrefix       :: !ByteString
    , _svcVersion      :: !ByteString
    , _svcTargetPrefix :: Maybe ByteString
    , _svcJSONVersion  :: Maybe ByteString
    , _svcHandle       :: Status -> Maybe (LazyByteString -> ServiceError (Er a))
    , _svcRetry        :: Retry a
    }

-- | Constants and predicates used to create a 'RetryPolicy'.
data Retry a = Exponential
    { _retryBase     :: !Double
    , _retryGrowth   :: !Int
    , _retryAttempts :: !Int
    , _retryCheck    :: Status -> Er a -> Bool
    }

-- | An unsigned request.
data Request a = Request
    { _rqMethod  :: !StdMethod
    , _rqPath    :: !ByteString
    , _rqQuery   :: Query
    , _rqHeaders :: [Header]
    , _rqBody    :: RqBody
    }

instance Default (Request a) where
    def = Request GET "/" mempty mempty ""

instance ToBuilder (Request a) where
    build Request{..} = mconcat $ intersperse "\n"
        [ "[Raw Request] {"
        , "  method  = "  <> build _rqMethod
        , "  path    = "  <> build _rqPath
        , "  query   = "  <> build _rqQuery
        , "  headers = "  <> build _rqHeaders
        , "  body    = {"
        , "    hash    = "  <> build (bodyHash _rqBody)
        , "    payload =\n" <> build (_bdyBody _rqBody)
        , "  }"
        , "}"
        ]

-- | The sum of available AWS regions.
data Region
    = Ireland         -- ^ Europe / eu-west-1
    | Frankfurt       -- ^ Europe / eu-central-1
    | Tokyo           -- ^ Asia Pacific / ap-northeast-1
    | Singapore       -- ^ Asia Pacific / ap-southeast-1
    | Sydney          -- ^ Asia Pacific / ap-southeast-2
    | Beijing         -- ^ China / cn-north-1
    | NorthVirginia   -- ^ US / us-east-1
    | NorthCalifornia -- ^ US / us-west-1
    | Oregon          -- ^ US / us-west-2
    | GovCloud        -- ^ AWS GovCloud / us-gov-west-1
    | GovCloudFIPS    -- ^ AWS GovCloud (FIPS 140-2) S3 Only / fips-us-gov-west-1
    | SaoPaulo        -- ^ South America / sa-east-1
      deriving (Eq, Ord, Read, Show, Generic)

instance Hashable Region

instance Default Region where
    def = NorthVirginia

instance FromText Region where
    parser = takeLowerText >>= \case
        "eu-west-1"          -> pure Ireland
        "eu-central-1"       -> pure Frankfurt
        "ap-northeast-1"     -> pure Tokyo
        "ap-southeast-1"     -> pure Singapore
        "ap-southeast-2"     -> pure Sydney
        "cn-north-1"         -> pure Beijing
        "us-east-1"          -> pure NorthVirginia
        "us-west-2"          -> pure Oregon
        "us-west-1"          -> pure NorthCalifornia
        "us-gov-west-1"      -> pure GovCloud
        "fips-us-gov-west-1" -> pure GovCloudFIPS
        "sa-east-1"          -> pure SaoPaulo
        e                    -> fail $
            "Failure parsing Region from " ++ show e

instance ToText Region where
    toText = \case
        Ireland         -> "eu-west-1"
        Frankfurt       -> "eu-central-1"
        Tokyo           -> "ap-northeast-1"
        Singapore       -> "ap-southeast-1"
        Sydney          -> "ap-southeast-2"
        Beijing         -> "cn-north-1"
        NorthVirginia   -> "us-east-1"
        NorthCalifornia -> "us-west-1"
        Oregon          -> "us-west-2"
        GovCloud        -> "us-gov-west-1"
        GovCloudFIPS    -> "fips-us-gov-west-1"
        SaoPaulo        -> "sa-east-1"

instance ToByteString Region
instance ToBuilder    Region

instance FromXML Region where parseXML = parseXMLText "Region"
instance ToXML   Region where toXML    = toXMLText

-- | A service's query action.
newtype Action = Action Text
    deriving (Eq, Ord, Show, IsString, ToText, ToByteString)

data Empty = Empty
    deriving (Eq, Show)

instance ToJSON Empty where
    toJSON = const Null

-- | A convenience alias to avoid type ambiguity.
type ClientRequest = Client.Request

-- | A convenience alias encapsulating the common 'Response'.
type ClientResponse = Client.Response ResponseBody

-- | A convenience alias encapsulating the common 'Response' body.
type ResponseBody = ResumableSource (ResourceT IO) ByteString

-- | Construct a 'ClientRequest' using common parameters such as TLS and prevent
-- throwing errors when receiving erroneous status codes in respones.
clientRequest :: ClientRequest
clientRequest = def
    { Client.secure      = True
    , Client.port        = 443
    , Client.checkStatus = \_ _ _ -> Nothing
    }

makePrisms ''ServiceError
makeLenses ''Request
