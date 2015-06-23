{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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

    -- * Errors
    , AsError       (..)
    , Error         (..)

    -- * Signing
    , AWSSigner     (..)
    , AWSPresigner  (..)
    , Signed        (..)
    , Meta
    , sgMeta
    , sgRequest
    , sign
    , presign
    , hmacSHA256

    -- * Requests
    , AWSRequest    (..)
    , Request       (..)
    , rqMethod
    , rqHeaders
    , rqPath
    , rqQuery
    , rqBody

    -- * Responses
    , Response

    -- * Logging
    , LogLevel      (..)
    , Logger

    -- * Regions
    , Region        (..)

    -- * Convenience
    , ClientRequest
    , ClientResponse
    , ResponseBody
    , clientRequest

    , _Coerce
    , _Default
    ) where

import           Control.Applicative
import           Control.Concurrent           (ThreadId)
import           Control.Exception            (Exception)
import           Control.Lens                 hiding (coerce)
import           Control.Monad.Except
import           Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA256           as SHA256
import qualified Crypto.MAC.HMAC              as HMAC
import           Data.Aeson                   hiding (Error)
import           Data.ByteString.Builder      (Builder)
import           Data.Coerce
import           Data.Conduit
import           Data.Hashable
import           Data.IORef
import           Data.Monoid
import           Data.String
import qualified Data.Text.Encoding           as Text
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           Network.HTTP.Client          hiding (Request, Response)
import qualified Network.HTTP.Client          as Client
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status    (Status)
import           Text.XML                     (def)

-- | Abbreviated service name.
type Abbrev = Text

-- | An error type representing the subset of errors that can be directly
-- attributed to this library.
data Error a
    = HttpError       HttpException
    | SerializerError Abbrev String
    | ServiceError    Abbrev Status a
    | Errors          [Error a]
      deriving (Show, Typeable, Functor, Foldable, Traversable)

instance (Show a, Typeable a) => Exception (Error a)

instance Monoid (Error a) where
    mempty      = Errors []
    mappend a b = Errors (f a <> f b)
      where
        f (Errors xs) = xs
        f x           = [x]

instance Show a => ToBuilder (Error a) where
    build = \case
        Errors          xs  -> buildLines (map build xs)
        HttpError       x   -> build x
        SerializerError a x -> buildLines
            [ "[SerializerError] {"
            , "  service = " <> build a
            , "  message = " <> build x
            ]
        ServiceError   a s x -> buildLines
            [ "[ServiceError] {"
            , "  service = " <> build a
            , "  status  = " <> build s
            , "  message = " <> build (show x)
            ]

class AsError e a | e -> a where
      _Error           :: Prism' e (Error a)
      {-# MINIMAL _Error #-}

      _HttpError       :: Prism' e HttpException
      _SerializerError :: Prism' e (Abbrev, String)
      _ServiceError    :: Prism' e (Abbrev, Status, a)
      _Errors          :: Prism' e [Error a]

      _HttpError       = _Error . _HttpError
      _SerializerError = _Error . _SerializerError
      _ServiceError    = _Error . _ServiceError
      _Errors          = _Error . _Errors

instance AsError (Error a) a where
    _Error = id

    _HttpError = prism
        HttpError
        (\case
            HttpError ex -> Right ex
            s            -> Left  s)

    _SerializerError = prism
        (uncurry SerializerError)
        (\case
            SerializerError a e -> Right (a, e)
            s                   -> Left  s)

    _ServiceError = prism
        (\(a, s, e) -> ServiceError a s e)
        (\case
            ServiceError a s e -> Right (a, s, e)
            s                  -> Left s)

    _Errors = prism
        Errors
        (\case
            Errors xs -> Right xs
            s         -> Left s)

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
-- type Response  a = Either (Error (Er (Sv a))) (Rs a)
-- type Response' a = Either (Error (Er (Sv a))) (Status, Rs a)

data LogLevel
    = Trace -- ^ Includes potentially sensitive signing metadata, and non-streaming response bodies.
    | Debug -- ^ Useful debug information + info + error levels.
    | Info  -- ^ Info messages supplied by the user - this level is not emitted by the library.
    | Error -- ^ Error messages.
      deriving (Eq, Ord, Enum, Show)

type Logger = LogLevel -> Builder -> IO ()

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

type Response a = Either (Error (Er (Sv a))) (Status, Rs a)

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
             -> m (Response a)

-- | Signing metadata data specific to a signing algorithm.
--
-- /Note:/ this is used for logging purposes, and is otherwise ignored.
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

-- Lens' specifically since 'a' cannot be substituted.
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

sign :: (MonadIO m, AWSRequest a, AWSSigner (Sg (Sv a)))
     => Auth      -- ^ AWS authentication credentials.
     -> Region    -- ^ AWS Region.
     -> Request a -- ^ Request to sign.
     -> UTCTime   -- ^ Signing time.
     -> m (Signed a (Sg (Sv a)))
sign a r rq t = withAuth a $ \e -> return (signed e r rq t)

presign :: (MonadIO m, AWSRequest a, AWSPresigner (Sg (Sv a)))
        => Auth      -- ^ AWS authentication credentials.
        -> Region    -- ^ AWS Region.
        -> Request a -- ^ Request to presign.
        -> UTCTime   -- ^ Signing time.
        -> Integer   -- ^ Expiry time in seconds.
        -> m (Signed a (Sg (Sv a)))
presign a r rq t ex = withAuth a $ \e -> return (presigned e r rq t ex)

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 = HMAC.hmac SHA256.hash 64

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

instance ToBuilder AuthEnv where
    build AuthEnv{..} = buildLines
        [ "[Amazonka Auth] {"
        , "  access key     = ****"
        , "  secret key     = ****"
        , "  security token = " <> build (const "****" <$> _authToken :: Maybe Builder)
        , "  expiry         = " <> build _authExpiry
        , "}"
        ]

instance FromJSON AuthEnv where
    parseJSON = withObject "AuthEnv" $ \o -> AuthEnv
        <$> f AccessKey (o .: "AccessKeyId")
        <*> f SecretKey (o .: "SecretAccessKey")
        <*> fmap (f SecurityToken) (o .:? "Token")
        <*> o .:? "Expiration"
      where
        f g = fmap (g . Text.encodeUtf8)

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

-- | Attributes specific to an AWS service.
data Service a = Service
    { _svcAbbrev  :: Text
    , _svcPrefix  :: ByteString
    , _svcVersion :: ByteString
    , _svcHandle  :: Status -> Maybe (LazyByteString -> Error (Er a))
    , _svcRetry   :: Retry a
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
    , _rqPath    :: ByteString
    , _rqQuery   :: QueryString
    , _rqHeaders :: [Header]
    , _rqBody    :: RqBody
    }

instance ToBuilder (Request a) where
    build Request{..} = buildLines
        [ "[Raw Request] {"
        , "  method  = "  <> build _rqMethod
        , "  path    = "  <> build _rqPath
        , "  query   = "  <> build _rqQuery
        , "  headers = "  <> build _rqHeaders
        , "  body    = {"
        , "    hash    = "  <> build (_rqBody ^. bodyHash)
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

instance ToBuilder Region where
    build = build . toBS

instance FromXML Region where parseXML = parseXMLText "Region"
instance ToXML   Region where toXML    = toXMLText

_Coerce :: (Coercible a b, Coercible b a) => Iso' a b
_Coerce = iso coerce coerce

-- | Invalid Iso, should be a Prism but exists for ease of composition
-- with the current 'Lens . Iso' chaining to hide internal types from the user.
_Default :: Monoid a => Iso' (Maybe a) a
_Default = iso f Just
  where
    f (Just x) = x
    f Nothing  = mempty

makeLenses ''Request
