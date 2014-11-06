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
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Types
    (
    -- * Errors
      AWSError        (..)
    , AWSServiceError (..)
    , Error           (..)
    , _ServiceError
    , _HttpError
    , _SerializerError
    , _Nested

    -- * Authentication
    -- ** Credentials
    , AccessKey       (..)
    , SecretKey       (..)
    , SecurityToken   (..)
    -- ** Environment
    , AuthEnv         (..)
    , Auth            (..)
    , withAuth

    -- * Logger
    , Logger          (..)
    , debug

    -- * Services
    , AWSService      (..)
    , Service         (..)
    -- ** Endpoints
    , Endpoint        (..)
    , Host            (..)
    , endpoint

    -- * Signing
    , AWSSigner       (..)
    , AWSPresigner    (..)
    , Signed          (..)
    , Meta
    , sgMeta
    , sgRequest

    -- * Requests
    , AWSRequest      (..)
    , AWSPager        (..)
    , Request         (..)
    , rqMethod
    , rqHeaders
    , rqPath
    , rqQuery
    , rqBody
    -- * Responses
    , Empty           (..)

    -- ** HTTP Client
    , ClientRequest
    , ClientResponse
    , ResponseBody
    , clientRequest

    -- * Regions
    , Region          (..)
    , Zone            (..)
    , zRegion
    , zSuffix

    -- * Miscellaneous
    , Action          (..)
    , Sensitive       (..)
    , _Sensitive
    ) where

import           Control.Applicative
import           Control.Concurrent           (ThreadId)
import           Control.Exception            (Exception)
import           Control.Lens                 hiding (Action)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson                   hiding (Error)
import qualified Data.Attoparsec.Text         as AText
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Char
import           Data.Conduit
import           Data.Default.Class
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           Network.AWS.Data
import qualified Network.HTTP.Client          as Client
import           Network.HTTP.Client          hiding (Request)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status    (Status)
import           System.Locale

-- | Abbreviated service name.
type Abbrev = Text

-- | An error type representing the subset of errors that can be directly
-- attributed to this library.
data Error
    = HttpError       HttpException
    | SerializerError Abbrev String
    | ServiceError    Abbrev Status String
    | Nested          [Error]
      deriving (Show, Typeable)

instance Exception Error

instance Monoid Error where
    mempty      = Nested []
    mappend a b = Nested (f a <> f b)
      where
        f (Nested xs) = xs
        f x           = [x]

-- | Convert from a specific error to the more general 'Error' type.
class AWSError a where
    awsError :: a -> Error

instance AWSError Error where
    awsError = id

instance AWSError HttpException where
    awsError = HttpError

-- | Convert from service specific errors to the more general service error.
class (Typeable a, AWSError a) => AWSServiceError a where
    httpError       :: HttpException -> a
    serializerError :: String        -> a
    serviceError    :: Status        -> Maybe (LBS.ByteString -> a)

-- | The properties (such as endpoint) for a service, as well as it's
-- associated signing algorithm and error types.
class (AWSSigner (Sg a), AWSServiceError (Er a)) => AWSService a where
    type Sg a :: *
    type Er a :: *

    service :: Service a

-- | Specify how a data type can be de/serialised.
class (AWSService (Sv a), AWSSigner (Sg (Sv a))) => AWSRequest a where
    type Sv a :: *
    type Rs a :: *

    request  :: a -> Request a
    response :: MonadResource m
             => a
             -> Either HttpException ClientResponse
             -> m (Either (Er (Sv a)) (Rs a))

-- | Specify how an 'AWSRequest' and it's associated 'Rs' response can generate
-- a subsequent request, if available.
class AWSRequest a => AWSPager a where
    next :: a -> Rs a -> Maybe a

-- | Signing metadata data specific to a signing algorithm.
--
-- Note: this is used for test and debug purposes, or is otherwise ignored.
data family Meta v :: *

-- | A signed 'ClientRequest' and associated metadata specific to the signing
-- algorithm that was used.
data Signed a v where
    Signed :: Show (Meta v)
           => { _sgMeta    :: Meta v
              , _sgRequest :: ClientRequest
              }
           -> Signed a v

sgMeta :: Lens' (Signed a v) (Meta v)
sgMeta f (Signed m rq) = f m <&> \y -> Signed y rq

sgRequest :: Lens' (Signed a v) ClientRequest
sgRequest f (Signed m rq) = f rq <&> \y -> Signed m y

instance ToText (Signed a v) where
    toText (Signed m rq) = Text.unlines
        [ Text.pack (show m)
        , "HTTP Request:"
        , Text.pack (show rq)
        ]

class AWSSigner v where
    signed :: v ~ Sg (Sv a)
           => Service (Sv a)
           -> AuthEnv
           -> Region
           -> Request a
           -> TimeLocale
           -> UTCTime
           -> Signed a v

class AWSPresigner v where
    presigned :: v ~ Sg (Sv a)
              => Service (Sv a)
              -> AuthEnv
              -> Region
              -> Request a
              -> TimeLocale
              -> UTCTime
              -> Int
              -> Signed a v

-- | Access key credential.
newtype AccessKey = AccessKey ByteString
    deriving (Eq, Show, IsString)

instance ToByteString AccessKey where
    toBS (AccessKey k) = k

instance ToText AccessKey where
    toText = Text.decodeUtf8 . toBS

-- | Secret key credential.
newtype SecretKey = SecretKey ByteString
    deriving (Eq, Show, IsString)

instance ToByteString SecretKey where
    toBS (SecretKey k) = k

instance ToText SecretKey where
    toText = Text.decodeUtf8 . toBS

-- | A security token used by STS to temporarily authorise access to an AWS resource.
newtype SecurityToken = SecurityToken ByteString
    deriving (Eq, Show, IsString)

instance ToByteString SecurityToken where
    toBS (SecurityToken t) = t

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

-- | An authorisation environment containing AWS credentials and potentially
-- a reference which can be refreshed out-of-band as they expire.
data Auth
    = Ref  ThreadId (IORef AuthEnv)
    | Auth AuthEnv

withAuth :: MonadIO m => Auth -> (AuthEnv -> m a) -> m a
withAuth (Auth  e) f = f e
withAuth (Ref _ r) f = liftIO (readIORef r) >>= f

-- | The log level and associated logger function.
data Logger
    = None
    | Debug (Text -> IO ())

-- | Log a message using the debug logger, or if none is specified noop.
debug :: MonadIO m => Logger -> Text -> m ()
debug None      = const (return ())
debug (Debug f) = liftIO . f

newtype Host = Host ByteString
    deriving (Eq, Show)

instance ToByteString Host where
    toBS (Host h) = h

-- | The scope for a service's endpoint.
data Endpoint
    = Global
    | Regional
    | Custom ByteString

instance IsString Endpoint where
    fromString = Custom . fromString

-- | Determine the full host address for a 'Service within the given 'Region'.
endpoint :: Service a -> Region -> Host
endpoint Service{..} reg =
    let suf = ".amazonaws.com"
     in Host $ case _svcEndpoint of
            Global   -> _svcPrefix <> suf
            Regional -> _svcPrefix <> "." <> toBS reg <> suf
            Custom x -> x

-- | Attributes specific to an AWS service.
data Service a = Service
    { _svcEndpoint :: !Endpoint
    , _svcPrefix   :: ByteString
    , _svcVersion  :: ByteString
    , _svcTarget   :: Maybe ByteString
    }

-- | An unsigned request.
data Request a = Request
    { _rqMethod  :: !StdMethod
    , _rqPath    :: ByteString
    , _rqQuery   :: Query
    , _rqHeaders :: [Header]
    , _rqBody    :: RqBody
    }

instance Default (Request a) where
    def = Request GET "/" mempty mempty ""

instance ToText (Request a) where
    toText Request{..} = Text.unlines
        [ "Request:"
        , "_rqMethod  = " <> toText _rqMethod
        , "_rqPath    = " <> toText _rqPath
        , "_rqQuery   = " <> toText _rqQuery
        , "_rqHeaders = " <> toText _rqHeaders
        , "_rqBody    = " <> toText _rqBody
        ]

-- | The sum of available AWS regions.
data Region
    = Ireland         -- ^ Europe / eu-west-1
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

instance Default Region where
    def = NorthVirginia

instance FromText Region where
    parser = match "eu-west-1"          Ireland
         <|> match "ap-northeast-1"     Tokyo
         <|> match "ap-southeast-1"     Singapore
         <|> match "ap-southeast-2"     Sydney
         <|> match "cn-north-1"         Beijing
         <|> match "us-east-1"          NorthVirginia
         <|> match "us-west-2"          NorthCalifornia
         <|> match "us-west-1"          Oregon
         <|> match "us-gov-west-1"      GovCloud
         <|> match "fips-us-gov-west-1" GovCloudFIPS
         <|> match "sa-east-1"          SaoPaulo

instance ToText Region where
    toText r = case r of
        Ireland         -> "eu-west-1"
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
instance FromXML      Region
instance ToXML        Region

-- | An availability zone.
data Zone = Zone
    { _zRegion :: !Region
    , _zSuffix :: !Char
    } deriving (Eq, Ord, Read, Show)

instance FromText Zone where
    parser = Zone <$> parser <*> AText.satisfy isAlpha <* AText.endOfInput

instance ToText Zone where
    toText Zone{..} = toText _zRegion `Text.snoc` _zSuffix

-- | A service action.
newtype Action = Action Text
    deriving (Eq, Ord, Show, IsString)

instance ToQuery Action where
    toQuery (Action a) = toQuery ("Action" :: ByteString, a)

-- newtype Boolean = Boolean Bool
--     deriving (Eq, Ord, Show)

-- -- | Base64 encoded binary date.
-- newtype Base64 = Base64 ByteString
--     deriving (Eq, Ord, Show, Generic)

-- base64 :: ByteString -> Base64
-- base64 = Base64 . Base64.encode

-- instance ToByteString Base64 where
--     toBS (Base64 bs) = bs

-- instance FromJSON Base64 where
--     parseJSON = withText "Base64" $
--         return . Base64 . Base64.decodeLenient . Text.encodeUtf8

-- instance ToJSON Base64 where
--     toJSON (Base64 bs) = toJSON (Text.decodeUtf8 bs)

newtype Sensitive a = Sensitive a
    deriving (Eq, Ord)

instance Show (Sensitive a) where
    show = const "******"

data Empty = Empty
    deriving (Eq, Show)

-- | A convenience alias to avoid type ambiguity.
type ClientRequest = Client.Request

-- | A convenience alias encapsulating the common 'Response'.
type ClientResponse = Response ResponseBody

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

makePrisms ''Error
makeLenses ''Request
makeLenses ''Zone
makePrisms ''Sensitive
