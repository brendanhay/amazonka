{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Network.AWS.Types where

import           Control.Applicative
import           Control.Concurrent        (ThreadId)
import           Control.Exception         (Exception)
import           Control.Lens              hiding (Action)
import           Control.Monad.IO.Class
import           Data.Aeson                hiding (Error)
import qualified Data.Attoparsec.Text      as AText
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base64    as Base64
import           Data.Char
import           Data.Conduit
import           Data.Default
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Text.Lazy            as LText
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           Network.AWS.Data
import qualified Network.HTTP.Client       as Client
import           Network.HTTP.Client       hiding (Request, Response)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           System.Locale

type ClientRequest    = Client.Request
type ClientResponse m = Client.Response (ResumableSource m ByteString)

clientRequest :: ClientRequest
clientRequest = def
    { Client.secure      = True
    , Client.port        = 443
    , Client.checkStatus = \_ _ _ -> Nothing
    }

data Error
    = ServiceError    String
    | ClientError     HttpException
    | SerializerError String
    | Nested          [Error]
      deriving (Show, Typeable)

instance Exception Error

instance IsString Error where
    fromString = ServiceError

instance Monoid Error where
    mempty      = Nested []
    mappend a b = Nested (f a <> f b)
      where
        f (Nested xs) = xs
        f x           = [x]

class AWSError a where
    awsError :: a -> Error

instance AWSError Error where
    awsError = id

instance AWSError String where
    awsError = ServiceError

instance AWSError Text where
    awsError = ServiceError . Text.unpack

instance AWSError LText.Text where
    awsError = ServiceError . LText.unpack

instance AWSError HttpException where
    awsError = ClientError

class AWSError a => AWSServiceError a where
    serviceError    :: String        -> a
    clientError     :: HttpException -> a
    serializerError :: String        -> a

instance AWSServiceError Error where
    serviceError    = ServiceError
    clientError     = ClientError
    serializerError = SerializerError

class (AWSSigner (Sg a), AWSServiceError (Er a)) => AWSService a where
    type Sg a :: *
    data Er a :: *

    service :: Service' a

deriving instance Typeable Er

class (AWSService (Sv a), AWSSigner (Sg (Sv a))) => AWSRequest a where
    type Sv a :: *
    type Rs a :: *

    request  :: a -> Request a
    response :: Monad m
             => a
             -> Either HttpException (ClientResponse m)
             -> m (Either (Er (Sv a)) (Rs a))

class AWSRequest a => AWSPager a where
    next :: a -> Rs a -> Maybe a

data family Meta v :: *

data Signed a v where
    Signed :: Show (Meta v)
           => { _sgMeta    :: Meta v
              , _sgRequest :: ClientRequest
              }
           -> Signed a v

instance ToText (Signed a v) where
    toText (Signed m rq) = Text.unlines
        [ Text.pack (show m)
        , "HTTP Request:"
        , Text.pack (show rq)
        ]

class AWSSigner v where
    signed :: v ~ Sg (Sv a)
           => Service' (Sv a)
           -> AuthEnv
           -> Region
           -> Request a
           -> TimeLocale
           -> UTCTime
           -> Signed a v

class AWSPresigner v where
    presigned :: v ~ Sg (Sv a)
              => Service' (Sv a)
              -> AuthEnv
              -> Region
              -> Request a
              -> TimeLocale
              -> UTCTime
              -> Int
              -> Signed a v

newtype AccessKey = AccessKey ByteString
    deriving (Eq, Show, IsString)

instance ToByteString AccessKey where
    toBS (AccessKey k) = k

newtype SecretKey = SecretKey ByteString
    deriving (Eq, Show, IsString)

instance ToByteString SecretKey where
    toBS (SecretKey k) = k

newtype SecurityToken = SecurityToken ByteString
    deriving (Eq, Show, IsString)

instance ToByteString SecurityToken where
    toBS (SecurityToken t) = t

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

data Auth
    = Ref  ThreadId (IORef AuthEnv)
    | Auth AuthEnv

withAuth :: MonadIO m => Auth -> (AuthEnv -> m a) -> m a
withAuth (Auth  e) f = f e
withAuth (Ref _ r) f = liftIO (readIORef r) >>= f

data Logging
    = None
    | Debug (Text -> IO ())

debug :: MonadIO m => Logging -> Text -> m ()
debug None      = const (return ())
debug (Debug f) = liftIO . f

data Endpoint'
    = Global
    | Regional
    | Custom ByteString

instance IsString Endpoint' where
    fromString = Custom . fromString

data Service' a = Service'
    { _svcEndpoint :: !Endpoint'
    , _svcPrefix   :: ByteString
    , _svcVersion  :: ByteString
    , _svcTarget   :: Maybe ByteString
    }

newtype Host = Host ByteString
    deriving (Eq, Show)

instance ToByteString Host where
    toBS (Host h) = h

endpoint :: Service' a -> Region -> Host
endpoint Service'{..} reg =
    let suf = ".amazonaws.com"
     in Host $ case _svcEndpoint of
            Global   -> _svcPrefix <> suf
            Regional -> _svcPrefix <> "." <> toBS reg <> suf
            Custom x -> x

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

data Zone = Zone
    { _zRegion :: !Region
    , _zSuffix :: !Char
    } deriving (Eq, Ord, Read, Show)

instance FromText Zone where
    parser = Zone <$> parser <*> AText.satisfy isAlpha <* AText.endOfInput

instance ToText Zone where
    toText Zone{..} = toText _zRegion `Text.snoc` _zSuffix

newtype Action = Action Text
    deriving (Eq, Show, IsString)

instance ToQuery Action where
    toQuery (Action a) = toQuery ("Action" :: ByteString, a)

data Switch a = Enabled | Disabled
    deriving (Eq, Show, Generic)

newtype Base64 = Base64 ByteString
    deriving (Eq, Show, Generic)

base64 :: ByteString -> Base64
base64 = Base64 . Base64.encode

instance ToByteString Base64 where
    toBS (Base64 bs) = bs

instance FromJSON Base64 where
    parseJSON = withText "Base64" $
        return . Base64 . Base64.decodeLenient . Text.encodeUtf8

instance ToJSON Base64 where
    toJSON (Base64 bs) = toJSON (Text.decodeUtf8 bs)

-- Sums
makePrisms ''Error

-- Products
makeLenses ''Request

sgMeta :: Functor f => LensLike' f (Signed a v) (Meta v)
sgMeta f (Signed m rq) = (\y -> Signed y rq) <$> f m

sgRequest :: Functor f => LensLike' f (Signed a v) ClientRequest
sgRequest f (Signed m rq) = (\y -> Signed m y) <$> f rq
