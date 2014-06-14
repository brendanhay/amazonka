{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
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
import           Control.Exception            (Exception)
import           Control.Lens                 hiding (Action)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson                   hiding (Error)
import qualified Data.Attoparsec.Text         as AText
import           Data.ByteString              (ByteString)
import           Data.Char
import           Data.Conduit
import           Data.Default
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Time
import           Data.Typeable
import           Network.AWS.Data
import qualified Network.HTTP.Client          as Client
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method

type ClientRequest  = Client.Request
type ClientResponse = Client.Response

clientRequest :: ClientRequest
clientRequest = def
    { Client.secure      = True
    , Client.port        = 443
    , Client.checkStatus = \_ _ _ -> Nothing
    }

data Error
    = Error  String
    | Nested [Error]
    deriving (Eq, Show, Typeable)

instance IsString Error where
    fromString = Error

instance Exception Error

-- FIXME: This has currently been defined only for purposes of
-- an Applicative instance for the monad transformer. Do the monoid laws hold?

instance Monoid Error where
    mempty = Nested []

    mappend (Nested a) (Nested b) = Nested (a ++ b)
    mappend (Nested a) b          = Nested (a ++ [b])
    mappend a          (Nested b) = Nested (a : b)
    mappend a          b          = Nested [a, b]

class AWSError e where
    toError :: e -> Error

instance AWSError Error where
    toError = id

class AWSService a where
    type Signer' a :: *
    type Error'  a :: *

    service :: Service a

class (AWSService (Service' a), AWSError (Error' (Service' a))) => AWSRequest a where
    type Service'  a :: *
    type Response' a :: *

    request  :: a -> Request a
    response :: MonadResource m
             => a
             -> ClientResponse (ResumableSource m ByteString)
             -> m (Either (Error' (Service' a)) (Response' a))

class AWSRequest a => AWSPager a where
    next :: a -> Response' a -> Maybe a

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
    = Ref  (IORef AuthEnv)
    | Auth AuthEnv

withAuth :: MonadIO m => Auth -> (AuthEnv -> m a) -> m a
withAuth (Ref  r) f = liftIO (readIORef r) >>= f
withAuth (Auth e) f = f e

data Endpoint
    = Global
    | Regional
    | Custom !ByteString

instance IsString Endpoint where
    fromString = Custom . fromString

data Service a = Service
    { _svcEndpoint :: !Endpoint
    , _svcName     :: !ByteString
    , _svcVersion  :: !ByteString
    , _svcTarget   :: Maybe ByteString
    }

newtype Host = Host ByteString
    deriving (Eq, Show)

instance ToByteString Host where
    toBS (Host h) = h

endpoint :: Service a -> Region -> Host
endpoint Service{..} reg =
    let suf = ".amazonaws.com"
     in Host $ case _svcEndpoint of
            Global   -> _svcName <> suf
            Regional -> _svcName <> "." <> toBS reg <> suf
            Custom x -> x

data Request a = Request
    { _rqMethod  :: !StdMethod
    , _rqPath    :: !ByteString
    , _rqQuery   :: Query
    , _rqHeaders :: [Header]
    , _rqBody    :: Body
    }

instance Default (Request a) where
    def = Request GET "/" mempty mempty ""

instance Show (Request a) where
    show Request{..} = unlines
        [ "Request:"
        , "_rqMethod  = " ++ show _rqMethod
        , "_rqPath    = " ++ show _rqPath
        , "_rqQuery   = " ++ show _rqQuery
        , "_rqHeaders = " ++ show _rqHeaders
        , "_rqBody    = " ++ show _rqBody
        ]

rqMethod :: Functor f => LensLike' f (Request a) StdMethod
rqMethod f x = (\y -> x { _rqMethod = y }) <$> f (_rqMethod x)

rqPath :: Functor f => LensLike' f (Request a) ByteString
rqPath f x = (\y -> x { _rqPath = y }) <$> f (_rqPath x)

rqQuery :: Functor f => LensLike' f (Request a) Query
rqQuery f x = (\y -> x { _rqQuery = y }) <$> f (_rqQuery x)

rqHeaders :: Functor f => LensLike' f (Request a) [Header]
rqHeaders f x = (\y -> x { _rqHeaders = y }) <$> f (_rqHeaders x)

rqBody :: Functor f => LensLike' f (Request a) Body
rqBody f x = (\y -> x { _rqBody = y }) <$> f (_rqBody x)

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
      deriving (Eq, Ord)

instance Default Region where
    def = NorthVirginia

instance Read Region where
    readsPrec = const readText

instance Show Region where
    show = showText

instance FromText Region where
    parser = AText.choice $ map (\(x, y) -> AText.string x >> return y)
        [ ("eu-west-1",          Ireland)
        , ("ap-northeast-1",     Tokyo)
        , ("ap-southeast-1",     Singapore)
        , ("ap-southeast-2",     Sydney)
        , ("cn-north-1",         Beijing)
        , ("us-east-1",          NorthVirginia)
        , ("us-west-2",          NorthCalifornia)
        , ("us-west-1",          Oregon)
        , ("us-gov-west-1",      GovCloud)
        , ("fips-us-gov-west-1", GovCloudFIPS)
        , ("sa-east-1",          SaoPaulo)
        ]

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

instance ToByteString Region where
    toBS = toBS . toText

data AZ = AZ
    { _azRegion :: !Region
    , _azSuffix :: !Char
    } deriving (Eq, Ord)

instance Read AZ where
    readsPrec = const readText

instance Show AZ where
    show = showText

instance FromText AZ where
    parser = AZ <$> parser <*> AText.satisfy isAlpha <* AText.endOfInput

instance ToText AZ where
    toText AZ{..} = toText _azRegion `Text.snoc` _azSuffix

newtype Action = Action Text
    deriving (Eq, Show, IsString)

instance ToQuery Action where
    toQuery (Action a) = toQuery ("Action" :: ByteString, a)
