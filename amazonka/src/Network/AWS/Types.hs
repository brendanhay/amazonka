{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
import           Control.Exception         (Exception)
import           Control.Lens              hiding (Action)
import           Control.Monad.Base
import           Data.Aeson                hiding (Error)
import qualified Data.Attoparsec.Text      as AText
import           Data.ByteString           (ByteString)
import           Data.Char
import           Data.Default
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           Network.AWS.Data
import qualified Network.HTTP.Client       as Client
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           System.Locale

type ClientRequest   = Client.Request
type ClientResponse  = Client.Response Client.BodyReader
type ClientException = Client.HttpException

clientRequest :: ClientRequest
clientRequest = def
    { Client.secure      = True
    , Client.port        = 443
    , Client.checkStatus = \_ _ _ -> Nothing
    }

data Error
    = AWSError    String
    | ClientError ClientException
      deriving (Show, Typeable)

instance IsString Error where
    fromString = AWSError

instance Exception Error

class AWSError a where
    awsError :: a -> Error

instance AWSError Error where
    awsError = id

instance AWSError String where
    awsError = AWSError

instance AWSError ClientException where
    awsError = ClientError

class ServiceError a where
    serviceError :: String          -> a
    clientError  :: ClientException -> a

instance ServiceError Error where
    serviceError = AWSError
    clientError  = ClientError

class AWSService a where
    type Sg a :: *
    data Er a :: *

    service :: Service a

deriving instance Typeable Er

class ( AWSService   (Sv a)
      , AWSSigner    (Sg (Sv a))
      , AWSError     (Er (Sv a))
      , ServiceError (Er (Sv a))
      ) => AWSRequest a where
    type Sv a :: *
    type Rs a :: *

    request  :: a -> Request a
    response :: MonadBase IO m
             => a
             -> ClientResponse
             -> m (Either (Er (Sv a)) (Rs a))

class AWSRequest a => AWSPager a where
    next :: a -> Rs a -> Maybe a

data family Meta v :: *

data Signed a v where
    Signed :: Show (Meta v) => Meta v -> ClientRequest -> Signed a v

instance ToText (Signed a v) where
    toText (Signed m rq) = Text.unlines
        [ Text.pack (show m)
        , "HTTP Request:"
        , Text.pack (show rq)
        ]

sgMeta :: Functor f => LensLike' f (Signed a v) (Meta v)
sgMeta f (Signed m rq) = (\y -> Signed y rq) <$> f m

sgRequest :: Functor f => LensLike' f (Signed a v) ClientRequest
sgRequest f (Signed m rq) = (\y -> Signed m y) <$> f rq

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

withAuth :: MonadBase IO m => Auth -> (AuthEnv -> m a) -> m a
withAuth (Ref  r) f = liftBase (readIORef r) >>= f
withAuth (Auth e) f = f e

data Logging
    = None
    | Debug (Text -> IO ())

debug :: MonadBase IO m => Logging -> Text -> m ()
debug l =
    case l of
        None    -> const (return ())
        Debug f -> liftBase . f

data Endpoint
    = Global
    | Regional
    | Custom ByteString

instance IsString Endpoint where
    fromString = Custom . fromString

data Service a = Service
    { _svcEndpoint :: !Endpoint
    , _svcPrefix   :: ByteString
    , _svcVersion  :: ByteString
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
            Global   -> _svcPrefix <> suf
            Regional -> _svcPrefix <> "." <> toBS reg <> suf
            Custom x -> x

data Request a = Request
    { _rqMethod  :: !StdMethod
    , _rqPath    :: ByteString
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

data Zone = Zone
    { _zRegion :: !Region
    , _zSuffix :: !Char
    } deriving (Eq, Ord)

instance Read Zone where
    readsPrec = const readText

instance Show Zone where
    show = showText

instance FromText Zone where
    parser = Zone <$> parser <*> AText.satisfy isAlpha <* AText.endOfInput

instance ToText Zone where
    toText Zone{..} = toText _zRegion `Text.snoc` _zSuffix

newtype Action = Action Text
    deriving (Eq, Show, IsString)

instance ToQuery Action where
    toQuery (Action a) = toQuery ("Action" :: ByteString, a)

newtype BucketName = BucketName Text
    deriving (Eq, Show, Generic, IsString)

instance ToByteString BucketName where toBS (BucketName b) = toBS b
instance FromText     BucketName where parser = BucketName <$> takeText
instance ToText       BucketName where toText (BucketName b) = b
instance FromXML      BucketName

newtype ObjectKey = ObjectKey Text
    deriving (Eq, Show, Generic, IsString)

instance ToByteString ObjectKey where toBS (ObjectKey k) = toBS k
instance FromText     ObjectKey where parser = ObjectKey <$> takeText
instance ToText       ObjectKey where toText (ObjectKey k) = k
instance FromXML      ObjectKey

newtype ObjectVersionId = ObjectVersionId Text
    deriving (Eq, Show, Generic, IsString)

instance ToByteString ObjectVersionId where toBS (ObjectVersionId v) = toBS v
instance FromText     ObjectVersionId where parser = ObjectVersionId <$> takeText
instance ToText       ObjectVersionId where toText (ObjectVersionId v) = v
instance FromXML      ObjectVersionId

newtype ETag = ETag Text
    deriving (Eq, Show, Generic, IsString)

instance ToByteString ETag where toBS (ETag t) = toBS t
instance FromText     ETag where parser = ETag <$> takeText
instance ToText       ETag where toText (ETag t) = t
instance FromXML      ETag

data Switch a = Enabled | Disabled
    deriving (Eq, Show, Generic)
