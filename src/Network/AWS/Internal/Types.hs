{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- Module      : Network.AWS.Internal.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Types where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Encoding
import           Data.Time
import           Network.Http.Client    hiding (ContentType, post, put)
import           System.Locale          (defaultTimeLocale)

data Region
    = NorthVirgnia
    | NorthCalifornia
    | Oregon
    | Ireland
    | Singapore
    | Tokyo
    | Sydney
    | SaoPaulo
      deriving (Eq)

instance Show Region where
    show = BS.unpack . toBS

instance IsByteString Region where
    toBS reg = case reg of
        NorthVirgnia    -> "us-east-1"
        NorthCalifornia -> "us-west-1"
        Oregon          -> "us-west-2"
        Ireland         -> "eu-west-1"
        Singapore       -> "ap-southeast-1"
        Tokyo           -> "ap-northeast-1"
        Sydney          -> "ap-southeast-2"
        SaoPaulo        -> "sa-east-1"

data Auth = Auth
    { accessKey :: !ByteString
    , secretKey :: !ByteString
    } deriving (Show)

instance FromJSON Auth where
    parseJSON (Object o) = Auth
        <$> o .: "AccessKeyId"
        <*> o .: "SecretAccessKey"
    parseJSON _ = mzero

data Env = Env
    { awsRegion :: !(Maybe Region)
    , awsAuth   :: !Auth
    }

data ContentType
    = FormEncoded
    | XML

instance Show ContentType where
    show = BS.unpack . toBS

instance IsByteString ContentType where
    toBS FormEncoded = "application/x-www-form-urlencoded"
    toBS XML         = "application/xml"

newtype AWS a = AWS { unWrap :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Env)

currentRegion :: AWS Region
currentRegion = fromMaybe NorthVirgnia <$> fmap awsRegion ask

data RawRequest s b where
    RawRequest :: { rqMethod  :: !Method
                  , rqContent :: !ContentType
                  , rqAction  :: !(Maybe ByteString)
                  , rqPath    :: !(Maybe ByteString)
                  , rqHeaders :: !(Map ByteString ByteString)
                  , rqQuery   :: ![(ByteString, ByteString)]
                  , rqBody    :: !(Maybe ByteString)
                  }
                -> RawRequest s b

deriving instance Show (RawRequest s b)

instance ToJSON (RawRequest s b) where
    toJSON RawRequest{..} = object
        [ "rqMethod"  .= (String . Text.pack $ show rqMethod)
        , "rqContent" .= (String $ toText rqContent)
        , "rqAction"  .= rqAction
        , "rqPath"    .= rqPath
        , "rqQuery"   .= rqQuery
        ]

emptyRequest :: IsByteString p
             => Method
             -> ContentType
             -> p
             -> Maybe ByteString
             -> RawRequest s b
emptyRequest meth content path body = RawRequest
    { rqMethod  = meth
    , rqContent = content
    , rqAction  = Nothing
    , rqHeaders = Map.empty
    , rqQuery   = []
    , rqBody    = body
    , rqPath    = let p = toBS path
                  in if BS.null p
                         then Nothing
                         else Just p
    }

data SignedRequest = SignedRequest
    { rqUrl     :: !ByteString
    , rqPayload :: !(Maybe ByteString)
    , rqRequest :: !Request
    } deriving (Show)

data SigningVersion
    = SigningVersion2
    | SigningVersion3
    | SigningVersion4
      deriving (Show)

data Service = Service
    { svcName     :: !ByteString
    , svcVersion  :: !ByteString
    , svcEndpoint :: !ByteString
    , svcSigner   :: !SigningVersion
    , svcRegion   :: !Region
    }

awsService :: ByteString -> ByteString -> SigningVersion -> AWS Service
awsService name ver signer = do
    reg <- currentRegion
    return $! Service name ver (endpoint reg) signer reg
 where
   endpoint reg = name <> "." <> toBS reg <> ".amazonaws.com"

class AWSService s where
    service :: RawRequest s b -> AWS Service

class AWSRequest s a b | a -> s b where
    request :: a -> RawRequest s b

class Template a where
    readTemplate :: a -> ByteString

class IsByteString a where
    toBS   :: a -> ByteString
    toText :: a -> Text

    toText = decodeUtf8 . toBS

instance IsByteString ByteString where
    toBS = id

instance IsByteString Int where
    toBS = BS.pack . show

instance IsByteString Integer where
    toBS = BS.pack . show

instance IsByteString UTCTime where
    toBS = BS.pack . formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT"
