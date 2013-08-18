{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
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
import           Data.String
import           Data.Time
import           Network.Http.Client    hiding (ContentType, post, put)
import           System.IO.Streams      (InputStream)
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
      deriving (Show)

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
    | Xml

instance IsByteString ContentType where
    toBS FormEncoded = "application/x-www-form-urlencoded"
    toBS Xml         = "application/xml"

newtype AWS a = AWS { unWrap :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Env)

currentRegion :: AWS Region
currentRegion = fromMaybe NorthVirgnia <$> fmap awsRegion ask

data RawRequest a b where
    RawRequest :: AWSService a
               => { rqMethod  :: !Method
                 , rqContent :: !ContentType
                 , rqAction  :: !(Maybe ByteString)
                 , rqPath    :: !(Maybe ByteString)
                 , rqHeaders :: !(Map ByteString ByteString)
                 , rqQuery   :: ![(ByteString, ByteString)]
                 , rqBody    :: !(Maybe (InputStream ByteString))
                 }
               -> RawRequest a b

emptyRequest :: (AWSService a, IsByteString p)
             => Method
             -> ContentType
             -> p
             -> Maybe (InputStream ByteString)
             -> RawRequest a b
emptyRequest meth content path body = RawRequest
    { rqMethod  = meth
    , rqContent = content
    , rqAction  = Nothing
    , rqHeaders = Map.empty
    , rqQuery   = []
    , rqBody    = body
    , rqPath    = let p = toBS path
                  in if BS.null p then Nothing else Just p
    }

data SignedRequest = SignedRequest
    { rqUrl     :: !ByteString
    , rqStream  :: !(Maybe (InputStream ByteString))
    , rqRequest :: !Request
    }

instance Show SignedRequest where
    show SignedRequest{..} = "SignedRequest: "
        ++ show rqUrl
        ++ "\n"
        ++ show rqRequest

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

class AWSService a where
    service :: RawRequest a b -> AWS Service

class AWSRequest c a b | a -> c b where
    request :: a -> AWS (RawRequest c b)

class Show a => Template a where
    readTemplate :: a -> ByteString

class IsByteString a where
    toBS :: a -> ByteString

instance IsByteString ByteString where
    toBS = id

instance IsByteString String where
    toBS = fromString

instance IsByteString Int where
    toBS = BS.pack . show

instance IsByteString Integer where
    toBS = BS.pack . show

instance IsByteString UTCTime where
    toBS = BS.pack . formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT"
