{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
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
import           Data.Monoid
import           Data.String
import           Data.Time
import           Network.Http.Client    hiding (post, put)
import           System.IO.Streams      (InputStream)

class IsByteString a where
    toBS :: a -> ByteString

instance IsByteString ByteString where
    toBS = id

newtype ApiVersion = ApiVersion ByteString
    deriving (Show, IsString, IsByteString)

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

newtype CallerRef = CallerRef String
    deriving (Show, IsString)

instance ToJSON CallerRef where
    toJSON (CallerRef s) = toJSON s

callerRef :: IO CallerRef
callerRef = fromString . show <$> getCurrentTime

data Protocol = HTTP | TCP
    deriving (Show)

instance ToJSON Protocol where
    toJSON = toJSON . show

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

newtype AWS a = AWS { unWrap :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Env)

data RawRequest a where
    RawRequest :: AWSSigner a
               => { rqMethod  :: !Method
                 , rqVersion :: !ApiVersion
                 , rqHost    :: !ByteString
                 , rqAction  :: !(Maybe ByteString)
                 , rqPath    :: !(Maybe ByteString)
                 , rqHeaders :: !(Map ByteString [ByteString])
                 , rqQuery   :: ![(ByteString, ByteString)]
                 , rqBody    :: !(Maybe (InputStream ByteString))
                 }
               -> RawRequest a

class AWSSigner a where
    sign :: RawRequest a -> AWS SignedRequest

class AWSRegion a where
    regionalise :: Region -> RawRequest a -> RawRequest a

class AWSSigner b => AWSRequest b a | a -> b where
    request :: a -> AWS (RawRequest b)

emptyRequest :: AWSSigner a
             => Method
             -> ApiVersion
             -> ByteString
             -> ByteString
             -> Maybe (InputStream ByteString)
             -> RawRequest a
emptyRequest meth ver host path body = RawRequest
    { rqMethod  = meth
    , rqVersion = ver
    , rqHost    = host
    , rqAction  = Nothing
    , rqPath    = if BS.null path then Nothing else Just path
    , rqHeaders = Map.empty
    , rqQuery   = []
    , rqBody    = body
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

class (Show a, ToJSON a) => Template a where
    readTemplate :: a -> ByteString

class Show a => QueryString a where
    queryString :: a -> [(ByteString, ByteString)]

class Show a => QueryParam a where
    queryParam :: ByteString -> a -> [(ByteString, ByteString)]

instance QueryParam a => QueryParam (Maybe a) where
    queryParam _ Nothing  = []
    queryParam k (Just v) = queryParam k v

instance QueryParam ByteString where
    queryParam k bstr = [(k, bstr)]

instance QueryParam [ByteString] where
    queryParam k = zipWith params ([1..] :: [Int])
      where
        params n v = (k <> "." <> BS.pack (show n), v)

instance QueryParam Integer where
    queryParam k n = [(k, BS.pack $ show n)]

