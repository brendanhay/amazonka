{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Network.Http.Client    hiding (post, put)
import           System.IO.Streams      (InputStream)

class IsByteString a where
    toBS :: a -> ByteString

newtype ApiVersion = ApiVersion ByteString
    deriving (Show)

instance IsString ApiVersion where
    fromString = ApiVersion . BS.pack

instance IsByteString ApiVersion where
    toBS (ApiVersion ver) = ver

data Region
    = NorthVirgnia
    | NorthCalifornia
    | Oregon
    | Ireland
    | Singapore
    | Tokyo
    | Sydney
    | SaoPaulo

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
    { accessKey :: ByteString
    , secretKey :: ByteString
    } deriving (Show)

instance FromJSON Auth where
    parseJSON (Object o) = Auth
        <$> o .: "AccessKeyId"
        <*> o .: "SecretAccessKey"
    parseJSON _ = mzero

newtype AWS a = AWS { unWrap :: ReaderT Auth IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Auth)

data RawRequest = RawRequest
    { rqMethod  :: !Method
    , rqVersion :: !ApiVersion
    , rqHost    :: !ByteString
    , rqAction  :: !(Maybe ByteString)
    , rqPath    :: !(Maybe ByteString)
    , rqHeaders :: !(Map ByteString [ByteString])
    , rqQuery   :: ![(ByteString, ByteString)]
    , rqBody    :: !(Maybe (InputStream ByteString))
    }

emptyRequest :: Method
             -> ApiVersion
             -> ByteString
             -> ByteString
             -> Maybe (InputStream ByteString)
             -> RawRequest
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

class Show a => GlobalRequest a where
    signGlobal :: a -> AWS SignedRequest

class Show a => RegionRequest a where
    signRegion :: Region -> a -> AWS SignedRequest

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
