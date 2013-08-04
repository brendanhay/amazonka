{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

-- |
-- Module      : Network.AWS.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Types where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.String
import           Network.Http.Client    hiding (post, put)
import           System.IO.Streams      (InputStream)

class IsByteString a where
    toBS :: a -> ByteString

data Credentials = Credentials
    { accessKey :: ByteString
    , secretKey :: ByteString
    } deriving (Show)

newtype AWS a = AWS { unWrap :: ReaderT Credentials IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Credentials)

data SigningVersion
    = Version2
    | Version3
      deriving (Show)

newtype ApiVersion = ApiVersion ByteString
    deriving (Show)

instance IsString ApiVersion where
    fromString = ApiVersion . BS.pack

instance IsByteString ApiVersion where
    toBS (ApiVersion ver) = ver

data RawRequest where
    RawRequest :: { rqMethod  :: !Method
                 , rqVersion :: !ApiVersion
                 , rqHost    :: !ByteString
                 , rqAction  :: !(Maybe ByteString)
                 , rqPath    :: !(Maybe ByteString)
                 , rqHeaders :: !(Map ByteString [ByteString])
                 , rqQuery   :: ![(ByteString, ByteString)]
                 , rqBody    :: !(Maybe (InputStream ByteString))
                 } -> RawRequest

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

class (Show a, ToJSON a) => AWSTemplate a where
    readTemplate :: a -> ByteString

class Show a => AWSRequest a where
    signRequest :: a -> AWS SignedRequest

class Show a => AWSQuery a where
    queryString :: a -> [(ByteString, ByteString)]

class Show a => AWSParam a where
    queryParam :: ByteString -> a -> [(ByteString, ByteString)]

instance AWSParam a => AWSParam (Maybe a) where
    queryParam _ Nothing  = []
    queryParam k (Just v) = queryParam k v

instance AWSParam ByteString where
    queryParam k bstr = [(k, bstr)]
