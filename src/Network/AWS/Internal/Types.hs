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
import           Data.Monoid
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Enc
import           Network.Http.Client    hiding (post, put)
import           System.IO.Streams      (InputStream)

class IsText a where
    toText :: a -> Text
    toBS   :: a -> ByteString
    toStr  :: a -> String

    toBS  = Enc.encodeUtf8 . toText
    toStr = Text.unpack . toText

instance IsText ByteString where
    toText = Enc.decodeUtf8
    toBS   = id

instance IsText Text where
    toText = id

newtype ApiVersion = ApiVersion ByteString
    deriving (Show, IsString, IsText)

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

instance IsText Region where
    toText reg = case reg of
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

newtype AWS a = AWS { unWrap :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Env)

data RawRequest a b where
    RawRequest :: (AWSService a, FromJSON b)
               => { rqMethod  :: !Method
                 , rqHost    :: !ByteString
                 , rqAction  :: !(Maybe ByteString)
                 , rqPath    :: !(Maybe ByteString)
                 , rqHeaders :: !(Map ByteString [ByteString])
                 , rqQuery   :: ![(ByteString, ByteString)]
                 , rqBody    :: !(Maybe (InputStream ByteString))
                 }
               -> RawRequest a b

data SignedRequest a where
    SignedRequest :: FromJSON a
                  => { rqUrl     :: !ByteString
                    , rqStream  :: !(Maybe (InputStream ByteString))
                    , rqRequest :: !Request
                    }
                  -> SignedRequest a

emptyRequest :: (AWSService a, FromJSON b, IsText p)
             => Method
             -> ByteString
             -> p
             -> Maybe (InputStream ByteString)
             -> RawRequest a b
emptyRequest meth host path body = RawRequest
    { rqMethod  = meth
    , rqHost    = host
    , rqAction  = Nothing
    , rqPath    = if BS.null path' then Nothing else Just path'
    , rqHeaders = Map.empty
    , rqQuery   = []
    , rqBody    = body
    }
  where
    path' = toBS path

data SigningVersion
    = Version2
    | Version3
    | Version4

data Service = Service
    { svcName     :: !ByteString
    , svcVersion  :: !ApiVersion
    , svcSigning  :: !SigningVersion
    , svcEndpoint :: !ByteString
    , svcRegion   :: !Region
    }

class AWSService a where
    service :: RawRequest a b -> AWS Service

class (AWSService a, FromJSON b) => AWSRequest a c b | c -> a b where
    request :: c -> AWS (RawRequest a b)

instance Show (SignedRequest a) where
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

instance QueryParam Text where
    queryParam k text = [(k, Enc.encodeUtf8 text)]

instance QueryParam Integer where
    queryParam k n = [(k, BS.pack $ show n)]
