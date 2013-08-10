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
import           Data.Aeson.XML
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Enc
import           Network.AWS.Internal.String
import           Network.Http.Client         hiding (ContentType, post, put)
import           System.IO.Streams           (InputStream)

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

data ContentType
    = FormEncoded
    | Xml

instance IsText ContentType where
    toText FormEncoded = "application/x-www-form-urlencoded"
    toText Xml         = "application/xml"

newtype AWS a = AWS { unWrap :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Env)

currentRegion :: AWS Region
currentRegion = fromMaybe NorthVirgnia <$> fmap awsRegion ask

data RawRequest a b where
    RawRequest :: (AWSService a, FromXML b)
               => { rqMethod  :: !Method
                 , rqContent :: !ContentType
                 , rqAction  :: !(Maybe ByteString)
                 , rqPath    :: !(Maybe ByteString)
                 , rqHeaders :: !(Map ByteString ByteString)
                 , rqQuery   :: ![(ByteString, ByteString)]
                 , rqBody    :: !(Maybe (InputStream ByteString))
                 }
               -> RawRequest a b

emptyRequest :: (AWSService a, FromXML b, IsText p)
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

class (AWSService a, FromJSON b) => AWSRequest a c b | c -> a b where
    request :: c -> AWS (RawRequest a b)

class (Show a, ToJSON a) => Template a where
    readTemplate :: a -> ByteString

class Show a => QueryString a where
    queryString :: ByteString -> a -> [(ByteString, ByteString)]

instance QueryString a => QueryString (Maybe a) where
    queryString _ Nothing  = []
    queryString k (Just v) = queryString k v

instance QueryString ByteString where
    queryString = packQS

instance QueryString Text where
    queryString = packQS

instance QueryString Integer where
    queryString = packQS

instance QueryString Bool where
    queryString k = packQS k . lowerAll . show

packQS :: IsText a => ByteString -> a -> [(ByteString, ByteString)]
packQS k v = [(strip '.' k, toBS v)]

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

instance IsText String where
    toText = fromString
    toBS   = fromString
    toStr  = id

instance IsText Integer where
    toText = Text.pack . toStr
    toBS   = BS.pack . toStr
    toStr  = show
