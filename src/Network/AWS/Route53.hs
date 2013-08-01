{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53 where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Data
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           GHC.Word
import           Network.AWS.Request
import           Network.AWS.TH
import           Network.Http.Client
import           System.IO.Streams      (InputStream, OutputStream, stdout)
import qualified System.IO.Streams      as Streams
import           Text.Hastache
import           Text.Hastache.Context

class (Data a, Typeable a) => AWSRequest a where
    template :: a -> ByteString
    version  :: a -> Method -> Credentials -> [(ByteString, ByteString)] -> IO Request

data CreateHealthCheck = CreateHealthCheck
    { chcCallerRef :: String
    , chcIpAddress :: String
    , chcPort      :: Word16
    , chcProtocol  :: String
    , chcResource  :: String
    , chcFQDN      :: String
    } deriving (Data, Typeable)

newtype AWS a = AWS { unWrap :: ReaderT Credentials IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Credentials)

runAWS :: Credentials -> AWS a -> IO a
runAWS creds aws = runReaderT (unWrap aws) creds

-- FIXME: Use template haskell for instances
-- Function which determines the template from the class name underscored
-- and takes the uri as an argument
-- use TH to read in the actual template at compile time and set it to
-- the template typeclass function :: ByteString
instance AWSRequest CreateHealthCheck where
    template _ = $(embedTemplate "create_health_check")
    version  _ = version3 route53Base "healthcheck"

route53Base :: ByteString
route53Base = "route53.amazonaws.com"

awsRequest :: AWSRequest a => a -> AWS ()
awsRequest rq = do
    Credentials{..} <- ask
    liftIO $ do
        c <- openConnection (requestUri rq) 80
        r <- version rq

    bodyStream >>= sendRequest c r . inputStreamBody

    receiveResponse c (\p i -> do
        x <- Streams.read i
        BS.putStr $ fromMaybe "" x)

    closeConnection c
  where
    bodyStream = do
       t <- readTemplate $ template rq
       b <- hastacheStr defaultConfig t $ mkGenericContext rq
       Streams.makeInputStream . return . Just $ LBS.toStrict b

    readTemplate = (BS.readFile =<<) . getDataFileName
