{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.Response
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Functions contained in this module fully consume the body and thus close
-- the connection. This is needed to avoid hitting this issue:
-- <https://github.com/brendanhay/amazonka/issues/490>.
--
-- The only exception is 'receiveBody', which passes a streaming response
-- body to a callback and thus is not allowed to close the connection. Users
-- of streaming functions are advised to be careful and consume the response
-- body manually if they want the connection to be closed promptly.
--
-- Note that using 'runResourceT' will always close the connection.
--
module Network.AWS.Response
    ( receiveNull
    , receiveEmpty
    , receiveXMLWrapper
    , receiveXML
    , receiveJSON
    , receiveBytes
    , receiveBody
    ) where

import Control.Applicative          (pure)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.Aeson
import Data.Conduit
import Data.Monoid
import Data.Proxy
import Data.Text    (Text)

import Network.AWS.Data.Body
import Network.AWS.Data.ByteString
import Network.AWS.Data.Log
import Network.AWS.Data.XML
import Network.AWS.Types
import Network.HTTP.Conduit        hiding (Proxy, Request, Response)
import Network.HTTP.Client.Conduit (responseClose)
import Network.HTTP.Types

import Text.XML (Node)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit.Binary  as Conduit

receiveNull :: (MonadResource m, MonadThrow m)
            => Rs a
            -> Logger
            -> Service
            -> Proxy a
            -> ClientResponse
            -> m (Response a)
receiveNull rs _ = stream $ \r _ _ _ ->
    responseClose r *> pure (Right rs)

receiveEmpty :: (MonadResource m, MonadThrow m)
             => (Int -> ResponseHeaders -> () -> Either String (Rs a))
             -> Logger
             -> Service
             -> Proxy a
             -> ClientResponse
             -> m (Response a)
receiveEmpty f _ = stream $ \r s h _ ->
    responseClose r *> pure (f s h ())

receiveXMLWrapper :: (MonadResource m, MonadThrow m)
                  => Text
                  -> (Int -> ResponseHeaders -> [Node] -> Either String (Rs a))
                  -> Logger
                  -> Service
                  -> Proxy a
                  -> ClientResponse
                  -> m (Response a)
receiveXMLWrapper n f = receiveXML (\s h x -> x .@ n >>= f s h)

receiveXML :: (MonadResource m, MonadThrow m)
           => (Int -> ResponseHeaders -> [Node] -> Either String (Rs a))
           -> Logger
           -> Service
           -> Proxy a
           -> ClientResponse
           -> m (Response a)
receiveXML = deserialise decodeXML

receiveJSON :: (MonadResource m, MonadThrow m)
            => (Int -> ResponseHeaders -> Object -> Either String (Rs a))
            -> Logger
            -> Service
            -> Proxy a
            -> ClientResponse
            -> m (Response a)
receiveJSON = deserialise eitherDecode'

receiveBytes :: (MonadResource m, MonadThrow m)
             => (Int -> ResponseHeaders -> ByteString -> Either String (Rs a))
             -> Logger
             -> Service
             -> Proxy a
             -> ClientResponse
             -> m (Response a)
receiveBytes = deserialise (Right . LBS.toStrict)

receiveBody :: (MonadResource m, MonadThrow m)
            => (Int -> ResponseHeaders -> RsBody -> Either String (Rs a))
            -> Logger
            -> Service
            -> Proxy a
            -> ClientResponse
            -> m (Response a)
receiveBody f _ = stream $ \_ s h x -> pure (f s h (RsBody x))

-- | Deserialise an entire response body, such as an XML or JSON payload.
deserialise :: (MonadResource m, MonadThrow m)
            => (LazyByteString -> Either String b)
            -> (Int -> ResponseHeaders -> b -> Either String (Rs a))
            -> Logger
            -> Service
            -> Proxy a
            -> ClientResponse
            -> m (Response a)
deserialise g f l Service{..} _ rs = do
    let s = responseStatus  rs
        h = responseHeaders rs
        x = responseBody    rs
    b <- sinkLBS x
    if not (_svcCheck s)
        then throwM (_svcError s h b)
        else do
            liftIO . l Debug . build $ "[Raw Response Body] {\n" <> b <> "\n}"
            case g b >>= f (fromEnum s) h of
                Right r -> pure (s, r)
                Left  e -> throwM . SerializeError $
                    SerializeError' _svcAbbrev s (Just b) e

-- | Stream a raw response body, such as an S3 object payload.
stream :: (MonadResource m, MonadThrow m)
       => (ClientResponse ->
           Int ->
           ResponseHeaders ->
           ResponseBody ->
           m (Either String (Rs a)))
       -> Service
       -> Proxy a
       -> ClientResponse
       -> m (Response a)
stream f Service{..} _ rs = do
    let s = responseStatus  rs
        h = responseHeaders rs
        x = responseBody    rs
    if not (_svcCheck s)
        then sinkLBS x >>= throwM . _svcError s h
        else do
            e <- f rs (fromEnum s) h x
            either (throwM . SerializeError . SerializeError' _svcAbbrev s Nothing)
                   (pure . (s,))
                   e

sinkLBS :: MonadResource m => ResponseBody -> m LazyByteString
sinkLBS bdy = liftResourceT (bdy `connect` Conduit.sinkLbs)
