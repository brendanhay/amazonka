{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

-- Module      : Network.AWS.Response
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Response
    (
    -- * Responses
      receiveNull
    , receiveXML
    , receiveJSON
    , receiveBody
    ) where

import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Bifunctor
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Monoid
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.XML
import           Network.AWS.Types
import           Network.HTTP.Client          hiding (Request, Response)
import           Network.HTTP.Types
import           Text.XML                     (Node)

receiveNull :: (MonadResource m, AWSService (Sv a))
            => Rs a
            -> Logger
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response' a)
receiveNull rs l = receive l $ \_ _ _ bdy ->
    liftResourceT (bdy $$+- return (Right rs))

receiveXML :: (MonadResource m, AWSService (Sv a))
           => (Int -> ResponseHeaders -> [Node] -> Either String (Rs a))
           -> Logger
           -> Request a
           -> Either HttpException ClientResponse
           -> m (Response' a)
receiveXML = deserialise decodeXML

receiveJSON :: (MonadResource m, AWSService (Sv a))
            => (Int -> ResponseHeaders -> Object -> Either String (Rs a))
            -> Logger
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response' a)
receiveJSON = deserialise eitherDecode'

receiveBody :: (MonadResource m, AWSService (Sv a))
            => (Int -> ResponseHeaders -> ResponseBody -> Either String (Rs a))
            -> Logger
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response' a)
receiveBody f l = receive l $ \a s h x ->
    return (SerializerError a `first` f s h x)

deserialise :: (AWSService (Sv a), MonadResource m)
            => (LazyByteString -> Either String b)
            -> (Int -> ResponseHeaders -> b -> Either String (Rs a))
            -> Logger
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response' a)
deserialise g f l = receive l $ \a s h x -> do
    lbs <- sinkLbs l x
    return $! case g lbs of
        Left  e -> Left (SerializerError a e)
        Right o ->
            case f s h o of
                Left  e -> Left (SerializerError a e)
                Right y -> Right y

receive :: forall m a. (MonadResource m, AWSService (Sv a))
        => Logger
        -> (Abbrev -> Int -> ResponseHeaders -> ResponseBody -> m (Response a))
        -> Request a
        -> Either HttpException ClientResponse
        -> m (Response' a)
receive l f = const (either (return . Left . HttpError) success)
  where
    success rs = do
        let s = responseStatus  rs
            h = responseHeaders rs
            x = responseBody    rs
        case _svcHandle svc s of
            Just g  -> Left . g <$> sinkLbs l x
            Nothing -> do
                y <- f (_svcAbbrev svc) (fromEnum s) h x
                case y of
                    Left  e -> return (Left e)
                    Right z -> return (Right (s, z))

    svc = service :: Service (Sv a)

sinkLbs :: MonadResource m => Logger -> ResponseBody -> m LazyByteString
sinkLbs l bdy = do
    lbs <- liftResourceT (bdy $$+- Conduit.sinkLbs)
-- FIXME:    liftIO $ l Trace ("[Client Response Body] {\n" <> build lbs <> "\n}")
    return lbs
