{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , receiveXMLWrapper
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
import           Data.Text                    (Text)
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.XML
import           Network.AWS.Logger
import           Network.AWS.Types
import           Network.HTTP.Client          hiding (Request, Response)
import           Network.HTTP.Types
import           Text.XML                     (Node)

receiveNull :: MonadResource m
            => Rs a
            -> Logger
            -> Service v (Er a)
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response a)
receiveNull rs _ = receive $ \_ _ _ bdy ->
    liftResourceT (bdy $$+- return (Right rs))

receiveXMLWrapper :: MonadResource m
                  => Text
                  -> (Int -> ResponseHeaders -> [Node] -> Either String (Rs a))
                  -> Logger
                  -> Service v (Er a)
                  -> Request a
                  -> Either HttpException ClientResponse
                  -> m (Response a)
receiveXMLWrapper n f = receiveXML (\s h x -> x .@ n >>= f s h)

receiveXML :: MonadResource m
           => (Int -> ResponseHeaders -> [Node] -> Either String (Rs a))
           -> Logger
           -> Service v (Er a)
           -> Request a
           -> Either HttpException ClientResponse
           -> m (Response a)
receiveXML = deserialise decodeXML

receiveJSON :: MonadResource m
            => (Int -> ResponseHeaders -> Object -> Either String (Rs a))
            -> Logger
            -> Service v (Er a)
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response a)
receiveJSON = deserialise eitherDecode'

receiveBody :: MonadResource m
            => (Int -> ResponseHeaders -> RsBody -> Either String (Rs a))
            -> Logger
            -> Service v (Er a)
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response a)
receiveBody f _ = receive $ \a s h x ->
    return (SerializerError a `first` f s h (RsBody x))

deserialise :: MonadResource m
            => (LazyByteString -> Either String b)
            -> (Int -> ResponseHeaders -> b -> Either String (Rs a))
            -> Logger
            -> Service v (Er a)
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response a)
deserialise g f l = receive $ \a s h x -> do
    lbs <- sinkLbs x
    logDebug l $ "[Raw Response Body] {\n" <> lbs <> "\n}"
    return $! case g lbs of
        Left  e -> Left (SerializerError a e)
        Right o ->
            case f s h o of
                Left  e -> Left (SerializerError a e)
                Right y -> Right y

receive :: MonadResource m
        => (Abbrev -> Int
                   -> ResponseHeaders
                   -> ResponseBody
                   -> m (Either (Error (Er a)) (Rs a)))
        -> Service v (Er a)
        -> Request a
        -> Either HttpException ClientResponse
        -> m (Response a)
receive f svc _ = either (return . Left . HttpError) success
  where
    success rs = do
        let s = responseStatus  rs
            h = responseHeaders rs
            x = responseBody    rs
        case _svcHandle svc s of
            Just g  -> Left . g <$> sinkLbs x
            Nothing -> do
                y <- f (_svcAbbrev svc) (fromEnum s) h x
                case y of
                    Left  e -> return $! Left e
                    Right z -> return $! Right (s, z)

sinkLbs :: MonadResource m => ResponseBody -> m LazyByteString
sinkLbs bdy = liftResourceT (bdy $$+- Conduit.sinkLbs)
