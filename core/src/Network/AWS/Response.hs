{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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

module Network.AWS.Response where

import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Monoid
import           Data.Text                    (Text)
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.XML
import           Network.AWS.Error
import           Network.AWS.Logger
import           Network.AWS.Types
import           Network.HTTP.Client          hiding (Request, Response)
import           Network.HTTP.Types
import           Text.XML                     (Node)

receiveNull :: MonadResource m
            => Rs a
            -> Logger
            -> Service v s
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response a)
receiveNull rs _ = receive $ \_ _ x ->
    liftResourceT (x $$+- return (Right rs))

receiveXMLWrapper :: MonadResource m
                  => Text
                  -> (Status -> ResponseHeaders -> [Node] -> Either String (Rs a))
                  -> Logger
                  -> Service v s
                  -> Request a
                  -> Either HttpException ClientResponse
                  -> m (Response a)
receiveXMLWrapper n f = receiveXML (\s h x -> x .@ n >>= f s h)

receiveXML :: MonadResource m
           => (Status -> ResponseHeaders -> [Node] -> Either String (Rs a))
           -> Logger
           -> Service v s
           -> Request a
           -> Either HttpException ClientResponse
           -> m (Response a)
receiveXML = deserialise decodeXML

receiveJSON :: MonadResource m
            => (Status -> ResponseHeaders -> Object -> Either String (Rs a))
            -> Logger
            -> Service v s
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response a)
receiveJSON = deserialise eitherDecode'

receiveBody :: MonadResource m
            => (Status -> ResponseHeaders -> RsBody -> Either String (Rs a))
            -> Logger
            -> Service v s
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response a)
receiveBody f _ = receive $ \s h x -> return (f s h (RsBody x))

deserialise :: MonadResource m
            => (LazyByteString -> Either String b)
            -> (Status -> ResponseHeaders -> b -> Either String (Rs a))
            -> Logger
            -> Service v s
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response a)
deserialise g f l = receive $ \s h x -> do
    lbs <- sinkLBS x
    logDebug l $ "[Raw Response Body] {\n" <> lbs <> "\n}"
    return $! g lbs >>= f s h

receive :: MonadResource m
        => (Status -> ResponseHeaders -> ResponseBody -> m (Either String (Rs a)))
        -> Service v s
        -> Request a
        -> Either HttpException ClientResponse
        -> m (Response a)
receive f Service{..} _ = either (return . Left . HTTPError) go
  where
    go rs = do
        let s = responseStatus  rs
            h = responseHeaders rs
            x = responseBody    rs
        if _svcStatus s
            then Left . _svcError _svcAbbrev s h <$> sinkLBS x
            else do
                y <- f s h x
                return $! case y of
                    Left  e -> serialiserError s e
                    Right z -> Right (s, z)

    serialiserError s e = Left (SerializerError _svcAbbrev s e)

sinkLBS :: MonadResource m => ResponseBody -> m LazyByteString
sinkLBS bdy = liftResourceT (bdy $$+- Conduit.sinkLbs)
