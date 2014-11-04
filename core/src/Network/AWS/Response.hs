{-# LANGUAGE FlexibleContexts #-}

-- Module      : Network.AWS.Response
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Response
    (
    -- * Pagination
      index
    , choice

    -- * Responses
    -- , headerResponse
    -- , cursorResponse
    , xmlResponse
    , jsonResponse
    , nullaryResponse
    , bodyResponse
    ) where

import           Control.Applicative
import           Control.Lens                 (Getting, view)
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Default.Class
import           Data.Monoid
import           Data.Text                    (Text)
import           Network.AWS.Data
import           Network.AWS.Types
import           Network.HTTP.Client
import           Network.HTTP.Types
import qualified Text.XML                     as XML
import           Text.XML.Cursor

index :: ToText c => Getting [b] a [b] -> Getting c b c -> a -> Maybe Text
index g f = fmap (toText . view f) . lastMay . view g

choice :: Alternative f => (a -> f b) -> (a -> f b) -> a -> f b
choice f g x = f x <|> g x

lastMay :: [a] -> Maybe a
lastMay []     = Nothing
lastMay (x:xs) = Just (go x xs)
  where
    go y []     = y
    go _ (y:ys) = go y ys

headerResponse :: (MonadResource m, AWSServiceError e)
               => (ResponseHeaders -> Either String a)
               -> Either HttpException ClientResponse
               -> m (Either e a)
headerResponse f = receive $ \hs bdy -> do
     liftResourceT (closeResumableSource bdy)
     return $! serializerError `first` f hs

-- cursorResponse :: (MonadResource m, AWSServiceError e)
--                => (ResponseHeaders -> Cursor -> Either String a)
--                -> Either HttpException ClientResponse
--                -> m (Either e a)
-- cursorResponse f = receive $ \hs bdy -> do
--     lbs <- sinkLbs bdy
--     case XML.parseLBS def lbs of
--         Left  ex  -> return . Left . serializerError $ show ex
--         Right doc ->
--             case f hs (fromDocument doc) of
--                 Left  s -> return . Left $ serviceError s
--                 Right x -> return (Right x)

xmlResponse :: (MonadResource m, AWSServiceError e, FromXML a)
            => Either HttpException ClientResponse
            -> m (Either e a)
xmlResponse = receive $ \_ bdy -> do
    lbs <- sinkLbs bdy
    return $! serializerError `first` decodeXML lbs

jsonResponse :: (MonadResource m, AWSServiceError e, FromJSON a)
             => Either HttpException ClientResponse
             -> m (Either e a)
jsonResponse = receive $ \_ bdy -> do
    lbs <- sinkLbs bdy
    return $! serializerError `first` eitherDecode lbs

bodyResponse :: (Monad m, AWSServiceError e)
             => (ResponseHeaders -> a -> m (Either String b))
             -> Either HttpException (Response a)
             -> m (Either e b)
bodyResponse f = receive (\hs bdy -> first serializerError `liftM` f hs bdy)

nullaryResponse :: (MonadResource m, AWSServiceError e)
                => a
                -> Either HttpException ClientResponse
                -> m (Either e a)
nullaryResponse x = receive (\_ bdy -> liftResourceT (bdy $$+- return (Right x)))

receive :: (Monad m, AWSServiceError e)
        => (ResponseHeaders -> a -> m (Either e b))
        -> Either HttpException (Response a)
        -> m (Either e b)
receive f = either failure success
  where
    failure = return . Left . httpError

    success rs
        | statusCode st >= 400 = failure (StatusCodeException st hs mempty)
        | otherwise = f hs (responseBody rs)
      where
        st = responseStatus  rs
        hs = responseHeaders rs

sinkLbs :: MonadResource m
        => ResumableSource (ResourceT IO) ByteString
        -> m LBS.ByteString
sinkLbs bdy = liftResourceT (bdy $$+- Conduit.sinkLbs)
