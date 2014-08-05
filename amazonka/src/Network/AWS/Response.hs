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
    -- * Responses
      headerResponse
    , cursorResponse
    , xmlResponse
    , bodyResponse

    -- * Pagination
    , keyed
    , choice
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import           Network.AWS.Data
import           Network.AWS.Types
import           Network.HTTP.Client
import           Network.HTTP.Types
import qualified Text.XML             as XML
import           Text.XML.Cursor

keyed :: ToText c => (b -> c) -> (a -> [b]) -> a -> Maybe Text
keyed g f = fmap (toText . g) . listToMaybe . reverse . f

choice :: Alternative f => (a -> f b) -> (a -> f b) -> a -> f b
choice f g x = f x <|> g x

-- FIXME: Keyed and choice into pagination module
-- FIXME: the return (Right Nullary) data ctor pattern doesn't correctly
-- check for status code errors
-- FIXME: Implement json responses

headerResponse :: (Monad m, AWSServiceError e)
               => (ResponseHeaders -> Either String a)
               -> Either HttpException (ClientResponse m)
               -> m (Either e a)
headerResponse f = receive (const . return . first serializerError . f)

cursorResponse :: (Monad m, AWSServiceError e)
               => (ResponseHeaders -> Cursor -> Either String a)
               -> Either HttpException (ClientResponse m)
               -> m (Either e a)
cursorResponse f = receive $ \hs bdy -> do
    lbs <- consume bdy
    case XML.parseLBS def lbs of
        Left  ex  -> return . Left . serializerError $ show ex
        Right doc ->
            case f hs (fromDocument doc) of
                Left  s -> return . Left $ serviceError s
                Right x -> return (Right x)


xmlResponse :: (Monad m, AWSServiceError e, FromXML a)
            => Either HttpException (ClientResponse m)
            -> m (Either e a)
xmlResponse = receive (const (liftM (first serializerError . decodeXML) . consume))

bodyResponse :: (Monad m, AWSServiceError e)
             => (ResponseHeaders -> m ByteString -> m (Either String b))
             -> Either HttpException (Response (m ByteString))
             -> m (Either e b)
bodyResponse f = receive (\hs bdy -> liftM (first serializerError) (f hs bdy))

receive :: (Monad m, AWSServiceError e)
        => (ResponseHeaders -> m ByteString -> m (Either e b))
        -> Either HttpException (Response (m ByteString))
        -> m (Either e b)
receive f = either failure success
  where
    failure = return . Left . clientError

    success rs
        | statusCode st >= 400 = failure (StatusCodeException st hs mempty)
        | otherwise = f hs (responseBody rs)
      where
        st = responseStatus  rs
        hs = responseHeaders rs

consume :: Monad m => m ByteString -> m LBS.ByteString
consume action = LBS.fromChunks `liftM` go id
  where
    go front = do
        x <- action
        if BS.null x
            then return (front [])
            else go (front . (x:))
