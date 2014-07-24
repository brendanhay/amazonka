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
    ( headerResponse
    , cursorResponse
    , xmlResponse
    , bodyResponse
    ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid
import           Network.AWS.Data
import           Network.AWS.Types
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Text.XML.Cursor

headerResponse :: (Monad m, ServiceError e)
               => (ResponseHeaders -> Either String a)
               -> Either HttpException (ClientResponse m)
               -> m (Either e a)
headerResponse f = bodyResponse (const . return . f)

cursorResponse :: (Monad m, ServiceError e)
               => (ResponseHeaders -> Cursor -> Either String a)
               -> Either HttpException (ClientResponse m)
               -> m (Either e a)
cursorResponse f = bodyResponse $ \hs bdy -> do
    lbs <- consume bdy
    return $ f hs (undefined lbs)

xmlResponse :: (Monad m, ServiceError e, FromXML a)
            => Either HttpException (ClientResponse m)
            -> m (Either e a)
xmlResponse = bodyResponse (const (liftM decodeXML . consume))

bodyResponse :: (Monad m, ServiceError e)
             => (ResponseHeaders -> m ByteString -> m (Either String b))
             -> Either HttpException (Response (m ByteString))
             -> m (Either e b)
bodyResponse f = either failure success
  where
    failure = return . Left . clientError

    success rs
        |  statusCode st >= 400 = failure (StatusCodeException st hs mempty)
        | otherwise = first serviceError `liftM` f hs (responseBody rs)
      where
        st = responseStatus rs
        hs = responseHeaders rs

consume :: Monad m => m ByteString -> m LBS.ByteString
consume action = LBS.fromChunks `liftM` go id
  where
    go front = do
        x <- action
        if BS.null x
            then return (front [])
            else go (front . (x:))
