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

module Network.AWS.Response where

import           Control.Applicative
import           Control.Error
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Monoid
import           Network.AWS.Data
import           Network.AWS.Types
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Text.XML.Cursor

headerResponse :: (MonadResource m, ServiceError e)
               => (ResponseHeaders -> Either String a)
               -> Either ClientException (ClientResponse m)
               -> m (Either e a)
headerResponse f = bodyResponse $ \hs bdy ->
    (bdy $$+- return ()) >> return (f hs)

cursorResponse :: (MonadResource m, ServiceError e)
               => (ResponseHeaders -> Cursor -> Either String a)
               -> Either ClientException (ClientResponse m)
               -> m (Either e a)
cursorResponse f = bodyResponse $ \hs bdy -> do
    lbs <- bdy $$+- Conduit.sinkLbs
    return $ f hs (undefined lbs)

xmlResponse :: (MonadResource m, ServiceError e, FromXML a)
            => Either ClientException (ClientResponse m)
            -> m (Either e a)
xmlResponse = bodyResponse $ \_ bdy ->
    decodeXML <$> (bdy $$+- Conduit.sinkLbs)

bodyResponse :: (MonadResource m, ServiceError e)
             => (ResponseHeaders -> a -> m (Either String b))
             -> Either ClientException (Response a)
             -> m (Either e b)
bodyResponse _ (Left  ex) = return . Left $ clientError ex
bodyResponse f (Right rs)
    | statusCode st >= 400 = return . Left $ clientError ex
    | otherwise            = fmapL serviceError <$> f hs bdy
  where
    ex  = StatusCodeException st hs mempty
    st  = responseStatus rs
    hs  = responseHeaders rs
    bdy = responseBody rs
