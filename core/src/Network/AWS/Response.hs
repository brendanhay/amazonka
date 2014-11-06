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
     -- * Status predicate
      httpStatus

    -- * Errors
    , xmlError

    -- * Responses
    , xmlResponse
    , jsonResponse
    , nullaryResponse
    , bodyResponse
    ) where

import           Control.Applicative
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Default.Class
import           Network.AWS.Data
import           Network.AWS.Types
import           Network.HTTP.Client
import           Network.HTTP.Types
import qualified Text.XML                     as XML
import           Text.XML.Cursor

-- index :: ToText c => Getting [b] a [b] -> Getting c b c -> a -> Maybe Text
-- index g f = fmap (toText . view f) . lastMay . view g

-- choice :: Alternative f => (a -> f b) -> (a -> f b) -> a -> f b
-- choice f g x = f x <|> g x

-- lastMay :: [a] -> Maybe a
-- lastMay []     = Nothing
-- lastMay (x:xs) = Just (go x xs)
--   where
--     go y []     = y
--     go _ (y:ys) = go y ys

httpStatus :: Status -> Bool
httpStatus = const False

xmlError :: (AWSServiceError e, FromXML a)
         => (Status -> Bool)
         -> (Status -> a -> e)
         -> Status
         -> Maybe (LBS.ByteString -> e)
xmlError p f s
    | p s       = Nothing
    | otherwise = Just (either serializerError (f s) . decodeXML)

xmlResponse :: (MonadResource m, AWSServiceError e)
            => (ResponseHeaders -> Cursor -> Either String a)
            -> Either HttpException ClientResponse
            -> m (Either e a)
xmlResponse f = receive $ \hs bdy -> do
    lbs <- sinkLbs bdy
    case XML.parseLBS def lbs of
        Left  ex  -> deserializeFailure (show ex)
        Right doc ->
            case f hs (fromDocument doc) of
                Left  s -> deserializeFailure s
                Right x -> return (Right x)

jsonResponse :: (MonadResource m, AWSServiceError e, FromJSON a)
             => (ResponseHeaders -> Object -> Either String a)
             -> Either HttpException ClientResponse
             -> m (Either e a)
jsonResponse f = receive $ \hs bdy -> do
    lbs <- sinkLbs bdy
    case eitherDecode' lbs of
        Left  s -> deserializeFailure s
        Right o ->
            case f hs o of
                Left  s -> deserializeFailure s
                Right x -> return (Right x)

bodyResponse :: (MonadResource m, AWSServiceError e)
             => (ResponseHeaders -> ResponseBody -> Either String a)
             -> Either HttpException ClientResponse
             -> m (Either e a)
bodyResponse f = receive $ \hs bdy ->
    return (serializerError `first` f hs bdy)

nullaryResponse :: (MonadResource m, AWSServiceError e)
                => a
                -> Either HttpException ClientResponse
                -> m (Either e a)
nullaryResponse x = receive $ \_ bdy ->
    liftResourceT (bdy $$+- return (Right x))

receive :: (MonadResource m, AWSServiceError e)
        => (ResponseHeaders -> ResponseBody -> m (Either e a))
        -> Either HttpException ClientResponse
        -> m (Either e a)
receive f = either httpFailure success
  where
    success rs =
        maybe (f hs bdy)
              (\g -> Left . g <$> sinkLbs bdy)
              (serviceError s)
      where
        s   = responseStatus  rs
        bdy = responseBody    rs
        hs  = responseHeaders rs

sinkLbs :: MonadResource m => ResponseBody -> m LBS.ByteString
sinkLbs bdy = liftResourceT (bdy $$+- Conduit.sinkLbs)

deserializeFailure :: (Monad m, AWSServiceError e) => String -> m (Either e a)
deserializeFailure = return . Left . serializerError

httpFailure :: (Monad m, AWSServiceError e) => HttpException -> m (Either e a)
httpFailure = return . Left . httpError
