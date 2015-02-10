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
      nullResponse
    , headerResponse
    , xmlResponse
    , xmlHeaderResponse
    , jsonResponse
    , jsonHeaderResponse
    , bodyResponse
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Monoid
import           Network.AWS.Data             (FromXML (..), LazyByteString,
                                               build, decodeXML)
import           Network.AWS.Types
import           Network.HTTP.Client          hiding (Request, Response)
import           Network.HTTP.Types
import           Text.XML                     (Node)

nullResponse :: (MonadResource m, AWSService (Sv a))
             => Rs a
             -> Logger
             -> Request a
             -> Either HttpException ClientResponse
             -> m (Response' a)
nullResponse rs l = receive l $ \_ _ _ bdy ->
    liftResourceT (bdy $$+- return (Right rs))

headerResponse :: (MonadResource m, AWSService (Sv a))
               => (ResponseHeaders -> Either String (Rs a))
               -> Logger
               -> Request a
               -> Either HttpException ClientResponse
               -> m (Response' a)
headerResponse f = deserialise (const (Right ())) (\hs _ _ -> f hs)

xmlResponse :: (MonadResource m, AWSService (Sv a), FromXML (Rs a))
            => Logger
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response' a)
xmlResponse = deserialise (decodeXML >=> parseXML) (\_ _ -> Right)

xmlHeaderResponse :: (MonadResource m, AWSService (Sv a))
                  => (ResponseHeaders -> [Node] -> Either String (Rs a))
                  -> Logger
                  -> Request a
                  -> Either HttpException ClientResponse
                  -> m (Response' a)
xmlHeaderResponse f = deserialise decodeXML (\hs _ -> f hs)

jsonResponse :: (MonadResource m, AWSService (Sv a), FromJSON (Rs a))
             => Logger
             -> Request a
             -> Either HttpException ClientResponse
             -> m (Response' a)
jsonResponse = deserialise eitherDecode' (\_ _ -> Right)

jsonHeaderResponse :: (MonadResource m, AWSService (Sv a))
                   => (ResponseHeaders -> Int -> Object -> Either String (Rs a))
                   -> Logger
                   -> Request a
                   -> Either HttpException ClientResponse
                   -> m (Response' a)
jsonHeaderResponse = deserialise eitherDecode'

bodyResponse :: (MonadResource m, AWSService (Sv a))
             => (ResponseHeaders -> Int -> ResponseBody -> Either String (Rs a))
             -> Logger
             -> Request a
             -> Either HttpException ClientResponse
             -> m (Response' a)
bodyResponse f l = receive l $ \a hs s bdy ->
    return (SerializerError a `first` f hs s bdy)

deserialise :: (AWSService (Sv a), MonadResource m)
            => (LazyByteString  -> Either String b)
            -> (ResponseHeaders -> Int -> b -> Either String (Rs a))
            -> Logger
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response' a)
deserialise g f l = receive l $ \a hs s bdy -> do
    lbs <- sinkLbs l bdy
    return $! case g lbs of
        Left  e -> Left (SerializerError a e)
        Right o ->
            case f hs s o of
                Left  e -> Left (SerializerError a e)
                Right x -> Right x

receive :: forall m a. (MonadResource m, AWSService (Sv a))
        => Logger
        -> (Abbrev -> ResponseHeaders -> Int -> ResponseBody -> m (Response a))
        -> Request a
        -> Either HttpException ClientResponse
        -> m (Response' a)
receive l f = const (either (return . Left . HttpError) success)
  where
    success rs = do
        let s   = responseStatus  rs
            bdy = responseBody    rs
            hs  = responseHeaders rs
        case _svcHandle svc s of
            Just g  -> Left . g <$> sinkLbs l bdy
            Nothing -> do
                x <- f (_svcAbbrev svc) hs (fromEnum s) bdy
                case x of
                    Left  e -> return (Left e)
                    Right y -> return (Right (s, y))

    svc = service :: Service (Sv a)

sinkLbs :: MonadResource m => Logger -> ResponseBody -> m LBS.ByteString
sinkLbs l bdy = do
    lbs <- liftResourceT (bdy $$+- Conduit.sinkLbs)
    liftIO $ l Trace ("[Client Response Body] {\n" <> build lbs <> "\n}")
    return lbs
