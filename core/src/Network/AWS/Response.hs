{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

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
import           Network.AWS.Data             (LazyByteString, FromXML(..), decodeXML, build)
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
nullResponse rs _ = receive $ \_ _ bdy ->
    liftResourceT (bdy $$+- return (Right rs))
{-# INLINE nullResponse #-}

headerResponse :: (MonadResource m, AWSService (Sv a))
               => (ResponseHeaders -> Either String (Rs a))
               -> Logger
               -> Request a
               -> Either HttpException ClientResponse
               -> m (Response' a)
headerResponse f = deserialise (const (Right ())) (const . f)
{-# INLINE headerResponse #-}

xmlResponse :: (MonadResource m, AWSService (Sv a), FromXML (Rs a))
            => Logger
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response' a)
xmlResponse = deserialise (decodeXML >=> parseXML) (const Right)
{-# INLINE xmlResponse #-}

xmlHeaderResponse :: (MonadResource m, AWSService (Sv a))
                  => (ResponseHeaders -> [Node] -> Either String (Rs a))
                  -> Logger
                  -> Request a
                  -> Either HttpException ClientResponse
                  -> m (Response' a)
xmlHeaderResponse = deserialise decodeXML
{-# INLINE xmlHeaderResponse #-}

jsonResponse :: (MonadResource m, AWSService (Sv a), FromJSON (Rs a))
             => Logger
             -> Request a
             -> Either HttpException ClientResponse
             -> m (Response' a)
jsonResponse = deserialise eitherDecode' (const Right)
{-# INLINE jsonResponse #-}

jsonHeaderResponse :: (MonadResource m, AWSService (Sv a))
                   => (ResponseHeaders -> Object -> Either String (Rs a))
                   -> Logger
                   -> Request a
                   -> Either HttpException ClientResponse
                   -> m (Response' a)
jsonHeaderResponse = deserialise eitherDecode'
{-# INLINE jsonHeaderResponse #-}

bodyResponse :: (MonadResource m, AWSService (Sv a))
             => (ResponseHeaders -> ResponseBody -> Either String (Rs a))
             -> Logger
             -> Request a
             -> Either HttpException ClientResponse
             -> m (Response' a)
bodyResponse f _ = receive $ \a hs bdy ->
    return (SerializerError a `first` f hs bdy)
{-# INLINE bodyResponse #-}

deserialise :: (AWSService (Sv a), MonadResource m)
            => (LazyByteString -> Either String b)
            -> (ResponseHeaders -> b -> Either String (Rs a))
            -> Logger
            -> Request a
            -> Either HttpException ClientResponse
            -> m (Response' a)
deserialise g f l = receive $ \a hs bdy -> do
    lbs <- sinkLbs bdy
    liftIO $ l Debug ("[Client Response Body]\n" <> build lbs)
    return $! case g lbs of
        Left  e -> Left (SerializerError a e)
        Right o ->
            case f hs o of
                Left  e -> Left (SerializerError a e)
                Right x -> Right x
{-# INLINE deserialise #-}

receive :: forall m a. (MonadResource m, AWSService (Sv a))
        => (Abbrev -> ResponseHeaders -> ResponseBody -> m (Response a))
        -> Request a
        -> Either HttpException ClientResponse
        -> m (Response' a)
receive f = const (either (return . Left . HttpError) success)
  where
    success rs = do
        let s   = responseStatus  rs
            bdy = responseBody    rs
            hs  = responseHeaders rs
        case _svcHandle svc s of
            Just g  -> Left . g <$> sinkLbs bdy
            Nothing -> do
                x <- f (_svcAbbrev svc) hs bdy
                case x of
                    Left  e -> return (Left e)
                    Right y -> return $! Right (s, y)

    svc = service :: Service (Sv a)
{-# INLINE receive #-}

sinkLbs :: MonadResource m => ResponseBody -> m LBS.ByteString
sinkLbs bdy = liftResourceT (bdy $$+- Conduit.sinkLbs)
{-# INLINE sinkLbs #-}
