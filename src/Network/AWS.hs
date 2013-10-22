{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Network.AWS
    (
    -- * AWS Context
      AWS
    , runAWS

    -- * Credentials
    , Credentials (..)

    -- * Regions
    , Region      (..)
    , within
    , getRegion

    -- * Debugging
    , getDebug
    , whenDebug

    -- * Pagination
    , paginate
    , paginateCatch

    -- * Synchronous Requests
    , send
    , send_
    , sendCatch

    -- * Asynchronous Futures
    , async
    , sendAsync
    , wait
    , wait_
    , waitAsync
    , waitAsync_

    -- * Error Lifting
    , AWSError    (..)
    , hoistError
    , liftEitherT
    ) where

import           Control.Applicative
import qualified Control.Concurrent.Async     as A
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8        as BS
import           Network.AWS.Internal.Monadic
import           Network.AWS.Internal.Signing
import           Network.AWS.Internal.Types
import           Network.Http.Client
import           Network.Http.Internal        (retrieveHeaders)
import           Pipes                        hiding (next)
import qualified System.IO.Streams            as Streams

-- | Create a pipes 'Producer' which yields the initial and subsequent repsonses
-- for requests that supported pagination.
paginate :: (Rq a, Pg a, ToError (Er a))
         => a
         -> Producer' (Rs a) AWS ()
paginate = paginateCatch ~> either (lift . liftEitherT . left . toError) yield

paginateCatch :: (Rq a, Pg a, ToError (Er a))
              => a
              -> Producer' (Either (Er a) (Rs a)) AWS ()
paginateCatch = go . Just
  where
    go Nothing   = return ()
    go (Just rq) = do
        rs <- lift $ sendCatch rq
        yield rs
        either (const $ return ()) (go . next rq) rs

-- | Send a request and return the associated response type.
send :: (Rq a, ToError (Er a)) => a -> AWS (Rs a)
send = (hoistError . fmapL toError =<<) . sendCatch

-- | Send a request ignoring the succesful response but propagating errors.
send_ :: (Rq a, ToError (Er a)) => a -> AWS ()
send_ = void . send

sendCatch :: Rq a => a -> AWS (Either (Er a) (Rs a))
sendCatch rq = do
    sig <- sign $ request rq
    whenDebug . liftIO . print $ srqRequest sig
    dbg <- getDebug
    rs  <- liftEitherT . fmapLT Ex . syncIO $ perform sig dbg
    rs' <- raise rs >>= response rq
    hoistError rs'
  where
    perform SignedRequest{..} dbg =
        bracket (establishConnection srqUrl) closeConnection $ \c -> do
            body <- case srqBody of
                Strict bs      -> inputStreamBody <$> Streams.fromByteString bs
                Streaming strm -> return $ inputStreamBody strm
                Empty          -> return emptyBody
            sendRequest c srqRequest body
            receiveResponse c $ \rs i -> do
                strm <- Streams.mapM (\x -> print "x!" >> print x >> return x) i
                return $ RawResponse (getStatusCode rs) (getStatusMessage rs)
                    (retrieveHeaders $ getHeaders rs) strm

    raise rs@RawResponse{..}
        | rsCode < 400 = return rs
        | otherwise    = throwError . Err $
            concat [show rsCode, ":", BS.unpack rsMessage]

async :: AWS a -> AWS (A.Async (Either AWSError a))
async aws = AWS ask >>= liftIO . A.async . runEnv aws

sendAsync :: Rq a => a -> AWS (A.Async (Either AWSError (Either (Er a) (Rs a))))
sendAsync = async . sendCatch

wait :: A.Async (Either AWSError a) -> AWS a
wait a = liftIO (A.waitCatch a) >>= hoistError . join . fmapL toError

wait_ :: A.Async (Either AWSError a) -> AWS ()
wait_ = void . wait

waitAsync :: ToError e => A.Async (Either AWSError (Either e a)) -> AWS a
waitAsync a = wait a >>= hoistError . fmapL toError

waitAsync_ :: ToError e => A.Async (Either AWSError (Either e a)) -> AWS ()
waitAsync_ = void . waitAsync
