{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

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
    , Credentials      (..)

    -- * Regions
    , Region           (..)
    , within
    , getRegion

    -- * Debugging
    , getDebug
    , whenDebug

    -- * Synchronous Requests
    , send
    , send_
    , sendCatch

    -- * Asynchronous Requests
    , async
    , sendAsync
    , wait
    , wait_
    , waitAsync
    , waitAsync_

    -- * Paginated Requests
    , paginate
    , paginateCatch

    -- * Errors
    , ToError          (..)
    , AWSError         (..)
    , hoistError
    , liftEitherT

    -- * Types
    , AvailabilityZone (..)
    , Body             (..)
    , InstanceType     (..)
    , Items            (..)
    , Members          (..)
    ) where

import           Control.Applicative
import qualified Control.Concurrent.Async   as A
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Trans.Reader
import           Network.AWS.Internal
import           Network.Http.Client
import           Network.Http.Internal      (retrieveHeaders)
import           Pipes                      hiding (next)
import qualified System.IO.Streams          as Streams

-- | Send a request and return the associated response type.
send :: (Rq a, ToError (Er a)) => a -> AWS (Rs a)
send = (hoistError . fmapL toError =<<) . sendCatch

-- | Send a request ignoring the succesful response but propagating errors.
send_ :: (Rq a, ToError (Er a)) => a -> AWS ()
send_ = void . send

sendCatch :: Rq a => a -> AWS (Either (Er a) (Rs a))
sendCatch rq = do
    sig <- request rq
    dbg <- getDebug
    rs  <- sync $ perform sig dbg
    hoistError rs
  where
    sync = liftEitherT . fmapLT Ex . syncIO

    perform Signed{..} dbg =
        bracket (establishConnection sHost) closeConnection $ \c -> do
            when dbg $ do
                print sRequest
                print sBody
            body sBody >>= sendRequest c sRequest
            receiveResponse c $ receive dbg

    body (Strict   bs) = inputStreamBody <$> Streams.fromByteString bs
    body (Streaming s) = return $ inputStreamBody s
    body Empty         = return emptyBody

    receive dbg rs i = do
        let c = getStatusCode rs
            m = getStatusMessage rs
        when dbg $ print rs
        response rq $ Response dbg c m (retrieveHeaders $ getHeaders rs) i

            -- else do
            --     bs <- BS.concat <$> Streams.toList i
            --     return . Left . Err . BS.unpack $
            --         BS.concat [BS.pack $ show c, " ", m, "\n", bs]

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
