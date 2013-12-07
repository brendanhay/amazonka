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

    -- * Asynchronous Actions
    , async
    , wait
    , wait_

    -- * Asynchronous Requests
    , sendAsync
    , waitAsync
    , waitAsync_

    -- * Paginated Requests
    , paginate
    , paginateCatch

    -- * File Bodies
    , requestBodyFile

    -- * Errors
    , ToError          (..)
    , AWSError         (..)
    , hoistError
    , liftEitherT

    -- * Types
    , AvailabilityZone (..)
--    , Body             (..)
    , InstanceType     (..)
    , Items            (..)
    , Members          (..)
    ) where

import qualified Control.Concurrent.Async              as A
import           Control.Error
import           Control.Exception
import qualified Control.Exception.Lifted              as L
import           Control.Monad.Error
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal
import           Data.Conduit
import qualified Data.Conduit.Binary                   as Conduit
import           Network.AWS.Internal
import           Network.HTTP.Conduit
import           System.IO

-- | Send a request and return the associated response type.
send :: (Rq a, ToError (Er a)) => a -> AWS (Rs a)
send = (hoistError . fmapL toError =<<) . sendCatch

send_ :: (Rq a, ToError (Er a)) => a -> AWS ()
send_ = void . send

sendCatch :: Rq a => a -> AWS (Either (Er a) (Rs a))
sendCatch rq = do
    s  <- sign $ request rq
    whenDebug . liftIO $ print s
    m  <- getManager
    h  <- http s m
    whenDebug . liftIO $ print h
    rs <- response rq h
    hoistError rs

async :: AWS a -> AWS (A.Async (Either AWSError a))
async aws = AWS ask >>= resourceAsync . lift . runEnv aws

wait :: A.Async (Either AWSError a) -> AWS a
wait a = liftIO (A.waitCatch a) >>= hoistError . join . fmapL toError

wait_ :: A.Async (Either AWSError a) -> AWS ()
wait_ = void . wait

sendAsync :: Rq a => a -> AWS (A.Async (Either AWSError (Either (Er a) (Rs a))))
sendAsync = async . sendCatch

waitAsync :: ToError e => A.Async (Either AWSError (Either e a)) -> AWS a
waitAsync a = wait a >>= hoistError . fmapL toError

waitAsync_ :: ToError e => A.Async (Either AWSError (Either e a)) -> AWS ()
waitAsync_ = void . waitAsync

-- | Create a 'Source' which yields the initial and subsequent repsonses
-- for requests that support pagination.
paginate :: (Rq a, Pg a, ToError (Er a))
         => a
         -> Source AWS (Rs a)
paginate = ($= go) . paginateCatch
  where
    go = do
        x <- await
        maybe (return ())
              (either (lift . liftEitherT . left . toError) yield)
              x

paginateCatch :: (Rq a, Pg a, ToError (Er a))
              => a
              -> Source AWS (Either (Er a) (Rs a))
paginateCatch = go . Just
  where
    go Nothing   = return ()
    go (Just rq) = do
        rs <- lift $ sendCatch rq
        yield rs
        either (const $ return ()) (go . next rq) rs

resourceAsync :: MonadResource m => ResourceT IO a -> m (A.Async a)
resourceAsync (ResourceT f) = liftResourceT . ResourceT $ \g -> L.mask $ \h ->
    bracket_
        (stateAlloc g)
        (return ())
        (A.async $ bracket_
            (return ())
            (stateCleanup g)
            (h $ f g))

requestBodyFile :: MonadIO m => FilePath -> m (Maybe RequestBody)
requestBodyFile f = runMaybeT $ do
    n <- join . hushT $ syncIO getFileSize
    return . requestBodySource n $ Conduit.sourceFile f
  where
    getFileSize = fmap hoistMaybe $
        bracket (openBinaryFile f ReadMode)
                hClose
                (fmap (Just . fromIntegral) . hFileSize)
