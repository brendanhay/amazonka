{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The core module for making requests to the various AWS services and
-- building your own Monad transformer stack.
module Network.AWS where
    -- (
    -- -- * Monad
    -- -- ** AWS
    --   AWS
    -- , runAWS
    -- -- ** AWST
    -- , AWST
    -- , runAWST
    -- -- ** Constraints
    -- , MonadAWS

    -- -- * Requests
    -- -- ** Synchronous
    -- , send
    -- , send_
    -- , sendWith
    -- -- ** Paginated
    -- , paginate
    -- , paginateWith
    -- -- ** Eventual consistency
    -- , await
    -- , awaitWith
    -- -- ** Pre-signing
    -- , presign
    -- , presignWith
    -- -- ** Asynchronous
    -- -- $async

    -- -- * Errors
    -- , AWSError  (..)
    -- , catching
    -- , throwing

    -- , Error
    -- , ServiceError
    -- , errorService
    -- , errorStatus
    -- , errorHeaders
    -- , errorCode
    -- , errorMessage
    -- , errorRequestId

    -- -- * Regionalisation
    -- , Region      (..)

    -- -- * Environment
    -- , AWSEnv (..)
    -- , Env
    -- -- ** Creating the environment
    -- , newEnv

    -- -- ** Specifying credentials
    -- , Credentials (..)
    -- , accessKey
    -- , secretKey
    -- , fromKeys
    -- , fromSession
    -- , getAuth

    -- -- * Streaming body helpers
    -- , module Network.AWS.Internal.Body

    -- -- * Types
    -- , module Network.AWS.Types
    -- , module Network.AWS.Logger
    -- ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch          (MonadCatch (..), catch, throwM)
import           Control.Monad.Error.Lens     (catching, throwing)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Control.Retry                (limitRetries)
import           Data.Bifunctor
import           Data.Conduit                 hiding (await)
import           Data.Time                    (getCurrentTime)
import           Network.AWS.Error
import           Network.AWS.Internal.Auth
import           Network.AWS.Internal.Body
import           Network.AWS.Internal.Env
import           Network.AWS.Internal.Retry
import           Network.AWS.Logger
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request          (requestURL)
import           Network.AWS.Types
import           Network.AWS.Waiter
import           Network.HTTP.Conduit         hiding (Proxy, Request, Response)

-- | This creates a new environment without debug logging and uses 'getAuth'
-- to expand/discover the supplied 'Credentials'.
--
-- Lenses such as 'envLogger' can be used to modify the 'Env' with a debug logger.
getEnv :: MonadIO m
       => Region
       -> Credentials
       -> Manager
       -> m (Either String Env)
getEnv r c m = runExceptT $ initial `liftM` ExceptT (getAuth m c)
  where
    initial = Env r logger check Nothing m

    logger _ _ = return ()
    check  _ _ = return True

-- | Send a data type which is an instance of 'AWSRequest', returning either the
-- associated 'Rs' response type if successful, or an 'Error'.
--
-- This includes 'HTTPExceptions', serialisation errors, and any service
-- errors returned as part of the 'Response'.
--
-- /Note:/ Requests will be retried depending upon each service's respective
-- strategy. This can be overriden using 'envRetry'. Requests which contain
-- streaming request bodies (such as S3's 'PutObject') are never considered for retries.
--
-- /See:/ 'sendWith'
send :: (MonadCatch m, MonadResource m, AWSEnv r, AWSRequest a)
     => r
     -> a
     -> m (Either Error (Rs a))
send e x = sendWith e (serviceOf x) x

sendWith :: ( MonadCatch    m
            , MonadResource m
            , AWSEnv        r
            , AWSSigner     (Sg s)
            , AWSRequest    a
            )
         => r
         -> Service s
         -> a
         -> m (Either Error (Rs a))
sendWith e svc (request -> rq) =
    second snd `liftM`
        retrier (e ^. env) svc rq (raw e svc rq)

-- | Repeatedly send an instance of 'AWSPager' and paginate over the associated
-- 'Rs' response type in the success case, while results are available.
-- Otherwise return the related 'ServiceError' upon encountering an error.
--
-- /Note:/ The 'ResumableSource' will close when there are no more results or the
-- 'ResourceT' computation is unwrapped.
--
-- /See:/ 'runResourceT' for more information.
--
-- /See:/ 'paginateWith'
paginate :: (MonadCatch m, MonadResource m, AWSEnv r, AWSPager a)
         => r
         -> a
         -> Source m (Either Error (Rs a))
paginate e x = paginateWith e (serviceOf x) x

paginateWith :: ( MonadCatch    m
                , MonadResource m
                , AWSEnv        r
                , AWSSigner     (Sg s)
                , AWSPager      a
                )
             => r
             -> Service s
             -> a
             -> Source m (Either Error (Rs a))
paginateWith e svc = go
  where
    go rq = do
        rs <- lift (sendWith e svc rq)
        yield rs
        case rs of
            Left  _ -> return ()
            Right x ->
                case page rq x of
                    Nothing -> return ()
                    Just !y -> go y

-- | Poll the API until a predefined condition is fulfilled using the
-- supplied 'Wait' specification from the respective service.
--
-- The response will be either the first error returned that is not handled
-- by the specification, or the successful response from the await request.
--
-- /Note:/ You can find any available 'Wait' specifications under then
-- @Network.AWS.<ServiceName>.Waiters@ namespace for supported services.
--
-- /See:/ 'awaitWith'
await :: (MonadCatch m, MonadResource m, AWSEnv r, AWSRequest a)
      => r
      -> Wait a
      -> a
      -> m (Either Error (Rs a))
await e w x = awaitWith e (serviceOf x) w x

awaitWith :: ( MonadCatch    m
             , MonadResource m
             , AWSEnv        r
             , AWSSigner     (Sg s)
             , AWSRequest    a
             )
          => r
          -> Service s
          -> Wait a
          -> a
          -> m (Either Error (Rs a))
awaitWith e svc w (request -> rq) =
    second snd `liftM`
        waiter (e ^. env) w rq (raw e svc rq)

-- /See:/ 'presign', 'presignWith'
presignURL :: (MonadIO m, AWSEnv r, AWSPresigner (Sg (Sv a)), AWSRequest a)
           => r
           -> UTCTime     -- ^ Signing time.
           -> Integer     -- ^ Expiry time in seconds.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL e t ex = liftM (view requestURL) . presign e t ex

-- | Presign an HTTP request that expires after the specified amount of time
-- in the future.
--
-- /Note:/ Requires the 'Service' signer to be an instance of 'AWSPresigner'.
-- Not all signing algorithms support this.
--
-- /See:/ 'presignWith'
presign :: (MonadIO m, AWSEnv r, AWSPresigner (Sg (Sv a)), AWSRequest a)
        => r
        -> UTCTime     -- ^ Signing time.
        -> Integer     -- ^ Expiry time in seconds.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign e t ex x = presignWith e (serviceOf x) t ex x

-- /Note:/ You can used "Network.AWS.Request.requestURL" to extract a fully
-- signed URL from the request.
presignWith :: (MonadIO m, AWSEnv r, AWSPresigner (Sg s), AWSRequest a)
            => r
            -> Service s -- ^ Service configuration.
            -> UTCTime   -- ^ Signing time.
            -> Integer   -- ^ Expiry time in seconds.
            -> a         -- ^ Request to presign.
            -> m ClientRequest
presignWith e svc t ex x =
    withAuth (e ^. envAuth) $ \a ->
        return . view sgRequest $
            presigned a (e ^. envRegion) t ex svc (request x)

raw :: ( MonadCatch    m
       , MonadResource m
       , AWSEnv        r
       , AWSSigner     (Sg s)
       , AWSRequest    a
       )
    => r
    -> Service s
    -> Request a
    -> m (Response a)
raw (view env -> Env{..}) svc rq = catch go err >>= response _envLogger svc rq
  where
    go = do
        t          <- liftIO getCurrentTime
        Signed m s <- withAuth _envAuth $ \a ->
            return (signed a _envRegion t svc rq)

        logDebug _envLogger s -- debug:Signed
        logTrace _envLogger m -- trace:Meta

        x          <- liftResourceT (http s _envManager)

        logDebug _envLogger x -- debug:ClientResponse

        return $! Right x

    err e = do
        logError _envLogger e -- error:HttpException
        return $! Left e
