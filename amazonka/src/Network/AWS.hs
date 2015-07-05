{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The core module for making requests to the various AWS services.
module Network.AWS
    (
    -- $usage

    -- * Requests
    -- ** Synchronous
      send
    , sendWith
    -- ** Paginated
    , paginate
    , paginateWith
    -- ** Eventual consistency
    , await
    , awaitWith
    -- ** Pre-signing
    , presign
    , presignURL
    , presignWith

    , module Network.AWS.Env
    , module Network.AWS.Auth
    , module Network.AWS.Logger
    , module Network.AWS.Internal.Body

    -- * Errors
    , ErrorCode    (..)
    , ErrorMessage (..)
    , RequestId    (..)

    -- ** General
    , AWSError     (..)
    , Error

    -- ** Service specific
    , ServiceError
    , errorService
    , errorStatus
    , errorHeaders
    , errorCode
    , errorMessage
    , errorRequestId

    -- ** Handling
    , catching

    -- * Types
    , module Network.AWS.Types

    -- * Seconds
    , Seconds      (..)
    , _Seconds
    , microseconds

    -- * Serialisation
    , ToText       (..)
    , FromText     (..)
    , ToBuilder    (..)
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch          (MonadCatch (..), catch)
import           Control.Monad.Error.Lens     (catching)
import           Control.Monad.Except
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Conduit                 hiding (await)
import           Data.Time                    (getCurrentTime)
import           Network.AWS.Auth
import           Network.AWS.Data.Time
import           Network.AWS.Env
import           Network.AWS.Error
import           Network.AWS.Internal.Body
import           Network.AWS.Internal.Retry
import           Network.AWS.Logger
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request          (requestURL)
import           Network.AWS.Types
import           Network.AWS.Waiter
import           Network.HTTP.Conduit         hiding (Proxy, Request, Response)

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
presignURL e t ex = liftM requestURL . presign e t ex

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

-- FIXME: info: Scope actions such that any HTTP response timeouts use
-- the supplied number of seconds.
--
-- By default timeouts are taken from the request's 'Service' definition.
-- This value is typically around 70s depending on the service.
--
-- If there is a timeout associated with the 'Env' 'Manager', this will take
-- precedence over the 'Service' default for _all_ requests.
--
--
-- The timeout selection is illustrated as:
--
-- * Use of 'timeout' to scope a specific timeout value.
--
-- * The 'Env' 'Manager' timeout if set.
--
-- * The 'Service' timeout if set.
--
-- * The default 'ClientRequest' timeout (approximately 30s).
-- timeout :: (MonadReader r m, AWSEnv r) => Seconds -> m a -> m a
-- timeout n = local (envManager . responseTimeout ?~ microseconds n)

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
raw (view env -> e@Env{..}) svc x =
    catch go err >>= response _envLogger svc x
  where
    go = do
        t          <- liftIO getCurrentTime
        Signed m s <- withAuth _envAuth $ \a ->
            return (signed a _envRegion t svc x)

        let rq = s { responseTimeout = timeoutFor e svc }

        logTrace _envLogger m  -- trace:Signing:Meta
        logDebug _envLogger rq -- debug:ClientRequest

        rs         <- liftResourceT (http rq _envManager)

        logDebug _envLogger rs -- debug:ClientResponse

        return $! Right rs

    err er = do
        logError _envLogger er  -- error:HttpException
        return $! Left er
