{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

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
module Network.AWS
    (
    -- * Monad
    -- ** AWS
      AWS
    , runAWS
    -- ** AWST
    , AWST
    , runAWST

    -- * Requests
    -- ** Synchronous
    , send
    , send_
    , sendWith
    -- ** Paginated
    , paginate
    , paginateWith
    -- ** Eventual consistency
    , await
    , awaitWith
    -- ** Pre-signing
    , presign
    , presignWith

    -- * Errors
    , AWSError  (..)
    , catching
    , throwing

    , Error
    , ServiceError
    , errorService
    , errorStatus
    , errorHeaders
    , errorCode
    , errorMessage
    , errorRequestId

    -- * Regionalisation
    , Region      (..)
    , within

    -- * Retries
    , once

    -- * Environment
    , AWSEnv (..)
    , Env
    -- ** Creating the environment
    , newEnv

    -- ** Specifying credentials
    , Credentials (..)
    , accessKey
    , secretKey
    , fromKeys
    , fromSession
    , getAuth

    -- * Streaming body helpers
    , module Network.AWS.Internal.Body

    -- * Types
    , module Network.AWS.Types
    , module Network.AWS.Logger
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch          (MonadCatch (..), catch)
import           Control.Monad.Error.Lens     (catching, throwing)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Control.Retry                (limitRetries)
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
import           Network.AWS.Types
import           Network.AWS.Waiter
import           Network.HTTP.Conduit         hiding (Request, Response)

-- FIXME: Add explanation about the use of constraints and
--   how to build a monad transformer stack, embed it, etc.
-- FIXME: Add notes about specialising the constraints.
-- FIXME: Add note about *With variants.
-- FIXME: Add note about using Control.Monad.Error.Lens.catching* + error prisms.

-- | A convenient alias that specialises the common constraints in this module.
type AWST m = ExceptT Error (ReaderT Env m)

runAWST :: MonadResource m => Env -> AWST m a -> m (Either Error a)
runAWST e m = runReaderT (runExceptT m) e

type AWS = AWST (ResourceT IO)

-- | Run an 'AWS' monadic action, calling all of the registered 'ResourceT'
-- release actions.
runAWS :: Env -> AWS a -> IO (Either Error a)
runAWS e = runResourceT . runAWST e

-- | This creates a new environment without debug logging and uses 'getAuth'
-- to expand/discover the supplied 'Credentials'.
--
-- Lenses such as 'envLogger' can be used to modify the 'Env' with a debug logger.
newEnv :: MonadIO m
       => Region
       -> Credentials
       -> Manager
       -> m (Either String Env)
newEnv r c m = runExceptT $ initial `liftM` ExceptT (getAuth m c)
  where
    initial = Env r logger check Nothing m

    logger _ _ = return ()
    check  _ _ = return True

-- | Scope an action within the specific 'Region'.
within :: (MonadReader r m, AWSEnv r) => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: (MonadReader r m, AWSEnv r) => m a -> m a
once = local $ \e ->
    e & envRetryPolicy ?~ limitRetries 0
      & envRetryCheck  .~ (\_ _ -> return False)


-- $ async
--
-- < lifted-async>

-- | A variant of 'send' which discards any successful response.
--
-- /See:/ 'send'
send_ :: ( MonadCatch      m
         , MonadResource   m
         , MonadReader   r m
         , MonadError    e m
         , AWSEnv        r
         , AWSError      e
         , AWSRequest    a
         )
      => a
      -> m ()
send_ = void . send

-- | Send a data type which is an instance of 'AWSRequest', returning either the
-- associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- This includes 'HTTPExceptions', serialisation errors, and any service
-- errors returned as part of the 'Response'.
--
-- /Note:/ Requests will be retried depending upon each service's respective
-- strategy. This can be overriden using 'envRetry'. Requests which contain
-- streaming request bodies (such as S3's 'PutObject') are never considered for retries.
--
-- /See:/ 'sendWith'
send :: ( MonadCatch      m
        , MonadResource   m
        , MonadReader   r m
        , MonadError    e m
        , AWSEnv        r
        , AWSError      e
        , AWSRequest    a
        )
     => a
     -> m (Rs a)
send x = sendWith (service x) x

sendWith :: ( MonadCatch      m
            , MonadResource   m
            , MonadReader   r m
            , MonadError    e m
            , AWSEnv        r
            , AWSError      e
            , AWSSigner     v
            , AWSRequest    a
            )
         => Service v s
         -> a
         -> m (Rs a)
sendWith svc (request -> rq) =
    environ env $ \e ->
        retrier e svc rq (request' e svc rq)
            >>= liftM snd . response' e rq

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
paginate :: ( MonadCatch      m
            , MonadResource   m
            , MonadReader   r m
            , MonadError    e m
            , AWSEnv        r
            , AWSError      e
            , AWSPager      a
            )
         => a
         -> Source m (Rs a)
paginate x = paginateWith (service x) x

paginateWith :: ( MonadCatch      m
                , MonadResource   m
                , MonadReader   r m
                , MonadError    e m
                , AWSEnv        r
                , AWSError      e
                , AWSSigner     v
                , AWSPager      a
                )
             => Service v s
             -> a
             -> Source m (Rs a)
paginateWith svc = go
  where
    go x = do
        y <- lift (sendWith svc x)
        yield y
        case page x y of
            Nothing -> return ()
            Just !z -> go z

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
await :: ( MonadCatch      m
         , MonadResource   m
         , MonadReader   r m
         , MonadError    e m
         , AWSEnv        r
         , AWSError      e
         , AWSRequest    a
         )
      => Wait a
      -> a
      -> m (Rs a)
await w x = awaitWith w (service x) x

awaitWith :: ( MonadCatch      m
             , MonadResource   m
             , MonadReader   r m
             , MonadError    e m
             , AWSEnv        r
             , AWSError      e
             , AWSSigner     v
             , AWSRequest    a
             )
          => Wait a
          -> Service v s
          -> a
          -> m (Rs a)
awaitWith w svc (request -> rq) =
    environ env $ \e ->
        waiter e w rq (request' e svc rq)
            >>= liftM snd . response' e rq

-- | Presign an HTTP request that expires after the specified amount of time
-- in the future.
--
-- /Note:/ You can used "Network.AWS.Request.requestURL" to extract a fully
-- signed URL from the request.
--
-- /Note:/ Requires the 'Service' signer to be an instance of 'AWSPresigner'.
-- Not all signing algorithms support this.
--
-- /See:/ 'presignWith'
presign :: ( MonadIO        m
           , MonadReader  r m
           , AWSEnv       r
           , AWSPresigner (Sg (Sv a))
           , AWSRequest   a
           )
        => UTCTime     -- ^ Signing time.
        -> Integer     -- ^ Expiry time in seconds.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign t ex x = presignWith t ex (service x) x

presignWith :: ( MonadIO        m
               , MonadReader  r m
               , AWSEnv       r
               , AWSPresigner v
               , AWSRequest   a
               )
            => UTCTime     -- ^ Signing time.
            -> Integer     -- ^ Expiry time in seconds.
            -> Service v s -- ^ Service configuration.
            -> a           -- ^ Request to presign.
            -> m ClientRequest
presignWith t ex svc x =
    environ env $ \Env{..} ->
        withAuth _envAuth $ \a ->
            return . view sgRequest $
                presigned a _envRegion t ex svc (request x)

environ :: (MonadReader r m, AWSEnv r) => Getter r a -> (a -> m b) -> m b
environ l f = view l >>= f

request' :: ( MonadCatch      m
            , MonadResource   m
            , MonadError    e m
            , AWSError      e
            , AWSSigner     v
            , AWSRequest    a
            )
         => Env
         -> Service v s
         -> Request a
         -> m (Response a)
request' Env{..} svc rq = catch go err >>= response _envLogger svc rq
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

response' :: ( MonadResource   m
             , MonadError    e m
             , AWSError      e
             )
          => Env
          -> Request a
          -> Response a
          -> m (Status, Rs a)
response' Env{..} _ = \case
    Right x -> return x
    Left  e -> do
        logError _envLogger e -- error:ServiceError
        throwing _Error     e
