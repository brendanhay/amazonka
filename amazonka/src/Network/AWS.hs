{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}


{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

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
--     (
--     -- * Requests
--     -- ** Synchronous
--       send
--     , send_
--     -- ** Paginated
--     , paginate
--     -- ** Eventual consistency
--     , await
--     -- ** Pre-signing URLs
--     , presign
--     , presignURL

--     -- * Monad stacks
-- --    , AWS
--     , AWST
--     -- ** Running
-- --    , runAWS
--     , runAWST

--     -- * Regionalisation
--     , Region      (..)
--     , within

--     -- * Retries
--     , once

--     -- * Environment
--     , AWSEnv (..)
--     , Env
--     -- ** Creating the environment
--     , newEnv

--     -- ** Specifying credentials
--     , Credentials (..)
--     , accessKey
--     , secretKey
--     , fromKeys
--     , fromSession
--     , getAuth

--     -- * Streaming body helpers
--     , module Network.AWS.Internal.Body

--     -- * Types
--     , module Network.AWS.Types
--     , module Network.AWS.Logger
--     , module Network.AWS.Error
--     ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch          (MonadCatch (..), catch)
import           Control.Monad.Error.Lens     (catching, throwing)
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Retry                (limitRetries)
import           Data.Bifunctor
import           Data.ByteString              (ByteString)
import           Data.Conduit                 hiding (await)
import           Data.Monoid
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
import qualified Network.HTTP.Conduit         as Client

import           Control.Exception            (Exception)
import           Network.AWS.Request          (defaultRequest)
import           Network.AWS.Sign.V2



data Baz = Baz deriving (Show, Generic)

instance Exception Baz

data Bar = Bar deriving (Show, Generic)

instance Exception Bar

data Foo = Foo deriving (Show, Generic)

-- instance AsError (Error Baz) Baz where
--     _Error = id

instance AWSService Bar where
    type Sg Bar = V2

    service = const undefined

data Qux = Qux deriving (Show, Generic)

instance ToText Qux where toText = const "qux"
instance ToPath Qux where toPath = const "/"
instance ToQuery Qux
instance ToHeaders Qux

instance AWSRequest Qux where
    type Rs Qux = Qux
    type Er Qux = Foo
    type Sv Qux = Bar

    request = defaultRequest
    response _ _ _ _ = undefined

-- FIXME: Add lengthy explanation about the use of constraints and
-- how to built your own monad transformer stack, embed it, etc.

-- newtype Error' = Error { unError :: forall a. Error a }

type AWSError e a = AsError e

type AWST m = ExceptT Error (ReaderT Env m)
type AWS    = AWST (ResourceT IO)

runAWST :: MonadResource m => Env -> AWST m a -> m (Either Error a)
runAWST e m = runReaderT (runExceptT m) e

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

-- -- | A variant of 'send' which discards any successful response.
-- --
-- -- /See:/ 'send'
-- send_ :: ( MonadCatch      m
--          , MonadResource   m
--          , MonadReader   r m
--          , MonadError    e m
--          , AWSEnv        r
--          , AWSError      e a
--          , AWSRequest      a
--          )
--       => a
--       -> m ()
-- send_ = void . send

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
sendWith :: ( MonadCatch      m
            , MonadResource   m
            , MonadReader   r m
            , MonadError    e m
            , AWSEnv        r
            , AWSError      e a
            , AWSSigner     v
            , AWSRequest    a
            )
         => Service v s (Er a)
         -> a
         -> m (Rs a)
sendWith svc (request -> rq) =
    environ env $ \e ->
        retrier e svc rq (request' e svc rq)
            >>= liftM snd . response' e rq

-- | Send an instance of 'AWSPager' and paginate over the associated 'Rs'
-- response type in the success case, or return the related service's 'Er' type
-- in the error case.
--
-- /Note:/ The 'ResumableSource' will close when there are no more results or the
-- 'ResourceT' computation is unwrapped.
--
-- /See:/ 'runResourceT' for more information.
paginateWith :: ( MonadCatch      m
                , MonadResource   m
                , MonadReader   r m
                , MonadError    e m
                , AWSEnv        r
                , AWSError      e a
                , AWSSigner     v
                , AWSPager      a
                )
             => Service v s (Er a)
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

await :: ( MonadCatch      m
         , MonadResource   m
         , MonadReader   r m
         , MonadError    e m
         , AWSEnv        r
         , AWSError      e a
         , AWSRequest    a
         )
      => Wait a
      -> a
      -> m (Rs a)
await w x = awaitWith w (service x) x

-- | Poll the API until a predefined condition is fulfilled using the
-- supplied 'Wait' specification from the respective service.
--
-- The response will be either the first error returned that is not handled
-- by the specification, or the successful response from the await request.
--
-- /Note:/ You can find any available 'Wait' specifications under then
-- @Network.AWS.<ServiceName>.Waiters@ namespace for supported services.
awaitWith :: ( MonadCatch      m
             , MonadResource   m
             , MonadReader   r m
             , MonadError    e m
             , AWSEnv        r
             , AWSError      e a
             , AWSSigner     v
             , AWSRequest    a
             )
          => Wait a
          -> Service v s (Er a)
          -> a
          -> m (Rs a)
awaitWith w svc (request -> rq) =
    environ env $ \e ->
        waiter e w rq (request' e svc rq)
            >>= liftM snd . response' e rq

-- | Presign an HTTP request that expires after the specified amount of time
-- in the future.
--
-- /Note:/ Requires the 'Service' signer to be an instance of 'AWSPresigner'.
-- Not all signing process support this.
presignWith :: ( MonadIO        m
               , MonadReader  r m
               , AWSEnv       r
               , AWSPresigner v
               , AWSRequest   a
               )
            => UTCTime            -- ^ Signing time.
            -> Integer            -- ^ Expiry time in seconds.
            -> Service v s (Er a) -- ^ Service configuration.
            -> a                  -- ^ Request to presign.
            -> m ClientRequest
presignWith t ex svc rq =
    environ env $ \Env{..} ->
        withAuth _envAuth $ \a ->
            return . view sgRequest $ presigned a _envRegion t ex svc (request rq)

-- | Presign a URL that expires after the specified amount of time
-- in the future.
--
-- /See:/ 'presignWith'
presignURLWith :: ( MonadIO        m
                  , MonadReader  r m
                  , AWSEnv       r
                  , AWSPresigner v
                  , AWSRequest   a
                  )
               => UTCTime            -- ^ Signing time.
               -> Integer            -- ^ Expiry time in seconds.
               -> Service v s (Er a) -- ^ Service configuration.
               -> a                  -- ^ Request to presign.
               -> m ByteString
presignURLWith t ex svc = liftM (toBS . uri) . presignWith t ex svc
  where
    uri x = scheme (secure      x)
          <> build (host        x)
          <> port' (port        x)
          <> build (path        x)
          <> build (queryString x)

    scheme True = "https://"
    scheme _    = "http://"

    port' = \case
        80  -> ""
        443 -> ""
        n   -> build ':' <> build n

environ :: (MonadReader r m, AWSEnv r) => Getter r a -> (a -> m b) -> m b
environ l f = view l >>= f

request' :: ( MonadCatch      m
            , MonadResource   m
            , MonadError    e m
            , AWSError      e a
            , AWSSigner     v
            , AWSRequest    a
            )
         => Env
         -> Service v s (Er a)
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
             , AWSError      e a
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
