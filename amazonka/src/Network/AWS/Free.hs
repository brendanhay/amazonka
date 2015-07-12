{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.AWS.Free
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The core module for making requests to the various AWS services.
module Network.AWS.Free where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch             (MonadCatch (..), catch)
import           Control.Monad.Error.Lens        (catching)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Conduit                    hiding (await)
import           Data.Time                       (getCurrentTime)
import           Network.AWS.Auth
import           Network.AWS.Data.Time
import           Network.AWS.Env
import           Network.AWS.Error
import           Network.AWS.Internal.Body
import           Network.AWS.Internal.Retry
import           Network.AWS.Logger
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request             (requestURL)
import           Network.AWS.Sign.V4
import           Network.AWS.Types
import           Network.AWS.Waiter
import           Network.HTTP.Conduit            hiding (Proxy, Request,
                                                  Response)

import           Control.Concurrent.Async.Lifted
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free        (FreeF (..), FreeT (..))
import           Control.Monad.Trans.Free.Church

data Command r where
    Send  :: (AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> a
          -> (Either Error (Rs a) -> r)
          -> Command r

    Await :: (AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> Wait a
          -> a
          -> (Either Error (Rs a) -> r)
          -> Command r

instance Functor Command where
    fmap f (Send  s   x a) = Send  s   x (fmap f a)
    fmap f (Await s w x a) = Await s w x (fmap f a)

type ProgramT = FreeT Command

instance MonadBase b m => MonadBase b (ProgramT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (ProgramT m) where
    type StM (ProgramT m) a = StM m (FreeF Command a (FreeT Command m a))

    liftBaseWith f = FreeT . liftM Pure $
        liftBaseWith $ \runInBase ->
            f $ \k ->
                runInBase (runFreeT k)

    restoreM = FreeT . restoreM

instance MonadThrow m => MonadThrow (ProgramT m) where
    throwM = lift . throwM

instance MonadResource m => MonadResource (ProgramT m) where
    liftResourceT = lift . liftResourceT

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
send :: (MonadFree Command m, AWSRequest a)
     => a
     -> m (Either Error (Rs a))
send x = sendWithF (serviceOf x) x

sendWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
         => Service s
         -> a
         -> m (Either Error (Rs a))
sendWith s x = liftF $ Send s x id

-- | Repeatedly send an instance of 'AWSPager' and paginate over the associated
-- 'Rs' response type in the success case, while results are available.
-- Otherwise return the related 'ServiceError' upon encountering an error.
--
-- /Note:/ The response's 'ResumableSource' will close when there are no more
-- results or the 'ResourceT' computation is unwrapped.
--
-- /See:/ 'runResourceT' for more information.
--
-- /See:/ 'paginateWith'
paginate :: (MonadFree Command m, AWSPager a)
         => a
         -> Source m (Either Error (Rs a))
paginate x = paginateWithF (serviceOf x) x

paginateWith :: (MonadFree Command m, AWSSigner (Sg s), AWSPager a)
             => Service s
             -> a
             -> Source m (Either Error (Rs a))
paginateWith s x = do
    !y <- lift (sendWithF s x)
    yield y
    case y of
        Left  _ -> pure ()
        Right z ->
            case page x z of
                Nothing -> pure ()
                Just !r -> paginateWithF s r

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
await :: (MonadFree Command m, AWSRequest a)
      => Wait a
      -> a
      -> m (Either Error (Rs a))
await w x = awaitWithF (serviceOf x) w x

awaitWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> Wait a
          -> a
          -> m (Either Error (Rs a))
awaitWith s w x = liftF $ Await s w x id
