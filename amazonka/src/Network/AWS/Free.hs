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
{-# LANGUAGE ViewPatterns               #-}

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
import           Control.Monad.Base
import           Control.Monad.Catch          (MonadCatch (..))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Conduit                 hiding (await)
import           Network.AWS.Env
import           Network.AWS.Error
import           Network.AWS.Internal.HTTP
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Waiter

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

runProgramT :: (MonadCatch m, MonadResource m, MonadReader r m, AWSEnv r)
            => ProgramT m a
            -> m a
runProgramT = iterT go
  where
    go (Send s (request -> x) k) = do
        e <- view env
        retrier e s x (perform e s x) >>= k . second snd

    go (Await s w (request -> x) k) = do
        e <- view env
        waiter e w x (perform e s x) >>= k . second snd

pureProgramT :: Monad m
             => (forall a. a -> Either Error (Rs a))
             -> ProgramT m b
             -> m b
pureProgramT f = iterT go
  where
    go (Send  _   x k) = k (f x)
    go (Await _ _ x k) = k (f x)

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
send x = sendWith (serviceOf x) x

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
paginate x = paginateWith (serviceOf x) x

paginateWith :: (MonadFree Command m, AWSSigner (Sg s), AWSPager a)
             => Service s
             -> a
             -> Source m (Either Error (Rs a))
paginateWith s x = do
    !y <- lift (sendWith s x)
    yield y
    case y of
        Left  _ -> pure ()
        Right z ->
            case page x z of
                Nothing -> pure ()
                Just !r -> paginateWith s r

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
await w x = awaitWith (serviceOf x) w x

awaitWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> Wait a
          -> a
          -> m (Either Error (Rs a))
awaitWith s w x = liftF $ Await s w x id
