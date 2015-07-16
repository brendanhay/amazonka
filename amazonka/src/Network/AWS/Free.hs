{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
-- Defines the core DSL, logic and interpreters for AWS behaviour.
module Network.AWS.Free where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Catch             (MonadCatch (..))
import           Control.Monad.Reader
import           Control.Monad.Trans.Free        (FreeT)
import           Control.Monad.Trans.Free.Church
import           Control.Monad.Trans.Resource
import           Data.Conduit                    (Source, yield)
import           Network.AWS.Env
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

-- | Interpret the 'Command' instruct set by performing HTTP calls to
-- retrieve the associated response type for a request.
--
-- Requests will be retried depending upon each service's respective retry
-- strategy. This can be overriden using 'envRetry'. Requests which contain
-- streaming request bodies (such as S3's 'PutObject') are never retried.
evalProgramT :: ( MonadCatch m
                , MonadResource m
                , MonadReader r m
                , AWSEnv r
                )
             => ProgramT m a
             -> m a
evalProgramT = iterT go . toFT
  where
    go (Send s (request -> x) k) = do
        e <- view env
        retrier e s x (perform e s x) >>= k . fmap snd

    go (Await s w (request -> x) k) = do
        e <- view env
        waiter e w x (perform e s x) >>= k . fmap snd

-- | Interpret the 'Command' instruction set purely.
pureProgramT :: Monad m
             => (forall s a. Service s ->           a -> Either Error (Rs a))
                -- ^ Define how responses for 'send' and 'paginate' can be obtained.
             -> (forall s a. Service s -> Wait a -> a -> Either Error (Rs a))
                -- ^ Define how responses for 'await' can be obtained.
             -> ProgramT m b
             -> m b
pureProgramT f g = iterT go . toFT
  where
    go (Send  s   x k) = k (f s   x)
    go (Await s w x k) = k (g s w x)

-- | Send a request, returning the associated response if successful,
-- otherwise an 'Error'.
--
-- 'Error' will include 'HTTPExceptions', serialisation errors, or any particular
-- errors returned by the AWS service.
--
-- /See:/ 'sendWith'
send :: (MonadFree Command m, AWSRequest a)
     => a
     -> m (Either Error (Rs a))
send = serviceFor sendWith

-- | A variant of 'send' that allows specifying the 'Service' definition
-- used to configure the request.
sendWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
         => Service s
         -> a
         -> m (Either Error (Rs a))
sendWith s x = liftF $ Send s x id

-- | Transparently paginate over multiple responses for supported requests
-- while results are available.
--
-- /See:/ 'paginateWith'
paginate :: (MonadFree Command m, AWSPager a)
         => a
         -> Source m (Either Error (Rs a))
paginate = serviceFor paginateWith

-- | A variant of 'paginate' that allows specifying the 'Service' definition
-- used to configure the request.
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

-- | Poll the API with the supplied request until a specific 'Wait' condition
-- is fulfilled.
--
-- The response will be either the first error returned that is not handled
-- by the specification, or any subsequent successful response from the await
-- request(s).
--
-- /Note:/ You can find any available 'Wait' specifications under then
-- @Network.AWS.<ServiceName>.Waiters@ namespace for supported services.
--
-- /See:/ 'awaitWith'
await :: (MonadFree Command m, AWSRequest a)
      => Wait a
      -> a
      -> m (Either Error (Rs a))
await w = serviceFor (flip awaitWith w)

-- | A variant of 'await' that allows specifying the 'Service' definition
-- used to configure the request.
awaitWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> Wait a
          -> a
          -> m (Either Error (Rs a))
awaitWith s w x = liftF $ Await s w x id
