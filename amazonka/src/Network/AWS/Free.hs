{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

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
        retrier e s x (perform e s x) >>= k . fmap snd

    go (Await s w (request -> x) k) = do
        e <- view env
        waiter e w x (perform e s x) >>= k . fmap snd

pureProgramT :: Monad m
             => (forall s a. Service s ->           a -> Either Error (Rs a))
             -> (forall s a. Service s -> Wait a -> a -> Either Error (Rs a))
             -> ProgramT m b
             -> m b
pureProgramT f g = iterT go
  where
    go (Send  s   x k) = k (f s   x)
    go (Await s w x k) = k (g s w x)

-- | Send a request, returning the associated response if successful,
-- or an 'Error'.
--
-- 'Error' will include 'HTTPExceptions', serialisation errors, or any service
-- specific errors.
--
-- /Note:/ Requests will be retried depending upon each service's respective
-- strategy. This can be overriden using 'envRetry'. Requests which contain
-- streaming request bodies (such as S3's 'PutObject') are never considered
-- for retries.
--
-- /See:/ 'sendWith'
send :: (MonadFree Command m, AWSRequest a)
     => a
     -> m (Either Error (Rs a))
send x = sendWith (serviceOf x) x

-- | A variant of 'send' that allows specifying the 'Service' definition to use
-- to configure the request properties.
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
paginate x = paginateWith (serviceOf x) x

-- | A variant of 'paginate' that allows specifying the 'Service' definition to use
-- to configure the request properties.
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

-- | Poll the API with the specified request until a 'Wait' condition is fulfilled.
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
await w x = awaitWith (serviceOf x) w x

-- | A variant of 'aait' that allows specifying the 'Service' definition to use
-- to configure the request properties.
awaitWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> Wait a
          -> a
          -> m (Either Error (Rs a))
awaitWith s w x = liftF $ Await s w x id
