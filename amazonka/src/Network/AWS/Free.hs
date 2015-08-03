{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

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
import           Control.Monad.Reader
import           Control.Monad.Trans.Free        (FreeT (..))
import           Control.Monad.Trans.Free.Church
import           Data.Conduit                    (Source, yield)
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Waiter

data Command r where
    Send  :: (AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> a
          -> (Rs a -> r)
          -> Command r

    Await :: (AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> Wait a
          -> a
          -> (Rs a -> r)
          -> Command r

instance Functor Command where
    fmap f = \case
        Send  s   x k -> Send  s   x (fmap f k)
        Await s w x k -> Await s w x (fmap f k)

type ProgramT = FreeT Command

#if MIN_VERSION_free(4,12,0)
#else
instance MonadThrow m => MonadThrow (ProgramT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (ProgramT m) where
    catch (FreeT m) f = FreeT $
        liftM (fmap (`catch` f)) m `catch` (runFreeT . f)
#endif

-- | Send a request, returning the associated response if successful,
-- otherwise an 'Error'.
--
-- 'Error' will include 'HTTPExceptions', serialisation errors, or any particular
-- errors returned by the AWS service.
--
-- /See:/ 'sendWith'
send :: (MonadFree Command m, AWSRequest a)
     => a
     -> m (Rs a)
send = serviceFor sendWith

-- | A variant of 'send' that allows specifying the 'Service' definition
-- used to configure the request.
sendWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
         => Service s
         -> a
         -> m (Rs a)
sendWith s x = liftF (Send s x id)

-- | Transparently paginate over multiple responses for supported requests
-- while results are available.
--
-- /See:/ 'paginateWith'
paginate :: (MonadFree Command m, AWSPager a)
         => a
         -> Source m (Rs a)
paginate = serviceFor paginateWith

-- | A variant of 'paginate' that allows specifying the 'Service' definition
-- used to configure the request.
paginateWith :: (MonadFree Command m, AWSSigner (Sg s), AWSPager a)
             => Service s
             -> a
             -> Source m (Rs a)
paginateWith s x = do
    !y <- lift (sendWith s x)
    yield y
    case page x y of
        Nothing -> pure ()
        Just !z -> paginateWith s z

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
      -> m (Rs a)
await w = serviceFor (`awaitWith` w)

-- | A variant of 'await' that allows specifying the 'Service' definition
-- used to configure the request.
awaitWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> Wait a
          -> a
          -> m (Rs a)
awaitWith s w x = liftF (Await s w x id)

serviceFor :: AWSService (Sv a) => (Service (Sv a) -> a -> b) -> a -> b
serviceFor f x = f (serviceOf x) x
