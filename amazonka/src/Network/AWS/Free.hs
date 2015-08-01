{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

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
import           Control.Monad.Trans.Free        (FreeT (..))
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
    fmap f = \case
        Send  s   x k -> Send  s   x (fmap f k)
        Await s w x k -> Await s w x (fmap f k)

type ProgramT = FreeT Command

#if MIN_VERSION_free(4,12,0)
#else
instance MonadCatch m => MonadCatch (ProgramT m) where
    catch (FreeT m) f = FreeT $
        liftM (fmap (`catch` f)) m `catch` (runFreeT . f)
#endif

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

pureProgramT :: Monad m
             => (forall s a. Service s ->           a -> Either Error (Rs a))
             -> (forall s a. Service s -> Wait a -> a -> Either Error (Rs a))
             -> ProgramT m b
             -> m b
pureProgramT f g = iterT go . toFT
  where
    go (Send  s   x k) = k (f s   x)
    go (Await s w x k) = k (g s w x)

sendWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
         => Service s
         -> a
         -> m (Either Error (Rs a))
sendWith s x = liftF $ Send s x id

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

awaitWith :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> Wait a
          -> a
          -> m (Either Error (Rs a))
awaitWith s w x = liftF $ Await s w x id

serviceFor :: AWSService (Sv a) => (Service (Sv a) -> a -> b) -> a -> b
serviceFor f x = f (serviceOf x) x
