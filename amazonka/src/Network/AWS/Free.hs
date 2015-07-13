{-# LANGUAGE BangPatterns               #-}
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
-- Defines the core DSL and interpreters for AWS logic.
module Network.AWS.Free where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch          (MonadCatch (..))
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer.Class
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

newtype ProgramT m a = ProgramT { runProgramT :: FreeT Command m a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadIO
        , MonadTrans
        , MonadCont
        , MonadFree Command
        )

instance MonadThrow m => MonadThrow (ProgramT m) where
    throwM = lift . throwM

instance MonadBase b m => MonadBase b (ProgramT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (ProgramT m) where
    type StM (ProgramT m) a = StM m (FreeF Command a (FreeT Command m a))

    liftBaseWith f = ProgramT . FreeT . liftM Pure $
        liftBaseWith $ \runInBase ->
            f $ \k ->
                runInBase (runFreeT (runProgramT k))

    restoreM = ProgramT . FreeT . restoreM

instance MonadResource m => MonadResource (ProgramT m) where
    liftResourceT = lift . liftResourceT

instance MonadError e m => MonadError e (ProgramT m) where
    throwError     = lift . throwError
    catchError m f = ProgramT (runProgramT m `catchError` (runProgramT . f))

instance MonadState s m => MonadState s (ProgramT m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (ProgramT m) where
    writer = lift . writer
    tell   = lift . tell
    listen = ProgramT . listen . runProgramT
    pass   = ProgramT . pass   . runProgramT

instance MonadReader r m => MonadReader r (ProgramT m) where
    ask     = ProgramT ask
    reader  = ProgramT . reader
    local f = ProgramT . local f . runProgramT

instance MFunctor ProgramT where
    hoist nat = ProgramT . hoistFreeT nat . runProgramT

evalProgramT :: (MonadCatch m, MonadResource m, MonadReader r m, AWSEnv r)
             => ProgramT m a
             -> m a
evalProgramT = iterT go . runProgramT
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
pureProgramT f g = iterT go . runProgramT
  where
    go (Send  s   x k) = k (f s   x)
    go (Await s w x k) = k (g s w x)

sendWithF :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> a
          -> m (Either Error (Rs a))
sendWithF s x = liftF $ Send s x id

paginateWithF :: (MonadFree Command m, AWSSigner (Sg s), AWSPager a)
              => Service s
              -> a
              -> Source m (Either Error (Rs a))
paginateWithF s x = do
    !y <- lift (sendWithF s x)
    yield y
    case y of
        Left  _ -> pure ()
        Right z ->
            case page x z of
                Nothing -> pure ()
                Just !r -> paginateWithF s r

awaitWithF :: (MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
           => Service s
           -> Wait a
           -> a
           -> m (Either Error (Rs a))
awaitWithF s w x = liftF $ Await s w x id
