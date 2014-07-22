{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

-- Module      : Control.Monad.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Monad.AWS where

import           Control.Applicative
import qualified Control.Concurrent.Async          (Async)
import           Control.Concurrent.Async          as Async
import           Control.Error
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Error         (ErrorT)
import qualified Control.Monad.Trans.Error         as Err
import           Control.Monad.Trans.List          (ListT)
import qualified Control.Monad.Trans.RWS.Lazy      as LRWS (RWST)
import           Control.Monad.Trans.RWS.Strict    (RWST)
import           Control.Monad.Trans.Reader        (ReaderT)
import qualified Control.Monad.Trans.State.Lazy    as LState (StateT)
import           Control.Monad.Trans.State.Strict  (StateT)
import qualified Control.Monad.Trans.Writer.Lazy   as LWriter (WriterT)
import           Control.Monad.Trans.Writer.Strict (WriterT)
import           Data.Monoid
import           Data.Time
import qualified Network.AWS                       as AWS
import           Network.AWS.Auth
import           Network.AWS.Signing.Types         hiding (presign)
import           Network.AWS.Types
import           Network.HTTP.Client

#if MIN_VERSION_transformers(0,4,0)
import           Control.Monad.Trans.Except        (ExceptT)
#endif

data Env = Env
    { _envAuth    :: Auth
    , _envRegion  :: Region
    , _envManager :: Manager
    , _envLogging :: Logging
    }

class ( Functor     m
      , Applicative m
      , Monad       m
      , MonadIO     m
      ) => MonadAWS m where
    liftAWS :: AWS a -> m a

newtype AWS a = AWS { _unAWS :: ReaderT Env (EitherT Error IO) a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        )

run :: Auth -> Region -> Manager -> Logging -> AWS a -> IO (Either Error a)
run a r m l (AWS k) = runEitherT (runReaderT k (Env a r m l))

-- | Pass the current environment to a function.
scoped :: MonadReader Env m => (Env -> m a) -> m a
scoped f = ask >>= f

-- | Regionalise an action inside a specific 'Region'.
within :: MonadReader Env m => Region -> m a -> m a
within r = local (\x -> x { _envRegion = r })

-- | Hoist an 'Either' throwing the 'Left' case, and returning the 'Right'.
hoist :: (MonadAWS m, AWSError e) => Either e a -> m a
hoist = liftAWS . AWS . lift . hoistEither . fmapL awsError

send :: (MonadAWS m, AWSRequest a) => a -> m (Rs a)
send = hoist <=< sendCatch

sendCatch :: (MonadAWS m, AWSRequest a) => a -> m (Either (Er (Sv a)) (Rs a))
sendCatch rq = liftAWS . scoped $ \Env{..} ->
    liftIO (AWS.send _envAuth _envRegion _envManager _envLogging rq)

paginate :: (MonadAWS m, AWSPager a) => a -> m (Rs a, Maybe a)
paginate = hoist <=< paginateCatch

paginateCatch :: (MonadAWS m, AWSPager a)
              => a
              -> m (Either (Er (Sv a)) (Rs a, Maybe a))
paginateCatch rq = liftAWS . scoped $ \Env{..} ->
    liftIO (AWS.paginate _envAuth _envRegion _envManager _envLogging rq)

presign :: (MonadAWS m, AWSRequest a, AWSPresigner (Sg (Sv a)))
        => a
        -> UTCTime
        -> Int
        -> m (Signed a (Sg (Sv a)))
presign rq t x = liftAWS . scoped $ \Env{..} ->
    AWS.presign _envAuth _envRegion (request rq) t x

async :: AWS a -> AWS (Async (Either Error a))
async (AWS r) = ask >>= liftIO . Async.async . runEitherT . runReaderT r

-- wait = undefined

-- waitCatch = undefined


-- MonadAWS Instances

instance MonadAWS AWS where
    liftAWS = id

instance (MonadAWS m, Err.Error e) => MonadAWS (ErrorT e m) where
    liftAWS = lift . liftAWS

#if MIN_VERSION_transformers(0,4,0)
instance (MonadAWS m, Monoid e) => MonadAWS (ExceptT e m) where
    liftAWS = lift . liftAWS
#endif

instance MonadAWS m => MonadAWS (ListT m) where
    liftAWS = lift . liftAWS

instance MonadAWS m => MonadAWS (ReaderT r m) where
    liftAWS = lift . liftAWS

instance MonadAWS m => MonadAWS (StateT s m) where
    liftAWS = lift . liftAWS

instance (MonadAWS m, Monoid w) => MonadAWS (RWST r w s m) where
    liftAWS = lift . liftAWS

instance (MonadAWS m, Monoid w) => MonadAWS (LRWS.RWST r w s m) where
    liftAWS = lift . liftAWS

instance MonadAWS m => MonadAWS (LState.StateT s m) where
    liftAWS = lift . liftAWS

instance (MonadAWS m, Monoid w) => MonadAWS (WriterT w m) where
    liftAWS = lift . liftAWS

instance (MonadAWS m, Monoid w) => MonadAWS (LWriter.WriterT w m) where
    liftAWS = lift . liftAWS
