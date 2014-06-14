{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}

-- Module      : Control.Monad.Trans.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Monad.Trans.AWS
    (
    -- * Data Types
      AWS
    , AWST

    -- * Running
    , runAWST
    , runAWST'

    -- * Monad Transformation
    , mapAWST

    -- * Lifting Errors
    , liftAWST
    , hoistAWST

    -- * Scoping Regions
    , within

    -- * Requests
    -- ** Synchronous
    , send
    , send_
    , sendCatch

    -- ** Asynchronous
    , sendAsync

    -- ** Pagination
    , paginate
    , paginateCatch

    -- ** Presigned URLs
    , presign

    -- * Asynchronous Actions
    , async
    , wait
    , wait_
    , waitCatch
    ) where

import           Control.Applicative
import           Control.Concurrent.Async.Lifted       (Async)
import qualified Control.Concurrent.Async.Lifted       as Async
import           Control.Error
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal
import           Data.Acquire
import           Data.Conduit
import qualified Data.Conduit.List                     as Conduit
import           Data.Time
import qualified Network.AWS                           as AWS
import           Network.AWS.Auth
import           Network.AWS.Signing.Types             hiding (presign)
import           Network.AWS.Types
import           Network.HTTP.Conduit

-- FIXME: Does switching to ExceptT gain anything? (Besides a hoist from mmorph)

data Env = Env
    { _envAuth     :: Auth
    , _envRegion   :: Region
    , _envMananger :: Manager
    , _envState    :: InternalState
    }

envRegion :: Functor f => LensLike' f Env Region
envRegion f x = (\y -> x { _envRegion = y }) <$> f (_envRegion x)

withEnv :: MonadReader Env m => (Env -> m a) -> m a
withEnv f = ask >>= f

type AWS = AWST IO

newtype AWST m a = AWST { _unAWST :: ReaderT Env (EitherT Error m) a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadIO
        , MonadPlus
        , MonadReader Env
        )

instance MonadTrans AWST where
    lift = AWST . lift . lift

instance MonadBase IO m => MonadBase IO (AWST m) where
    liftBase = liftBaseDefault

instance MonadThrow m => MonadThrow (AWST m) where
    throwM = AWST . lift . lift . throwM

instance MonadCatch m => MonadCatch (AWST m) where
    catch m f = AWST (catch (_unAWST m) (_unAWST . f))

instance (MonadIO m, MonadBase IO m, MonadThrow m) => MonadResource (AWST m) where
    liftResourceT f = AWST $ asks _envState >>= liftIO . runInternalState f

instance MFunctor AWST where
    hoist nat m = mapAWST nat m

runAWST :: (MonadIO m, MonadBaseControl IO m)
        => AWST m b
        -> Credentials
        -> Region
        -> m (Either Error b)
runAWST m c r = do
    e <- liftBase $ runEitherT (getAuth c)
    either (return . Left)
           (\a -> runAWST' m a r conduitManagerSettings)
           e

runAWST' :: MonadBaseControl IO m
         => AWST m a
         -> Auth
         -> Region
         -> ManagerSettings
         -> m (Either Error a)
runAWST' (AWST m) a r s = control $ \run ->
    mask $ \restore -> do
        env <- liftBase $ Env a r <$> newManager s <*> createInternalState
        rs  <- restore (run (runEitherT (runReaderT m env)))
            `onException` stateCleanup ReleaseException (_envState env)
        stateCleanup ReleaseNormal (_envState env)
        return rs

mapAWST :: forall (m :: * -> *) (n :: * -> *) a b
         . (m (Either Error a) -> n (Either Error b))
        -> AWST m a
        -> AWST n b
mapAWST f m = AWST . ReaderT $ \r -> EitherT (unwrap r)
  where
    unwrap = f . runEitherT . runReaderT (_unAWST m)

liftAWST :: (Monad m, AWSError e) => EitherT e m a -> AWST m a
liftAWST = AWST . lift . fmapLT toError

hoistAWST :: (Monad m, AWSError e) => Either e a -> AWST m a
hoistAWST = liftAWST . hoistEither

-- | Scope an 'AWST' action inside a specific 'Region'.
within :: MonadReader Env m => Region -> m a -> m a
within r = local (envRegion .~ r)

send :: ( MonadIO m
        , MonadBase IO m
        , MonadThrow m
        , AWSRequest a
        , AWSSigner (Sg (Sv a))
        )
     => a -- ^ Request to send.
     -> AWST m (Rs a)
send = hoistAWST <=< sendCatch

-- | A variant of 'send' that discards the result.
--
-- > send_ = void . send
send_ :: ( MonadIO m
         , MonadBase IO m
         , MonadThrow m
         , AWSRequest a
         , AWSSigner (Sg (Sv a))
         )
      => a -- ^ Request to send.
      -> AWST m ()
send_ = void . send

sendCatch :: ( MonadIO m
             , MonadBase IO m
             , MonadThrow m
             , AWSRequest a
             , AWSSigner (Sg (Sv a))
             )
          => a
          -> AWST m (Either (Er (Sv a)) (Rs a))
sendCatch rq = withEnv $ \Env{..} ->
    AWS.send _envAuth _envRegion rq _envMananger

sendAsync :: ( MonadIO m
             , MonadBaseControl IO m
             , MonadMask m
             , AWSRequest a
             , AWSSigner (Sg (Sv a))
             )
          => a
          -> AWST m (Async (StM m (Either Error (Either (Er (Sv a)) (Rs a)))))
sendAsync = async . sendCatch

paginate :: ( MonadIO m
            , MonadBase IO m
            , MonadThrow m
            , AWSPager a
            , AWSSigner (Sg (Sv a))
            )
         => a -- ^ Seed request to send.
         -> Source (AWST m) (Rs a)
paginate = ($= Conduit.mapM hoistAWST) . paginateCatch

paginateCatch :: ( MonadIO m
                 , MonadBase IO m
                 , MonadThrow m
                 , AWSPager a
                 , AWSSigner (Sg (Sv a))
                 )
              => a -- ^ Seed request to send.
              -> Source (AWST m) (Either (Er (Sv a)) (Rs a))
paginateCatch rq = withEnv $ \Env{..} ->
    AWS.paginate _envAuth _envRegion rq _envMananger

presign :: ( MonadIO m
           , AWSRequest a
           , AWSPresigner (Sg (Sv a))
           )
        => a
        -> Int     -- ^ Expiry time in seconds.
        -> UTCTime -- ^ Signing time.
        -> AWST m (Signed a (Sg (Sv a)))
presign rq e t = withEnv $ \Env{..} ->
    AWS.presign _envAuth _envRegion rq e t

async :: (MonadBaseControl IO m, MonadMask m)
      => AWST m a
      -> AWST m (Async (StM m (Either Error a)))
async (AWST m) = AWST $ ReaderT $ \r -> EitherT $ mask $ \restore ->
    bracket'
        (stateAlloc (_envState r))
        (return ())
        (return ())
        (fmap Right . Async.async $ bracket'
            (return ())
            (stateCleanup ReleaseNormal (_envState r))
            (stateCleanup ReleaseException (_envState r))
            (restore $ runEitherT (runReaderT m r)))
  where
    bracket' alloc free ex g =
        control $ \run ->
            mask $ \restore ->
                alloc *> (restore (run g) `onException` ex) <* free

wait :: MonadBaseControl IO m
     => Async (StM m (Either Error a))
     -> AWST m a
wait = hoistAWST <=< waitCatch

-- | A variant of 'wait' that discards the result.
--
-- > wait_ = void . wait
wait_ :: MonadBaseControl IO m
      => Async (StM m (Either Error a))
      -> AWST m ()
wait_ = void . wait

waitCatch :: MonadBaseControl IO m
          => Async (StM m (Either Error a))
          -> AWST m (Either Error a)
waitCatch = lift . Async.wait
