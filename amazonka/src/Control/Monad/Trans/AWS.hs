{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
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
    -- * Monad
      AWS

    -- * Transformer
    , AWST
    , runAWST

    -- * Helpers
    , hoistEither
    , scoped

    -- * Regionalisation
    , within

    -- * Synchronous requests
    -- ** Strict
    , send
    , sendCatch
    -- ** Streaming
    , with
    , withCatch
    -- ** Pagination
    , paginate
    , paginateCatch

    -- * Asynchronous actions
    , async
    , wait

    -- * Signing URLs
    , presign
    ) where

import           Control.Applicative
import           Control.Concurrent.Async.Lifted (Async)
import qualified Control.Concurrent.Async.Lifted as Async
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.ByteString                 (ByteString)
import           Data.Time
import           Network.AWS                     (Env(..), envRegion)
import qualified Network.AWS                     as AWS
import           Network.AWS.Types

type AWS = AWST IO

newtype AWST m a = AWST { unAWST :: ReaderT Env (ExceptT Error m) a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadIO
        , MonadFix
        , MonadPlus
        , MonadThrow
        , MonadCatch
        , MonadReader Env
        , MonadError Error
        )

instance MonadTrans AWST where
    lift = AWST . lift . lift
    {-# INLINE lift #-}

instance MonadBase b m => MonadBase b (AWST m) where
    liftBase = liftBaseDefault
    {-# INLINE liftBase #-}

instance MonadTransControl AWST where
    newtype StT AWST a = StAWSTT
        { unStAWSTT :: StT (ExceptT Error) (StT (ReaderT Env) a)
        }

    liftWith = \f -> AWST $
        liftWith $ \g ->
            liftWith $ \h ->
                f $ liftM StAWSTT . h . g . unAWST
    {-# INLINE liftWith #-}

    restoreT = AWST . restoreT . restoreT . liftM unStAWSTT
    {-# INLINE restoreT #-}

-- NOTE: Requires UndecidableInstances
instance MonadBaseControl b m => MonadBaseControl b (AWST m) where
    newtype StM (AWST m) a = StAWSTM { unStAWSTM :: ComposeSt AWST m a }

    liftBaseWith = defaultLiftBaseWith StAWSTM
    {-# INLINE liftBaseWith #-}

    restoreM = defaultRestoreM unStAWSTM
    {-# INLINE restoreM #-}

instance MFunctor AWST where
    hoist nat m = AWST $ ReaderT (ExceptT . nat . runAWST m)
    {-# INLINE hoist #-}

instance MMonad AWST where
    embed f m = ask >>= f . runAWST m >>= either throwError return
    {-# INLINE embed #-}

runAWST :: AWST m a -> Env -> m (Either Error a)
runAWST (AWST k) = runExceptT . runReaderT k

-- | HoistAWS an 'Either' throwing the 'Left' case, and returning the 'Right'.
hoistEither :: (MonadError Error m, AWSError e) => Either e a -> m a
hoistEither = either (throwError . awsError) return

-- | Pass the current environment to a function.
scoped :: MonadReader Env m => (Env -> m a) -> m a
scoped f = ask >>= f

-- | Scope a monadic action within the specific 'Region'.
within :: MonadReader Env m => Region -> m a -> m a
within r = local (envRegion .~ r)

send :: ( MonadBaseControl IO m
        , MonadReader Env m
        , MonadError Error m
        , AWSRequest a
        )
     => a
     -> m (Rs a)
send = hoistEither <=< sendCatch

sendCatch :: (MonadBaseControl IO m, MonadReader Env m, AWSRequest a)
          => a
          -> m (Either (Er (Sv a)) (Rs a))
sendCatch rq = scoped $ \e -> AWS.send e rq

with :: ( MonadBaseControl IO m
        , MonadReader Env m
        , MonadError Error m
        , AWSRequest a
        )
     => a
     -> (Rs a -> m ByteString -> m b)
     -> m b
with rq = hoistEither <=< withCatch rq

withCatch :: (MonadBaseControl IO m, MonadReader Env m, AWSRequest a)
          => a
          -> (Rs a -> m ByteString -> m b)
          -> m (Either (Er (Sv a)) b)
withCatch rq f = scoped $ \e -> AWS.with e rq f

paginate :: ( MonadBaseControl IO m
            , MonadReader Env m
            , MonadError Error m
            , AWSPager a
            )
         => a
         -> m (Rs a, Maybe a)
paginate = hoistEither <=< paginateCatch

paginateCatch :: (MonadBaseControl IO m, MonadReader Env m, AWSPager a)
              => a
              -> m (Either (Er (Sv a)) (Rs a, Maybe a))
paginateCatch rq = scoped $ \e -> AWS.paginate e rq

async :: (MonadBaseControl IO m, MonadReader Env m)
      => AWST m a
      -> m (Async (StM m (Either Error a)))
async m = ask >>= Async.async . runAWST m

wait :: (MonadBaseControl IO m, MonadError Error m)
     => Async (StM m (Either Error a))
     -> m a
wait = hoistEither <=< Async.wait

presign :: ( MonadBase IO m
           , MonadReader Env m
           , AWSRequest a
           , AWSPresigner (Sg (Sv a))
           )
        => a
        -> UTCTime
        -> Int
        -> m (Signed a (Sg (Sv a)))
presign rq t x = scoped $ \Env{..} ->
    AWS.presign _envAuth _envRegion (request rq) t x
