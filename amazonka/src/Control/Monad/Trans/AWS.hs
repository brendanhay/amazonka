{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE TupleSections       #-}

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

    -- * Requests
    -- ** Synchronous
    , send
    , sendCatch
    -- ** Paginated
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
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Conduit
import           Data.Time
import           Network.AWS                     (Env(..), envRegion)
import qualified Network.AWS                     as AWS
import           Network.AWS.Types

type AWS = AWST IO

newtype AWST m a = AWST
    { unAWST :: ReaderT (Env, InternalState) (ExceptT Error m) a
    } deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadIO
        , MonadFix
        , MonadPlus
        , MonadThrow
        , MonadCatch
        , MonadError Error
        )

instance Monad m => MonadReader Env (AWST m) where
    ask = AWST (fst `liftM` ask)
    {-# INLINE ask #-}

    local f = hoist . local (first f)
    {-# INLINE local #-}

instance MonadTrans AWST where
    lift = AWST . lift . lift
    {-# INLINE lift #-}

instance MonadBase b m => MonadBase b (AWST m) where
    liftBase = liftBaseDefault
    {-# INLINE liftBase #-}

instance MonadTransControl AWST where
    newtype StT AWST a = StTAWS
        { unStTAWS :: StT (ExceptT Error) (StT (ReaderT (Env, InternalState)) a)
        }

    liftWith = \f -> AWST $
        liftWith $ \g ->
            liftWith $ \h ->
                f $ liftM StTAWS . h . g . unAWST
    {-# INLINE liftWith #-}

    restoreT = AWST . restoreT . restoreT . liftM unStTAWS
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (AWST m) where
    newtype StM (AWST m) a = StMAWST { unStMAWST :: ComposeSt AWST m a }

    liftBaseWith = defaultLiftBaseWith StMAWST
    {-# INLINE liftBaseWith #-}

    restoreM = defaultRestoreM unStMAWST
    {-# INLINE restoreM #-}

instance MFunctor AWST where
    hoist nat m = AWST (ReaderT (ExceptT . nat . runAWST' m))
    {-# INLINE hoist #-}

instance MMonad AWST where
    embed f m = liftM2 (,) ask resources
            >>= f . runAWST' m
            >>= either throwError return
    {-# INLINE embed #-}

instance MonadResource AWS where
    liftResourceT f = resources >>= liftIO . runInternalState f
    {-# INLINE liftResourceT #-}

runAWST :: MonadBaseControl IO m => AWST m a -> Env -> m (Either Error a)
runAWST m e = runResourceT (withInternalState (runAWST' m . (e,)))

runAWST' :: AWST m a -> (Env, InternalState) -> m (Either Error a)
runAWST' (AWST k) = runExceptT . runReaderT k

resources :: Monad m => AWST m InternalState
resources = AWST (ReaderT (return . snd))

-- | Hoist an 'Either' throwing the 'Left' case, and returning the 'Right'.
hoistEither :: (MonadError Error m, AWSError e) => Either e a -> m a
hoistEither = either (throwError . awsError) return

-- | Pass the current environment to a function.
scoped :: MonadReader Env m => (Env -> m a) -> m a
scoped f = ask >>= f

-- | Scope a monadic action within the specific 'Region'.
within :: MonadReader Env m => Region -> m a -> m a
within r = local (envRegion .~ r)

send :: ( MonadCatch m
        , MonadResource m
        , MonadReader Env m
        , MonadError Error m
        , AWSRequest a
        )
     => a
     -> m (Rs a)
send = sendCatch >=> hoistEither

sendCatch :: ( MonadCatch m
             , MonadResource m
             , MonadReader Env m
             , AWSRequest a
             )
          => a
          -> m (Either (Er (Sv a)) (Rs a))
sendCatch rq = scoped (\e -> AWS.send e rq)

paginate :: ( MonadCatch m
            , MonadResource m
            , MonadReader Env (ResumableSource m)
            , MonadError Error m
            , AWSPager a
            )
         => a
         -> ResumableSource m (Rs a)
paginate rq = paginateCatch rq $=+ awaitForever (hoistEither >=> yield)

paginateCatch :: ( MonadCatch m
                 , MonadResource m
                 , MonadReader Env (ResumableSource m)
                 , AWSPager a
                 )
              => a
              -> ResumableSource m (Either (Er (Sv a)) (Rs a))
paginateCatch rq = scoped (\e -> AWS.paginate e rq)

async :: (MonadBaseControl IO m)
      => m a
      -> m (Async (StM m a))
async = Async.async

wait :: (MonadBaseControl IO m, MonadError Error m, AWSError e)
     => Async (StM m (Either e a))
     -> m a
wait = Async.wait >=> hoistEither

presign :: ( MonadIO m
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
