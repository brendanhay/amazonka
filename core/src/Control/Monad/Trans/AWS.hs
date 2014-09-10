{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
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

--
module Control.Monad.Trans.AWS
    (
    -- * Transformer
      AWST
    , AWS

    -- * Run
    , runAWST

    -- * Environment
    , Env
    , envAuth
    , envRegion
    , envManager
    , envLogging
    -- ** Creating the environment
    , Credentials (..)
    , newEnv

    -- * Debugging
    , debug
    , whenDebug

    -- * Regionalisation
    , Region      (..)
    , within

    -- * Helpers
    , hoistEither
    , scoped

    -- * Requests
    -- ** Synchronous
    , send
    , sendCatch
    -- ** Paginated
    , paginate
    , paginateCatch
    -- ** Pre-signing URLs
    , presign

    -- * Asynchronous actions
    , Async.async
    , wait

    -- * Types
    , module Network.AWS.Types
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
import           Data.Text                       (Text)
import           Data.Time
import           Network.AWS                     (Env, newEnv, envRegion, envLogging, envAuth, envManager)
import qualified Network.AWS                     as AWS
import           Network.AWS.Auth
import qualified Network.AWS.Types               as Types
import           Network.AWS.Types               hiding (debug)

-- | A convenient alias for 'AWST' 'IO'.
type AWS = AWST IO

-- | The transformer. This satisfies all of the constraints that the functions
-- in this module require, such as providing 'MonadResource' instances,
-- as well as keeping track of the internal 'Env' environment.
--
-- The 'MonadError' instance for this transformer internally uses 'ExceptT'
-- to handle actions that result in an 'Error'. For more information see
-- 'sendCatch' and 'paginateCatch'.
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

    local f = AWST . local (first f) . unAWST
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

    liftWith f = AWST $
        liftWith $ \g ->
            liftWith $ \h ->
                f (liftM StTAWS . h . g . unAWST)
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

-- | Unwrap an 'AWST' transformer, calling all of the registered 'ResourceT'
-- release actions.
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

-- | Use the logger from 'envLogging' to log a debug message.
debug :: (MonadIO m, MonadReader Env m) => Text -> m ()
debug t = view envLogging >>= (`Types.debug` t)

-- | Perform a monadic action if 'envLogging' is set to 'Debug'.
--
-- Analogous to 'when'.
whenDebug :: MonadReader Env m => m () -> m ()
whenDebug f = do
    l <- view envLogging
    case l of
        Debug _ -> f
        _       -> return ()

-- | Scope a monadic action within the specific 'Region'.
within :: MonadReader Env m => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Send a data type which is an instance of 'AWSRequest', returning it's
-- associated 'Rs' response type.
--
-- This will throw any 'HTTPException' or 'AWSServiceError' returned by the
-- service using the 'MonadError' instance. In the case of 'AWST' this will
-- cause the internal 'ExceptT' to short-circuit and return an 'Error' in
-- the 'Left' case as the result of the computation.
send :: ( MonadCatch m
        , MonadResource m
        , MonadReader Env m
        , MonadError Error m
        , AWSRequest a
        )
     => a
     -> m (Rs a)
send = sendCatch >=> hoistEither

-- | Send a data type which is an instance of 'AWSRequest', returning either the
-- associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- This includes 'HTTPExceptions', serialisation errors, and any service
-- errors returned as part of the 'Response'.
sendCatch :: ( MonadCatch m
             , MonadResource m
             , MonadReader Env m
             , AWSRequest a
             )
          => a
          -> m (Either (Er (Sv a)) (Rs a))
sendCatch rq = scoped (`AWS.send` rq)

-- | Send a data type which is an instance of 'AWSPager' and paginate while
-- there are more results as defined by the related service operation.
--
-- Errors will be handle identically to 'send'.
--
-- Note: The 'ResumableSource' will close when there are no more results or the
-- 'ResourceT' computation is unwrapped. See: 'runResourceT' for more information.
paginate :: ( MonadCatch m
            , MonadResource m
            , MonadReader Env (ResumableSource m)
            , MonadError Error m
            , AWSPager a
            )
         => a
         -> ResumableSource m (Rs a)
paginate rq = paginateCatch rq $=+ awaitForever (hoistEither >=> yield)

-- | Send a data type which is an instance of 'AWSPager' and paginate over
-- the associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- Note: The 'ResumableSource' will close when there are no more results or the
-- 'ResourceT' computation is unwrapped. See: 'runResourceT' for more information.
paginateCatch :: ( MonadCatch m
                 , MonadResource m
                 , MonadReader Env (ResumableSource m)
                 , AWSPager a
                 )
              => a
              -> ResumableSource m (Either (Er (Sv a)) (Rs a))
paginateCatch rq = scoped (`AWS.paginate` rq)

-- | Wait for an asynchronous computation initiated by 'async' to complete and
-- raise any returned error case using 'hoistEither'.
wait :: (MonadBaseControl IO m, MonadError Error m, AWSError e)
     => Async (StM m (Either e a))
     -> m a
wait = Async.wait >=> hoistEither

-- | Presign a URL with expiry to be used at a later time.
--
-- Note: Requires the service's signer to be an instance of 'AWSPresigner'.
-- Not all signing process support this.
presign :: ( MonadIO m
           , MonadReader Env m
           , AWSRequest a
           , AWSPresigner (Sg (Sv a))
           )
        => a       -- ^ Request to presign.
        -> UTCTime -- ^ Signing time.
        -> Int     -- ^ Expiry time in seconds.
        -> m (Signed a (Sg (Sv a)))
presign rq t x = scoped $ \e -> AWS.presign e rq t x
