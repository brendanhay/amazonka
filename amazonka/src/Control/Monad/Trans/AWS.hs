{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- This is required due to the MonadBaseControl instance.
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

-- | A monad transformer built on top of functions from "Network.AWS" which
-- encapsulates various common parameters, errors, and usage patterns.
module Control.Monad.Trans.AWS
    (
    -- * Transformer
      AWS
    , AWST
    , MonadAWS

    -- * Running
    , runAWST

    -- * Environment
    , Env
    , envAuth
    , envRegion
    , envManager
    , envLogger
    , envRetry
    -- ** Creating the environment
    , Credentials (..)
    , AWS.newEnv
    , AWS.getEnv

    -- * Logging
    , LogLevel    (..)
    , Logger
    , newLogger
    , logInfo
    , logDebug
    , logTrace

    -- * Regionalisation
    , Region      (..)
    , within

    -- * Retries
    , once

    -- * Errors
    , Error
    , hoistEither
    , throwAWSError
    , verify
    , verifyWith

    -- * Requests
    -- ** Synchronous
    , send
    , send_
    , sendCatch
    -- ** Paginated
    , paginate
    , paginateCatch
    -- ** Eventual consistency
    , await
    -- ** Pre-signing URLs
    , presign

    -- * Types
    , ToBuilder   (..)
    , module Network.AWS.Types
    , module Network.AWS.Error
    ) where

import           Control.Applicative
import           Control.Arrow                (first)
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Control.Retry                (limitRetries)
import           Data.Conduit                 hiding (await)
import           Data.Time
import qualified Network.AWS                  as AWS
import           Network.AWS.Auth
import           Network.AWS.Data             (ToBuilder(..))
import           Network.AWS.Error
import           Network.AWS.Internal.Env
import           Network.AWS.Internal.Log
import           Network.AWS.Types

-- | The top-level error type.
type Error = ServiceError String

-- | A convenient alias for 'AWST' 'IO'.
type AWS = AWST IO

-- | Provides an alias for shortening type signatures if preferred.
--
-- /Note:/ requires the @ConstraintKinds@ extension.
type MonadAWS m =
    ( MonadBaseControl IO m
    , MonadCatch m
    , MonadResource m
    , MonadError Error m
    , MonadReader Env m
    )

-- | The transformer. This satisfies all of the constraints that the functions
-- in this module require, such as providing 'MonadResource' instances,
-- and keeping track of the 'Env' environment.
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

instance (Applicative m, MonadIO m, MonadBase IO m, MonadThrow m)
    => MonadResource (AWST m) where
        liftResourceT f = resources >>= liftIO . runInternalState f
        {-# INLINE liftResourceT #-}

-- | Unwrap an 'AWST' transformer, calling all of the registered 'ResourceT'
-- release actions.
runAWST :: MonadBaseControl IO m => Env -> AWST m a -> m (Either Error a)
runAWST e m = runResourceT . withInternalState $ runAWST' f . (e,)
  where
    f = liftBase (_envLogger e Debug (build e)) >> m

runAWST' :: AWST m a -> (Env, InternalState) -> m (Either Error a)
runAWST' (AWST k) = runExceptT . runReaderT k

resources :: Monad m => AWST m InternalState
resources = AWST (ReaderT (return . snd))

-- | Hoist an 'Either' throwing the 'Left' case, and returning the 'Right'.
hoistEither :: (MonadError Error m, AWSError e) => Either e a -> m a
hoistEither = either throwAWSError return

throwAWSError :: (MonadError Error m, AWSError e) => e -> m a
throwAWSError = throwError . awsError

verify :: (AWSError e, MonadError Error m)
       => Prism' e a
       -> e
       -> m ()
verify p e
    | isn't p e = throwAWSError e
    | otherwise = return ()

verifyWith :: (AWSError e, MonadError Error m)
           => Prism' e a
           -> (a -> Bool)
           -> e
           -> m ()
verifyWith p f e = either (const err) g (matching p e)
  where
    g x | f x       = return ()
        | otherwise = err

    err = throwAWSError e

-- | Pass the current environment to a function.
scoped :: MonadReader Env m => (Env -> m a) -> m a
scoped f = ask >>= f

-- | Use the supplied logger from 'envLogger' to log info messages.
logInfo :: (MonadIO m, MonadReader Env m, ToBuilder a) => a -> m ()
logInfo x = view envLogger >>= (`info` x)

-- | Use the supplied logger from 'envLogger' to log debug messages.
logDebug :: (MonadIO m, MonadReader Env m, ToBuilder a) => a -> m ()
logDebug x = view envLogger >>= (`debug` x)

-- | Use the supplied logger from 'envLogger' to log trace messages.
logTrace :: (MonadIO m, MonadReader Env m, ToBuilder a) => a -> m ()
logTrace x = view envLogger >>= (`trace` x)

-- | Scope a monadic action within the specific 'Region'.
within :: MonadReader Env m => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope a monadic action such that any potential retry logic for the
-- 'Service' is ignored.
--
-- /Example:/ Any requests will at most be sent once.
once :: MonadReader Env m => m a -> m a
once = local (envRetry .~ limitRetries 1)

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

-- | A variant of 'send' which discards any successful response.
send_ :: ( MonadCatch m
         , MonadResource m
         , MonadReader Env m
         , MonadError Error m
         , AWSRequest a
         )
      => a
      -> m ()
send_ = void . send

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
          -> m (Response a)
sendCatch x = scoped (`AWS.send` x)

-- | Send a data type which is an instance of 'AWSPager' and paginate while
-- there are more results as defined by the related service operation.
--
-- Errors will be handle identically to 'send'.
--
-- /Note:/ The 'ResumableSource' will close when there are no more results or the
-- 'ResourceT' computation is unwrapped. See: 'runResourceT' for more information.
paginate :: ( MonadCatch m
            , MonadResource m
            , MonadReader Env m
            , MonadError Error m
            , AWSPager a
            )
         => a
         -> Source m (Rs a)
paginate x = paginateCatch x $= awaitForever (hoistEither >=> yield)

-- | Send a data type which is an instance of 'AWSPager' and paginate over
-- the associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- /Note:/ The 'ResumableSource' will close when there are no more results or the
-- 'ResourceT' computation is unwrapped. See: 'runResourceT' for more information.
paginateCatch :: ( MonadCatch m
                 , MonadResource m
                 , MonadReader Env m
                 , AWSPager a
                 )
              => a
              -> Source m (Response a)
paginateCatch x = scoped (`AWS.paginate` x)

-- |
await :: ( MonadCatch m
         , MonadResource m
         , MonadReader Env m
         , MonadError Error m
         , AWSRequest a
         )
      => Wait a
      -> a
      -> m (Rs a)
await w = awaitCatch w >=> hoistEither

-- |
awaitCatch :: ( MonadCatch m
              , MonadResource m
              , MonadReader Env m
              , AWSRequest a
              )
           => Wait a
           -> a
           -> m (Response a)
awaitCatch w x = scoped (\e -> AWS.await e w x)

-- | Presign a URL with expiry to be used at a later time.
--
-- /Note:/ Requires the service's signer to be an instance of 'AWSPresigner'.
-- Not all signing process support this.
presign :: ( MonadIO m
           , MonadReader Env m
           , AWSRequest a
           , AWSPresigner (Sg (Sv a))
           )
        => a       -- ^ Request to presign.
        -> UTCTime -- ^ Signing time.
        -> UTCTime -- ^ Expiry time.
        -> m (Signed a (Sg (Sv a)))
presign x t1 t2 = scoped (\e -> AWS.presign e x t1 t2)
