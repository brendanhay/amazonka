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
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- * Requests
    -- ** Synchronous
      send
    , send_
    , sendCatch
    -- ** Paginated
    , paginate
    , paginateCatch
    -- ** Eventual consistency
    , await
    , awaitCatch
    -- ** Pre-signing URLs
    , presign
    , presignURL

    -- * Transformer
    , AWS
    , AWST
    , MonadAWS

    -- * Running
    , runAWST

    -- * Regionalisation
    , Region      (..)
    , within

    -- * Retries
    , once

    -- * Environment
    , Env
    -- ** Lenses
    , envRegion
    , envLogger
    , envRetryCheck
    , envRetryPolicy
    , envManager
    , envAuth
    -- ** Creating the environment
    , AWS.newEnv
    , AWS.getEnv
    -- ** Specifying credentials
    , Credentials (..)
    , fromKeys
    , fromSession
    , getAuth
    , accessKey
    , secretKey

    -- * Logging
    , newLogger
    , info
    , debug
    , trace

    -- * Errors
    , Error
    , hoistEither
    , throwAWSError
    , verify
    , verifyWith

    -- ** Streaming body helpers
    , sourceFile
    , sourceHandle
    , sourceBody

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
import           Data.ByteString              (ByteString)
import           Data.Conduit                 hiding (await)
import           Data.Time
import qualified Network.AWS                  as AWS
import           Network.AWS.Data             (ToBuilder (..))
import           Network.AWS.Error
import           Network.AWS.Internal.Auth
import           Network.AWS.Internal.Body
import           Network.AWS.Internal.Env
import           Network.AWS.Internal.Log     hiding (debug, info, trace)
import qualified Network.AWS.Internal.Log     as Log
import           Network.AWS.Types
import           Network.AWS.Waiters
import qualified Network.HTTP.Conduit         as Client

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
    type StT AWST a =
        StT (ExceptT Error) (StT (ReaderT (Env, InternalState)) a)

    liftWith f = AWST $
        liftWith $ \g ->
            liftWith $ \h ->
                f (h . g . unAWST)
    {-# INLINE liftWith #-}

    restoreT = AWST . restoreT . restoreT
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (AWST m) where
    type StM (AWST m) a = ComposeSt AWST m a

    liftBaseWith = defaultLiftBaseWith
    {-# INLINE liftBaseWith #-}

    restoreM = defaultRestoreM
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

-- | Throw any 'AWSError' using 'throwError'.
throwAWSError :: (MonadError Error m, AWSError e) => e -> m a
throwAWSError = throwError . awsError

-- | Verify that an 'AWSError' matches the given 'Prism', otherwise throw the
-- error using 'throwAWSError'.
verify :: (AWSError e, MonadError Error m)
       => Prism' e a
       -> e
       -> m ()
verify p e
    | isn't p e = throwAWSError e
    | otherwise = return ()

-- | Verify that an 'AWSError' matches the given 'Prism', with an additional
-- guard on the result of the 'Prism'.
--
-- /See:/ 'verify'
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
--
-- /Note:/ By default, the library does not output 'Info' level messages.
-- Exclusive output is guaranteed via use of this function.
info :: (MonadIO m, MonadReader Env m, ToBuilder a) => a -> m ()
info x = view envLogger >>= (`Log.info` x)

-- | Use the supplied logger from 'envLogger' to log debug messages.
debug :: (MonadIO m, MonadReader Env m, ToBuilder a) => a -> m ()
debug x = view envLogger >>= (`Log.debug` x)

-- | Use the supplied logger from 'envLogger' to log trace messages.
trace :: (MonadIO m, MonadReader Env m, ToBuilder a) => a -> m ()
trace x = view envLogger >>= (`Log.trace` x)

-- | Scope a monadic action within the specific 'Region'.
within :: MonadReader Env m => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope a monadic action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: MonadReader Env m => m a -> m a
once = local $ \e ->
    e & envRetryPolicy ?~ limitRetries 0
      & envRetryCheck  .~ (\_ _ -> return False)

-- | Send a data type which is an instance of 'AWSRequest', returning it's
-- associated 'Rs' response type.
--
-- This will throw any 'HTTPException' or 'AWSServiceError' returned by the
-- service using the 'MonadError' instance. In the case of 'AWST' this will
-- cause the internal 'ExceptT' to short-circuit and return an 'Error' in
-- the 'Left' case as the result of the computation.
--
-- /See:/ 'sendCatch'
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
--
-- /See:/ 'send'
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
--
-- /Note:/ Requests will be retried depending upon each service's respective
-- strategy. This can be overriden using 'once' or 'envRetry'.
-- Requests which contain streaming request bodies (such as S3's 'PutObject') are
-- never considered for retries.
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
--
-- /See:/ 'paginateCatch'
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

-- | Poll the API until a predfined condition is fulfilled using the
-- supplied 'Wait' specification from the respective service.
--
-- Any errors which are unhandled by the 'Wait' specification during retries
-- will be thrown in the same manner as 'send'.
--
-- /See:/ 'awaitCatch'
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

-- | Poll the API until a predfined condition is fulfilled using the
-- supplied 'Wait' specification from the respective service.
--
-- The response will be either the first error returned that is not handled
-- by the specification, or the successful response from the await request.
--
-- /Note:/ You can find any available 'Wait' specifications under the
-- namespace @Network.AWS.<ServiceName>.Waiters@ for supported services.
awaitCatch :: ( MonadCatch m
              , MonadResource m
              , MonadReader Env m
              , AWSRequest a
              )
           => Wait a
           -> a
           -> m (Response a)
awaitCatch w x = scoped (\e -> AWS.await e w x)

-- | Presign an HTTP request that expires at the specified amount of time
-- in the future.
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
        -> Integer -- ^ Expiry time in seconds.
        -> m Client.Request
presign x t ex = scoped (\e -> AWS.presign e x t ex)

-- | Presign a URL that expires at the specified amount of time in the future.
--
-- /See:/ 'presign'
presignURL :: ( MonadIO m
             , MonadReader Env m
             , AWSRequest a
             , AWSPresigner (Sg (Sv a))
             )
           => a       -- ^ Request to presign.
           -> UTCTime -- ^ Signing time.
           -> Integer -- ^ Expiry time in seconds.
           -> m ByteString
presignURL x t ex = scoped (\e -> AWS.presignURL e x t ex)
