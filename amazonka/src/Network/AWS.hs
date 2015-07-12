{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Contains a specalised version of the "Control.Monad.Trans.AWS" transformer
-- with lifted 'send', 'paginate' and 'await' functions suitable for embedding
-- as a layer directly into your own application monad.
module Network.AWS where

import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy     as LS
import qualified Control.Monad.State.Strict   as S
import           Control.Monad.Trans.AWS      as AWST
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Conduit                 hiding (await)
import qualified Network.AWS.Env              as Env
import           Network.AWS.Pager
import           Network.AWS.Waiter

-- | 'IO' specialisation of the 'AWST' transformer.
type AWS = AWST IO

-- | Monads in which 'AWS' actions may be embedded.
class (Functor m, Applicative m, Monad m) => MonadAWS m where
    -- | Lift a computation to the 'AWS' monad.
    liftAWS :: AWS a -> m a

instance MonadAWS AWS where
    liftAWS = id

instance MonadAWS m => MonadAWS (IdentityT   m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (MaybeT      m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ExceptT   e m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ReaderT   r m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (S.StateT  s m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (LS.StateT s m) where liftAWS = lift . liftAWS

-- | Run the 'AWS' monad.
--
-- /Note:/ Any outstanding HTTP responses' 'ResumableSource' will be closed when
-- the 'ResourceT' computation is unwrapped.
--
-- /See:/ 'runResourceT' for more information.
runAWS :: (MonadCatch m, MonadResource m) => Env -> AWS a -> m a
runAWS e = liftResourceT . runAWST e . hoist (withInternalState . const)

-- | Run any remote requests against the specified 'Region'.
within :: MonadAWS m => Region -> AWS a -> m a
within r = liftAWS . Env.within r

-- | Ignore any retry logic and ensure that any requests will be sent (at most) once.
once :: MonadAWS m => AWS a -> m a
once = liftAWS . Env.once

-- | Configure any HTTP connections to use this response timeout value.
timeout :: MonadAWS m => Seconds -> AWS a -> m a
timeout s = liftAWS . Env.timeout s

-- | Send a request, returning the associated response if successful,
-- or an 'Error'.
--
-- 'Error' will include 'HTTPExceptions', serialisation errors, or any service
-- specific errors.
--
-- /Note:/ Requests will be retried depending upon each service's respective
-- strategy. This can be overriden using 'envRetry'. Requests which contain
-- streaming request bodies (such as S3's 'PutObject') are never considered
-- for retries.
--
-- /See:/ 'sendWith'
send :: (MonadAWS m, AWSRequest a) => a -> m (Either Error (Rs a))
send = liftAWS . AWST.send

-- | Transparently paginate over multiple responses for supported requests
-- while results are available.
--
-- /See:/ 'paginateWith'
paginate :: (MonadAWS m, AWSPager a) => a -> Source m (Either Error (Rs a))
paginate = hoist liftAWS . AWST.paginate

-- | Poll the API with the specified request until a 'Wait' condition is fulfilled.
--
-- The response will be either the first error returned that is not handled
-- by the specification, or any subsequent successful response from the await
-- request(s).
--
-- /Note:/ You can find any available 'Wait' specifications under then
-- @Network.AWS.<ServiceName>.Waiters@ namespace for supported services.
--
-- /See:/ 'awaitWith'
await :: (MonadAWS m, AWSRequest a) => Wait a -> a -> m (Either Error (Rs a))
await w = liftAWS . AWST.await w
