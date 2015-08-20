{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Network.AWS.Free
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Defines the core AST, logic and interpreters for AWS behaviour.
module Network.AWS.Free where

import           Control.Applicative
import           Control.Exception.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Free.Church
import           Data.Conduit                    (Source, yield)
import           Network.AWS.EC2.Metadata        (Dynamic, Metadata)
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request             (requestURL)
import           Network.AWS.Types
import           Network.AWS.Waiter
#if MIN_VERSION_free(4,12,0)
#else
import           Control.Monad.Catch
import           Control.Monad.Trans.Free        (FreeT (..))
#endif

import           Prelude

data Command r where
    CheckF ::             (Bool                                    -> r) -> Command r
    DynF   :: Dynamic  -> (Either HttpException ByteString         -> r) -> Command r
    MetaF  :: Metadata -> (Either HttpException ByteString         -> r) -> Command r
    UserF  ::             (Either HttpException (Maybe ByteString) -> r) -> Command r

    SignF  :: (AWSPresigner (Sg s), AWSRequest a)
           => Service s
           -> UTCTime
           -> Seconds
           -> a
           -> (ClientRequest -> r)
           -> Command r

    SendF  :: (AWSSigner (Sg s), AWSRequest a)
           => Service s
           -> a
           -> (Either Error (Rs a) -> r)
           -> Command r

    AwaitF :: (AWSSigner (Sg s), AWSRequest a)
           => Service s
           -> Wait a
           -> a
           -> (Maybe Error -> r)
           -> Command r

instance Functor Command where
    fmap f = \case
        CheckF         k -> CheckF         (fmap f k)
        DynF         x k -> DynF         x (fmap f k)
        MetaF        x k -> MetaF        x (fmap f k)
        UserF          k -> UserF          (fmap f k)
        SignF  s t e x k -> SignF  s t e x (fmap f k)
        SendF  s     x k -> SendF  s     x (fmap f k)
        AwaitF s w   x k -> AwaitF s w   x (fmap f k)

#if MIN_VERSION_free(4,12,0)
#else
instance MonadThrow m => MonadThrow (FreeT Command m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (FreeT Command m) where
    catch (FreeT m) f = FreeT $
        liftM (fmap (`catch` f)) m `catch` (runFreeT . f)

instance MonadThrow m => MonadThrow (FT Command m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (FT Command m) where
    catch m f = toFT $ fromFT m `catch` (fromFT . f)
#endif

-- | Send a request, returning the associated response if successful.
--
-- /See:/ 'sendWith'
send :: (MonadThrow m, MonadFree Command m, AWSRequest a)
     => a
     -> m (Rs a)
send = sendWith id

-- | A variant of 'send' that allows modifying the default 'Service' definition
-- used to configure the request.
sendWith :: (MonadThrow m, MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
         => (Service (Sv a) -> Service s) -- ^ Modify the default service configuration.
         -> a                             -- ^ Request.
         -> m (Rs a)
sendWith f x =
    liftF (SendF (f (serviceOf x)) x id)
        >>= hoistError _Error

-- | Repeatedly send a request, automatically setting markers and
-- paginating over multiple responses while available.
--
-- /See:/ 'paginateWith'
paginate :: (MonadThrow m, MonadFree Command m, AWSPager a)
         => a
         -> Source m (Rs a)
paginate = paginateWith id

-- | A variant of 'paginate' that allows modifying the default 'Service' definition
-- used to configure the request.
paginateWith :: (MonadThrow m, MonadFree Command m, AWSSigner (Sg s), AWSPager a)
             => (Service (Sv a) -> Service s) -- ^ Modify the default service configuration.
             -> a                             -- ^ Initial request.
             -> Source m (Rs a)
paginateWith f rq = go rq
  where
    go !x = do
        !e <- lift $ liftF (SendF s x id)
        !y <- hoistError _Error e
        yield y
        maybe (pure ())
              go
              (page x y)

    !s = f (serviceOf rq)

-- | Poll the API with the supplied request until a specific 'Wait' condition
-- is fulfilled.
--
-- /See:/ 'awaitWith'
await :: (MonadThrow m, MonadFree Command m, AWSRequest a)
      => Wait a
      -> a
      -> m ()
await = awaitWith id

-- | A variant of 'await' that allows modifying the default 'Service' definition
-- used to configure the request.
awaitWith :: (MonadThrow m, MonadFree Command m, AWSSigner (Sg s), AWSRequest a)
          => (Service (Sv a) -> Service s) -- ^ Modify the default service configuration.
          -> Wait a                        -- ^ Polling, error and acceptance criteria.
          -> a                             -- ^ Request to poll with.
          -> m ()
awaitWith f w x =
    liftF (AwaitF (f (serviceOf x)) w x id)
        >>= maybe (return ()) (throwingM _Error)

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'presign', 'presignWith'
presignURL :: (MonadFree Command m, AWSPresigner (Sg (Sv a)), AWSRequest a)
           => UTCTime     -- ^ Signing time.
           -> Seconds     -- ^ Expiry time.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL ts ex = liftM requestURL . presign ts ex

-- | Presign an HTTP request that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'presignWith'
presign :: (MonadFree Command m, AWSPresigner (Sg (Sv a)), AWSRequest a)
        => UTCTime     -- ^ Signing time.
        -> Seconds     -- ^ Expiry time.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign = presignWith id

-- | A variant of 'presign' that allows specifying the 'Service' definition
-- used to configure the request.
presignWith :: (MonadFree Command m, AWSPresigner (Sg s), AWSRequest a)
            => (Service (Sv a) -> Service s) -- ^ Function to modify the service configuration.
            -> UTCTime                       -- ^ Signing time.
            -> Seconds                       -- ^ Expiry time.
            -> a                             -- ^ Request to presign.
            -> m ClientRequest
presignWith f ts ex x = liftF (SignF (f (serviceOf x)) ts ex x id)

-- | Test whether the underlying host is running on EC2.
-- For 'IO' based interpretations of 'MonadFree' 'Command', this is memoised and
-- any external check occurs for the first call only.
isEC2 :: MonadFree Command m => m Bool
isEC2 = liftF (CheckF id)

-- | Retrieve the specified 'Dynamic' data.
dynamic :: (MonadThrow m, MonadFree Command m)
        => Dynamic
        -> m ByteString
dynamic d = liftF (DynF d id) >>= hoistError _TransportError

-- | Retrieve the specified 'Metadata'.
metadata :: (MonadThrow m, MonadFree Command m)
         => Metadata
         -> m ByteString
metadata m = liftF (MetaF m id) >>= hoistError _TransportError

-- | Retrieve the user data. Returns 'Nothing' if no user data is assigned
-- to the instance.
userdata :: (MonadThrow m, MonadFree Command m)
         => m (Maybe ByteString)
userdata = liftF (UserF id) >>= hoistError _TransportError

--hoistError :: MonadThrow m => Either Error a -> m a
hoistError p = either (throwingM p) return

-- hoistTransportError :: MonadThrow m => Either Error a -> m a
-- hoistTransportErrorError = either (throwingM _Error) return

