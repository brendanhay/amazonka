{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}

-- Module      : Control.Monad.Trans.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The core module for making requests to the various AWS services and
-- building your own Monad transformer stack.
module Control.Monad.Trans.AWS
    (
    -- * Constraints
    -- $constraints
      MonadAWS

    -- ** AWST
    , AWST
    , runAWST

    -- ** AWS
    , AWS
    , runAWS

    -- ** Usage
    -- $usage

    -- * Requests
    -- ** Synchronous
    , send_
    , send
    , sendWith
    -- ** Asynchronous
    -- $async
    -- ** Paginated
    , paginate
    , paginateWith
    -- ** Eventual consistency
    , await
    , awaitWith
    -- ** Pre-signing
    , presign
    , presignURL
    , presignWith
    -- ** Configuring
    , within
    , once
    , timeout

    , module Network.AWS.Env
    , module Network.AWS.Auth
    , module Network.AWS.Logger
    , module Network.AWS.Internal.Body

    -- * Errors
    -- ** General
    , AWSError     (..)
    , Error

    -- ** Service specific errors
    , ServiceError
    , errorService
    , errorStatus
    , errorHeaders
    , errorCode
    , errorMessage
    , errorRequestId

    -- ** Catching errors
    , catching

    -- ** Error types
    , ErrorCode    (..)
    , ErrorMessage (..)
    , RequestId    (..)

    -- * Types
    , module Network.AWS.Types

    -- * Seconds
    , Seconds      (..)
    , _Seconds
    , microseconds

    -- * Serialisation
    , ToText       (..)
    , FromText     (..)
    , ToBuilder    (..)
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Error.Lens     (catching, throwing)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Conduit                 hiding (await)
import qualified Network.AWS                  as AWS
import           Network.AWS.Auth
import           Network.AWS.Env
import           Network.AWS.Error
import           Network.AWS.Internal.Body
import           Network.AWS.Logger
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Types
import           Network.AWS.Waiter

-- FIXME: Add explanation about the use of constraints and
--   how to build a monad transformer stack, embed it, etc.
-- FIXME: Add notes about specialising the constraints.
-- FIXME: Add note about *With variants.
-- FIXME: Add note about using Control.Monad.Error.Lens.catching* + error prisms

-- $constraints
--
-- The function signatures in this module are specified using constraints in
-- an attempt to keep them as general as possible. In fact, 'AWST' and 'AWS'
-- are simply type aliases representing potential specialisations of 'MonadAWS'.
-- These functions will also specialise to whatever Monad stack you might be using
-- if they also fulfill these constraints making it easy to embed any AWS related
-- computation in your application. An extended example is provided in #usage, below.
--
-- The two commonly used constraints you will see are:
--
-- @
-- -- For some environment 'r' - a 'Lens' is provided by 'AWSEnv' 'r' to obtain the AWS specific 'Env' contained in 'r':
--
-- (MonadReader r m, AWSEnv r)
-- @
--
-- and:
--
-- @
-- -- For some error 'e' - a 'Prism' is provided to de/construct the AWS specific 'Error' within 'e':
-- (MonadError e m, AWSEnv e)
-- @

-- #usage#
-- $usage
--
-- @
-- newtype MyApp a = MyApp
--     { unApp :: ExceptT MyErr (ReaderT MyEnv (ResourceT IO)) a
--     } deriving ( Functor
--                , Applicative
--                , Monad
--                , MonadIO
--                , MonadThrow
--                , MonadBase IO
--                , MonadCatch
--                , MonadError  MyErr
--                , MonadReader MyEnv
--                , MonadResource
--                )
--
-- runApp :: MyEnv -> MyApp a -> IO (Either MyErr a)
-- runApp e m = runResourceT $ runReaderT (runExceptT (unApp m)) e
--
-- -- A custom application environment for whatever Monad stack you're using:
-- data MyEnv = MyEnv
--     { _config :: Int
--     , _env    :: Env
--     }
--
-- -- This class adds a lens pointing to the Network.AWS.Env in the user's custom MyEnv type:
-- instance AWSEnv MyEnv where
--     env f s = f (_env s) <&> \a -> s { _env = a }
--
-- -- A custom error for whatever application, containing a single constructor that wraps the AWS errors:
-- data MyErr where
--     EndpointDisabled :: MyErr
--     EndpointNotFound :: Text  -> MyErr
--     NoEndpointArn    :: MyErr
--     NoToken          :: Text  -> MyErr
--     ReadError        :: Text  -> String -> MyErr
--     InvalidArn       :: Text  -> String -> MyErr
--     -- The actual Network.AWS.Types.Error is embedd here:
--     GeneralError     :: Error -> MyErr
--
-- -- This class adds a prism to point to the Network.AWS.Types.Error in the user's custom MyErr type:
-- instance AWSError MyErr where
--     _Error = prism
--         GeneralError
--         (\case GeneralError e -> Right e
--                x              -> Left  x)
--
-- -- Control.Monad.Error.Lens.'catching' can be used to catch AWS specific errors:
-- catching _ServiceError $ MyApp (send Bar)
--   :: (ServiceError -> MyApp Bar) -> MyApp Bar

-- $async
--
-- /See:/ <http://hackage.haskell.org/package/lifted-async Control.Concurrent.Async.Lifted>

-- | A convenient alias that specialises the common <http://hackage.haskell.org/package/mtl mtl>
-- constraints in this moduleto the related <http://hackage.haskell.org/package/transformers transformers>
-- types.
type AWST m = ExceptT Error (ReaderT Env m)

runAWST :: MonadResource m => Env -> AWST m a -> m (Either Error a)
runAWST e m = runReaderT (runExceptT m) e

type AWS = AWST (ResourceT IO)

-- | Run an 'AWS' monadic action, calling all of the registered 'ResourceT'
-- release actions.
runAWS :: Env -> AWS a -> IO (Either Error a)
runAWS e = runResourceT . runAWST e

-- | A type alias used to abbreviate the most commonly used constraints.
type MonadAWS r e m =
    ( MonadCatch      m
    , MonadResource   m
    , MonadReader   r m
    , MonadError    e m
    , AWSEnv        r
    , AWSError      e
    )

-- | Scope an action within the specific 'Region'.
within :: (MonadReader r m, AWSEnv r) => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: (MonadReader r m, AWSEnv r) => m a -> m a
once = local noRetries

-- | Scope an action such that any HTTP response use this timeout value.
timeout :: (MonadReader r m, AWSEnv r) => Seconds -> m a -> m a
timeout s = local (envTimeout ?~ s)

send_ :: (MonadAWS r e m, AWSRequest a) => a -> m ()
send_ = void . send

send :: (MonadAWS r e m, AWSRequest a) => a -> m (Rs a)
send x = sendWith (serviceOf x) x

sendWith :: (MonadAWS r e m, AWSSigner (Sg s), AWSRequest a)
         => Service s
         -> a
         -> m (Rs a)
sendWith svc x =
    scoped $ \e ->
        AWS.sendWith e svc x
            >>= hoistError e

paginate :: (MonadAWS r e m, AWSPager a) => a -> Source m (Rs a)
paginate x = paginateWith (serviceOf x) x

paginateWith :: (MonadAWS r e m, AWSSigner (Sg s), AWSPager a)
             => Service s
             -> a
             -> Source m (Rs a)
paginateWith svc = go
  where
    go x = do
        y <- lift (sendWith svc x)
        yield y
        maybe (return ())
              go
              (page x y)

await :: (MonadAWS r e m, AWSRequest a) => Wait a -> a -> m (Rs a)
await w x = awaitWith (serviceOf x) w x

awaitWith :: (MonadAWS r e m, AWSSigner (Sg s), AWSRequest a)
          => Service s
          -> Wait a
          -> a
          -> m (Rs a)
awaitWith svc w x =
    scoped $ \e ->
        AWS.awaitWith e svc w x
            >>= hoistError e

presign :: ( MonadIO        m
           , MonadReader  r m
           , AWSEnv       r
           , AWSPresigner (Sg (Sv a))
           , AWSRequest   a
           )
        => UTCTime     -- ^ Signing time.
        -> Integer     -- ^ Expiry time in seconds.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign t ex x = scoped $ \e -> AWS.presign e t ex x

presignURL :: ( MonadIO        m
              , MonadReader  r m
              , AWSEnv       r
              , AWSPresigner (Sg (Sv a))
              , AWSRequest   a
              )
           => UTCTime     -- ^ Signing time.
           -> Integer     -- ^ Expiry time in seconds.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL t ex x = scoped $ \e -> AWS.presignURL e t ex x

presignWith :: ( MonadIO        m
               , MonadReader  r m
               , AWSEnv       r
               , AWSPresigner (Sg s)
               , AWSRequest   a
               )
            => Service s -- ^ Service configuration.
            -> UTCTime   -- ^ Signing time.
            -> Integer   -- ^ Expiry time in seconds.
            -> a         -- ^ Request to presign.
            -> m ClientRequest
presignWith svc t ex x = scoped $ \e -> AWS.presignWith e svc t ex x

hoistError :: (MonadIO m, MonadError e m, AWSEnv r, AWSError e)
           => r
           -> Either Error a
           -> m a
hoistError e = \case
    Right r -> return r
    Left  l -> do
        logError (e ^. envLogger) l -- error:ServiceError
        throwing _Error           l

scoped :: (MonadReader r m, AWSEnv r) => (Env -> m a) -> m a
scoped f = view env >>= f
