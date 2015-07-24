{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Control.Monad.Trans.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module offers the 'AWST' transformer which is used as the core for
-- other modules such as "Network.AWS" and "Control.Monad.Error.AWS".
-- The function signatures use the minimum satisfiable constraints in order to
-- stay as general as possible, at the possible cost of readability.
--
-- For a simpler interface see "Network.AWS", or, for the pre-@1.0@
-- behaviour of implicitly lifting errors, see "Control.Monad.Error.AWS".
module Control.Monad.Trans.AWS
    (
    -- * Monad constraints
    -- $constraints

    -- * Running AWS Actions
      AWST
    , runAWST
    , pureAWST

    -- * Environment Setup
    , Credentials (..)
    , AWSEnv      (..)
    , Env
    , newEnv

    -- * Runtime Configuration
    , within
    , once
    , timeout

    -- * EC2 Metadata
    , metadata
    , dynamic
    , userdata

    -- * Sending Requests
    -- ** Synchronous
    , send
    , await
    , paginate
    -- ** Overriding Defaults
    , sendWith
    , awaitWith
    , paginateWith
    -- ** Asynchronous
    -- $async

    , module Network.AWS.Presign

    , module Network.AWS.Internal.Body

    -- * Logging
    , Logger
    , newLogger
    -- ** Levels
    , LogLevel    (..)
    , logError
    , logInfo
    , logDebug
    , logTrace

    -- * Types
    , module Network.AWS.Types
    ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer.Class
import           Network.AWS.Auth
import           Network.AWS.Env
import           Network.AWS.Free
import           Network.AWS.Internal.Body
import           Network.AWS.Logger
import           Network.AWS.Prelude
import           Network.AWS.Presign
import           Network.AWS.Types
import           Network.AWS.Waiter

-- FIXME: Add notes about specialising the constraints.
-- FIXME: Add note about *With variants.
-- FIXME: Add note about using Control.Monad.Error.Lens.catching* + error prisms
-- FIXME: Philosophical notes on the use of lenses, and notes about template-haskell usage.
-- FIXME: Notes about associated response types, signers, service configuration.
-- FIXME: Note/example about mocking.

-- Base64
-- Blob
-- ObjectKey

newtype AWST m a = AWST { unAWST :: FreeT Command (ReaderT Env m) a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadIO
        , MonadFree Command
        , MonadReader Env
        )

instance MonadThrow m => MonadThrow (AWST m) where
    throwM = lift . throwM

instance MonadBase b m => MonadBase b (AWST m) where
    liftBase = liftBaseDefault

instance MFunctor AWST where
    hoist nat = AWST . hoistFreeT (hoist nat) . unAWST

instance MonadBaseControl b m => MonadBaseControl b (AWST m) where
    type StM (AWST m) a =
         StM m (FreeF Command a (FreeT Command (ReaderT Env m) a))

    liftBaseWith f = AWST . FreeT . liftM Pure $
        liftBaseWith $ \runInBase ->
            f $ \k ->
                runInBase (runFreeT (unAWST k))

    restoreM = AWST . FreeT . restoreM

instance MonadTrans AWST where
    lift = AWST . lift . lift

instance MonadResource m => MonadResource (AWST m) where
    liftResourceT = lift . liftResourceT

instance MonadError e m => MonadError e (AWST m) where
    throwError     = lift . throwError
    catchError m f = AWST (unAWST m `catchError` (unAWST . f))

instance MonadState s m => MonadState s (AWST m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (AWST m) where
    writer = lift . writer
    tell   = lift . tell
    listen = AWST . listen . unAWST
    pass   = AWST . pass   . unAWST

runAWST :: (MonadCatch m, MonadResource m, AWSEnv r) => r -> AWST m a -> m a
runAWST e (AWST m) = runReaderT (evalProgramT m) (e ^. env)

{- $embedding
The following is a more advanced example, of how you might embed Amazonka actions
within an application specific Monad transformer stack. This demonstrates using
a custom application environment and application specific error type.

The base application Monad could be defined as:

> newtype MyApp a = MyApp (ExceptT MyErr (ReaderT MyEnv (ResourceT IO)) a)
>     deriving ( Functor
>              , Applicative
>              , Monad
>              , MonadIO
>              , MonadThrow
>              , MonadCatch
>              , MonadError  MyErr
>              , MonadReader MyEnv
>              , MonadResource
>              )

The environment contains whatever environment the application might need, as
well as a field for the AWS 'Env':

> data MyEnv = MyEnv
>     { _config :: Config
>     , _env    :: Env -- ^ Here the AWS environment is embedded.
>     }

Adding a class instance for 'AWSEnv' to the above environment requires defining
a lens pointing to where the AWS 'Env' is located:

> instance AWSEnv MyEnv where
>     env = lens _env (\s a -> s { _env = a })

The custom error for the application, contains whatever errors the application
might return, as well a single constructor that wraps any AWS errors:

> data MyErr
>     = GeneralError
>     | SpecificError  Text
>     | ElaborateError Text String
>     | AmazonError    Error -- ^ Here the AWS error is embedded.

Adding a class instances requires defining a prism to wrap/unwrap AWS 'Error'
in the application's custom @MyErr@ type:

> instance AWSError MyErr where
>     _Error = prism AmazonError $
>         case e of
>             AmazonError x -> Right x
>             _             -> Left  e

Running the application can return the application's @MyErr@ in the error case:

> runApp :: MyEnv -> MyApp a -> IO (Either MyErr a)
> runApp e (MyApp k) = runResourceT $ runReaderT (runExceptT k) e

Functions from "Control.Monad.Error.Lens" such as 'catching' can be used to
handle AWS specific errors:

> catching _ServiceError $ MyApp (send Bar) :: (ServiceError -> MyApp Bar) -> MyApp Bar
-}

{- $async
Requests can be sent asynchronously, but due to guarantees about resource closure
require the use of <http://hackage.haskell.org/package/lifted-async lifted-async>.

The following example demonstrates retrieving two objects from S3 concurrently:

> import Control.Concurrent.Async.Lifted
> import Control.Lens
> import Control.Monad.Trans.AWS
> import Network.AWS.S3
>
> do x   <- async . send $ getObject "bucket" "prefix/object-foo"
>    y   <- async . send $ getObject "bucket" "prefix/object-bar"
>    foo <- wait x
>    bar <- wait y
>    ...

/See:/ <http://hackage.haskell.org/package/lifted-async Control.Concurrent.Async.Lifted>
-}
