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
-- This module offers a more flexible version of the 'ProgramT' transformer
-- comparing to "Network.AWS". As such, the function signatures are as general
-- as possible and illustrate the minimum satisfiable constraints at the cost
-- of readability.
--
-- Many functions in this module have their 'Error's lifted and can be
-- caught\/rethrown by using "Control.Monad.Error.Lens" and 'catching' / 'throwing'
-- along with 'Prism's such as '_Error' and '_ServiceError'.
--
-- For a simpler interface see "Network.AWS".
module Control.Monad.Trans.AWS
    (
    -- * Monad constraints
    -- $constraints

    -- * Running AWS Actions
      AWST
    , runAWST
    , pureAWST

    -- * Environment Setup
    , Auth.Credentials (..)
    , Env.AWSEnv       (..)
    , Env
    , Env.newEnv

    -- * Runtime Configuration
    , within
    , once
    , timeout

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

    , module Network.AWS.Internal.Body

    -- * Logging
    , Logger
    , newLogger
    -- ** Levels
    , LogLevel  (..)
    , logError
    , logInfo
    , logDebug
    , logTrace

    -- * Handling Errors
    , hoistError

    , AWSError         (..)
    , Error

    -- ** Service Errors
    , ServiceError
    , errorService
    , errorStatus
    , errorHeaders
    , errorCode
    , errorMessage
    , errorRequestId

    -- * Types
    , module Network.AWS.Types
    )

    -- -- ** AWST
    --   AWST
    -- , runAWST
    -- , pureAWST

    -- -- ** Manipulating the environment
    -- , within
    -- , once
    -- , timeout

    -- -- * Requests
    -- -- ** Synchronous
    -- , send
    -- , sendWith
    -- -- ** Asynchronous
    -- -- $async
    -- -- ** Paginated
    -- , paginate
    -- , paginateWith
    -- -- ** Eventual consistency
    -- , await
    -- , awaitWith

    -- , module Network.AWS.Env
    -- , module Network.AWS.Auth
    -- , module Network.AWS.Logger
    -- , module Network.AWS.Internal.Body

    -- -- * Errors
    -- -- ** General
    -- , AWSError     (..)
    -- , Error

    -- -- ** Service specific errors
    -- , ServiceError
    -- , errorService
    -- , errorStatus
    -- , errorHeaders
    -- , errorCode
    -- , errorMessage
    -- , errorRequestId

    -- -- ** Catching errors
    -- , catching

    -- -- ** Hoisting errors
    -- , throwing
    -- , hoistError

    -- -- ** Error types
    -- , ErrorCode    (..)
    -- , ErrorMessage (..)
    -- , RequestId    (..)

    -- -- * Types
    -- , module Network.AWS.Types
    -- -- ** Seconds
    -- , Seconds      (..)
    -- , _Seconds
    -- , microseconds

    -- -- * Serialisation
    -- -- ** Text
    -- , ToText       (..)
    -- , FromText     (..)
    -- -- ** Log messages
    -- , ToBuilder    (..)
    -- ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Error.Lens     (catching, throwing)
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.Conduit
import qualified Data.Conduit.List            as Conduit
import           Network.AWS.Auth
import           Network.AWS.Data.Time
import           Network.AWS.Env
import           Network.AWS.Error
import           Network.AWS.Free
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
-- FIXME: Maybe a .Tutorial module?
-- FIXME: Philosophical notes on the use of lenses, and notes about template-haskell usage.
-- FIXME: Notes about associated response types, signers, service configuration.
-- FIXME: {-# OPTIONS_HADDOCK show-extensions #-} everywhere?
-- FIXME: Correct haddock module headings.
-- FIXME: Remove personal email address.
-- FIXME: Note/example about mocking.

type AWST m = ProgramT (ExceptT Error (ReaderT Env m))

runAWST :: (MonadCatch m, MonadResource m)
        => Env
        -> AWST m a
        -> m (Either Error a)
runAWST e m = runReaderT (runExceptT (evalProgramT m)) e

pureAWST :: Monad m
         => (forall s a. Service s ->           a -> Either Error (Rs a))
         -> (forall s a. Service s -> Wait a -> a -> Either Error (Rs a))
         -> Env
         -> AWST m b
         -> m (Either Error b)
pureAWST f g e m = runReaderT (runExceptT (pureProgramT f g m)) e

-- | Scope an action within the specific 'Region'.
within :: (MonadReader r m, AWSEnv r) => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: (MonadReader r m, AWSEnv r) => m a -> m a
once = local $ \e -> e
    & envRetryPolicy ?~ limitRetries 0
    & envRetryCheck  .~ (\_ _ -> return False)

-- | Scope an action such that any HTTP response use this timeout value.
timeout :: (MonadReader r m, AWSEnv r) => Seconds -> m a -> m a
timeout s = local (envTimeout ?~ s)

hoistError :: (MonadError e m, AWSError e) => Either Error a -> m a
hoistError = either (throwing _Error) pure

send :: ( MonadFree Command   m
        , MonadError        e m
        , AWSError          e
        , AWSRequest        a
        )
     => a
     -> m (Rs a)
send = serviceFor sendWith

sendWith :: ( MonadFree Command   m
            , MonadError        e m
            , AWSError          e
            , AWSSigner         (Sg s)
            , AWSRequest        a
            )
         => Service s
         -> a
         -> m (Rs a)
sendWith s = sendWithF s >=> hoistError

paginate :: ( MonadFree Command   m
            , MonadError        e m
            , AWSError          e
            , AWSPager          a
            )
         => a
         -> Source m (Rs a)
paginate = serviceFor paginateWith

paginateWith :: ( MonadFree Command   m
                , MonadError        e m
                , AWSError          e
                , AWSSigner         (Sg s)
                , AWSPager          a
                )
             => Service s
             -> a
             -> Source m (Rs a)
paginateWith s x = paginateWithF s x =$= Conduit.mapM hoistError

await :: ( MonadFree Command   m
         , MonadError        e m
         , AWSError          e
         , AWSRequest        a
         )
      => Wait a
      -> a
      -> m (Rs a)
await w = serviceFor (flip awaitWith w)

awaitWith :: ( MonadFree Command   m
             , MonadError        e m
             , AWSError          e
             , AWSSigner         (Sg s)
             , AWSRequest        a
             )
          => Service s
          -> Wait a
          -> a
          -> m (Rs a)
awaitWith s w = awaitWithF s w >=> hoistError

--await
-- awaitWith

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
