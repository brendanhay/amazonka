{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Control.Monad.Trans.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : MPL 2.0
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The core module for making requests to the various AWS services and
-- building your own Monad transformer stack.
module Control.Monad.Trans.AWS
    (
    -- * Usage
    -- $usage
    -- $embedding

    -- * Monad constraints
    -- $constraints
      MonadAWS
    -- ** AWST
    , AWST
    , runAWST
    -- ** AWS
    , AWS
    , runAWS

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
    -- ** Seconds
    , Seconds      (..)
    , _Seconds
    , microseconds

    -- * Serialisation
    -- ** Text
    , ToText       (..)
    , FromText     (..)
    -- ** Log messages
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
import           Network.AWS.Types            hiding ()
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
-- FIXME: Correct haddock module headings
-- FIXME: Remove personal email address

{- $usage
This modules provides a set of common operations against the remote
Amazon Web Services APIs for the various @amazonka-*@ libraries.

The key functions dealing with the request/response life cycle are:

* 'send'

* 'paginate'

* 'await'

To utilise these, you will need to specify what 'Region' you wish to operate in
and your Amazon credentials for AuthN/AuthZ purposes.

'Credentials' can be supplied in a number of ways. Either via explicit keys,
via session profiles, or have Amazonka determine the credentials from an
underlying IAM Role/Profile.

As a basic example, you might wish to store an object in an S3 bucket using
<http://hackage.haskell.org/package/amazonka-s3 amazonka-s3>:

@
import Control.Lens
import Network.AWS
import Network.AWS.S3
import System.IO

example :: IO (Either Error PutObjectResponse)
example = do
    -- To specify configuration preferences, 'newEnv' is used to create a new 'Env'. The 'Region' denotes the AWS region requests will be performed against,
    -- and 'Credentials' is used to specify the desired mechanism for supplying or retrieving AuthN/AuthZ information.
    -- In this case, 'Discover' will cause the library to try a number of options such as default environment variables, or an instance's IAM Profile:
    e <- newEnv Frankfurt Discover

    -- A new 'Logger' to replace the default noop logger is created, with the logger set to print debug information and errors to stdout:
    l <- newLogger Debug stdout

    -- The payload (and hash) for the S3 object is retrieved from a FilePath:
    b <- sourceFileIO "local\/path\/to\/object-payload"

    -- We now run the AWS computation with the overriden logger, performing the PutObject request:
    runAWS (e & envLogger .~ l) $
        send (putObject "bucket-name" "object-key" b)
@
-}

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
>     , _env    :: Env ^ Here the AWS environment is embedded.
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
>     | AmazonError    Error ^ Here the AWS error is embedded.

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
> import Network.AWS
> import Network.AWS.S3
>
> do x   <- async . send $ getObject "bucket" "prefix/object-foo"
>    y   <- async . send $ getObject "bucket" "prefix/object-bar"
>    foo <- wait x
>    bar <- wait y
>    ...

/See:/ <http://hackage.haskell.org/package/async Control.Concurrent.Async> and <http://hackage.haskell.org/package/lifted-async Control.Concurrent.Async.Lifted>
-}

{- $constraints
The function signatures in this module specify constraints using <http://hackage.haskell.org/package/mtl mtl>
classes in order to keep assumptions as general as possible.  In fact, 'AWST' and 'AWS' are simply
type aliases representing potential specialisations of 'MonadAWS'. All functions
in this module will specialise to your application stack if it also fulfils these
constraints, making it easy to embed any AWS related computation in your application.
An extended example is provided in #usage.

The two core constraints that you will frequently see are:

For some environment 'r', a 'Lens' is provided by 'AWSEnv' 'r' to obtain the AWS specific 'Env' contained in 'r':

@
(MonadReader r m, AWSEnv r)
@

and for some error 'e', a 'Prism' is provided to de/construct the AWS specific 'Error' within 'e':

@
(MonadError e m, AWSEnv e)
@
-}

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
