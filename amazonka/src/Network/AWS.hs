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
module Network.AWS
    (
    -- * Usage
    -- $usage

    -- * Running AWS Actions
      AWS
    , MonadAWS    (..)
    -- $embed
    , runAWS

    -- * Environment Setup
    , Auth.Credentials (..)
    , Env
    , Env.newEnv

    -- * Runtime Configuration
    , within
    , once
    , timeout

    -- * Requests
    -- ** Synchronous
    , send
    , await
    , paginate
    -- ** Asynchronous
    -- $async
    -- ** Streaming
    , module Network.AWS.Internal.Body

    -- * Errors
    , AWSError    (..)
    , Error

    -- ** Service Errors
    , ServiceError
    , errorService
    , errorStatus
    , errorHeaders
    , errorCode
    , errorMessage
    , errorRequestId

    -- * Units
    , Seconds     (..)

    -- * Re-exported Modules
    , module Network.AWS.Logger
    , module Network.AWS.Types
    ) where

import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy     as LS
import qualified Control.Monad.State.Strict   as S
import           Control.Monad.Trans.AWS      (AWST, runAWST)
import qualified Control.Monad.Trans.AWS      as AWST
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Conduit                 hiding (await)
import qualified Network.AWS.Auth             as Auth
import           Network.AWS.Data.Time
import           Network.AWS.Env              (Env)
import qualified Network.AWS.Env              as Env
import           Network.AWS.Error
import           Network.AWS.Free
import           Network.AWS.Internal.Body
import           Network.AWS.Logger
import           Network.AWS.Pager
import           Network.AWS.Types
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

{- $usage
This module provides a simple 'AWS' monad and a set of common operations which
can be performed against remote Amazon Web Services APIs, for use with the types
supplied by the various @amazonka-*@ libraries.

The key functions dealing with the request/response lifecycle are:

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

{- $embed
'MonadAWS' can be used to embed 'AWS' actions inside your own transformer stack.

For a trivial base monad:

> newtype MyApp a = MyApp (ReaderT MyEnv AWS a)
>     deriving (Monad, MonadTrans, ...)

You can define a 'MonadAWS' instance as follows:

> instance MonadAWS MyApp where
>     liftAWS = MyApp . lift

This instance allows all of the functions in this module to operate over your
own monad without having to lift/run any AWS specific operations.

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

/See:/ <http://hackage.haskell.org/package/lifted-async Control.Concurrent.Async.Lifted>
-}
