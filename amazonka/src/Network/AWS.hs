{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Network.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides a simple 'AWS' monad and a set of operations which
-- can be performed against remote Amazon Web Services APIs, for use with the types
-- supplied by the various @amazonka-*@ libraries.
--
-- A 'MonadAWS' typeclass is used as a function constraint to provide automatic
-- lifting of functions when embedding 'AWS' as a layer inside your own
-- application stack.
--
-- "Control.Monad.Trans.AWS" contains the underlying 'AWST' transformer.
module Network.AWS
    (
    -- * Usage
    -- $usage

    -- * Running AWS Actions
      AWS
    , MonadAWS    (..)
    , runAWS

    -- * Authentication and Environment
    , newEnv
    , Env
    , HasEnv      (..)

    -- ** Credential Discovery
    , Credentials (..)

    -- ** Supported Regions
    , Region      (..)

    -- * Sending Requests
    -- ** Send, Paginate and Await
    , send
    , paginate
    , await

    -- ** Presigning
    , presignURL

    -- ** EC2 Instance Metadata
    , isEC2
    , dynamic
    , metadata
    , userdata

    , EC2.Dynamic  (..)
    , EC2.Metadata (..)

    -- ** Overriding Service Configuration
    -- $service

    -- *** Scoped Actions
    , within
    , once
    , timeout

    -- ** Running Asynchronous Actions
    -- $async

    -- ** Streaming
    -- $streaming

    -- *** Request Bodies
    , ToBody      (..)
    , sourceBody
    , sourceHandle
    , sourceFile
    , sourceFileIO

    -- *** Response Bodies
    , sinkBody

    -- *** Calculating File Size and SHA256
    , getFileSize
    , sinkHash

    -- * Handling Errors
    -- $errors

    , AsError     (..)
    , AsAuthError (..)

    , AWST.trying
    , AWST.catching

    -- * Logging
    -- $logging

    , Logger

    -- ** Constructing a Logger
    , newLogger

    -- ** Emitting Log Messages
    , logError
    , logInfo
    , logDebug
    , logTrace

    , ToLog       (..)

    -- * Re-exported Types
    , module Network.AWS.Types
    , AWST.RqBody
    , AWST.RsBody
    ) where

import           Control.Applicative
import           Control.Monad.Catch             (MonadCatch)
import           Control.Monad.Morph             (hoist)
import qualified Control.Monad.RWS.Lazy          as LRW
import qualified Control.Monad.RWS.Strict        as RW
import qualified Control.Monad.State.Lazy        as LS
import qualified Control.Monad.State.Strict      as S
import           Control.Monad.Trans.AWS         (AWST)
import qualified Control.Monad.Trans.AWS         as AWST
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Cont        (ContT)
import           Control.Monad.Trans.Except      (ExceptT)
import           Control.Monad.Trans.Free        (FreeT)
import           Control.Monad.Trans.Free.Church (FT)
import           Control.Monad.Trans.Identity    (IdentityT)
import           Control.Monad.Trans.Iter        (IterT)
import           Control.Monad.Trans.List        (ListT)
import           Control.Monad.Trans.Maybe       (MaybeT)
import           Control.Monad.Trans.Reader      (ReaderT)
import           Control.Monad.Trans.Resource
import qualified Control.Monad.Writer.Lazy       as LW
import qualified Control.Monad.Writer.Strict     as W
import           Data.Conduit                    (Source)
import           Data.Monoid
import           Network.AWS.Auth
import qualified Network.AWS.EC2.Metadata        as EC2
import           Network.AWS.Env                 (Env, HasEnv (..), newEnv)
import           Network.AWS.Internal.Body
import           Network.AWS.Logger
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Types
import           Network.AWS.Waiter

import           Prelude

-- | A specialisation of the 'AWST' transformer.
type AWS = AWST IO

-- | Monads in which 'AWS' actions may be embedded.
class (Functor m, Applicative m, Monad m) => MonadAWS m where
    -- | Lift a computation to the 'AWS' monad.
    liftAWS :: AWS a -> m a

instance MonadAWS AWS where
    liftAWS = id

instance MonadAWS m => MonadAWS (IdentityT   m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ListT       m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (MaybeT      m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ExceptT   e m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ContT     r m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ReaderT   r m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (S.StateT  s m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (LS.StateT s m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (IterT       m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (FT        f m) where liftAWS = lift . liftAWS

instance (Monoid w, MonadAWS m) => MonadAWS (W.WriterT w m) where
    liftAWS = lift . liftAWS

instance (Monoid w, MonadAWS m) => MonadAWS (LW.WriterT w m) where
    liftAWS = lift . liftAWS

instance (Monoid w, MonadAWS m) => MonadAWS (RW.RWST r w s m) where
    liftAWS = lift . liftAWS

instance (Monoid w, MonadAWS m) => MonadAWS (LRW.RWST r w s m) where
    liftAWS = lift . liftAWS

instance (Functor f, MonadAWS m) => MonadAWS (FreeT f m) where
    liftAWS = lift . liftAWS

-- FIXME: verify the use of withInternalState to create a ResourceT here

-- | Run the 'AWS' monad. Any outstanding HTTP responses' 'ResumableSource' will
-- be closed when the 'ResourceT' computation is unwrapped with 'runResourceT'.
--
-- Throws 'Error', which will include 'HTTPExceptions', serialisation errors,
-- or any particular errors returned by the respective AWS service.
--
-- /See:/ 'runAWST', 'runResourceT'.
runAWS :: (MonadCatch m, MonadResource m, HasEnv r) => r -> AWS a -> m a
runAWS e = liftResourceT . AWST.runAWST e . hoist (withInternalState . const)

-- | Run any remote requests against the specified 'Region'.
within :: MonadAWS m => Region -> AWS a -> m a
within r = liftAWS . AWST.within r

-- | Ignore any retry logic and ensure that any requests will be sent (at most) once.
once :: MonadAWS m => AWS a -> m a
once = liftAWS . AWST.once

-- | Configure any HTTP connections to use this response timeout value.
timeout :: MonadAWS m => Seconds -> AWS a -> m a
timeout s = liftAWS . AWST.timeout s

-- | Send a request, returning the associated response if successful,
-- otherwise an 'Error' will be thrown.
--
-- /See:/ 'AWST.sendWith'
send :: (MonadAWS m, AWSRequest a) => a -> m (Rs a)
send = liftAWS . AWST.send

-- | Repeatedly send a request, automatically setting markers and
-- paginating over multiple responses while available.
--
-- Requests that can potentially return multiple pages of results are instances
-- of 'AWSPager',
--
-- /See:/ 'AWST.paginateWith'
paginate :: (MonadAWS m, AWSPager a) => a -> Source m (Rs a)
paginate = hoist liftAWS . AWST.paginate

-- | Poll the API with the supplied request until a specific 'Wait' condition
-- is fulfilled.
--
-- The response will be either the first error returned that is not handled
-- by the specification, or any subsequent successful response from the await
-- request(s).
--
-- /Note:/ You can find any available 'Wait' specifications under then
-- @Network.AWS.{ServiceName}.Waiters@ namespace for supported services.
--
-- /See:/ 'AWST.awaitWith'
await :: (MonadAWS m, AWSRequest a) => Wait a -> a -> m (Rs a)
await w = liftAWS . AWST.await w

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'AWST.presign', 'AWST.presignWith'
presignURL :: (MonadAWS m, AWSPresigner (Sg (Sv a)), AWSRequest a)
           => UTCTime     -- ^ Signing time.
           -> Seconds     -- ^ Expiry time.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL t ex = liftAWS . AWST.presignURL t ex

-- | Test whether the underlying host is running on EC2.
--
-- This function is memoised.
isEC2 :: MonadAWS m => m Bool
isEC2 = liftAWS AWST.isEC2

-- | Retrieve the specified 'Dynamic' data.
dynamic :: MonadAWS m => EC2.Dynamic -> m ByteString
dynamic = liftAWS . AWST.dynamic

-- | Retrieve the specified 'Metadata'.
metadata :: MonadAWS m => EC2.Metadata -> m ByteString
metadata = liftAWS . AWST.metadata

-- | Retrieve the user data. Returns 'Nothing' if no user data is assigned
-- to the instance.
userdata :: MonadAWS m => m (Maybe ByteString)
userdata = liftAWS AWST.userdata

{- $usage
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

example :: IO PutObjectResponse
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

{- $service
When a request is sent, various configuration values such as the endpoint,
retry strategy, timeout and error handlers are taken from the associated 'Service'
configuration.

You can override the default configuration for a series of one or more actions
by using 'within', 'once' and 'timeout', or by using the @*With@ suffixed
functions on an individual request basis below.
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

{- $errors
Errors are thrown by the library using 'MonadThrow' (unless "Control.Monad.Error.AWS" is used).
Sub-errors of the canonical 'Error' type can be caught using 'trying' or
'catching' and the appropriate 'AsError' 'Prism':

@
trying '_Error'          (send $ ListObjects "bucket-name") :: Either 'Error'          ListObjectsResponse
trying '_TransportError' (send $ ListObjects "bucket-name") :: Either 'HttpException'  ListObjectsResponse
trying '_SerializeError' (send $ ListObjects "bucket-name") :: Either 'SerializeError' ListObjectsResponse
trying '_ServiceError'   (send $ ListObjects "bucket-name") :: Either 'ServiceError'   ListObjectsResponse
@
-}

{- $streaming
Streaming request bodies (such as 'PutObject') require a precomputed
'SHA256' for signing purposes.

The 'ToBody' typeclass has instances available to construct a 'RqBody',
automatically calculating the hash as needed for types such as 'Text' and 'ByteString'.

For reading files and handles, functions such 'sourceFileIO' or 'sourceHandle'
can be used.
-}

{- $logging
The exposed logging interface is a primitive 'Logger' function which gets
threaded through service calls and serialisation routines.

The 'newLogger' function can be used to construct a simple logger which writes
output to a 'Handle', but in most production code you should probably consider
using a more robust logging library such as
<http://hackage.haskell.org/package/tiny-log tiny-log> or
<http://hackage.haskell.org/package/fast-logger fast-logger>.
-}
