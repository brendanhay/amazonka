{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
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
    , HasEnv       (..)

    -- ** Credential Discovery
    , Credentials  (..)
    -- $discovery

    -- ** Supported Regions
    , Region       (..)

    -- * Sending Requests
    -- $sending

    , send

    -- ** Pagination
    -- $pagination

    , paginate

    -- ** Waiters
    -- $waiters

    , await

    -- ** Overriding Service Configuration
    -- $service

    , within
    , once
    , timeout
    , endpoint
    , signer

    -- ** Environment Overrides
    , Env.setRetry
    , Env.setTimeout
    , Env.setEndpoint
    , Env.setSigner

    -- ** Streaming
    -- $streaming

    -- *** Request Bodies
    , ToBody       (..)
    , sourceBody
    , sourceHandle
    , sourceFile
    , sourceFileIO

    -- *** Response Bodies
    , sinkBody

    -- *** File Size and MD5/SHA256
    , getFileSize
    , sinkMD5
    , sinkSHA256

    -- * Presigning Requests
    -- $presigning

    , presignURL

    -- * EC2 Instance Metadata
    -- $metadata

    , isEC2
    , dynamic
    , metadata
    , userdata

    , EC2.Dynamic  (..)
    , EC2.Metadata (..)

    -- * Running Asynchronous Actions
    -- $async

    -- * Handling Errors
    -- $errors

    , AsError      (..)
    , AsAuthError  (..)

    , AWST.trying
    , AWST.catching

    -- * Logging
    -- $logging

    , Logger
    , LogLevel     (..)

    -- ** Constructing a Logger
    , newLogger

    -- * Re-exported Types
    , RqBody
    , RsBody
    , module Network.AWS.Types
    , module Network.AWS.Waiter
    , module Network.AWS.Pager

    -- * runResourceT
    , runResourceT
    ) where

import           Control.Applicative
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Morph          (hoist)
import qualified Control.Monad.RWS.Lazy       as LRW
import qualified Control.Monad.RWS.Strict     as RW
import qualified Control.Monad.State.Lazy     as LS
import qualified Control.Monad.State.Strict   as S
import           Control.Monad.Trans.AWS      (AWST)
import qualified Control.Monad.Trans.AWS      as AWST
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.List     (ListT)
import           Control.Monad.Trans.Maybe    (MaybeT)
import           Control.Monad.Trans.Reader   (ReaderT)
import           Control.Monad.Trans.Resource
import qualified Control.Monad.Writer.Lazy    as LW
import qualified Control.Monad.Writer.Strict  as W
import           Data.Conduit                 (Source)
import           Data.Monoid
import           Network.AWS.Auth
import qualified Network.AWS.EC2.Metadata     as EC2
import           Network.AWS.Env              (Env, HasEnv (..), newEnv)
import qualified Network.AWS.Env              as Env
import           Network.AWS.Internal.Body
import           Network.AWS.Internal.Logger
import           Network.AWS.Pager            (AWSPager)
import           Network.AWS.Prelude
import           Network.AWS.Types            hiding (LogLevel (..))
import           Network.AWS.Waiter           (Wait)

import           Prelude

-- | A specialisation of the 'AWST' transformer.
type AWS = AWST (ResourceT IO)

-- | Monads in which 'AWS' actions may be embedded.
class (Functor m, Applicative m, Monad m, MonadIO m, MonadCatch m) => MonadAWS m
  where
    -- | Lift a computation to the 'AWS' monad.
    liftAWS :: AWS a -> m a

instance MonadAWS AWS where
    liftAWS = id

instance MonadAWS m => MonadAWS (IdentityT   m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ListT       m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (MaybeT      m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ExceptT   e m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (ReaderT   r m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (S.StateT  s m) where liftAWS = lift . liftAWS
instance MonadAWS m => MonadAWS (LS.StateT s m) where liftAWS = lift . liftAWS

instance (Monoid w, MonadAWS m) => MonadAWS (W.WriterT w m) where
    liftAWS = lift . liftAWS

instance (Monoid w, MonadAWS m) => MonadAWS (LW.WriterT w m) where
    liftAWS = lift . liftAWS

instance (Monoid w, MonadAWS m) => MonadAWS (RW.RWST r w s m) where
    liftAWS = lift . liftAWS

instance (Monoid w, MonadAWS m) => MonadAWS (LRW.RWST r w s m) where
    liftAWS = lift . liftAWS

-- | Run the 'AWS' monad. Any outstanding HTTP responses' 'ResumableSource' will
-- be closed when the 'ResourceT' computation is unwrapped with 'runResourceT'.
--
-- Throws 'Error', which will include 'HTTPExceptions', serialisation errors,
-- or any particular errors returned by the respective AWS service.
--
-- /See:/ 'AWST.runAWST', 'runResourceT'.
runAWS :: (MonadResource m, HasEnv r) => r -> AWS a -> m a
runAWS e = liftResourceT . AWST.runAWST e

-- | Scope an action within the specific 'Region'.
within :: MonadAWS m => Region -> AWS a -> m a
within r = liftAWS . AWST.within r

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: MonadAWS m => AWS a -> m a
once = liftAWS . AWST.once

-- | Scope an action such that any HTTP response will use this timeout value.
timeout :: MonadAWS m => Seconds -> AWS a -> m a
timeout s = liftAWS . AWST.timeout s

-- | Scope an action such that any HTTP requests and signing logic use
-- a modified endpoint.
endpoint :: MonadAWS m => (Endpoint -> Endpoint) -> AWS a -> m a
endpoint f = liftAWS . AWST.endpoint f

-- | Scope an action such that the specified signing algorithm is used.
signer :: MonadAWS m => Signer -> AWS a -> m a
signer v = liftAWS . AWST.signer v

-- | Send a request, returning the associated response if successful.
send :: (MonadAWS m, AWSRequest a) => a -> m (Rs a)
send = liftAWS . AWST.send

-- | Repeatedly send a request, automatically setting markers and
-- paginating over multiple responses while available.
paginate :: (MonadAWS m, AWSPager a) => a -> Source m (Rs a)
paginate = hoist liftAWS . AWST.paginate

-- | Poll the API with the supplied request until a specific 'Wait' condition
-- is fulfilled.
await :: (MonadAWS m, AWSRequest a) => Wait a -> a -> m ()
await w = liftAWS . AWST.await w

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
presignURL :: (MonadAWS m, AWSRequest a)
           => UTCTime     -- ^ Signing time.
           -> Seconds     -- ^ Expiry time.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL t ex = liftAWS . AWST.presignURL t ex

-- | Test whether the underlying host is running on EC2.
-- This is memoised and an HTTP request is made to the host's metadata
-- endpoint for the first call only.
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

{- $discovery
AuthN/AuthZ information is handled similarly to other AWS SDKs. You can read
some of the options available <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs here>.

When running on an EC2 instance and using 'FromProfile' or 'Discover', a thread
is forked which transparently handles the expiry and subsequent refresh of IAM
profile information. See 'Network.AWS.Auth.fromProfileName' for more information.
-}

{- $sending
To send a request you need to create a value of the desired operation type using
the relevant constructor, as well as any further modifications of default/optional
parameters using the appropriate lenses. This value can then be sent using 'send'
or 'paginate' and the library will take care of serialisation/authentication and
so forth.

The default 'Service' configuration for a request contains retry configuration that is used to
determine if a request can safely be retried and what kind of back off/on strategy
should be used. (Usually exponential.)
Typically services define retry strategies that handle throttling, general server
errors and transport errors. Streaming requests are never retried.
-}

{- $pagination
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
Error conditions that are not handled by the 'Wait' configuration will be thrown,
or the first successful response that fulfills the success condition will be
returned.

'Wait' specifications can be found under the @Network.AWS.{ServiceName}.Waiters@
namespace for services which support 'await'.
-}

{- $service
When a request is sent, various values such as the endpoint,
retry strategy, timeout and error handlers are taken from the associated 'Service'
for a request. For example, 'DynamoDB' will use the 'Network.AWS.DynamoDB.dynamoDB'
configuration for 'PutItem', 'Query' etc.

You can apply overrides to all 'Service' configurations with two different
mechanisms. The setter functions such as 'setEndpoint', 'setTimeout' etc.
are used to wholly override all configurations before use and the
'local' functions such as 'within', 'once', 'timeout' etc. modify all configurations
for requests within scope.

An example of how you might use these overrides is demonstrated using
'setEndpoint' vs 'endpoint' to create environment which communicates with a
local service, instead of the remote AWS APIs.

Firsly, a function with which to modify the 'Service' 'Endpoint' is defined:

> let local :: Endpoint -> Endpoint
>     local = (endpointHost   .~ "localhost")
>           . (endpointPort   .~ 8000)
>           . (endpointSecure .~ False)

then, setting the initial environment using 'setEndpoint':

> e <- newEnv Frankfurt Discover <&> setEndpoint local
> runAWS e $ do
>     -- Any service calls here will _always_ communicate with localhost:8000.
>     ...

versus scoping the 'endpoint' modifications:

> e <- newEnv Frankfurt Discover
> runAWS e $ do
>     -- Service calls here will comminucate with remote AWS APIs.
>     ...
>     endpoint local $ do
>        -- Any service calls here will communicate with localhost:8000.
>        ...

/Note:/ Functions such as 'setSigner' and 'signer' should be used with the
various AWS constraints in mind. For example: if you're operating in @eu-central-1@ which
only supports Version 4 signing and you use 'setSigner' 'v2' ..., you'll only
receive signing failures for responses.
-}

{- $streaming
Streaming request bodies (such as 'PutObject') require a precomputed
'SHA256' for signing purposes.
The 'ToBody' typeclass has instances available to construct a 'RqBody',
automatically calculating the hash as needed for types such as 'Text' and 'ByteString'.

For reading files and handles, functions such 'sourceFileIO' or 'sourceHandle'
can be used.
For responses that contain streaming bodies (such as 'GetObject'), you can use
'sinkBody' to connect the response body to a <http://hackage.haskell.org/package/conduit conduit>
compatible sink.
-}

{- $presigning
Presigning requires the 'Service' signer to be an instance of 'AWSPresigner'.
Not all signing algorithms support this.
-}

{- $metadata
Metadata can be retrieved from the underlying host assuming that you're running
the code on an EC2 instance or have a compatible @instance-data@ endpoint available.
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

Many of the individual @amazonka-*@ libraries export compatible 'Getter's for
matching service specific error codes and messages in the style above.
See the @Error Matchers@ heading in each respective library for details.
-}

{- $logging
The exposed logging interface is a primitive 'Logger' function which gets
threaded through service calls and serialisation routines. This allows the
library to output useful information and diagnostics.

The 'newLogger' function can be used to construct a simple logger which writes
output to a 'Handle', but in most production code you should probably consider
using a more robust logging library such as
<http://hackage.haskell.org/package/tiny-log tiny-log> or
<http://hackage.haskell.org/package/fast-logger fast-logger>.
-}
