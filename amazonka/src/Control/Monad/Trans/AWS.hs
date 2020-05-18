{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Control.Monad.Trans.AWS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- The 'AWST' transformer provides the environment required to perform AWS
-- operations. The transformer is intended to be used directly
-- or embedded as a layer within a transformer stack.
--
-- "Network.AWS" contains an 'IO' specialised version of 'AWST' with a typeclass
-- to assist in automatically lifting operations.
module Control.Monad.Trans.AWS
    (
    -- * Running AWS Actions
      AWST
    , AWST'
    , runAWST
    , runResourceT
    , AWSConstraint

    -- * Authentication and Environment
    , newEnv
    , Env
    , HasEnv       (..)
    , askEnv

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

    -- ** Service Configuration
    -- $service

    -- *** Overriding Defaults
    , configure
    , override

    -- *** Scoped Actions
    , reconfigure
    , within
    , once
    , timeout

    -- ** Streaming
    -- $streaming

    -- *** Request Bodies
    , ToHashedBody (..)
    , hashedFile
    , hashedBody

    -- *** Chunked Request Bodies
    , ToBody       (..)
    , ChunkSize    (..)
    , defaultChunkSize
    , chunkedFile
    , unsafeChunkedBody

    -- *** Response Bodies
    , sinkBody

    -- *** File Size and MD5/SHA256
    , getFileSize
    , sinkMD5
    , sinkSHA256

    -- * Presigning Requests
    -- $presigning

    , presignURL
    , presign

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

    , trying
    , catching

    -- ** Building Error Prisms
    , Error._MatchServiceError
    , Error.hasService
    , Error.hasStatus
    , Error.hasCode

    -- * Logging
    -- $logging

    , Logger
    , LogLevel     (..)

    -- ** Constructing a Logger
    , newLogger

    -- ** Endpoints
    , Endpoint
    , setEndpoint

    -- * Re-exported Types
    , module Network.AWS.Types
    , module Network.AWS.Waiter
    , module Network.AWS.Pager
    , RqBody
    , HashedBody
    , ChunkedBody
    , RsBody
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class    (MonadError (..))
import Control.Monad.IO.Unlift
import Control.Monad.Morph
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Writer.Class
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
#endif

import Data.Conduit      hiding (await)
import Data.Conduit.Lazy (MonadActive (..))
import Data.IORef
import Data.Monoid

import Network.AWS.Auth
import Network.AWS.Env
import Network.AWS.Internal.Body
import Network.AWS.Internal.HTTP
import Network.AWS.Internal.Logger
import Network.AWS.Lens            (catching, throwingM, trying, view, (^.))
import Network.AWS.Pager           (AWSPager (..))
import Network.AWS.Prelude         as AWS
import Network.AWS.Request         (requestURL)
import Network.AWS.Types           hiding (LogLevel (..))
import Network.AWS.Waiter          (Accept, Wait)

import qualified Network.AWS.EC2.Metadata as EC2
import qualified Network.AWS.Error        as Error
import qualified Network.AWS.Presign      as Sign

type AWST = AWST' Env

newtype AWST' r m a = AWST' { unAWST :: ReaderT r m a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadIO
        , MonadActive
        , MonadTrans
#if MIN_VERSION_base(4,9,0)
        , MonadFail
#endif
        )

instance MonadThrow m => MonadThrow (AWST' r m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (AWST' r m) where
    catch (AWST' m) f = AWST' (catch m (unAWST . f))

instance MonadMask m => MonadMask (AWST' r m) where
    mask a = AWST' $ mask $ \u ->
        unAWST $ a (AWST' . u . unAWST)

    uninterruptibleMask a = AWST' $ uninterruptibleMask $ \u ->
        unAWST $ a (AWST' . u . unAWST)

#if MIN_VERSION_exceptions(0,10,0)
    generalBracket acquire rel action = AWST' $
        generalBracket
            (unAWST acquire)
            (\a ex -> unAWST $ rel a ex)
            (\a -> unAWST $ action a)
#endif


instance MonadBase b m => MonadBase b (AWST' r m) where
    liftBase = liftBaseDefault

instance MonadTransControl (AWST' r) where
    type StT (AWST' r) a = StT (ReaderT r) a

    liftWith = defaultLiftWith AWST' unAWST
    restoreT = defaultRestoreT AWST'

instance MonadBaseControl b m => MonadBaseControl b (AWST' r m) where
    type StM (AWST' r m) a = ComposeSt (AWST' r) m a

    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

instance MonadUnliftIO m => MonadUnliftIO (AWST' r m) where
#if MIN_VERSION_unliftio_core(0,2,0)
    {-# INLINE withRunInIO #-}
    withRunInIO inner =
        AWST' $
        withRunInIO $ \run ->
        inner (run . unAWST)
#else
    {-# INLINE askUnliftIO #-}
    askUnliftIO = AWST' $ (\(UnliftIO f) -> UnliftIO $ f . unAWST)
        <$> askUnliftIO
#endif

instance MonadResource m => MonadResource (AWST' r m) where
    liftResourceT = lift . liftResourceT

instance MonadError e m => MonadError e (AWST' r m) where
    throwError     = lift . throwError
    catchError m f = AWST' (unAWST m `catchError` (unAWST . f))

instance Monad m => MonadReader r (AWST' r m) where
    ask     = AWST' ask
    local f = AWST' . local f . unAWST
    reader  = AWST' . reader

instance MonadWriter w m => MonadWriter w (AWST' r m) where
    writer = lift . writer
    tell   = lift . tell
    listen = AWST' . listen . unAWST
    pass   = AWST' . pass   . unAWST

instance MonadState s m => MonadState s (AWST' r m) where
    get = lift get
    put = lift . put

instance MFunctor (AWST' r) where
    hoist nat = AWST' . hoist nat . unAWST

instance PrimMonad m => PrimMonad (AWST' r m) where
    type PrimState (AWST' r m) = PrimState m
    primitive = AWST' . primitive

-- | Run an 'AWST' action with the specified environment.
runAWST :: HasEnv r => r -> AWST' r m a -> m a
runAWST r (AWST' m) = runReaderT m r

askEnv :: (Monad m, HasEnv r) => AWST' r m Env
askEnv = AWST' (asks (^. environment))

-- | An alias for the constraints required to send requests,
-- which 'AWST' implicitly fulfils.
type AWSConstraint r m =
    ( MonadThrow     m
    , MonadCatch     m
    , MonadResource  m
    , MonadReader  r m
    , HasEnv       r
    )

-- | Send a request, returning the associated response if successful.
--
-- Throws 'Error'.
send :: (AWSConstraint r m, AWSRequest a)
     => a
     -> m (Rs a)
send = retrier >=> fmap snd . hoistError

-- | Repeatedly send a request, automatically setting markers and
-- paginating over multiple responses while available.
--
-- Throws 'Error'.
paginate :: (AWSConstraint r m, AWSPager a)
         => a
         -> ConduitM () (Rs a) m ()
paginate = go
  where
    go !x = do
        !y <- lift $ send x
        yield y
        maybe (pure ()) go (page x y)

-- | Poll the API with the supplied request until a specific 'Wait' condition
-- is fulfilled.
--
-- Throws 'Error'.
await :: (AWSConstraint r m, AWSRequest a)
      => Wait a
      -> a
      -> m Accept
await w = waiter w >=> hoistError

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
presignURL :: ( MonadIO m
              , MonadReader r m
              , HasEnv r
              , AWSRequest a
              )
           => UTCTime     -- ^ Signing time.
           -> Seconds     -- ^ Expiry time.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL ts ex = liftM requestURL . presign ts ex

-- | Presign an HTTP request that is valid from the specified time until the
-- number of seconds expiry has elapsed.
presign :: ( MonadIO m
           , MonadReader r m
           , HasEnv r
           , AWSRequest a
           )
        => UTCTime     -- ^ Signing time.
        -> Seconds     -- ^ Expiry time.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign ts ex x = do
    Env{..} <- view environment
    Sign.presignWith (appEndo (getDual _envOverride)) _envAuth _envRegion ts ex x

-- | Test whether the underlying host is running on EC2.
-- This is memoised and any external check occurs for the first invocation only.
isEC2 :: (MonadIO m, MonadReader r m, HasEnv r) => m Bool
isEC2 = do
    ref <- view envEC2
    mp  <- liftIO (readIORef ref)
    case mp of
        Just p  -> return p
        Nothing -> do
            m  <- view envManager
            !p <- EC2.isEC2 m
            liftIO (atomicWriteIORef ref (Just p))
            return p

-- | Retrieve the specified 'Dynamic' data.
--
-- Throws 'HttpException'.
dynamic :: (MonadIO m, MonadThrow m, MonadReader r m, HasEnv r)
        => EC2.Dynamic
        -> m ByteString
dynamic d = view envManager >>= flip EC2.dynamic d

-- | Retrieve the specified 'Metadata'.
--
-- Throws 'HttpException'.
metadata :: (MonadIO m, MonadThrow m, MonadReader r m, HasEnv r)
         => EC2.Metadata
         -> m ByteString
metadata m = view envManager >>= flip EC2.metadata m

-- | Retrieve the user data. Returns 'Nothing' if no user data is assigned
-- to the instance.
--
-- Throws 'HttpException'.
userdata :: (MonadIO m, MonadCatch m, MonadReader r m, HasEnv r)
         => m (Maybe ByteString)
userdata = view envManager >>= EC2.userdata

hoistError :: MonadThrow m => Either Error a -> m a
hoistError = either (throwingM _Error) return

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
configuration when sending 'PutItem', 'Query' and all other operations.

You can modify a specific 'Service''s default configuration by using
'configure' or 'reconfigure'. To modify all configurations simultaneously, see 'override'.

An example of how you might alter default configuration using these mechanisms
is demonstrated below. Firstly, the default 'dynamoDB' service is configured to
use non-SSL localhost as the endpoint:

> let dynamo :: Service
>     dynamo = setEndpoint False "localhost" 8000 dynamoDB

The updated configuration is then passed to the 'Env' during setup:

> e <- newEnv Discover <&> configure dynamo
> runResourceT . runAWS e $ do
>     -- This S3 operation will communicate with remote AWS APIs.
>     x <- send listBuckets
>
>     -- DynamoDB operations will communicate with localhost:8000.
>     y <- send listTables
>
>     -- Any operations for services other than DynamoDB, are not affected.
>     ...

You can also scope the 'Endpoint' modifications (or any other 'Service' configuration)
to specific actions:

> e <- newEnv Discover
> runResourceT . runAWS e $ do
>     -- Service operations here will communicate with AWS, even DynamoDB.
>     x <- send listTables
>
>     reconfigure dynamo $ do
>        -- In here, DynamoDB operations will communicate with localhost:8000,
>        -- with operations for services not being affected.
>        ...

Functions such as 'within', 'once', and 'timeout' likewise modify the underlying
configuration for all service requests within their respective scope.
-}

{- $streaming
Streaming comes in two flavours. 'HashedBody' represents a request
that requires a precomputed 'SHA256' hash, or a 'ChunkedBody' type for those services
that can perform incremental signing and do not require the entire payload to
be hashed (such as 'S3'). The type signatures for request smart constructors
advertise which respective body type is required, denoting the underlying signing
capabilities.

'ToHashedBody' and 'ToBody' typeclass instances are available to construct the
streaming bodies, automatically calculating any hash or size as needed for types
such as 'Text', 'ByteString', or Aeson's 'Value' type. To read files and other
'IO' primitives, functions such as 'hashedFile', 'chunkedFile', or 'hashedBody'
should be used.

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
<http://hackage.haskell.org/package/tinylog tinylog> or
<http://hackage.haskell.org/package/fast-logger fast-logger>.
-}
