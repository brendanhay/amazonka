{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.AWS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This module provides simple 'Env' and 'IO'-based operations which
-- can be performed against remote Amazon Web Services APIs, for use with the types
-- supplied by the various @amazonka-*@ libraries.
module Network.AWS
  ( -- * Usage
    -- $usage

    -- * Authentication and Environment
    Env.Env (..),
    Env.newEnv,
    Env.newEnvWith,

    -- ** Service Configuration
    -- $service
    Env.override,
    Env.configure,
    Env.within,
    Env.once,
    Env.timeout,

    -- ** Running AWS Actions
    runResourceT,

    -- ** Credential Discovery
    AccessKey (..),
    SecretKey (..),
    SessionToken (..),
    Credentials (..),
    -- $discovery

    -- ** Supported Regions
    Region (..),

    -- ** Service Endpoints
    Endpoint (..),
    Endpoint.setEndpoint,

    -- * Sending Requests
    -- $sending
    send,
    sendEither,

    -- ** Pagination
    -- $pagination
    paginate,
    paginateEither,

    -- ** Waiters
    -- $waiters
    await,
    awaitEither,

    -- ** Streaming
    -- $streaming
    ToBody (..),
    RequestBody (..),
    ResponseBody (..),

    -- *** Hashed Request Bodies
    ToHashedBody (..),
    HashedBody (..),
    Body.hashedFile,
    Body.hashedFileRange,
    Body.hashedBody,

    -- *** Chunked Request Bodies
    ChunkedBody (..),
    ChunkSize (..),
    defaultChunkSize,
    Body.chunkedFile,
    Body.chunkedFileRange,
    Body.unsafeChunkedBody,

    -- *** Response Bodies
    Body.sinkBody,

    -- *** File Size and MD5/SHA256
    Body.getFileSize,
    Crypto.sinkMD5,
    Crypto.sinkSHA256,

    -- * Presigning Requests
    -- $presigning
    presignURL,
    presign,

    -- * EC2 Instance Metadata
    -- $metadata
    EC2.Dynamic (..),
    dynamic,
    EC2.Metadata (..),
    metadata,
    userdata,

    -- * Running Asynchronous Actions
    -- $async

    -- * Handling Errors
    -- $errors
    AsError (..),
    AsAuthError (..),
    Lens.trying,
    Lens.catching,

    -- ** Building Error Prisms
    Error._MatchServiceError,
    Error.hasService,
    Error.hasStatus,
    Error.hasCode,

    -- * Logging
    -- $logging
    LogLevel (..),
    Logger,

    -- ** Constructing a Logger
    newLogger,

    -- * Re-exported Types
    module Network.AWS.Core,
  )
where

import qualified Control.Exception as Exception
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (ConduitM)
import qualified Data.Conduit as Conduit
import Data.Monoid (Dual (..), Endo (..))
import Network.AWS.Auth
import Network.AWS.Core
import qualified Network.AWS.Crypto as Crypto
import qualified Network.AWS.Data.Body as Body
import qualified Network.AWS.EC2.Metadata as EC2
import qualified Network.AWS.Endpoint as Endpoint
import Network.AWS.Env (Env)
import qualified Network.AWS.Env as Env
import qualified Network.AWS.Error as Error
import qualified Network.AWS.HTTP as HTTP
import qualified Network.AWS.Lens as Lens
import Network.AWS.Logger
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Presign as Presign
import Network.AWS.Request (clientRequestURL)
import qualified Network.AWS.Waiter as Waiter
import qualified Network.HTTP.Client as Client

-- $usage
-- The key functions dealing with the request/response lifecycle are:
--
-- * 'send', 'sendThrow'
--
-- * 'paginate', 'paginateThrow'
--
-- * 'await', 'awaitThrow'
--
-- These functions have constraints that types from the @amazonka-*@ libraries
-- satisfy. To utilise these, you will need to specify what 'Region' you wish to
-- operate in and your Amazon credentials for AuthN/AuthZ purposes.
--
-- 'Credentials' can be supplied in a number of ways. Either via explicit keys,
-- via session profiles, or have Amazonka retrieve the credentials from an
-- underlying IAM Role/Profile.
--
-- As a basic example, you might wish to store an object in an S3 bucket using
-- <http://hackage.haskell.org/package/amazonka-s3 amazonka-s3>:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import qualified Network.AWS as AWS
-- import qualified Network.AWS.S3 as S3
-- import qualified System.IO as IO
--
-- example :: IO S3.PutObjectResponse
-- example = do
--     -- A new 'Logger' to replace the default noop logger is created, with the logger
--     -- set to print debug information and errors to stdout:
--     logger <- AWS.newLogger AWS.Debug IO.stdout
--
--     -- To specify configuration preferences, 'newEnv' is used to create a new
--     -- configuration environment. The 'Credentials' parameter is used to specify
--     -- mechanism for supplying or retrieving AuthN/AuthZ information.
--     -- In this case 'Discover' will cause the library to try a number of options such
--     -- as default environment variables, or an instance's IAM Profile and identity document:
--     discover <- AWS.newEnv AWS.Discover
--
--     let env =
--             discover
--                 { AWS.envLogger = logger
--                 , AWS.envRegion = AWS.Frankfurt
--                 }
--
--     -- The payload (and hash) for the S3 object is retrieved from a 'FilePath',
--     -- either 'hashedFile' or 'chunkedFile' can be used, with the latter ensuring
--     -- the contents of the file is enumerated exactly once, during send:
--     body <- AWS.chunkedFile AWS.defaultChunkSize "local\/path\/to\/object-payload"
--
--     -- We now run the 'AWS' computation with the overriden logger, performing the
--     -- 'PutObject' request. 'envRegion' or 'within' can be used to set the
--     -- remote AWS 'Region':
--     AWS.runResourceT $
--         AWS.send (S3.putObject "bucket-name" "object-key" body)
-- @

-- $discovery
-- AuthN/AuthZ information is handled similarly to other AWS SDKs. You can read
-- some of the options available <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs here>.
--
-- When running on an EC2 instance and using 'FromProfile' or 'Discover', a thread
-- is forked which transparently handles the expiry and subsequent refresh of IAM
-- profile information. See 'Network.AWS.Auth.fromProfileName' for more information.

-- $sending
-- To send a request you need to create a value of the desired operation type using
-- the relevant constructor, as well as any further modifications of default/optional
-- parameters using the appropriate lenses. This value can then be sent using 'send'
-- or 'paginate' and the library will take care of serialisation/authentication and
-- so forth.
--
-- The default 'Service' configuration for a request contains retry configuration that is used to
-- determine if a request can safely be retried and what kind of back off/on strategy
-- should be used. (Usually exponential.)
-- Typically services define retry strategies that handle throttling, general server
-- errors and transport errors. Streaming requests are never retried.

-- $pagination
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
-- Error conditions that are not handled by the 'Wait' configuration will be thrown,
-- or the first successful response that fulfills the success condition will be
-- returned.
--
-- 'Wait' specifications can be found under the @Network.AWS.{ServiceName}.Waiters@
-- namespace for services which support 'await'.

-- $service
-- When a request is sent, various values such as the endpoint,
-- retry strategy, timeout and error handlers are taken from the associated 'Service'
-- for a request. For example, 'DynamoDB' will use the 'Network.AWS.DynamoDB.defaultService'
-- configuration when sending 'PutItem', 'Query' and all other operations.
--
-- You can modify a specific 'Service''s default configuration by using
-- 'configure' or 'reconfigure'. To modify all configurations simultaneously, see 'override'.
--
-- An example of how you might alter default configuration using these mechanisms
-- is demonstrated below. Firstly, the default 'dynamoDB' service is configured to
-- use non-SSL localhost as the endpoint:
--
--
-- > import qualified Network.AWS as AWS
-- > import qualified Network.AWS.DynamoDB as Dynamo
-- >
-- > let dynamo :: AWS.Service
-- >     dynamo = AWS.setEndpoint False "localhost" 8000 DynamoDB.defaultService
--
-- The updated configuration is then passed to the 'Env' during setup:
--
-- > env <- AWS.configure dynamo <$> AWS.newEnv AWS.Discover
-- >
-- > AWS.runResourceT $ do
-- >     -- This S3 operation will communicate with remote AWS APIs.
-- >     x <- AWS.send env listBuckets
-- >
-- >     -- DynamoDB operations will communicate with localhost:8000.
-- >     y <- AWS.send env Dynamo.listTables
-- >
-- >     -- Any operations for services other than DynamoDB, are not affected.
-- >     ...
--
-- You can also scope the service configuration modifications to specific actions:
--
-- > env <- AWS.newEnv AWS.Discover
-- >
-- > AWS.runResourceT $ do
-- >     -- Service operations here will communicate with AWS, even remote DynamoDB.
-- >     x <- AWS.send env Dynamo.listTables
-- >
-- >     -- Here DynamoDB operations will communicate with localhost:8000.
-- >     y <- AWS.send (AWS.configure dynamo) Dynamo.listTables
--
-- Functions such as 'within', 'once', and 'timeout' can also be used to modify
-- service configuration for all (or specific) requests.

-- $streaming
-- Streaming comes in two flavours. 'HashedBody' represents a request
-- that requires a precomputed 'SHA256' hash, or a 'ChunkedBody' type for those services
-- that can perform incremental signing and do not require the entire payload to
-- be hashed (such as 'S3'). The type signatures for request smart constructors
-- advertise which respective body type is required, denoting the underlying signing
-- capabilities.
--
-- 'ToHashedBody' and 'ToBody' typeclass instances are available to construct the
-- streaming bodies, automatically calculating any hash or size as needed for types
-- such as 'Text', 'ByteString', or Aeson's 'Value' type. To read files and other
-- 'IO' primitives, functions such as 'hashedFile', 'chunkedFile', or 'hashedBody'
-- should be used.
--
-- For responses that contain streaming bodies (such as 'GetObject'), you can use
-- 'sinkBody' to connect the response body to a <http://hackage.haskell.org/package/conduit conduit>
-- compatible sink.

-- $presigning
-- Presigning requires the 'Service' signer to be an instance of 'AWSPresigner'.
-- Not all signing algorithms support this.

-- $metadata
-- Metadata can be retrieved from the underlying host assuming that you're running
-- the code on an EC2 instance or have a compatible @instance-data@ endpoint available.

-- $async
-- Requests can be sent asynchronously, but due to guarantees about resource closure
-- require the use of <http://hackage.haskell.org/package/lifted-async lifted-async>.
--
-- The following example demonstrates retrieving two objects from S3 concurrently:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import qualified Control.Concurrent.Async as Async
-- > import qualfiied Network.AWS as AWS
-- > import qualfiied Network.AWS.S3 as S3
-- >
-- > let a = S3.getObject "bucket" "prefix/object-a"
-- > let b = S3.getObject "bucket" "prefix/object-b"
-- >
-- > Async.withAsync (AWS.runResourceT (AWS.send env a)) $ \aAsync ->
-- >     Async.withAsync (AWS.runResourceT (AWS.send env b)) $ \bAsync -> do
-- >         a' <- Async.wait aAsync
-- >         b' <- Async.wait bAsync
-- >         ...
--
-- See <http://hackage.haskell.org/package/async Control.Concurrent.Async>

-- $errors
-- Errors are either returned or thrown by the library using 'IO'. Sub-errors of
-- the canonical 'Error' type can be caught using 'trying' or 'catching' and the
-- appropriate 'AsError' 'Prism' when using the non-'Either' send variants:
--
-- @
-- trying '_Error'          (send $ ListObjects "bucket-name") :: Either 'Error'          ListObjectsResponse
-- trying '_TransportError' (send $ ListObjects "bucket-name") :: Either 'HttpException'  ListObjectsResponse
-- trying '_SerializeError' (send $ ListObjects "bucket-name") :: Either 'SerializeError' ListObjectsResponse
-- trying '_ServiceError'   (send $ ListObjects "bucket-name") :: Either 'ServiceError'   ListObjectsResponse
-- @
--
-- Many of the individual @amazonka-*@ libraries export compatible 'Getter's for
-- matching service specific error codes and messages in the style above.
-- See the @Error Matchers@ heading in each respective library for details.

-- $logging
-- The exposed logging interface is a primitive 'Logger' function which gets
-- threaded through service calls and serialisation routines. This allows the
-- library to output useful information and diagnostics.
--
-- The 'newLogger' function can be used to construct a simple logger which writes
-- output to a 'Handle', but in most production code you should probably consider
-- using a more robust logging library such as
-- <http://hackage.haskell.org/package/tinylog tinylog> or
-- <http://hackage.haskell.org/package/fast-logger fast-logger>.

-- | Send a request, returning the associated response if successful.
--
-- See 'send'.
sendEither ::
  ( MonadResource m,
    AWSRequest a
  ) =>
  Env ->
  a ->
  m (Either Error (AWSResponse a))
sendEither env =
  fmap (second Client.responseBody) . HTTP.retryRequest env

-- | Send a request, returning the associated response if successful.
--
-- Errors are thrown in 'IO'.
--
-- See 'sendEither'.
send ::
  ( MonadResource m,
    AWSRequest a
  ) =>
  Env ->
  a ->
  m (AWSResponse a)
send env =
  sendEither env >=> hoistEither

-- | Repeatedly send a request, automatically setting markers and performing pagination.
--
-- Exits on the first encountered error.
--
-- See 'paginate'.
paginateEither ::
  ( MonadResource m,
    AWSPager a
  ) =>
  Env ->
  a ->
  ConduitM () (AWSResponse a) m (Either Error ())
paginateEither env = go
  where
    go rq =
      lift (sendEither env rq) >>= \case
        Left err -> pure (Left err)
        Right rs -> do
          Conduit.yield rs
          maybe (pure (Right ())) go (Pager.page rq rs)

-- | Repeatedly send a request, automatically setting markers and performing pagination.
-- Exits on the first encountered error.
--
-- Errors are thrown in 'IO'.
--
-- See 'paginateEither'.
paginate ::
  ( MonadResource m,
    AWSPager a
  ) =>
  Env ->
  a ->
  ConduitM () (AWSResponse a) m ()
paginate env =
  paginateEither env >=> hoistEither

-- | Poll the API with the supplied request until a specific 'Wait' condition
-- is fulfilled.
--
-- See 'await'.
awaitEither ::
  ( MonadResource m,
    AWSRequest a
  ) =>
  Env ->
  Waiter.Wait a ->
  a ->
  m (Either Error Waiter.Accept)
awaitEither env wait =
  HTTP.awaitRequest env wait

-- | Poll the API with the supplied request until a specific 'Wait' condition
-- is fulfilled.
--
-- Errors are thrown in 'IO'.
--
-- See 'awaitEither'.
await ::
  ( MonadResource m,
    AWSRequest a
  ) =>
  Env ->
  Waiter.Wait a ->
  a ->
  m Waiter.Accept
await env wait =
  awaitEither env wait >=> hoistEither

hoistEither :: MonadIO m => Either Error a -> m a
hoistEither = either (liftIO . Exception.throwIO) pure

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
presignURL ::
  ( MonadIO m,
    AWSRequest a
  ) =>
  Env ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  m ByteString
presignURL env time expires =
  fmap clientRequestURL
    . presign env time expires

-- | Presign an HTTP request that is valid from the specified time until the
-- number of seconds expiry has elapsed.
presign ::
  ( MonadIO m,
    AWSRequest a
  ) =>
  Env ->
  -- | Signing time.
  UTCTime ->
  -- | Expiry time.
  Seconds ->
  -- | Request to presign.
  a ->
  m ClientRequest
presign env time expires rq =
  Presign.presignWith
    (appEndo (getDual (Env.envOverride env)))
    (Env.envAuth env)
    (Env.envRegion env)
    time
    expires
    rq

-- | Retrieve the specified 'Dynamic' data.
dynamic :: MonadIO m => Env -> EC2.Dynamic -> m ByteString
dynamic env = EC2.dynamic (Env.envManager env)

-- | Retrieve the specified 'Metadata'.
metadata :: MonadIO m => Env -> EC2.Metadata -> m ByteString
metadata env = EC2.metadata (Env.envManager env)

-- | Retrieve the user data. Returns 'Nothing' if no user data is assigned
-- to the instance.
userdata :: MonadIO m => Env -> m (Maybe ByteString)
userdata = EC2.userdata . Env.envManager
