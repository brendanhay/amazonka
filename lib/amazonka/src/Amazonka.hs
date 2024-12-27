{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Amazonka
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This module provides simple 'Env' and 'IO'-based operations which
-- can be performed against remote Amazon Web Services APIs, for use with the types
-- supplied by the various @amazonka-*@ libraries.
module Amazonka
  ( -- * Usage
    -- $usage

    -- * Authentication and Environment
    Env.Env,
    Env.EnvNoAuth,
    Env.Env' (..),
    Env.newEnv,
    Env.newEnvFromManager,
    Env.newEnvNoAuth,
    Env.newEnvNoAuthFromManager,
    Env.authMaybe,

    -- ** Service Configuration
    -- $service
    Env.overrideService,
    Env.configureService,
    Env.globalTimeout,
    Env.once,

    -- ** Running AWS Actions
    runResourceT,

    -- ** Credential Discovery
    -- $discovery
    AccessKey (..),
    SecretKey (..),
    SessionToken (..),
    discover,

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

    -- ** Unsigned
    sendUnsigned,
    sendUnsignedEither,

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

    -- * Running Asynchronous Actions
    -- $async

    -- * Handling Errors
    -- $errors
    AsError (..),
    AsAuthError (..),

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
    module Amazonka.Core,
  )
where

import Amazonka.Auth
import Amazonka.Core hiding (presign)
import qualified Amazonka.Crypto as Crypto
import qualified Amazonka.Data.Body as Body
import qualified Amazonka.Endpoint as Endpoint
import qualified Amazonka.Env as Env
import qualified Amazonka.Error as Error
import Amazonka.Logger
import Amazonka.Prelude
import qualified Amazonka.Presign as Presign
import Amazonka.Request (clientRequestURL)
import Amazonka.Send
  ( await,
    awaitEither,
    paginate,
    paginateEither,
    send,
    sendEither,
    sendUnsigned,
    sendUnsignedEither,
  )
import Control.Monad.Trans.Resource (runResourceT)

-- $usage
-- The key functions dealing with the request/response lifecycle are:
--
-- * 'send', 'sendEither'
--
-- * 'paginate', 'paginateEither'
--
-- * 'await', 'awaitEither'
--
-- These functions have constraints that types from the @amazonka-*@ libraries
-- satisfy. To utilise these, you will need to specify what 'Region' you wish to
-- operate in and your Amazon credentials for AuthN/AuthZ purposes.
--
-- Credentials can be supplied in a number of ways. Either via explicit keys,
-- via session profiles, or have Amazonka retrieve the credentials from an
-- underlying IAM Role/Profile.
--
-- As a basic example, you might wish to store an object in an S3 bucket using
-- <http://hackage.haskell.org/package/amazonka-s3 amazonka-s3>:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import qualified Amazonka as AWS
-- import qualified Amazonka.S3 as S3
-- import qualified System.IO as IO
--
-- example :: IO S3.PutObjectResponse
-- example = do
--     -- A new 'Logger' to replace the default noop logger is created, with the logger
--     -- set to print debug information and errors to stdout:
--     logger <- AWS.'Amazonka.newLogger' AWS.'Amazonka.Debug' IO.stdout
--
--     -- To specify configuration preferences, 'Amazonka.newEnv' is used to create a new
--     -- configuration environment. The argument to 'Amazonka.newEnv' is used to specify the
--     -- mechanism for supplying or retrieving AuthN/AuthZ information.
--     -- In this case 'Amazonka.discover' will cause the library to try a number of options such
--     -- as default environment variables, or an instance's IAM Profile and identity document:
--     discoveredEnv <- AWS.'Amazonka.newEnv' AWS.'Amazonka.discover'
--
--     let env =
--             discoveredEnv
--                 { AWS.logger = logger
--                 , AWS.region = AWS.'Amazonka.Frankfurt'
--                 }
--
--     -- The payload (and hash) for the S3 object is retrieved from a 'FilePath',
--     -- either 'hashedFile' or 'chunkedFile' can be used, with the latter ensuring
--     -- the contents of the file is enumerated exactly once, during send:
--     body <- AWS.chunkedFile AWS.defaultChunkSize "local\/path\/to\/object-payload"
--
--     -- We now run the 'AWS' computation with the overriden logger, performing the
--     -- 'PutObject' request.
--     AWS.runResourceT $
--         AWS.'Amazonka.send' env (S3.newPutObject "bucket-name" "object-key" body)
-- @

-- $discovery AuthN/AuthZ information is handled similarly to other AWS SDKs. You can read some of
-- the options available <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs here>.
--
-- 'Amazonka.Auth.discover' should be your default way of requesting credentials, as it searches the
-- standard places that the official AWS SDKs use.
--
-- Authentication methods which return short-lived credentials (e.g., when running on
-- an EC2 instance) fork a background thread which transparently handles the expiry
-- and subsequent refresh of IAM profile information. See
-- 'Amazonka.Auth.Background.fetchAuthInBackground' for more information.
--
-- /See:/ "Amazonka.Auth", if you want to commit to specific authentication methods.
--
-- /See:/ 'Amazonka.Auth.runCredentialChain' if you want to build your own credential chain.

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
-- requests, correctly setting markers and other request facets to iterate through
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
-- 'Wait' specifications can be found under the @Amazonka.{ServiceName}.Waiters@
-- namespace for services which support 'await'.

-- $service
-- When a request is sent, various values such as the endpoint,
-- retry strategy, timeout and error handlers are taken from the associated 'Service'
-- for a request. For example, 'DynamoDB' will use the 'Amazonka.DynamoDB.defaultService'
-- configuration when sending 'PutItem', 'Query' and all other operations.
--
-- You can modify a specific 'Service''s default configuration by using
-- 'Amazonka.Env.configureService'. To modify all configurations simultaneously, see
-- 'Amazonka.Env.overrideService'.
--
-- An example of how you might alter default configuration using these mechanisms
-- is demonstrated below. Firstly, the default 'dynamoDB' service is configured to
-- use non-SSL localhost as the endpoint:
--
-- @
-- import qualified Amazonka as AWS
-- import qualified Amazonka.DynamoDB as DynamoDB
--
-- let dynamo :: AWS.Service
--     dynamo = AWS.setEndpoint False "localhost" 8000 DynamoDB.defaultService
-- @
--
-- The updated configuration is then passed to the 'Env' during setup:
--
-- @
-- env <- AWS.'Amazonka.configureService' dynamo \<$\> AWS.'Amazonka.newEnv' AWS.'Amazonka.discover'
--
-- AWS.runResourceT $ do
--     -- This S3 operation will communicate with remote AWS APIs.
--     x <- AWS.send env newListBuckets
--
--     -- DynamoDB operations will communicate with localhost:8000.
--     y <- AWS.send env Dynamo.newListTables
--
--     -- Any operations for services other than DynamoDB, are not affected.
--     ...
-- @
--
-- You can also scope the service configuration modifications to specific actions:
--
-- @
-- env <- AWS.'Amazonka.newEnv' AWS.'Amazonka.discover'
--
-- AWS.runResourceT $ do
--     -- Service operations here will communicate with AWS, even remote DynamoDB.
--     x <- AWS.send env Dynamo.newListTables
--
--     -- Here DynamoDB operations will communicate with localhost:8000.
--     y <- AWS.send (AWS.configure dynamo env) Dynamo.newListTables
-- @
--
-- Functions such as 'Amazonka.once' and 'Amazonka.globalTimeout' can
-- also be used to modify service configuration for all (or specific)
-- requests.

-- $streaming
-- Streaming comes in two flavours. 'HashedBody' represents a request
-- that requires a precomputed 'SHA256' hash, or a 'ChunkedBody' type for those services
-- that can perform incremental signing and do not require the entire payload to
-- be hashed (such as S3). The type signatures for request smart constructors
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
-- 'sinkBody' to connect the response body to a
-- <http://hackage.haskell.org/package/conduit conduit>-compatible sink.

-- $presigning
-- Presigning requires the 'Service' signer to be an instance of 'AWSPresigner'.
-- Not all signing algorithms support this.

-- $async
-- Requests can be sent asynchronously, but due to guarantees about resource closure
-- require the use of "UnliftIO.Async".
--
-- The following example demonstrates retrieving two objects from S3 concurrently:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import qualified Amazonka as AWS
-- import qualified Amazonka.S3 as S3
-- import qualified UnliftIO.Async as Async
--
-- let requestA = S3.newGetObject "bucket" "prefix/object-a"
-- let requestB = S3.newGetObject "bucket" "prefix/object-b"
--
-- runResourceT $
--   Async.'UnliftIO.Async.withAsync' (send env requestA) $ \\asyncA ->
--     Async.'UnliftIO.Async.withAsync' (send env requestB) $ \\asyncB -> do
--       Async.'UnliftIO.Async.waitBoth' asyncA asyncB
-- @
--
-- If you are running many async requests in parallel, using
-- 'Control.Monad.Trans.Cont.ContT' can hide the giant callback pyramid:
--
-- @
-- runResourceT . 'Control.Monad.Trans.Cont.evalContT' $ do
--   asyncA <- ContT $ Async.'UnliftIO.Async.withAsync' (send env requestA)
--   asyncB <- ContT $ Async.'UnliftIO.Async.withAsync' (send env requestB)
--   Async.'UnliftIO.Async.waitBoth' asyncA asyncB
-- @

-- $errors
-- Errors are either returned or thrown by the library using 'IO'. Sub-errors of
-- the canonical 'Error' type can be caught using 'trying' or 'catching' and the
-- appropriate 'AsError' 'Prism' when using the non-'Either' send variants:
--
-- @
-- trying '_Error'          (send $ newListObjects "bucket-name") :: Either 'Amazonka.Types.Error'          ListObjectsResponse
-- trying '_TransportError' (send $ newListObjects "bucket-name") :: Either 'HttpException'  ListObjectsResponse
-- trying '_SerializeError' (send $ newListObjects "bucket-name") :: Either 'Amazonka.Types.SerializeError' ListObjectsResponse
-- trying '_ServiceError'   (send $ newListObjects "bucket-name") :: Either 'Amazonka.Types.ServiceError'   ListObjectsResponse
-- @
--
-- Many of the individual @amazonka-*@ libraries export compatible 'Control.Lens.Fold's for
-- matching service specific error codes and messages in the style above.
-- See the @Error Matchers@ heading in each respective library for details.

-- $logging
-- The exposed logging interface is a primitive 'Logger' function which the
-- hooks system calls throughout the request/response process. This allows the
-- library to output useful information and diagnostics.
--
-- The 'newLogger' function can be used to construct a simple logger which writes
-- output to a 'Handle', but in most production code you should probably consider
-- using a more robust logging library such as
-- <http://hackage.haskell.org/package/tinylog tinylog> or
-- <http://hackage.haskell.org/package/fast-logger fast-logger>.

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
presign env time expires rq = withAuth (runIdentity $ Env.auth env) $ \ae ->
  pure $!
    Presign.presignWith
      (Env.overrides env)
      ae
      (Env.region env)
      time
      expires
      rq
