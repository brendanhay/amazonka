{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Control.Monad.Trans.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- The 'AWST' transformer provides the environment required to perform AWS
-- operations and constructs a 'Command' DSL using 'FreeT' which can then be
-- interpreted using 'runAWST'. The transformer is intended to be used directly
-- or embedded as a layer within a transformer stack.
--
-- "Network.AWS" contains a 'IO' specialised version of 'AWST' with a typeclass
-- to assist in automatically lifting operations.
module Control.Monad.Trans.AWS
    (
    -- * Running AWS Actions
      AWST
    , runAWST
    , execAWST

    -- ** ResourceT
    , runResourceT

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

    -- *** Scoped Actions
    , within
    , once
    , timeout

    -- *** Per Request
    , sendWith
    , paginateWith
    , awaitWith
    , presignWith

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
    , module Network.AWS.Pager
    , module Network.AWS.Waiter
    ) where

import           Control.Applicative
import           Control.Exception.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Error.Class       (MonadError (..))
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free
import qualified Control.Monad.Trans.Free.Church as Free
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer.Class
import           Data.IORef
import           Network.AWS.Auth
import qualified Network.AWS.EC2.Metadata        as EC2
import           Network.AWS.Env
import           Network.AWS.Free
import           Network.AWS.Internal.Body
import           Network.AWS.Internal.HTTP
import           Network.AWS.Logger
import           Network.AWS.Pager               (AWSPager)
import           Network.AWS.Prelude             as AWS
import qualified Network.AWS.Presign             as Sign
import           Network.AWS.Types               hiding (LogLevel (..))
import           Network.AWS.Waiter              (Wait)

-- | The 'AWST' transformer.
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

instance MonadCatch m => MonadCatch (AWST m) where
    catch (AWST m) f = AWST (m `catch` \e -> unAWST (f e))

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

-- | Run an 'AWST' action with the specified 'HasEnv' environment.
-- Any outstanding HTTP responses' 'ResumableSource' will
-- be closed when the 'ResourceT' computation is unwrapped with 'runResourceT'.
--
-- Throws 'Error' during interpretation of the underlying 'FreeT' 'Command' DSL.
--
-- /See:/ 'runResourceT'.
runAWST :: (MonadCatch m, MonadResource m, HasEnv r) => r -> AWST m a -> m a
runAWST = execAWST hoistError

-- | Run an 'AWST' action with configurable 'Error' handling.
--
-- Does not explictly throw 'Error's and instead uses the supplied lift function.
execAWST :: (MonadCatch m, MonadResource m, HasEnv r)
         => (forall a. Either Error a -> m a)
            -- ^ Lift an 'Error' into the base Monad.
         -> r
         -> AWST m b
         -> m b
execAWST f = innerAWST go
  where
    go (CheckF k) = do
        io <- view envEC2
        mp <- liftIO (readIORef io)
        case mp of
            Just p  -> k p
            Nothing -> do
                m  <- view envManager
                !r <- lift . f =<< tryT (EC2.isEC2 m)
                liftIO (atomicWriteIORef io (Just r))
                k r

    go (DynF x k) = do
        m <- view envManager
        r <- lift . f =<< tryT (EC2.dynamic m x)
        k r

    go (MetaF x k) = do
        m <- view envManager
        r <- lift . f =<< tryT (EC2.metadata m x)
        k r

    go (UserF k) = do
        m <- view envManager
        r <- lift . f =<< tryT (EC2.userdata m)
        k r

    go (SignF s ts ex x k) = do
        a <- view envAuth
        g <- view envRegion
        r <- Sign.presignWith (const s) a g ts ex x
        k r

    go (SendF s (request -> x) k) = do
        e <- view environment
        r <- lift . f =<< retrier e s x (perform e s x)
        k (snd r)

    go (AwaitF s w (request -> x) k) = do
        e <- view environment
        r <- lift . f =<< waiter e w x (perform e s x)
        k (snd r)

    tryT m = either (Left . TransportError) Right <$> try m

innerAWST :: (Monad m, HasEnv r)
          => (Command (ReaderT Env m a) -> ReaderT Env m a)
          -> r
          -> AWST m a
          -> m a
innerAWST f e (AWST m) =
    runReaderT (f `Free.iterT` Free.toFT m) (e ^. environment)

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

The default 'Service' configuration for a request (or the supplied 'Service' configuration
when using the @*With@ variants) contains retry configuration that is used to
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
When a request is sent, various configuration values such as the endpoint,
retry strategy, timeout and error handlers are taken from the associated 'Service'
configuration.

You can override the default configuration for a series of one or more actions
by using 'within', 'once' and 'timeout', or by using the @*With@ suffixed
functions on an individual request basis below.
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
