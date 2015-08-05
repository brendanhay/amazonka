{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Control.Monad.Trans.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The 'AWST' transformer provides interpretations for the 'FreeT' 'Command'
-- DSL. It can be used directly or embedded as a layer within a transformer DSL.
--
-- /See:/ "Network.AWS".
module Control.Monad.Trans.AWS
    (
    -- * Running AWS Actions
      AWST
    , runAWST
    , execAWST
    , pureAWST

    -- * Environment Setup
    , HasEnv      (..)
    , Env
    , newEnv

    -- ** Credentials
    , Credentials (..)

    -- ** Region
    , Region      (..)

    -- * Runtime Configuration
    , within
    , once
    , timeout

    -- * Sending Requests
    , send
    , await
    , paginate

    -- ** Presigning
    , presignURL
    , presign

    -- ** Overriding Service Configuration
    -- $service
    , sendWith
    , awaitWith
    , paginateWith
    , presignWith

    -- ** Asynchronous Actions
    -- $async

    -- ** Streaming
    -- $streaming

    , ToBody      (..)
    , RqBody
    , sourceBody
    , sourceHandle
    , sourceFile
    , sourceFileIO
    , getFileSize
    , sinkHash

    , RsBody
    , sinkBody

    -- * Handling Errors
    -- $errors
    , AsError (..)
    , trying
    , catching

    -- * Logging
    -- $logging
    , Logger

    -- ** Constructing a Logger
    , newLogger

    -- ** Logging Functions
    , logError
    , logInfo
    , logDebug
    , logTrace

    -- ** Constructing Log Messages
    , ToLog (..)

    -- * Re-exported Types
    , module Network.AWS.Types
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
import           Network.AWS.Auth
import           Network.AWS.Env
import           Network.AWS.Free
import           Network.AWS.Internal.Body
import           Network.AWS.Internal.HTTP
import           Network.AWS.Internal.Presign
import           Network.AWS.Logger
import           Network.AWS.Prelude
import           Network.AWS.Types
import           Network.AWS.Waiter

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
--
-- This does not finalise any 'ResourceT' actions and needs to be wrapped
-- with 'runResourceT'.
--
-- Throws 'Error' during interpretation of the underlying 'FreeT' 'Command' DSL.
--
-- /See:/ 'runAWST', 'runResourceT'.
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
    go (Send s (request -> x) k) = do
        e <- view env
        retrier e s x (perform e s x) >>= lift . f >>= k . snd

    go (Await s w (request -> x) k) = do
        e <- view env
        waiter e w x (perform e s x) >>= lift . f >>= k . snd

-- | Run an 'AWST' action with configurable response construction. This allows
-- mocking of responses and does not perform any external API calls.
--
-- Throws 'Error' during interpretation of the underlying 'FreeT' 'Command' DSL.
pureAWST :: (MonadThrow m, HasEnv r)
         => (forall s a. Service s ->           a -> Either Error (Rs a))
            -- ^ Construct a response for any 'send' command.
         -> (forall s a. Service s -> Wait a -> a -> Either Error (Rs a))
            -- ^ Construct a response for any 'await' command.
         -> r
         -> AWST m b
         -> m b
pureAWST f g = innerAWST go
  where
    go (Send  s   x k) = hoistError (f s   x) >>= k
    go (Await s w x k) = hoistError (g s w x) >>= k

innerAWST :: (Monad m, HasEnv r)
          => (Command (ReaderT Env m a) -> ReaderT Env m a)
          -> r
          -> AWST m a
          -> m a
innerAWST f e (AWST m) = runReaderT (f `Free.iterT` Free.toFT m) (e ^. env)

hoistError :: MonadThrow m => Either Error a -> m a
hoistError = either (throwingM _Error) return

{- $service
When a request is sent, various configuration values such as the endpoint,
retry strategy, timeout and error handlers are taken from the associated 'Service'
configuration.

You can override the default configuration by using the following @*With@
function variants.
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
