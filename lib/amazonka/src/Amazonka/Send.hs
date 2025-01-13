module Amazonka.Send
  ( send,
    sendEither,
    paginate,
    paginateEither,
    await,
    awaitEither,
    sendUnsigned,
    sendUnsignedEither,
  )
where

import Amazonka.Core (AWSPager, AWSRequest, AWSResponse, Error)
import Amazonka.Env (Env, Env' (..))
import qualified Amazonka.HTTP as HTTP
import qualified Amazonka.Pager as Pager
import Amazonka.Prelude
import qualified Amazonka.Waiter as Waiter
import qualified Control.Exception as Exception
import Data.Conduit (ConduitM)
import Data.Typeable (Typeable)
import qualified Data.Conduit as Conduit
import qualified Network.HTTP.Client as Client

-- | Send a request, returning the associated response if successful.
--
-- See 'send'.
sendEither ::
  (MonadResource m, AWSRequest a, Typeable a) =>
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
  (MonadResource m, AWSRequest a, Typeable a) =>
  Env ->
  a ->
  m (AWSResponse a)
send env =
  sendEither env >=> hoistEither

-- | Make a request without signing it. You will almost never need to
-- do this, but some authentication methods
-- (e.g. @sts:AssumeRoleWithWebIdentity@ and @sso:GetRoleCredentials@)
-- require you to exchange a token using an unsigned
-- request. Amazonka's support for these authentication methods calls
-- 'sendUnsigned', and we re-export these functions in case you need
-- to support similar authentication methods in your code.
--
-- See 'sendUnsigned'.
sendUnsignedEither ::
  (MonadResource m, AWSRequest a, Typeable a) =>
  Env' withAuth ->
  a ->
  m (Either Error (AWSResponse a))
sendUnsignedEither env =
  fmap (second Client.responseBody) . HTTP.retryRequest (env {auth = Proxy})

-- | Make an unsigned request, returning the associated response if successful.
--
-- Errors are thrown in 'IO'.
--
-- See 'sendUnsignedEither'.
sendUnsigned ::
  (MonadResource m, AWSRequest a, Typeable a) =>
  Env' withAuth ->
  a ->
  m (AWSResponse a)
sendUnsigned env =
  sendUnsignedEither env >=> hoistEither

-- | Repeatedly send a request, automatically setting markers and performing pagination.
--
-- Exits on the first encountered error.
--
-- See 'paginate'.
paginateEither ::
  (MonadResource m, AWSPager a, Typeable a) =>
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
  (MonadResource m, AWSPager a, Typeable a) =>
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
  (MonadResource m, AWSRequest a, Typeable a) =>
  Env ->
  Waiter.Wait a ->
  a ->
  m (Either Error Waiter.Accept)
awaitEither = HTTP.awaitRequest

-- | Poll the API with the supplied request until a specific 'Wait' condition
-- is fulfilled.
--
-- Errors are thrown in 'IO'.
--
-- See 'awaitEither'.
await ::
  (MonadResource m, AWSRequest a, Typeable a) =>
  Env ->
  Waiter.Wait a ->
  a ->
  m Waiter.Accept
await env wait =
  awaitEither env wait >=> hoistEither

hoistEither :: (MonadIO m) => Either Error a -> m a
hoistEither = either (liftIO . Exception.throwIO) pure
