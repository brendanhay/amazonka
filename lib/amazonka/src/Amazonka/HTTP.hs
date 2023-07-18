-- |
-- Module      : Amazonka.HTTP
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.HTTP
  ( retryRequest,
    awaitRequest,
    httpRequest,
    configureRequest,
    retryService,
    retryStream,
  )
where

import Amazonka.Core.Lens.Internal (to, (^?), _Just)
import Amazonka.Data.Body (isStreaming)
import Amazonka.Env hiding (auth)
import Amazonka.Env.Hooks (Finality (..))
import qualified Amazonka.Env.Hooks as Hooks
import Amazonka.Prelude
import Amazonka.Types
import Amazonka.Waiter
import Control.Exception as Exception
import Control.Monad.Trans.Resource (liftResourceT, transResourceT)
import qualified Control.Retry as Retry
import Data.Foldable (traverse_)
import qualified Data.Time as Time
import Data.Typeable (Typeable)
import qualified Network.HTTP.Conduit as Client.Conduit

retryRequest ::
  forall m a withAuth.
  ( MonadResource m,
    AWSRequest a,
    Typeable a,
    Typeable (AWSResponse a),
    Foldable withAuth
  ) =>
  Env' withAuth ->
  a ->
  m (Either Error (ClientResponse (AWSResponse a)))
retryRequest env@Env {hooks} rq = do
  rq' <- liftIO $ Hooks.request hooks env rq
  cfgRq <- configureRequest env rq'

  let attempt _ = httpRequest env cfgRq
      policy = retryStream cfgRq <> retryService (service cfgRq)
      Request
        { service = Service {retry = Exponential {check = serviceRetryCheck}}
        } = cfgRq

      shouldRetry :: Retry.RetryStatus -> Either Error b -> m Bool
      shouldRetry s =
        liftIO . \case
          Left r
            | Just True <- r ^? transportErr ->
                True <$ Hooks.requestRetry hooks env (cfgRq, "http_error", s)
            | Just name <- r ^? serviceErr ->
                True <$ Hooks.requestRetry hooks env (cfgRq, name, s)
          _other -> pure False
        where
          transportErr =
            _TransportError . to (retryCheck env (Retry.rsIterNumber s))

          serviceErr =
            _ServiceError . to serviceRetryCheck . _Just

  Retry.retrying policy shouldRetry attempt >>= \case
    Left e -> Left e <$ liftIO (Hooks.error hooks env (Final, cfgRq, e))
    Right a -> pure $ Right a

awaitRequest ::
  ( MonadResource m,
    AWSRequest a,
    Typeable a,
    Foldable withAuth
  ) =>
  Env' withAuth ->
  Wait a ->
  a ->
  m (Either Error Accept)
awaitRequest env@Env {hooks} w rq = do
  rq' <- liftIO $ Hooks.request hooks env rq
  cfgRq <- configureRequest env rq'
  w'@Wait {..} <- liftIO $ Hooks.wait hooks env w

  let handleResult res = (fromMaybe AcceptRetry $ accept w' cfgRq res, res)
      attempt _ = handleResult <$> httpRequest env cfgRq
      policy =
        Retry.limitRetries attempts
          <> Retry.constantDelay (toMicroseconds delay)

      check retryStatus (a, _) = do
        liftIO $ Hooks.awaitRetry hooks env (cfgRq, w', a, retryStatus)
        pure $ case a of
          AcceptSuccess -> False
          AcceptFailure -> False
          AcceptRetry -> True

  Retry.retrying policy check attempt >>= \case
    (AcceptSuccess, _) -> pure $ Right AcceptSuccess
    (_, Left e) -> Left e <$ liftIO (Hooks.error hooks env (Final, cfgRq, e))
    (a, _) -> pure $ Right a

-- | Make a one-shot request to AWS, using a configured 'Request'
-- (which contains the 'Service', plus any overrides).
httpRequest ::
  ( MonadResource m,
    AWSRequest a,
    Typeable a,
    Foldable withAuth
  ) =>
  Env' withAuth ->
  Request a ->
  m (Either Error (ClientResponse (AWSResponse a)))
httpRequest env@Env {hooks, manager, region} cfgRq =
  liftResourceT (transResourceT (`Exception.catches` handlers) go)
  where
    go = do
      time <- liftIO Time.getCurrentTime

      clientRq :: ClientRequest <-
        liftIO . Hooks.clientRequest hooks env =<< case authMaybe env of
          Nothing -> pure $! requestUnsigned cfgRq region
          Just auth -> withAuth auth $ \a -> do
            let s@(Signed _ rq) = requestSign cfgRq a region time
            liftIO $ Hooks.signedRequest hooks env s
            pure $! rq

      rs <- Client.Conduit.http clientRq manager
      liftIO $ Hooks.clientResponse hooks env (cfgRq, void rs)
      parsedRs <-
        response
          (Hooks.rawResponseBody hooks env)
          (service cfgRq)
          (proxy cfgRq)
          rs
      traverse_ (liftIO . Hooks.response hooks env . (cfgRq,)) parsedRs
      pure parsedRs

    handlers :: [Handler (Either Error b)]
    handlers =
      [ Handler err,
        Handler $ err . TransportError
      ]
      where
        err e = Left e <$ Hooks.error hooks env (NotFinal, cfgRq, e)

    proxy :: Request a -> Proxy a
    proxy _ = Proxy

-- Configures an AWS request `a` into its `Request a` form, applying
-- service overrides from `env` and running hooks on the configured
-- (Request a).
configureRequest ::
  (AWSRequest a, Typeable a, MonadIO m) => Env' withAuth -> a -> m (Request a)
configureRequest env@Env {overrides, hooks} =
  liftIO
    . Hooks.configuredRequest hooks env
    . request overrides

retryStream :: Request a -> Retry.RetryPolicy
retryStream Request {body} =
  Retry.RetryPolicyM $ \_ -> pure $ if isStreaming body then Nothing else Just 0

retryService :: Service -> Retry.RetryPolicy
retryService Service {retry = Exponential {..}} =
  Retry.limitRetries attempts <> Retry.RetryPolicyM (pure . delay)
  where
    delay (Retry.rsIterNumber -> n)
      | n >= 0 = Just $ truncate (grow * 1000000)
      | otherwise = Nothing
      where
        grow = base * (fromIntegral growth ^^ (n - 1))
