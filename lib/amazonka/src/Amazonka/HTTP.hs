{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Amazonka.HTTP
-- Copyright   : (c) 2013-2021 Brendan Hay
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

import Amazonka.Data.Body (isStreaming)
import Amazonka.Env
import Amazonka.Lens (to, (%~), (^.), (^?), _Just)
import Amazonka.Logger
import Amazonka.Prelude
import Amazonka.Types
import Amazonka.Waiter
import Control.Exception as Exception
import Control.Monad.Trans.Resource (liftResourceT, transResourceT)
import qualified Control.Retry as Retry
import qualified Data.List as List
import Data.Monoid (Dual (..), Endo (..))
import qualified Data.Time as Time
import qualified Network.HTTP.Conduit as Client.Conduit

retryRequest ::
  ( MonadResource m,
    AWSRequest a,
    Foldable withAuth
  ) =>
  Env' withAuth ->
  a ->
  m (Either Error (ClientResponse (AWSResponse a)))
retryRequest env x = do
  let rq = configureRequest env x
      attempt _ = httpRequest env rq

  Retry.retrying (policy rq) (check rq) attempt
  where
    policy rq =
      retryStream rq <> retryService (_requestService rq)

    check rq s = \case
      Left r
        | Just True <- r ^? transportErr -> logger "http_error" s >> return True
        | Just m <- r ^? serviceErr -> logger m s >> return True
      _other -> return False
      where
        transportErr =
          _TransportError . to (envRetryCheck env (Retry.rsIterNumber s))

        serviceErr =
          _ServiceError . to rc . _Just

        rc = rq ^. requestService . serviceRetry . retryCheck

    logger m s =
      logDebug (envLogger env)
        . mconcat
        . List.intersperse " "
        $ [ "[Retry " <> build m <> "]",
            "after",
            build (Retry.rsIterNumber s + 1),
            "attempts."
          ]

awaitRequest ::
  ( MonadResource m,
    AWSRequest a,
    Foldable withAuth
  ) =>
  Env' withAuth ->
  Wait a ->
  a ->
  m (Either Error Accept)
awaitRequest env@Env {..} w@Wait {..} x = do
  let rq = configureRequest env x
      attempt _ = handleResult rq <$> httpRequest env rq

  Retry.retrying policy (check envLogger) attempt <&> \case
    (AcceptSuccess, _) -> Right AcceptSuccess
    (_, Left e) -> Left e
    (a, _) -> Right a
  where
    policy =
      Retry.limitRetries _waitAttempts
        <> Retry.constantDelay (toMicroseconds _waitDelay)

    check e n (a, _) = logger e n a >> return (retry a)
      where
        retry AcceptSuccess = False
        retry AcceptFailure = False
        retry AcceptRetry = True

    handleResult rq =
      first (fromMaybe AcceptRetry . accept w rq)
        . join (,)

    logger l s a =
      logDebug l
        . mconcat
        . List.intersperse " "
        $ [ "[Await " <> build _waitName <> "]",
            build a,
            "after",
            build (Retry.rsIterNumber s + 1),
            "attempts."
          ]

-- | The 'Service' is configured + unwrapped at this point.
httpRequest ::
  ( MonadResource m,
    AWSRequest a,
    Foldable withAuth
  ) =>
  Env' withAuth ->
  Request a ->
  m (Either Error (ClientResponse (AWSResponse a)))
httpRequest env@Env {..} x =
  liftResourceT (transResourceT (`Exception.catches` handlers) go)
  where
    go = do
      time <- liftIO Time.getCurrentTime

      rq <- case envAuthMaybe env of
        Nothing -> pure $! requestUnsigned x envRegion
        Just auth -> withAuth auth $ \a -> do
          let Signed meta rq = requestSign x a envRegion time
          logTrace envLogger meta -- trace:Signing:Meta
          pure $! rq

      logDebug envLogger rq -- debug:ClientRequest
      rs <- Client.Conduit.http rq envManager

      logDebug envLogger rs -- debug:ClientResponse
      response envLogger (_requestService x) (proxy x) rs

    handlers =
      [ Handler $ err,
        Handler $ err . TransportError
      ]
      where
        err e = logError envLogger e >> return (Left e)

    proxy :: Request a -> Proxy a
    proxy _ = Proxy

configureRequest :: AWSRequest a => Env' withAuth -> a -> Request a
configureRequest env x =
  let overrides = envOverride env
   in request x & requestService %~ appEndo (getDual overrides)

retryStream :: Request a -> Retry.RetryPolicy
retryStream x =
  Retry.RetryPolicyM (\_ -> return (listToMaybe [0 | not streaming]))
  where
    streaming = isStreaming (_requestBody x)

retryService :: Service -> Retry.RetryPolicy
retryService s =
  Retry.limitRetries _retryAttempts <> Retry.RetryPolicyM (return . delay)
  where
    delay (Retry.rsIterNumber -> n)
      | n >= 0 = Just $ truncate (grow * 1000000)
      | otherwise = Nothing
      where
        grow = _retryBase * (fromIntegral _retryGrowth ^^ (n - 1))

    Exponential {..} = _serviceRetry s
