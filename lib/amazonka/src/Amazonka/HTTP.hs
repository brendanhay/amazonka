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

import Amazonka.Core.Lens.Internal (to, (^?), _Just)
import Amazonka.Data.Body (isStreaming)
import Amazonka.Env hiding (auth)
import Amazonka.Logger
import Amazonka.Prelude
import Amazonka.Types
import Amazonka.Waiter
import Control.Exception as Exception
import Control.Monad.Trans.Resource (liftResourceT, transResourceT)
import qualified Control.Retry as Retry
import qualified Data.List as List
import qualified Data.Time as Time
import qualified Network.HTTP.Conduit as Client.Conduit

retryRequest ::
  forall m a withAuth.
  ( MonadResource m,
    AWSRequest a,
    Foldable withAuth
  ) =>
  Env' withAuth ->
  a ->
  m (Either Error (ClientResponse (AWSResponse a)))
retryRequest env x =
  Retry.retrying policy shouldRetry attempt
  where
    rq = configureRequest env x
    attempt _ = httpRequest env rq

    policy = retryStream rq <> retryService (service rq)

    shouldRetry :: Retry.RetryStatus -> Either Error b -> m Bool
    shouldRetry s = \case
      Left r
        | Just True <- r ^? transportErr -> writeLog "http_error" s >> return True
        | Just m <- r ^? serviceErr -> writeLog m s >> return True
      _other -> return False
      where
        transportErr =
          _TransportError . to (retryCheck env (Retry.rsIterNumber s))

        serviceErr =
          _ServiceError . to serviceRetryCheck . _Just

        Request
          { service = Service {retry = Exponential {check = serviceRetryCheck}}
          } = rq

    writeLog m s =
      logDebug (logger env)
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

  Retry.retrying policy (check logger) attempt <&> \case
    (AcceptSuccess, _) -> Right AcceptSuccess
    (_, Left e) -> Left e
    (a, _) -> Right a
  where
    policy =
      Retry.limitRetries attempts
        <> Retry.constantDelay (toMicroseconds delay)

    check e n (a, _) = writeLog e n a >> return (retry a)
      where
        retry AcceptSuccess = False
        retry AcceptFailure = False
        retry AcceptRetry = True

    handleResult rq =
      first (fromMaybe AcceptRetry . accept w rq)
        . join (,)

    writeLog l s a =
      logDebug l
        . mconcat
        . List.intersperse " "
        $ [ "[Await " <> build name <> "]",
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

      rq <- case authMaybe env of
        Nothing -> pure $! requestUnsigned x region
        Just auth -> withAuth auth $ \a -> do
          let Signed meta rq = requestSign x a region time
          logTrace logger meta -- trace:Signing:Meta
          pure $! rq

      logDebug logger rq -- debug:ClientRequest
      rs <- Client.Conduit.http rq manager

      logDebug logger rs -- debug:ClientResponse
      response logger (service x) (proxy x) rs

    handlers =
      [ Handler $ err,
        Handler $ err . TransportError
      ]
      where
        err e = logError logger e >> return (Left e)

    proxy :: Request a -> Proxy a
    proxy _ = Proxy

configureRequest :: AWSRequest a => Env' withAuth -> a -> Request a
configureRequest Env {overrides} = request overrides

retryStream :: Request a -> Retry.RetryPolicy
retryStream Request {body} =
  Retry.RetryPolicyM $ \_ -> pure $ if isStreaming body then Nothing else Just 0

retryService :: Service -> Retry.RetryPolicy
retryService Service {retry = Exponential {..}} =
  Retry.limitRetries attempts <> Retry.RetryPolicyM (return . delay)
  where
    delay (Retry.rsIterNumber -> n)
      | n >= 0 = Just $ truncate (grow * 1000000)
      | otherwise = Nothing
      where
        grow = base * (fromIntegral growth ^^ (n - 1))
