{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Network.AWS.HTTPUnsigned
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.HTTPUnsigned
  ( 
    sendUnsigned,
    retryStream,
    retryService,
    retryConnectionFailure
  )
where

import Control.Exception as Exception
import Control.Monad.Trans.Resource (liftResourceT, transResourceT)
import qualified Control.Retry as Retry
import qualified Data.List as List
import Network.AWS.Data.Body (isStreaming)
import Network.AWS.Lens (to, (^.), (^?), _Just)
import Network.AWS.Logger
import Network.AWS.Prelude
import Network.AWS.Types
import qualified Network.HTTP.Conduit as Client.Conduit
import qualified Network.HTTP.Client as Client
import Network.AWS.Core ( toBS, toRequestBody, collapsePath, escapePath )

retryRequest ::
  ( MonadResource m,
    AWSRequest a
  ) =>
  Client.Manager ->
  Logger ->
  (Int -> Client.HttpException -> Bool) ->
  Region ->
  a ->
  m (Either Error (ClientResponse (AWSResponse a)))
retryRequest manager logger_ retryCheckE region x = do
  let rq = request x
      attempt _ = httpRequest manager logger_ region rq

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
          _TransportError . to (retryCheckE (Retry.rsIterNumber s))

        serviceErr =
          _ServiceError . to rc . _Just

        rc = rq ^. requestService . serviceRetry . retryCheck

    logger m s =
      logDebug logger_
        . mconcat
        . List.intersperse " "
        $ [ "[Retry " <> build m <> "]",
            "after",
            build (Retry.rsIterNumber s + 1),
            "attempts."
          ]

-- | The 'Service' is configured + unwrapped at this point.
httpRequest ::
  ( MonadResource m,
    AWSRequest a
  ) =>
  Client.Manager ->
  Logger ->
  Region ->
  Request a ->
  m (Either Error (ClientResponse (AWSResponse a)))
httpRequest manager logger region x =
  liftResourceT (transResourceT (`Exception.catches` handlers) go)
  where
    rq = newClientRequest (((_serviceEndpoint . _requestService) x) region) (_serviceTimeout . _requestService $ x)
    rq2 = rq {Client.queryString = toBS $ _requestQuery x
            , Client.requestBody = toRequestBody $ _requestBody x
            , Client.method = toBS $ _requestMethod x
            , Client.path = toBS $ toBS $ escapePath $  collapsePath (_requestPath x)
            , Client.requestHeaders = _requestHeaders x
            }
    go = do

      logDebug logger rq2 -- debug:ClientRequest
      rs <- Client.Conduit.http rq2 manager

      logDebug logger rs -- debug:ClientResponse
      response logger (_requestService x) (proxy x) rs

    handlers =
      [ Handler $ err,
        Handler $ err . TransportError
      ]
      where
        err e = logError logger e >> return (Left e)

    proxy :: Request a -> Proxy a
    proxy _ = Proxy

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


-- | Send a request, returning the associated response if successful.
-- See 'send'.
sendUnsigned ::
  ( MonadResource m,
    AWSRequest a
  ) =>
  Client.Manager ->
  Logger ->
  (Int -> Client.HttpException -> Bool) ->
  Region ->
  a ->
  m (Either Error (AWSResponse a))
sendUnsigned manager logger retry region req =
  fmap (second Client.responseBody) $
   (retryRequest manager logger retry region req)


-- | Retry the subset of transport specific errors encompassing connection
-- failure up to the specific number of times.
retryConnectionFailure :: Int -> Int -> Client.HttpException -> Bool
retryConnectionFailure limit n = \case
  Client.InvalidUrlException {} -> False
  Client.HttpExceptionRequest _ ex
    | n >= limit -> False
    | otherwise ->
      case ex of
        Client.NoResponseDataReceived -> True
        Client.ConnectionTimeout -> True
        Client.ConnectionClosed -> True
        Client.ConnectionFailure {} -> True
        Client.InternalException {} -> True
        _other -> False