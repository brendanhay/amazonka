{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE CPP #-} -- b/w compat for http-client < 2.3.0

-- |
-- Module      : Network.AWS.Internal.HTTP
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Internal.HTTP
    ( retrier
    , waiter
    ) where

import Control.Arrow                (first)
import Control.Monad
import Control.Monad.Catch (MonadCatch, Handler (Handler), catches)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Retry

import Data.List   (intersperse)
import Data.Monoid
import Data.Proxy
import Data.Time

import Network.AWS.Env
import Network.AWS.Internal.Logger
import Network.AWS.Lens            ((%~), (&), (^.), (^?))
import Network.AWS.Lens            (to, view, _Just)
import Network.AWS.Prelude
import Network.AWS.Waiter
import Network.HTTP.Conduit        hiding (Proxy, Request, Response)
#if MIN_VERSION_http_conduit(2, 3, 0)
#else
import Data.Conduit (unwrapResumable, addCleanup)
#endif

retrier :: ( MonadThrow m
           , MonadCatch m
           , MonadResource m
           , MonadReader r m
           , HasEnv r
           , AWSRequest a
           )
        => a
        -> m (Either Error (Response a))
retrier x = do
    e  <- view environment
    rq <- configured x
    retrying (policy rq) (check e rq) (\_ -> perform e rq)
  where
    policy rq = retryStream rq <> retryService (_rqService rq)

    check e rq s (Left r)
        | Just p <- r ^? transportErr, p = msg e "http_error" s >> return True
        | Just m <- r ^? serviceErr      = msg e m            s >> return True
      where
        transportErr = _TransportError . to (_envRetryCheck e (rsIterNumber s))
        serviceErr   = _ServiceError . to rc . _Just

        rc = rq ^. rqService . serviceRetry . retryCheck

    check _ _ _ _                          = return False

    msg :: MonadIO m => Env -> Text -> RetryStatus -> m ()
    msg e m s = logDebug (_envLogger e)
        . mconcat
        . intersperse " "
        $ [ "[Retry " <> build m <> "]"
          , "after"
          , build (rsIterNumber s + 1)
          , "attempts."
          ]

waiter :: ( MonadThrow m
          , MonadCatch m
          , MonadResource m
          , MonadReader r m
          , HasEnv r
          , AWSRequest a
          )
       => Wait a
       -> a
       -> m (Either Error Accept)
waiter w@Wait{..} x = do
   e@Env{..} <- view environment
   rq        <- configured x
   retrying policy (check _envLogger) (\_ -> result rq <$> perform e rq) >>= exit
  where
    policy = limitRetries _waitAttempts
          <> constantDelay (toMicroseconds _waitDelay)

    check e n (a, _) = msg e n a >> return (retry a)
      where
        retry AcceptSuccess = False
        retry AcceptFailure = False
        retry AcceptRetry   = True

    result rq = first (fromMaybe AcceptRetry . accept w rq) . join (,)

    exit (AcceptSuccess, _) = return (Right AcceptSuccess)
    exit (_,        Left e) = return (Left e)
    exit (a,        _)      = return (Right a)

    msg l s a = logDebug l
        . mconcat
        . intersperse " "
        $ [ "[Await " <> build _waitName <> "]"
          , build a
          , "after"
          , build (rsIterNumber s + 1)
          , "attempts."
          ]

-- | The 'Service' is configured + unwrapped at this point.
perform :: ( MonadThrow m
           , MonadCatch m
           , MonadResource m
           , AWSRequest a
           )
        => Env
        -> Request a
        -> m (Either Error (Response a))
perform Env{..} x = catches go handlers
  where
    go = do
        t           <- liftIO getCurrentTime
        Signed m rq <-
            withAuth _envAuth $ \a ->
                return $! rqSign x a _envRegion t

        logTrace _envLogger m  -- trace:Signing:Meta
        logDebug _envLogger rq -- debug:ClientRequest

#if MIN_VERSION_http_conduit(2, 3, 0)
        rs          <- liftResourceT (http rq _envManager)
#else
        rs'         <- liftResourceT (http rq _envManager)
        let resSrc   = responseBody rs'
        (src', fin) <- liftResourceT (unwrapResumable resSrc)
        let src = addCleanup (const fin) src'
        let rs  = src <$ rs'
#endif
        logDebug _envLogger rs -- debug:ClientResponse

        Right <$> response _envLogger (_rqService x) (p x) rs

    handlers =
        [ Handler $ err
        , Handler $ err . TransportError
        ]
      where
        err e = logError _envLogger e >> return (Left e)

    p :: Request a -> Proxy a
    p = const Proxy

configured :: (MonadReader r m, HasEnv r, AWSRequest a)
           => a
           -> m (Request a)
configured (request -> x) = do
    o <- view envOverride
    return $! x & rqService %~ appEndo (getDual o)

retryStream :: Request a -> RetryPolicy
retryStream x = RetryPolicyM (\_ -> return (listToMaybe [0 | not p]))
  where
    !p = isStreaming (_rqBody x)

retryService :: Service -> RetryPolicy
retryService s = limitRetries _retryAttempts <> RetryPolicyM (return . delay)
  where
    delay (rsIterNumber -> n)
        | n >= 0 = Just $ truncate (grow * 1000000)
        | otherwise = Nothing
      where
        grow = _retryBase * (fromIntegral _retryGrowth ^^ (n - 1))

    Exponential{..} = _svcRetry s
