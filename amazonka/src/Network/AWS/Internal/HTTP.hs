{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Network.AWS.Internal.HTTP
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Internal.HTTP
    ( retrier
    , waiter
    ) where

import           Control.Arrow                (first)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.List                    (intersperse)
import           Data.Monoid
import           Data.Proxy
import           Data.Time
import           Network.AWS.Env
import           Network.AWS.Internal.Logger
import           Network.AWS.Prelude
import           Network.AWS.Waiter
import           Network.HTTP.Conduit         hiding (Proxy, Request, Response)

import           Prelude

retrier :: ( MonadCatch m
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
    retrying (policy rq) (check e rq) (perform e x rq)
  where
    policy rq = retryStream rq <> retryService (_rqService rq)

    check e rq n (Left r)
        | Just p <- r ^? transportErr, p = msg e "http_error" n >> return True
        | Just m <- r ^? serviceErr      = msg e m            n >> return True
      where
        transportErr = _TransportError . to (_envRetryCheck e n)
        serviceErr   = _ServiceError . to rc . _Just

        rc = rq ^. rqService . serviceRetry . retryCheck

    check _ _ _ _                          = return False

    msg :: MonadIO m => Env -> Text -> Int -> m ()
    msg e m n = logDebug (_envLogger e)
        . mconcat
        . intersperse " "
        $ [ "[Retry " <> build m <> "]"
          , "after"
          , build (n + 1)
          , "attempts."
          ]

waiter :: ( MonadCatch m
          , MonadResource m
          , MonadReader r m
          , HasEnv r
          , AWSRequest a
          )
       => Wait a
       -> a
       -> m (Maybe Error)
waiter w@Wait{..} x = do
   e@Env{..} <- view environment
   rq        <- configured x
   retrying policy (check _envLogger) (result rq <$> perform e x rq) >>= exit
  where
    policy = limitRetries _waitAttempts
          <> constantDelay (microseconds _waitDelay)

    check e n (a, _) = msg e n a >> return (retry a)
      where
        retry AcceptSuccess = False
        retry AcceptFailure = False
        retry AcceptRetry   = True

    result rq = first (fromMaybe AcceptRetry . accept w rq) . join (,)

    exit (AcceptSuccess, _) = return Nothing
    exit (_,        Left e) = return (Just e)
    exit (_,             _) = return Nothing

    msg l n a = logDebug l
        . mconcat
        . intersperse " "
        $ [ "[Await " <> build _waitName <> "]"
          , build a
          , "after"
          , build (n + 1)
          , "attempts."
          ]

-- | The 'Service' is configured + unwrapped at this point.
perform :: (MonadCatch m, MonadResource m, AWSRequest a)
        => Env
        -> a
        -> Request a
        -> m (Either Error (Response a))
perform Env{..} x rq = catches go handlers
  where
    go = do
        t           <- liftIO getCurrentTime
        Signed m sg <-
            withAuth _envAuth $ \a ->
                return $! rqSign rq a _envRegion t

        logTrace _envLogger m  -- trace:Signing:Meta
        logDebug _envLogger sg -- debug:ClientRequest

        rs          <- liftResourceT (http sg _envManager)

        logDebug _envLogger rs -- debug:ClientResponse

        Right <$> response _envLogger (_rqService rq) x rs

    handlers =
        [ Handler $ err
        , Handler $ err . TransportError
        ]
      where
        err e = logError _envLogger e >> return (Left e)

configured :: (MonadReader r m, HasEnv r, AWSRequest a)
           => a
           -> m (Request a)
configured (request -> x) = do
    o <- view envOverride
    return $! x & rqService %~ appEndo (getDual o)

retryStream :: Request a -> RetryPolicy
retryStream x = RetryPolicy (const $ listToMaybe [0 | not p])
  where
    !p = isStreaming (_rqBody x)

retryService :: Service -> RetryPolicy
retryService s = limitRetries _retryAttempts <> RetryPolicy delay
  where
    delay n
        | n > 0     = Just $ truncate (grow * 1000000)
        | otherwise = Nothing
      where
        grow = _retryBase * (fromIntegral _retryGrowth ^^ (n - 1))

    Exponential{..} = _svcRetry s
