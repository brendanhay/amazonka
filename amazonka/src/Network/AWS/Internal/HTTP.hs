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
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.List                    (intersperse)
import           Data.Monoid
import           Data.Time
import           Network.AWS.Env
import           Network.AWS.Internal.Logger
import           Network.AWS.Prelude
import           Network.AWS.Waiter
import           Network.HTTP.Conduit         hiding (Request, Response)

import           Prelude

retrier :: (MonadCatch m, MonadResource m, AWSSigner (Sg s), AWSRequest a)
        => Env
        -> Service s
        -> a
        -> m (Either Error (Response a))
retrier e@Env{..} s@Service{..} (request -> rq) =
    retrying policy check (perform e s rq)
  where
    policy = limitRetries _retryAttempts
       <> RetryPolicy (const $ listToMaybe [0 | not stream])
       <> RetryPolicy delay
      where
        !stream = bodyStream (_rqBody rq)

        delay n
            | n > 0     = Just $ truncate (grow * 1000000)
            | otherwise = Nothing
          where
            grow = _retryBase * (fromIntegral _retryGrowth ^^ (n - 1))

    check n (Left x)
        | Just p <- x ^? transportErr n, p = msg "http_error" n >> return True
        | Just m <- x ^? serviceErr        = msg m            n >> return True
    check _ _                              = return False

    transportErr n = _TransportError . to (_envRetryCheck n)
    serviceErr     = _ServiceError . to _retryCheck . _Just

    msg x n = logDebug _envLogger
        . mconcat
        . intersperse " "
        $ [ "[Retry " <> build x <> "]"
          , "after"
          , build (n + 1)
          , "attempts."
          ]

    Exponential{..} = _svcRetry

waiter :: (MonadCatch m, MonadResource m, AWSSigner (Sg s), AWSRequest a)
       => Env
       -> Service s
       -> Wait a
       -> a
       -> m (Maybe Error)
waiter e@Env{..} s w@Wait{..} (request -> rq) =
   retrying policy check (result <$> perform e s rq) >>= exit
  where
    policy = limitRetries _waitAttempts
          <> constantDelay (microseconds _waitDelay)

    check n (a, _) = msg n a >> return (retry a)
      where
        retry AcceptSuccess = False
        retry AcceptFailure = False
        retry AcceptRetry   = True

    result = first (fromMaybe AcceptRetry . accept w rq) . join (,)

    exit (AcceptSuccess, _) = return Nothing
    exit (_,        Left x) = return (Just x)
    exit (_,             _) = return Nothing

    msg n a = logDebug _envLogger
        . mconcat
        . intersperse " "
        $ [ "[Await " <> build _waitName <> "]"
          , build a
          , "after"
          , build (n + 1)
          , "attempts."
          ]

perform :: (MonadCatch m, MonadResource m, AWSSigner (Sg s), AWSRequest a)
        => Env
        -> Service s
        -> Request a
        -> m (Either Error (Response a))
perform Env{..} svc x = catches go handlers
  where
    go = do
        t           <- liftIO getCurrentTime
        Signed m rq <- withAuth _envAuth $ \a ->
            return (signed a _envRegion t svc x)

        logTrace _envLogger m  -- trace:Signing:Meta
        logDebug _envLogger rq -- debug:ClientRequest

        rs          <- liftResourceT (http rq _envManager)

        logDebug _envLogger rs -- debug:ClientResponse

        Right <$>
            response _envLogger svc x rs

    handlers =
        [ Handler $ err
        , Handler $ err . TransportError
        ]
      where
        err e = logError _envLogger e >> return (Left e)
