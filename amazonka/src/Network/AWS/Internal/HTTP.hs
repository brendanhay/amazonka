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
import           Data.Time
import           Network.AWS.Env
import           Network.AWS.Internal.Logger
import           Network.AWS.Prelude
import           Network.AWS.Waiter
import           Network.HTTP.Conduit         hiding (Request, Response)

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
    retrying (policy rq) (check e rq) (perform e rq)
  where
    policy rq = retryStream rq <> retryService (_rqService rq)

    check e rq n (Left r)
        | Just p <- r ^? transportErr n, p = msg e "http_error" n >> return True
        | Just m <- r ^? serviceErr        = msg e m            n >> return True
      where
        transportErr n = _TransportError . to (_envRetryCheck e n)
        serviceErr     = _ServiceError . to rc . _Just

        rc = rq ^. rqService . svcRetry . to _retryCheck

    check _ _ _ _                          = return False

    msg :: MonadIO m => Env -> Text -> Int -> m ()
    msg e x n = logDebug (_envLogger e)
        . mconcat
        . intersperse " "
        $ [ "[Retry " <> build x <> "]"
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
   retrying policy (check _envLogger) (result rq <$> perform e rq) >>= exit
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
        -> Request a
        -> m (Either Error (Response a))
perform Env{..} x = catches go handlers
  where
    go = do
        t           <- liftIO getCurrentTime
        Signed m rq <-
            withAuth _envAuth $ \a ->
                return $! rqSigner a _envRegion t x

        logTrace _envLogger m  -- trace:Signing:Meta
        logDebug _envLogger rq -- debug:ClientRequest

        rs          <- liftResourceT (http rq _envManager)

        logDebug _envLogger rs -- debug:ClientResponse

        Right <$>
            response _envLogger svc x rs

    handlers =
        [ Handler $ return . Left
        , Handler $ \er -> do
            logError _envLogger er
            return (Left (TransportError er))
        ]

    svc = _rqService x

configured :: (MonadReader r m, HasEnv r, AWSRequest a)
           => a
           -> m (Request a)
configured (request -> x) = do
    c <- view envConfig
    return $! x & rqService %~ configure c

retryStream :: Request a -> RetryPolicy
retryStream x = RetryPolicy (const $ listToMaybe [0 | not p])
  where
    !p = bodyStream (_rqBody x)

retryService :: Service -> RetryPolicy
retryService s = limitRetries _retryAttempts <> RetryPolicy delay
  where
    delay n
        | n > 0     = Just $ truncate (grow * 1000000)
        | otherwise = Nothing
      where
        grow = _retryBase * (fromIntegral _retryGrowth ^^ (n - 1))

    Exponential{..} = _svcRetry s
