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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Internal.HTTP
    ( perform
    , retrier
    , waiter
    ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.List                    (intersperse)
import           Data.Monoid
import           Data.Time
import           Network.AWS.Env
import           Network.AWS.Logger
import           Network.AWS.Prelude
import           Network.AWS.Waiter
import           Network.HTTP.Conduit         hiding (Request, Response)

perform :: (MonadCatch m, MonadResource m, AWSSigner (Sg s), AWSRequest a)
        => Env
        -> Service s
        -> Request a
        -> m (Response a)
perform e@Env{..} svc (_svcPreflight svc -> x) =
    catch go err >>= response _envLogger svc x
  where
    go = do
        t          <- liftIO getCurrentTime
        Signed m s <- withAuth _envAuth $ \a ->
            return (signed a _envRegion t svc x)

        let rq = s { responseTimeout = timeoutFor e svc }

        logTrace _envLogger m  -- trace:Signing:Meta
        logDebug _envLogger rq -- debug:ClientRequest

        rs         <- liftResourceT (http rq _envManager)

        logDebug _envLogger rs -- debug:ClientResponse

        return $! Right rs

    err er = do
        logError _envLogger er  -- error:HttpException
        return $! Left er

retrier :: MonadIO m
        => Env
        -> Service s
        -> Request a
        -> m (Response a)
        -> m (Response a)
retrier Env{..} Service{..} rq =
    retrying (fromMaybe policy _envRetryPolicy) check
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

    check n = \case
        Left (HTTPError e) -> do
            p <- liftIO (_envRetryCheck n e)
            when p (msg "http_error" n) >> return p

        Left e | Just x <- e ^? _ServiceError . to _retryCheck . _Just ->
            msg x n >> return True

        _ -> return False

    msg x n = logDebug _envLogger
        . mconcat
        . intersperse " "
        $ [ "[Retry " <> build x <> "]"
          , " after "
          , build (n + 1)
          , "attempts."
          ]

    Exponential{..} = _svcRetry

waiter :: MonadIO m
       => Env
       -> Wait a
       -> Request a
       -> m (Response a)
       -> m (Response a)
waiter Env{..} w@Wait{..} rq = retrying policy check
  where
    policy = limitRetries _waitAttempts <> constantDelay (_waitDelay * 1000000)

    check n rs = do
        let a = fromMaybe AcceptRetry (accept w rq rs)
        msg n a >> return (retry a)

    retry AcceptSuccess = False
    retry AcceptFailure = False
    retry AcceptRetry   = True

    msg n a = logDebug _envLogger
        . mconcat
        . intersperse " "
        $ [ "[Await " <> build _waitName <> "]"
          , build a
          , " after "
          , build (n + 1)
          , "attempts."
          ]
