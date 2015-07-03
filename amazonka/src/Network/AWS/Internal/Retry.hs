{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Internal.Retry
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Retry
    ( retrier
    , waiter
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Retry
import           Data.List              (intersperse)
import           Data.Monoid
import           Network.AWS.Env
import           Network.AWS.Logger
import           Network.AWS.Prelude
import           Network.AWS.Waiter

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
        !stream = isStreaming (_rqBody rq)

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
