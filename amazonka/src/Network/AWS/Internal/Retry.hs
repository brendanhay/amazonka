{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Internal.Retry
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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

import Control.Monad.IO.Class
import Control.Retry
import Data.List                (intersperse)
import Data.Monoid
import Network.AWS.Internal.Env
import Network.AWS.Internal.Log
import Network.AWS.Prelude
import Network.AWS.Types
import Network.AWS.Waiters

retrier :: (MonadIO m, AWSService (Sv a))
        => Env
        -> Request a
        -> m (Response' a)
        -> m (Response' a)
retrier Env{..} rq = retrying (fromMaybe (retryPolicy rq) _envRetry) check
  where
    check n rs
        | Left (HttpError    _)     <- rs
                    = msg n >> return True
        | Left (ServiceError _ s e) <- rs
        , _retryCheck (_svcRetry (serviceOf rq)) s e
                    = msg n >> return True
        | otherwise = return False

    msg n = debug _envLogger $
        "[Retrying] after " <> build (n + 1) <> " attempts."

waiter :: MonadIO m
       => Env
       -> Wait a
       -> Request a
       -> m (Response' a)
       -> m (Response' a)
waiter Env{..} w rq = retrying (awaitPolicy w) check
  where
    check n rs = do
        let a = fromMaybe AcceptRetry (accept w rq rs)
        msg n a >> return (retry a)

    retry AcceptSuccess = False
    retry AcceptFailure = False
    retry AcceptRetry   = True

    msg n a = debug _envLogger
        . mconcat
        . intersperse " "
        $ [ "[Await " <> build (_waitName w) <> "]"
          , build a
          , " after "
          , build (n + 1)
          , "attempts."
          ]

awaitPolicy :: Wait a -> RetryPolicy
awaitPolicy Wait{..} = limitRetries _waitAttempts <> constantDelay _waitDelay

retryPolicy :: AWSService (Sv a) => Request a -> RetryPolicy
retryPolicy rq = streamingPolicy rq <> servicePolicy rq

servicePolicy :: AWSService (Sv a) => Request a -> RetryPolicy
servicePolicy rq = limitRetries _retryAttempts <> RetryPolicy delay
  where
    delay n
        | n > 0     = Just $ truncate (grow n * 1000000)
        | otherwise = Nothing

    grow n = _retryBase * (fromIntegral _retryGrowth ^^ (n - 1))

    Exponential{..} = _svcRetry (serviceOf rq)

streamingPolicy :: Request a -> RetryPolicy
streamingPolicy rq = RetryPolicy . const $ listToMaybe [0 | not p]
  where
    !p = isStreaming (_rqBody rq)
