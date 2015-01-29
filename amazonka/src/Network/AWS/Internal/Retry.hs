{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import Control.Monad
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
retrier Env{..} rq = retrying (fromMaybe policy _envRetryPolicy) check
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
        Left (ServiceError _ s e)
            | _retryCheck s e -> msg n >> return True
        Left (HttpError e)    -> do
            p <- liftIO (_envRetryCheck n e)
            when p (msg n) >> return p
        _                     -> return False

    msg n = debug _envLogger $
        "[Retrying] after " <> build (n + 1) <> " attempts."

    Exponential{..} = _svcRetry (serviceOf rq)

waiter :: MonadIO m
       => Env
       -> Wait a
       -> Request a
       -> m (Response' a)
       -> m (Response' a)
waiter Env{..} w@Wait{..} rq = retrying policy check
  where
    policy = limitRetries _waitAttempts <> constantDelay (_waitDelay * 1000000)

    check n rs = do
        let a = fromMaybe AcceptRetry (accept w rq rs)
        msg n a >> return (retry a)

    retry AcceptSuccess = False
    retry AcceptFailure = False
    retry AcceptRetry   = True

    msg n a = debug _envLogger
        . mconcat
        . intersperse " "
        $ [ "[Await " <> build _waitName <> "]"
          , build a
          , " after "
          , build (n + 1)
          , "attempts."
          ]
