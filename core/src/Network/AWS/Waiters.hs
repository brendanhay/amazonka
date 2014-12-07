{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Waiters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Waiters
    (
    -- * Types
      Acceptor
    , Accept (..)
    , Wait   (..)

    -- * Acceptors
    , accept

    -- * Matchers
    , matchAll
    , matchAny
    , matchError
    , matchStatus

    -- * Lenses
    , module Control.Lens
    ) where

import Control.Lens
import Data.ByteString    (ByteString)
import Data.Maybe
import Network.AWS.Error
import Network.AWS.Types
import Network.HTTP.Types

type Acceptor a = a -> Status -> Response a -> Maybe Accept

data Accept
    = AcceptSuccess
    | AcceptFailure
    | AcceptRetry
      deriving (Eq, Show)

-- | Timing and acceptance criteria to check fulfillment of a remote operation.
data Wait a = Wait
    { _waitName      :: !ByteString
    , _waitAttempts  :: !Int
    , _waitDelay     :: !Int
    , _waitAcceptors :: [Acceptor a]
    }

accept :: [Acceptor a] -> Acceptor a
accept xs rq s rs = listToMaybe $ mapMaybe (\f -> f rq s rs) xs

matchAll :: Eq b => b -> Accept -> Fold (Rs a) b -> Acceptor a
matchAll x a l = match (allOf l (== x)) a

matchAny :: Eq b => b -> Accept -> Fold (Rs a) b -> Acceptor a
matchAny x a l = match (anyOf l (== x)) a

matchStatus :: Int -> Accept -> Acceptor a
matchStatus x a _ (statusCode -> y) _
    | x == y    = Just a
    | otherwise = Nothing

matchError :: AWSErrorCode (Er (Sv a)) => ErrorCode -> Accept -> Acceptor a
matchError c a _ _ = \case
    Left (ServiceError _ _ e)
        | c == awsErrorCode e -> Just a
    _                         -> Nothing

match :: (Rs a -> Bool) -> Accept -> Acceptor a
match f a _ _ = \case
    Right rs
        | f rs -> Just a
    _          -> Nothing
