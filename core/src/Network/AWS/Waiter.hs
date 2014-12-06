{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Waiter
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Waiter
    (
    -- * Types
      Acceptor
    , Accept (..)
    , Wait   (..)

    -- * Run Acceptors
    , accept

    -- * Matchers
    , path
    , pathAll
    , pathAny
    , error
    , status
    ) where

import Control.Lens
import Data.ByteString    (ByteString)
import Data.Maybe
import Network.AWS.Error
import Network.AWS.Types
import Network.HTTP.Types
import Prelude            hiding (error)

type Acceptor a = a -> Status -> Response a -> Maybe Accept

data Accept
    = Success
    | Failure
    | Retry
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

path :: Eq b => Getter (Rs a) b -> b -> Accept -> Acceptor a
path l x = pathAll l x

pathAll :: Eq b => Fold (Rs a) b -> b -> Accept -> Acceptor a
pathAll l x = match (allOf l (== x))

pathAny :: Eq b => Fold (Rs a) b -> b -> Accept -> Acceptor a
pathAny l x = match (anyOf l (== x))

match :: (Rs a -> Bool) -> Accept -> Acceptor a
match l a _ _ = \case
    Right rs
        | l rs -> Just a
    _          -> Nothing

status :: Int -> Accept -> Acceptor a
status x a _ (statusCode -> y) _
    | x == y    = Just a
    | otherwise = Nothing

error :: AWSErrorCode (Er (Sv a)) => ErrorCode -> Accept -> Acceptor a
error c a _ _ = \case
    Left (ServiceError _ _ e)
        | c == awsErrorCode e -> Just a
    _                         -> Nothing
