{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- Module      : Network.AWS.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

    -- * Util
    , nonEmpty

    -- * Lenses
    , module Control.Lens
    ) where

import           Control.Lens
import           Data.ByteString    (ByteString)
import           Data.Maybe
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Network.AWS.Data
import           Network.AWS.Error
import           Network.AWS.Types
import           Network.HTTP.Types

type Acceptor a = Request a -> Response' a -> Maybe Accept

data Accept
    = AcceptSuccess
    | AcceptFailure
    | AcceptRetry
      deriving (Eq, Show)

instance ToBuilder Accept where
    build = \case
        AcceptSuccess -> "Success"
        AcceptFailure -> "Failure"
        AcceptRetry   -> "Retry"

-- | Timing and acceptance criteria to check fulfillment of a remote operation.
data Wait a = Wait
    { _waitName      :: !ByteString
    , _waitAttempts  :: !Int
    , _waitDelay     :: !Int
    , _waitAcceptors :: [Acceptor a]
    }

accept :: Wait a -> Acceptor a
accept w rq rs = listToMaybe . mapMaybe (\f -> f rq rs) $ _waitAcceptors w

matchAll :: Eq b => b -> Accept -> Fold (Rs a) b -> Acceptor a
matchAll x a l = match (allOf l (== x)) a

matchAny :: Eq b => b -> Accept -> Fold (Rs a) b -> Acceptor a
matchAny x a l = match (anyOf l (== x)) a

matchStatus :: Int -> Accept -> Acceptor a
matchStatus x a _ = \case
    Left (ServiceError _ s _)
        | x == statusCode s -> Just a
    Right (s, _)
        | x == statusCode s -> Just a
    _                       -> Nothing

matchError :: AWSErrorCode (Er (Sv a)) => ErrorCode -> Accept -> Acceptor a
matchError c a _ = \case
    Left (ServiceError _ _ e)
        | c == awsErrorCode e -> Just a
    _                         -> Nothing

match :: (Rs a -> Bool) -> Accept -> Acceptor a
match f a _ = \case
    Right (_, rs)
        | f rs -> Just a
    _          -> Nothing

nonEmpty :: Fold a Text -> Fold a Bool
nonEmpty l = l . to Text.null
