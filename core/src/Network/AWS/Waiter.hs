{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Network.AWS.Waiter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Waiter
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
    , matchNonEmpty
    , matchError
    , matchStatus

    -- * Util
    , nonEmptyText
    ) where

import           Control.Lens                (Fold, allOf, anyOf, to, (^..),
                                              (^?))
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Log
import           Network.AWS.Error
import           Network.AWS.Types

type Acceptor a = Request a -> Either Error (Response a) -> Maybe Accept

data Accept
    = AcceptSuccess
    | AcceptFailure
    | AcceptRetry
      deriving (Eq, Show)

instance ToLog Accept where
    build = \case
        AcceptSuccess -> "Success"
        AcceptFailure -> "Failure"
        AcceptRetry   -> "Retry"

-- | Timing and acceptance criteria to check fulfillment of a remote operation.
data Wait a = Wait
    { _waitName      :: ByteString
    , _waitAttempts  :: !Int
    , _waitDelay     :: !Seconds
    , _waitAcceptors :: [Acceptor a]
    }

accept :: Wait a -> Acceptor a
accept w rq rs = listToMaybe . mapMaybe (\f -> f rq rs) $ _waitAcceptors w

matchAll :: Eq b => b -> Accept -> Fold (Rs a) b -> Acceptor a
matchAll x a l = match (allOf l (== x)) a

matchAny :: Eq b => b -> Accept -> Fold (Rs a) b -> Acceptor a
matchAny x a l = match (anyOf l (== x)) a

matchNonEmpty :: Bool -> Accept -> Fold (Rs a) b -> Acceptor a
matchNonEmpty x a l = match (\rs -> null (rs ^.. l) == x) a

matchStatus :: Int -> Accept -> Acceptor a
matchStatus x a _ = \case
    Right (s, _) | x == fromEnum s                           -> Just a
    Left  e      | Just x == (fromEnum <$> e ^? _HttpStatus) -> Just a
    _                                                        -> Nothing

matchError :: ErrorCode -> Accept -> Acceptor a
matchError c a _ = \case
    Left e | Just c == e ^? _ServiceError . serviceCode -> Just a
    _                                                   -> Nothing

match :: (Rs a -> Bool) -> Accept -> Acceptor a
match f a _ = \case
    Right (_, rs) | f rs -> Just a
    _                    -> Nothing

nonEmptyText :: Fold a Text -> Fold a Bool
nonEmptyText f = f . to Text.null
