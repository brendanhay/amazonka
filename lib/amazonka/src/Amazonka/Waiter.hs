-- |
-- Module      : Amazonka.Waiter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Waiter
  ( -- * Types
    Acceptor,
    Accept (..),
    Wait (..),

    -- * Acceptors
    accept,

    -- * Matchers
    matchAll,
    matchAny,
    matchNonEmpty,
    matchError,
    matchStatus,

    -- * Util
    nonEmptyText,
  )
where

import Amazonka.Data
import Amazonka.Error (_HttpStatus)
import Amazonka.Lens
  ( Fold,
    allOf,
    anyOf,
    to,
    (^..),
    (^?),
  )
import Amazonka.Prelude
import Amazonka.Types
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client

type Acceptor a = Request a -> Either Error (ClientResponse (AWSResponse a)) -> Maybe Accept

data Accept
  = AcceptSuccess
  | AcceptFailure
  | AcceptRetry
  deriving stock (Eq, Show)

instance ToLog Accept where
  build = \case
    AcceptSuccess -> "Success"
    AcceptFailure -> "Failure"
    AcceptRetry -> "Retry"

-- | Timing and acceptance criteria to check fulfillment of a remote operation.
data Wait a = Wait
  { _waitName :: ByteString,
    _waitAttempts :: Int,
    _waitDelay :: Seconds,
    _waitAcceptors :: [Acceptor a]
  }

accept :: Wait a -> Acceptor a
accept w rq rs = listToMaybe . mapMaybe (\f -> f rq rs) $ _waitAcceptors w

matchAll :: Eq b => b -> Accept -> Fold (AWSResponse a) b -> Acceptor a
matchAll x a l = match (allOf l (== x)) a

matchAny :: Eq b => b -> Accept -> Fold (AWSResponse a) b -> Acceptor a
matchAny x a l = match (anyOf l (== x)) a

matchNonEmpty :: Bool -> Accept -> Fold (AWSResponse a) b -> Acceptor a
matchNonEmpty x a l = match (\rs -> null (rs ^.. l) == x) a

matchStatus :: Int -> Accept -> Acceptor a
matchStatus x a _ = \case
  Right rs | x == fromEnum (Client.responseStatus rs) -> Just a
  Left e | Just x == (fromEnum <$> e ^? _HttpStatus) -> Just a
  _ -> Nothing

matchError :: ErrorCode -> Accept -> Acceptor a
matchError c a _ = \case
  Left e | Just c == e ^? _ServiceError . serviceCode -> Just a
  _ -> Nothing

match :: (AWSResponse a -> Bool) -> Accept -> Acceptor a
match f a _ = \case
  Right rs | f (Client.responseBody rs) -> Just a
  _ -> Nothing

nonEmptyText :: Fold a Text -> Fold a Bool
nonEmptyText f = f . to Text.null
