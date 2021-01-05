-- |
-- Module      : Network.AWS.Waiter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Waiter
  ( -- * Types
    Acceptor,
    Accept (..),
    Wait (..),

    -- * Acceptoresponse
    accept,

    -- * Matcheresponse
    matchAll,
    matchAny,
    matchNonEmpty,
    matchError,
    matchStatus,
  )
where

import Control.Lens (Fold, (^..), (^?))
import qualified Control.Lens as Lens
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Network.AWS.Data.ByteString
import Network.AWS.Data.Log
import Network.AWS.Error
import Network.AWS.Types

type Acceptor request response =
  Request request ->
  Either Error (Response response) ->
  Maybe Accept

data Accept
  = AcceptSuccess
  | AcceptFailure
  | AcceptRetry
  deriving (Eq, Show)

-- instance ToLog Accept where
--   build = \case
--     AcceptSuccess -> "Success"
--     AcceptFailure -> "Failure"
--     AcceptRetry -> "Retry"

-- | Timing and acceptance criteria to check fulfillment of a remote operation.
data Wait request response = Wait
  { waitName :: ByteString,
    waitAttempts :: Int,
    waitDelay :: Seconds,
    waitAcceptors :: Vector (Acceptor request response)
  }

accept :: Wait request response -> Acceptor request response
accept wait request response =
  Maybe.listToMaybe
    . Maybe.mapMaybe (\f -> f request response)
    $ waitAcceptoresponse wait
{-# INLINEABLE accept #-}

matchAll :: Eq a => a -> Accept -> Fold response a -> Acceptor request response
matchAll x a l = match (Lens.allOf l (== x)) a
{-# INLINEABLE matchAll #-}

{-# INLINEABLE accept #-}

matchAny :: Eq a => a -> Accept -> Fold response a -> Acceptor request response
matchAny x a l = match (Lens.anyOf l (== x)) a
{-# INLINEABLE matchAny #-}

matchNonEmpty :: Bool -> Accept -> Fold response a -> Acceptor request response
matchNonEmpty x a l = match (\response -> null (response ^.. l) == x) a
{-# INLINEABLE matchNonEmpty #-}

matchStatus :: Int -> Accept -> Acceptor request response
matchStatus x a _ = \case
  Right (s, _) | x == fromEnum s -> Just a
  Left e | Just x == (fromEnum <$> e ^? httpStatus) -> Just a
  _ -> Nothing
{-# INLINEABLE matchStatus #-}

matchError :: ErrorCode -> Accept -> Acceptor request response
matchError c a _ = \case
  Left e | Just c == e ^? _ServiceError . serviceCode -> Just a
  _ -> Nothing
{-# INLINEABLE matchError #-}

match :: (response -> Bool) -> Accept -> Acceptor request response
match f a _ = \case
  Right (_, response) | f response -> Just a
  _ -> Nothing
{-# INLINEABLE match #-}
