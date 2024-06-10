-- |
-- Module      : Amazonka.Waiter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Waiter
  ( -- * Types
    Acceptor,
    Accept (..),
    Wait (..),

    -- ** Lenses
    wait_name,
    wait_attempts,
    wait_delay,
    wait_acceptors,

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

import Amazonka.Core.Lens.Internal
  ( Fold,
    Lens,
    allOf,
    anyOf,
    fromSimpleFold,
    to,
    (^..),
    (^?),
  )
import Amazonka.Data
import Amazonka.Error (_HttpStatus)
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
  { name :: ByteString,
    attempts :: Int,
    delay :: Seconds,
    acceptors :: [Acceptor a]
  }

{-# INLINE wait_name #-}
wait_name :: Lens' (Wait a) ByteString
wait_name f w@Wait {name} = f name <&> \name' -> w {name = name'}

{-# INLINE wait_attempts #-}
wait_attempts :: forall a. Lens' (Wait a) Int
wait_attempts f w@Wait {attempts} = f attempts <&> \attempts' -> (w :: Wait a) {attempts = attempts'}

{-# INLINE wait_delay #-}
wait_delay :: Lens' (Wait a) Seconds
wait_delay f w@Wait {delay} = f delay <&> \delay' -> w {delay = delay'}

{-# INLINE wait_acceptors #-}
wait_acceptors :: Lens (Wait a) (Wait b) [Acceptor a] [Acceptor b]
wait_acceptors f w@Wait {acceptors} = f acceptors <&> \acceptors' -> w {acceptors = acceptors'}

accept :: Wait a -> Acceptor a
accept w rq rs = listToMaybe . mapMaybe (\f -> f rq rs) $ acceptors w

matchAll :: (Eq b) => b -> Accept -> Fold (AWSResponse a) b -> Acceptor a
matchAll x a l = match (allOf l (== x)) a

matchAny :: (Eq b) => b -> Accept -> Fold (AWSResponse a) b -> Acceptor a
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
  Left e | Just c == e ^? _ServiceError . to code -> Just a
  _ -> Nothing

match :: (AWSResponse a -> Bool) -> Accept -> Acceptor a
match f a _ = \case
  Right rs | f (Client.responseBody rs) -> Just a
  _ -> Nothing

nonEmptyText :: Fold a Text -> Fold a Bool
nonEmptyText f = f . fromSimpleFold (to Text.null)
