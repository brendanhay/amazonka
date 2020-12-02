{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationState where

import Network.AWS.Prelude

-- | Current reservation state
data ReservationState
  = RSActive
  | RSCanceled
  | RSDeleted
  | RSExpired
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ReservationState where
  parser =
    takeLowerText >>= \case
      "active" -> pure RSActive
      "canceled" -> pure RSCanceled
      "deleted" -> pure RSDeleted
      "expired" -> pure RSExpired
      e ->
        fromTextError $
          "Failure parsing ReservationState from value: '" <> e
            <> "'. Accepted values: active, canceled, deleted, expired"

instance ToText ReservationState where
  toText = \case
    RSActive -> "ACTIVE"
    RSCanceled -> "CANCELED"
    RSDeleted -> "DELETED"
    RSExpired -> "EXPIRED"

instance Hashable ReservationState

instance NFData ReservationState

instance ToByteString ReservationState

instance ToQuery ReservationState

instance ToHeader ReservationState

instance FromJSON ReservationState where
  parseJSON = parseJSONText "ReservationState"
