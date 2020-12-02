{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionPlacementState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionPlacementState where

import Network.AWS.Prelude

data GameSessionPlacementState
  = GSPSCancelled
  | GSPSFailed
  | GSPSFulfilled
  | GSPSPending
  | GSPSTimedOut
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

instance FromText GameSessionPlacementState where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure GSPSCancelled
      "failed" -> pure GSPSFailed
      "fulfilled" -> pure GSPSFulfilled
      "pending" -> pure GSPSPending
      "timed_out" -> pure GSPSTimedOut
      e ->
        fromTextError $
          "Failure parsing GameSessionPlacementState from value: '" <> e
            <> "'. Accepted values: cancelled, failed, fulfilled, pending, timed_out"

instance ToText GameSessionPlacementState where
  toText = \case
    GSPSCancelled -> "CANCELLED"
    GSPSFailed -> "FAILED"
    GSPSFulfilled -> "FULFILLED"
    GSPSPending -> "PENDING"
    GSPSTimedOut -> "TIMED_OUT"

instance Hashable GameSessionPlacementState

instance NFData GameSessionPlacementState

instance ToByteString GameSessionPlacementState

instance ToQuery GameSessionPlacementState

instance ToHeader GameSessionPlacementState

instance FromJSON GameSessionPlacementState where
  parseJSON = parseJSONText "GameSessionPlacementState"
