{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingConfigurationStatus where

import Network.AWS.Prelude

data MatchmakingConfigurationStatus
  = MCSCancelled
  | MCSCompleted
  | MCSFailed
  | MCSPlacing
  | MCSQueued
  | MCSRequiresAcceptance
  | MCSSearching
  | MCSTimedOut
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

instance FromText MatchmakingConfigurationStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure MCSCancelled
      "completed" -> pure MCSCompleted
      "failed" -> pure MCSFailed
      "placing" -> pure MCSPlacing
      "queued" -> pure MCSQueued
      "requires_acceptance" -> pure MCSRequiresAcceptance
      "searching" -> pure MCSSearching
      "timed_out" -> pure MCSTimedOut
      e ->
        fromTextError $
          "Failure parsing MatchmakingConfigurationStatus from value: '" <> e
            <> "'. Accepted values: cancelled, completed, failed, placing, queued, requires_acceptance, searching, timed_out"

instance ToText MatchmakingConfigurationStatus where
  toText = \case
    MCSCancelled -> "CANCELLED"
    MCSCompleted -> "COMPLETED"
    MCSFailed -> "FAILED"
    MCSPlacing -> "PLACING"
    MCSQueued -> "QUEUED"
    MCSRequiresAcceptance -> "REQUIRES_ACCEPTANCE"
    MCSSearching -> "SEARCHING"
    MCSTimedOut -> "TIMED_OUT"

instance Hashable MatchmakingConfigurationStatus

instance NFData MatchmakingConfigurationStatus

instance ToByteString MatchmakingConfigurationStatus

instance ToQuery MatchmakingConfigurationStatus

instance ToHeader MatchmakingConfigurationStatus

instance FromJSON MatchmakingConfigurationStatus where
  parseJSON = parseJSONText "MatchmakingConfigurationStatus"
