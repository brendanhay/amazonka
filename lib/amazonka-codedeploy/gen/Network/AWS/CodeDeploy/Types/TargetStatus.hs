{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetStatus where

import Network.AWS.Prelude

data TargetStatus
  = TSFailed
  | TSInProgress
  | TSPending
  | TSReady
  | TSSkipped
  | TSSucceeded
  | TSUnknown
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

instance FromText TargetStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure TSFailed
      "inprogress" -> pure TSInProgress
      "pending" -> pure TSPending
      "ready" -> pure TSReady
      "skipped" -> pure TSSkipped
      "succeeded" -> pure TSSucceeded
      "unknown" -> pure TSUnknown
      e ->
        fromTextError $
          "Failure parsing TargetStatus from value: '" <> e
            <> "'. Accepted values: failed, inprogress, pending, ready, skipped, succeeded, unknown"

instance ToText TargetStatus where
  toText = \case
    TSFailed -> "Failed"
    TSInProgress -> "InProgress"
    TSPending -> "Pending"
    TSReady -> "Ready"
    TSSkipped -> "Skipped"
    TSSucceeded -> "Succeeded"
    TSUnknown -> "Unknown"

instance Hashable TargetStatus

instance NFData TargetStatus

instance ToByteString TargetStatus

instance ToQuery TargetStatus

instance ToHeader TargetStatus

instance FromJSON TargetStatus where
  parseJSON = parseJSONText "TargetStatus"
