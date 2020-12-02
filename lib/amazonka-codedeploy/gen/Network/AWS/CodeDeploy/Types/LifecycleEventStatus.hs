{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleEventStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleEventStatus where

import Network.AWS.Prelude

data LifecycleEventStatus
  = LESFailed
  | LESInProgress
  | LESPending
  | LESSkipped
  | LESSucceeded
  | LESUnknown
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

instance FromText LifecycleEventStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure LESFailed
      "inprogress" -> pure LESInProgress
      "pending" -> pure LESPending
      "skipped" -> pure LESSkipped
      "succeeded" -> pure LESSucceeded
      "unknown" -> pure LESUnknown
      e ->
        fromTextError $
          "Failure parsing LifecycleEventStatus from value: '" <> e
            <> "'. Accepted values: failed, inprogress, pending, skipped, succeeded, unknown"

instance ToText LifecycleEventStatus where
  toText = \case
    LESFailed -> "Failed"
    LESInProgress -> "InProgress"
    LESPending -> "Pending"
    LESSkipped -> "Skipped"
    LESSucceeded -> "Succeeded"
    LESUnknown -> "Unknown"

instance Hashable LifecycleEventStatus

instance NFData LifecycleEventStatus

instance ToByteString LifecycleEventStatus

instance ToQuery LifecycleEventStatus

instance ToHeader LifecycleEventStatus

instance ToJSON LifecycleEventStatus where
  toJSON = toJSONText

instance FromJSON LifecycleEventStatus where
  parseJSON = parseJSONText "LifecycleEventStatus"
