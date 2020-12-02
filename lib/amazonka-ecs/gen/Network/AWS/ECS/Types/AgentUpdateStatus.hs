{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AgentUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AgentUpdateStatus where

import Network.AWS.Prelude

data AgentUpdateStatus
  = AUSFailed
  | AUSPending
  | AUSStaged
  | AUSStaging
  | AUSUpdated
  | AUSUpdating
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

instance FromText AgentUpdateStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure AUSFailed
      "pending" -> pure AUSPending
      "staged" -> pure AUSStaged
      "staging" -> pure AUSStaging
      "updated" -> pure AUSUpdated
      "updating" -> pure AUSUpdating
      e ->
        fromTextError $
          "Failure parsing AgentUpdateStatus from value: '" <> e
            <> "'. Accepted values: failed, pending, staged, staging, updated, updating"

instance ToText AgentUpdateStatus where
  toText = \case
    AUSFailed -> "FAILED"
    AUSPending -> "PENDING"
    AUSStaged -> "STAGED"
    AUSStaging -> "STAGING"
    AUSUpdated -> "UPDATED"
    AUSUpdating -> "UPDATING"

instance Hashable AgentUpdateStatus

instance NFData AgentUpdateStatus

instance ToByteString AgentUpdateStatus

instance ToQuery AgentUpdateStatus

instance ToHeader AgentUpdateStatus

instance FromJSON AgentUpdateStatus where
  parseJSON = parseJSONText "AgentUpdateStatus"
