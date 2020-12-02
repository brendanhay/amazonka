{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelSetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ThreatIntelSetStatus where

import Network.AWS.Prelude

data ThreatIntelSetStatus
  = Activating
  | Active
  | Deactivating
  | DeletePending
  | Deleted
  | Error'
  | Inactive
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

instance FromText ThreatIntelSetStatus where
  parser =
    takeLowerText >>= \case
      "activating" -> pure Activating
      "active" -> pure Active
      "deactivating" -> pure Deactivating
      "delete_pending" -> pure DeletePending
      "deleted" -> pure Deleted
      "error" -> pure Error'
      "inactive" -> pure Inactive
      e ->
        fromTextError $
          "Failure parsing ThreatIntelSetStatus from value: '" <> e
            <> "'. Accepted values: activating, active, deactivating, delete_pending, deleted, error, inactive"

instance ToText ThreatIntelSetStatus where
  toText = \case
    Activating -> "ACTIVATING"
    Active -> "ACTIVE"
    Deactivating -> "DEACTIVATING"
    DeletePending -> "DELETE_PENDING"
    Deleted -> "DELETED"
    Error' -> "ERROR"
    Inactive -> "INACTIVE"

instance Hashable ThreatIntelSetStatus

instance NFData ThreatIntelSetStatus

instance ToByteString ThreatIntelSetStatus

instance ToQuery ThreatIntelSetStatus

instance ToHeader ThreatIntelSetStatus

instance FromJSON ThreatIntelSetStatus where
  parseJSON = parseJSONText "ThreatIntelSetStatus"
