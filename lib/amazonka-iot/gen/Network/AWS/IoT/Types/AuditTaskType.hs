{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditTaskType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditTaskType where

import Network.AWS.Prelude

data AuditTaskType
  = OnDemandAuditTask
  | ScheduledAuditTask
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

instance FromText AuditTaskType where
  parser =
    takeLowerText >>= \case
      "on_demand_audit_task" -> pure OnDemandAuditTask
      "scheduled_audit_task" -> pure ScheduledAuditTask
      e ->
        fromTextError $
          "Failure parsing AuditTaskType from value: '" <> e
            <> "'. Accepted values: on_demand_audit_task, scheduled_audit_task"

instance ToText AuditTaskType where
  toText = \case
    OnDemandAuditTask -> "ON_DEMAND_AUDIT_TASK"
    ScheduledAuditTask -> "SCHEDULED_AUDIT_TASK"

instance Hashable AuditTaskType

instance NFData AuditTaskType

instance ToByteString AuditTaskType

instance ToQuery AuditTaskType

instance ToHeader AuditTaskType

instance ToJSON AuditTaskType where
  toJSON = toJSONText

instance FromJSON AuditTaskType where
  parseJSON = parseJSONText "AuditTaskType"
