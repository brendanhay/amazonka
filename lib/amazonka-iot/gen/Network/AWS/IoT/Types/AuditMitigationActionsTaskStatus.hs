{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus where

import Network.AWS.Prelude

data AuditMitigationActionsTaskStatus
  = Canceled
  | Completed
  | Failed
  | InProgress
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

instance FromText AuditMitigationActionsTaskStatus where
  parser =
    takeLowerText >>= \case
      "canceled" -> pure Canceled
      "completed" -> pure Completed
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      e ->
        fromTextError $
          "Failure parsing AuditMitigationActionsTaskStatus from value: '" <> e
            <> "'. Accepted values: canceled, completed, failed, in_progress"

instance ToText AuditMitigationActionsTaskStatus where
  toText = \case
    Canceled -> "CANCELED"
    Completed -> "COMPLETED"
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"

instance Hashable AuditMitigationActionsTaskStatus

instance NFData AuditMitigationActionsTaskStatus

instance ToByteString AuditMitigationActionsTaskStatus

instance ToQuery AuditMitigationActionsTaskStatus

instance ToHeader AuditMitigationActionsTaskStatus

instance ToJSON AuditMitigationActionsTaskStatus where
  toJSON = toJSONText

instance FromJSON AuditMitigationActionsTaskStatus where
  parseJSON = parseJSONText "AuditMitigationActionsTaskStatus"
