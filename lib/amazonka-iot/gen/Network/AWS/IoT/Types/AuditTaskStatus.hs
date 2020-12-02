{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditTaskStatus where

import Network.AWS.Prelude

data AuditTaskStatus
  = ATSCanceled
  | ATSCompleted
  | ATSFailed
  | ATSInProgress
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

instance FromText AuditTaskStatus where
  parser =
    takeLowerText >>= \case
      "canceled" -> pure ATSCanceled
      "completed" -> pure ATSCompleted
      "failed" -> pure ATSFailed
      "in_progress" -> pure ATSInProgress
      e ->
        fromTextError $
          "Failure parsing AuditTaskStatus from value: '" <> e
            <> "'. Accepted values: canceled, completed, failed, in_progress"

instance ToText AuditTaskStatus where
  toText = \case
    ATSCanceled -> "CANCELED"
    ATSCompleted -> "COMPLETED"
    ATSFailed -> "FAILED"
    ATSInProgress -> "IN_PROGRESS"

instance Hashable AuditTaskStatus

instance NFData AuditTaskStatus

instance ToByteString AuditTaskStatus

instance ToQuery AuditTaskStatus

instance ToHeader AuditTaskStatus

instance ToJSON AuditTaskStatus where
  toJSON = toJSONText

instance FromJSON AuditTaskStatus where
  parseJSON = parseJSONText "AuditTaskStatus"
