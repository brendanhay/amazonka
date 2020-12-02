{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditCheckRunStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditCheckRunStatus where

import Network.AWS.Prelude

data AuditCheckRunStatus
  = ACRSCanceled
  | ACRSCompletedCompliant
  | ACRSCompletedNonCompliant
  | ACRSFailed
  | ACRSInProgress
  | ACRSWaitingForDataCollection
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

instance FromText AuditCheckRunStatus where
  parser =
    takeLowerText >>= \case
      "canceled" -> pure ACRSCanceled
      "completed_compliant" -> pure ACRSCompletedCompliant
      "completed_non_compliant" -> pure ACRSCompletedNonCompliant
      "failed" -> pure ACRSFailed
      "in_progress" -> pure ACRSInProgress
      "waiting_for_data_collection" -> pure ACRSWaitingForDataCollection
      e ->
        fromTextError $
          "Failure parsing AuditCheckRunStatus from value: '" <> e
            <> "'. Accepted values: canceled, completed_compliant, completed_non_compliant, failed, in_progress, waiting_for_data_collection"

instance ToText AuditCheckRunStatus where
  toText = \case
    ACRSCanceled -> "CANCELED"
    ACRSCompletedCompliant -> "COMPLETED_COMPLIANT"
    ACRSCompletedNonCompliant -> "COMPLETED_NON_COMPLIANT"
    ACRSFailed -> "FAILED"
    ACRSInProgress -> "IN_PROGRESS"
    ACRSWaitingForDataCollection -> "WAITING_FOR_DATA_COLLECTION"

instance Hashable AuditCheckRunStatus

instance NFData AuditCheckRunStatus

instance ToByteString AuditCheckRunStatus

instance ToQuery AuditCheckRunStatus

instance ToHeader AuditCheckRunStatus

instance FromJSON AuditCheckRunStatus where
  parseJSON = parseJSONText "AuditCheckRunStatus"
