{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
  ( MaintenanceWindowExecutionStatus
    ( MaintenanceWindowExecutionStatus'
    , MaintenanceWindowExecutionStatusPending
    , MaintenanceWindowExecutionStatusInProgress
    , MaintenanceWindowExecutionStatusSuccess
    , MaintenanceWindowExecutionStatusFailed
    , MaintenanceWindowExecutionStatusTimedOut
    , MaintenanceWindowExecutionStatusCancelling
    , MaintenanceWindowExecutionStatusCancelled
    , MaintenanceWindowExecutionStatusSkippedOverlapping
    , fromMaintenanceWindowExecutionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MaintenanceWindowExecutionStatus = MaintenanceWindowExecutionStatus'{fromMaintenanceWindowExecutionStatus
                                                                             :: Core.Text}
                                             deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                             Core.Show, Core.Generic)
                                             deriving newtype (Core.IsString, Core.Hashable,
                                                               Core.NFData, Core.ToJSONKey,
                                                               Core.FromJSONKey, Core.ToJSON,
                                                               Core.FromJSON, Core.ToXML,
                                                               Core.FromXML, Core.ToText,
                                                               Core.FromText, Core.ToByteString,
                                                               Core.ToQuery, Core.ToHeader)

pattern MaintenanceWindowExecutionStatusPending :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatusPending = MaintenanceWindowExecutionStatus' "PENDING"

pattern MaintenanceWindowExecutionStatusInProgress :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatusInProgress = MaintenanceWindowExecutionStatus' "IN_PROGRESS"

pattern MaintenanceWindowExecutionStatusSuccess :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatusSuccess = MaintenanceWindowExecutionStatus' "SUCCESS"

pattern MaintenanceWindowExecutionStatusFailed :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatusFailed = MaintenanceWindowExecutionStatus' "FAILED"

pattern MaintenanceWindowExecutionStatusTimedOut :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatusTimedOut = MaintenanceWindowExecutionStatus' "TIMED_OUT"

pattern MaintenanceWindowExecutionStatusCancelling :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatusCancelling = MaintenanceWindowExecutionStatus' "CANCELLING"

pattern MaintenanceWindowExecutionStatusCancelled :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatusCancelled = MaintenanceWindowExecutionStatus' "CANCELLED"

pattern MaintenanceWindowExecutionStatusSkippedOverlapping :: MaintenanceWindowExecutionStatus
pattern MaintenanceWindowExecutionStatusSkippedOverlapping = MaintenanceWindowExecutionStatus' "SKIPPED_OVERLAPPING"

{-# COMPLETE 
  MaintenanceWindowExecutionStatusPending,

  MaintenanceWindowExecutionStatusInProgress,

  MaintenanceWindowExecutionStatusSuccess,

  MaintenanceWindowExecutionStatusFailed,

  MaintenanceWindowExecutionStatusTimedOut,

  MaintenanceWindowExecutionStatusCancelling,

  MaintenanceWindowExecutionStatusCancelled,

  MaintenanceWindowExecutionStatusSkippedOverlapping,
  MaintenanceWindowExecutionStatus'
  #-}
