{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
  ( MaintenanceWindowExecutionStatus
      ( MaintenanceWindowExecutionStatus',
        MWESCancelled,
        MWESCancelling,
        MWESFailed,
        MWESInProgress,
        MWESPending,
        MWESSkippedOverlapping,
        MWESSuccess,
        MWESTimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MaintenanceWindowExecutionStatus = MaintenanceWindowExecutionStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern MWESCancelled :: MaintenanceWindowExecutionStatus
pattern MWESCancelled = MaintenanceWindowExecutionStatus' "CANCELLED"

pattern MWESCancelling :: MaintenanceWindowExecutionStatus
pattern MWESCancelling = MaintenanceWindowExecutionStatus' "CANCELLING"

pattern MWESFailed :: MaintenanceWindowExecutionStatus
pattern MWESFailed = MaintenanceWindowExecutionStatus' "FAILED"

pattern MWESInProgress :: MaintenanceWindowExecutionStatus
pattern MWESInProgress = MaintenanceWindowExecutionStatus' "IN_PROGRESS"

pattern MWESPending :: MaintenanceWindowExecutionStatus
pattern MWESPending = MaintenanceWindowExecutionStatus' "PENDING"

pattern MWESSkippedOverlapping :: MaintenanceWindowExecutionStatus
pattern MWESSkippedOverlapping = MaintenanceWindowExecutionStatus' "SKIPPED_OVERLAPPING"

pattern MWESSuccess :: MaintenanceWindowExecutionStatus
pattern MWESSuccess = MaintenanceWindowExecutionStatus' "SUCCESS"

pattern MWESTimedOut :: MaintenanceWindowExecutionStatus
pattern MWESTimedOut = MaintenanceWindowExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  MWESCancelled,
  MWESCancelling,
  MWESFailed,
  MWESInProgress,
  MWESPending,
  MWESSkippedOverlapping,
  MWESSuccess,
  MWESTimedOut,
  MaintenanceWindowExecutionStatus'
  #-}
