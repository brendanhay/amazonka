{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionStatus
  ( AutomationExecutionStatus
      ( AutomationExecutionStatus',
        AutomationExecutionStatusPending,
        AutomationExecutionStatusInProgress,
        AutomationExecutionStatusWaiting,
        AutomationExecutionStatusSuccess,
        AutomationExecutionStatusTimedOut,
        AutomationExecutionStatusCancelling,
        AutomationExecutionStatusCancelled,
        AutomationExecutionStatusFailed,
        fromAutomationExecutionStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AutomationExecutionStatus = AutomationExecutionStatus'
  { fromAutomationExecutionStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AutomationExecutionStatusPending :: AutomationExecutionStatus
pattern AutomationExecutionStatusPending = AutomationExecutionStatus' "Pending"

pattern AutomationExecutionStatusInProgress :: AutomationExecutionStatus
pattern AutomationExecutionStatusInProgress = AutomationExecutionStatus' "InProgress"

pattern AutomationExecutionStatusWaiting :: AutomationExecutionStatus
pattern AutomationExecutionStatusWaiting = AutomationExecutionStatus' "Waiting"

pattern AutomationExecutionStatusSuccess :: AutomationExecutionStatus
pattern AutomationExecutionStatusSuccess = AutomationExecutionStatus' "Success"

pattern AutomationExecutionStatusTimedOut :: AutomationExecutionStatus
pattern AutomationExecutionStatusTimedOut = AutomationExecutionStatus' "TimedOut"

pattern AutomationExecutionStatusCancelling :: AutomationExecutionStatus
pattern AutomationExecutionStatusCancelling = AutomationExecutionStatus' "Cancelling"

pattern AutomationExecutionStatusCancelled :: AutomationExecutionStatus
pattern AutomationExecutionStatusCancelled = AutomationExecutionStatus' "Cancelled"

pattern AutomationExecutionStatusFailed :: AutomationExecutionStatus
pattern AutomationExecutionStatusFailed = AutomationExecutionStatus' "Failed"

{-# COMPLETE
  AutomationExecutionStatusPending,
  AutomationExecutionStatusInProgress,
  AutomationExecutionStatusWaiting,
  AutomationExecutionStatusSuccess,
  AutomationExecutionStatusTimedOut,
  AutomationExecutionStatusCancelling,
  AutomationExecutionStatusCancelled,
  AutomationExecutionStatusFailed,
  AutomationExecutionStatus'
  #-}
