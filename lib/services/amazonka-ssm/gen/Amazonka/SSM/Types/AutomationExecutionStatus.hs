{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Types.AutomationExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AutomationExecutionStatus
  ( AutomationExecutionStatus
      ( ..,
        AutomationExecutionStatus_Approved,
        AutomationExecutionStatus_Cancelled,
        AutomationExecutionStatus_Cancelling,
        AutomationExecutionStatus_ChangeCalendarOverrideApproved,
        AutomationExecutionStatus_ChangeCalendarOverrideRejected,
        AutomationExecutionStatus_CompletedWithFailure,
        AutomationExecutionStatus_CompletedWithSuccess,
        AutomationExecutionStatus_Failed,
        AutomationExecutionStatus_InProgress,
        AutomationExecutionStatus_Pending,
        AutomationExecutionStatus_PendingApproval,
        AutomationExecutionStatus_PendingChangeCalendarOverride,
        AutomationExecutionStatus_Rejected,
        AutomationExecutionStatus_RunbookInProgress,
        AutomationExecutionStatus_Scheduled,
        AutomationExecutionStatus_Success,
        AutomationExecutionStatus_TimedOut,
        AutomationExecutionStatus_Waiting
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutomationExecutionStatus = AutomationExecutionStatus'
  { fromAutomationExecutionStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern AutomationExecutionStatus_Approved :: AutomationExecutionStatus
pattern AutomationExecutionStatus_Approved = AutomationExecutionStatus' "Approved"

pattern AutomationExecutionStatus_Cancelled :: AutomationExecutionStatus
pattern AutomationExecutionStatus_Cancelled = AutomationExecutionStatus' "Cancelled"

pattern AutomationExecutionStatus_Cancelling :: AutomationExecutionStatus
pattern AutomationExecutionStatus_Cancelling = AutomationExecutionStatus' "Cancelling"

pattern AutomationExecutionStatus_ChangeCalendarOverrideApproved :: AutomationExecutionStatus
pattern AutomationExecutionStatus_ChangeCalendarOverrideApproved = AutomationExecutionStatus' "ChangeCalendarOverrideApproved"

pattern AutomationExecutionStatus_ChangeCalendarOverrideRejected :: AutomationExecutionStatus
pattern AutomationExecutionStatus_ChangeCalendarOverrideRejected = AutomationExecutionStatus' "ChangeCalendarOverrideRejected"

pattern AutomationExecutionStatus_CompletedWithFailure :: AutomationExecutionStatus
pattern AutomationExecutionStatus_CompletedWithFailure = AutomationExecutionStatus' "CompletedWithFailure"

pattern AutomationExecutionStatus_CompletedWithSuccess :: AutomationExecutionStatus
pattern AutomationExecutionStatus_CompletedWithSuccess = AutomationExecutionStatus' "CompletedWithSuccess"

pattern AutomationExecutionStatus_Failed :: AutomationExecutionStatus
pattern AutomationExecutionStatus_Failed = AutomationExecutionStatus' "Failed"

pattern AutomationExecutionStatus_InProgress :: AutomationExecutionStatus
pattern AutomationExecutionStatus_InProgress = AutomationExecutionStatus' "InProgress"

pattern AutomationExecutionStatus_Pending :: AutomationExecutionStatus
pattern AutomationExecutionStatus_Pending = AutomationExecutionStatus' "Pending"

pattern AutomationExecutionStatus_PendingApproval :: AutomationExecutionStatus
pattern AutomationExecutionStatus_PendingApproval = AutomationExecutionStatus' "PendingApproval"

pattern AutomationExecutionStatus_PendingChangeCalendarOverride :: AutomationExecutionStatus
pattern AutomationExecutionStatus_PendingChangeCalendarOverride = AutomationExecutionStatus' "PendingChangeCalendarOverride"

pattern AutomationExecutionStatus_Rejected :: AutomationExecutionStatus
pattern AutomationExecutionStatus_Rejected = AutomationExecutionStatus' "Rejected"

pattern AutomationExecutionStatus_RunbookInProgress :: AutomationExecutionStatus
pattern AutomationExecutionStatus_RunbookInProgress = AutomationExecutionStatus' "RunbookInProgress"

pattern AutomationExecutionStatus_Scheduled :: AutomationExecutionStatus
pattern AutomationExecutionStatus_Scheduled = AutomationExecutionStatus' "Scheduled"

pattern AutomationExecutionStatus_Success :: AutomationExecutionStatus
pattern AutomationExecutionStatus_Success = AutomationExecutionStatus' "Success"

pattern AutomationExecutionStatus_TimedOut :: AutomationExecutionStatus
pattern AutomationExecutionStatus_TimedOut = AutomationExecutionStatus' "TimedOut"

pattern AutomationExecutionStatus_Waiting :: AutomationExecutionStatus
pattern AutomationExecutionStatus_Waiting = AutomationExecutionStatus' "Waiting"

{-# COMPLETE
  AutomationExecutionStatus_Approved,
  AutomationExecutionStatus_Cancelled,
  AutomationExecutionStatus_Cancelling,
  AutomationExecutionStatus_ChangeCalendarOverrideApproved,
  AutomationExecutionStatus_ChangeCalendarOverrideRejected,
  AutomationExecutionStatus_CompletedWithFailure,
  AutomationExecutionStatus_CompletedWithSuccess,
  AutomationExecutionStatus_Failed,
  AutomationExecutionStatus_InProgress,
  AutomationExecutionStatus_Pending,
  AutomationExecutionStatus_PendingApproval,
  AutomationExecutionStatus_PendingChangeCalendarOverride,
  AutomationExecutionStatus_Rejected,
  AutomationExecutionStatus_RunbookInProgress,
  AutomationExecutionStatus_Scheduled,
  AutomationExecutionStatus_Success,
  AutomationExecutionStatus_TimedOut,
  AutomationExecutionStatus_Waiting,
  AutomationExecutionStatus'
  #-}
