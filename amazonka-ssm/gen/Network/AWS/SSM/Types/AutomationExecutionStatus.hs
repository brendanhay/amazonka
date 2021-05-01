{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype AutomationExecutionStatus = AutomationExecutionStatus'
  { fromAutomationExecutionStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
