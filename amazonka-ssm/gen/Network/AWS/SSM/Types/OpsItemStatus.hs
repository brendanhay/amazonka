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
-- Module      : Network.AWS.SSM.Types.OpsItemStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemStatus
  ( OpsItemStatus
      ( ..,
        OpsItemStatus_Approved,
        OpsItemStatus_Cancelled,
        OpsItemStatus_Cancelling,
        OpsItemStatus_ChangeCalendarOverrideApproved,
        OpsItemStatus_ChangeCalendarOverrideRejected,
        OpsItemStatus_CompletedWithFailure,
        OpsItemStatus_CompletedWithSuccess,
        OpsItemStatus_Failed,
        OpsItemStatus_InProgress,
        OpsItemStatus_Open,
        OpsItemStatus_Pending,
        OpsItemStatus_PendingApproval,
        OpsItemStatus_PendingChangeCalendarOverride,
        OpsItemStatus_Rejected,
        OpsItemStatus_Resolved,
        OpsItemStatus_RunbookInProgress,
        OpsItemStatus_Scheduled,
        OpsItemStatus_TimedOut
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype OpsItemStatus = OpsItemStatus'
  { fromOpsItemStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern OpsItemStatus_Approved :: OpsItemStatus
pattern OpsItemStatus_Approved = OpsItemStatus' "Approved"

pattern OpsItemStatus_Cancelled :: OpsItemStatus
pattern OpsItemStatus_Cancelled = OpsItemStatus' "Cancelled"

pattern OpsItemStatus_Cancelling :: OpsItemStatus
pattern OpsItemStatus_Cancelling = OpsItemStatus' "Cancelling"

pattern OpsItemStatus_ChangeCalendarOverrideApproved :: OpsItemStatus
pattern OpsItemStatus_ChangeCalendarOverrideApproved = OpsItemStatus' "ChangeCalendarOverrideApproved"

pattern OpsItemStatus_ChangeCalendarOverrideRejected :: OpsItemStatus
pattern OpsItemStatus_ChangeCalendarOverrideRejected = OpsItemStatus' "ChangeCalendarOverrideRejected"

pattern OpsItemStatus_CompletedWithFailure :: OpsItemStatus
pattern OpsItemStatus_CompletedWithFailure = OpsItemStatus' "CompletedWithFailure"

pattern OpsItemStatus_CompletedWithSuccess :: OpsItemStatus
pattern OpsItemStatus_CompletedWithSuccess = OpsItemStatus' "CompletedWithSuccess"

pattern OpsItemStatus_Failed :: OpsItemStatus
pattern OpsItemStatus_Failed = OpsItemStatus' "Failed"

pattern OpsItemStatus_InProgress :: OpsItemStatus
pattern OpsItemStatus_InProgress = OpsItemStatus' "InProgress"

pattern OpsItemStatus_Open :: OpsItemStatus
pattern OpsItemStatus_Open = OpsItemStatus' "Open"

pattern OpsItemStatus_Pending :: OpsItemStatus
pattern OpsItemStatus_Pending = OpsItemStatus' "Pending"

pattern OpsItemStatus_PendingApproval :: OpsItemStatus
pattern OpsItemStatus_PendingApproval = OpsItemStatus' "PendingApproval"

pattern OpsItemStatus_PendingChangeCalendarOverride :: OpsItemStatus
pattern OpsItemStatus_PendingChangeCalendarOverride = OpsItemStatus' "PendingChangeCalendarOverride"

pattern OpsItemStatus_Rejected :: OpsItemStatus
pattern OpsItemStatus_Rejected = OpsItemStatus' "Rejected"

pattern OpsItemStatus_Resolved :: OpsItemStatus
pattern OpsItemStatus_Resolved = OpsItemStatus' "Resolved"

pattern OpsItemStatus_RunbookInProgress :: OpsItemStatus
pattern OpsItemStatus_RunbookInProgress = OpsItemStatus' "RunbookInProgress"

pattern OpsItemStatus_Scheduled :: OpsItemStatus
pattern OpsItemStatus_Scheduled = OpsItemStatus' "Scheduled"

pattern OpsItemStatus_TimedOut :: OpsItemStatus
pattern OpsItemStatus_TimedOut = OpsItemStatus' "TimedOut"

{-# COMPLETE
  OpsItemStatus_Approved,
  OpsItemStatus_Cancelled,
  OpsItemStatus_Cancelling,
  OpsItemStatus_ChangeCalendarOverrideApproved,
  OpsItemStatus_ChangeCalendarOverrideRejected,
  OpsItemStatus_CompletedWithFailure,
  OpsItemStatus_CompletedWithSuccess,
  OpsItemStatus_Failed,
  OpsItemStatus_InProgress,
  OpsItemStatus_Open,
  OpsItemStatus_Pending,
  OpsItemStatus_PendingApproval,
  OpsItemStatus_PendingChangeCalendarOverride,
  OpsItemStatus_Rejected,
  OpsItemStatus_Resolved,
  OpsItemStatus_RunbookInProgress,
  OpsItemStatus_Scheduled,
  OpsItemStatus_TimedOut,
  OpsItemStatus'
  #-}
