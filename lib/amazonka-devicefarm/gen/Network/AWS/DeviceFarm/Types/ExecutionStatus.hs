-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionStatus
  ( ExecutionStatus
      ( ExecutionStatus',
        Completed,
        Pending,
        PendingConcurrency,
        PendingDevice,
        Preparing,
        Processing,
        Running,
        Scheduling,
        Stopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExecutionStatus = ExecutionStatus' Lude.Text
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

pattern Completed :: ExecutionStatus
pattern Completed = ExecutionStatus' "COMPLETED"

pattern Pending :: ExecutionStatus
pattern Pending = ExecutionStatus' "PENDING"

pattern PendingConcurrency :: ExecutionStatus
pattern PendingConcurrency = ExecutionStatus' "PENDING_CONCURRENCY"

pattern PendingDevice :: ExecutionStatus
pattern PendingDevice = ExecutionStatus' "PENDING_DEVICE"

pattern Preparing :: ExecutionStatus
pattern Preparing = ExecutionStatus' "PREPARING"

pattern Processing :: ExecutionStatus
pattern Processing = ExecutionStatus' "PROCESSING"

pattern Running :: ExecutionStatus
pattern Running = ExecutionStatus' "RUNNING"

pattern Scheduling :: ExecutionStatus
pattern Scheduling = ExecutionStatus' "SCHEDULING"

pattern Stopping :: ExecutionStatus
pattern Stopping = ExecutionStatus' "STOPPING"

{-# COMPLETE
  Completed,
  Pending,
  PendingConcurrency,
  PendingDevice,
  Preparing,
  Processing,
  Running,
  Scheduling,
  Stopping,
  ExecutionStatus'
  #-}
