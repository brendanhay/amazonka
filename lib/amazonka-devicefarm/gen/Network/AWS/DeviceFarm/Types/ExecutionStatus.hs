{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ESPending,
        ESPendingConcurrency,
        ESPendingDevice,
        ESProcessing,
        ESScheduling,
        ESPreparing,
        ESRunning,
        ESCompleted,
        ESStopping
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

pattern ESPending :: ExecutionStatus
pattern ESPending = ExecutionStatus' "PENDING"

pattern ESPendingConcurrency :: ExecutionStatus
pattern ESPendingConcurrency = ExecutionStatus' "PENDING_CONCURRENCY"

pattern ESPendingDevice :: ExecutionStatus
pattern ESPendingDevice = ExecutionStatus' "PENDING_DEVICE"

pattern ESProcessing :: ExecutionStatus
pattern ESProcessing = ExecutionStatus' "PROCESSING"

pattern ESScheduling :: ExecutionStatus
pattern ESScheduling = ExecutionStatus' "SCHEDULING"

pattern ESPreparing :: ExecutionStatus
pattern ESPreparing = ExecutionStatus' "PREPARING"

pattern ESRunning :: ExecutionStatus
pattern ESRunning = ExecutionStatus' "RUNNING"

pattern ESCompleted :: ExecutionStatus
pattern ESCompleted = ExecutionStatus' "COMPLETED"

pattern ESStopping :: ExecutionStatus
pattern ESStopping = ExecutionStatus' "STOPPING"

{-# COMPLETE
  ESPending,
  ESPendingConcurrency,
  ESPendingDevice,
  ESProcessing,
  ESScheduling,
  ESPreparing,
  ESRunning,
  ESCompleted,
  ESStopping,
  ExecutionStatus'
  #-}
