{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.ExecutionStatus
  ( ExecutionStatus
    ( ExecutionStatus'
    , ExecutionStatusPending
    , ExecutionStatusPendingConcurrency
    , ExecutionStatusPendingDevice
    , ExecutionStatusProcessing
    , ExecutionStatusScheduling
    , ExecutionStatusPreparing
    , ExecutionStatusRunning
    , ExecutionStatusCompleted
    , ExecutionStatusStopping
    , fromExecutionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ExecutionStatus = ExecutionStatus'{fromExecutionStatus ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern ExecutionStatusPending :: ExecutionStatus
pattern ExecutionStatusPending = ExecutionStatus' "PENDING"

pattern ExecutionStatusPendingConcurrency :: ExecutionStatus
pattern ExecutionStatusPendingConcurrency = ExecutionStatus' "PENDING_CONCURRENCY"

pattern ExecutionStatusPendingDevice :: ExecutionStatus
pattern ExecutionStatusPendingDevice = ExecutionStatus' "PENDING_DEVICE"

pattern ExecutionStatusProcessing :: ExecutionStatus
pattern ExecutionStatusProcessing = ExecutionStatus' "PROCESSING"

pattern ExecutionStatusScheduling :: ExecutionStatus
pattern ExecutionStatusScheduling = ExecutionStatus' "SCHEDULING"

pattern ExecutionStatusPreparing :: ExecutionStatus
pattern ExecutionStatusPreparing = ExecutionStatus' "PREPARING"

pattern ExecutionStatusRunning :: ExecutionStatus
pattern ExecutionStatusRunning = ExecutionStatus' "RUNNING"

pattern ExecutionStatusCompleted :: ExecutionStatus
pattern ExecutionStatusCompleted = ExecutionStatus' "COMPLETED"

pattern ExecutionStatusStopping :: ExecutionStatus
pattern ExecutionStatusStopping = ExecutionStatus' "STOPPING"

{-# COMPLETE 
  ExecutionStatusPending,

  ExecutionStatusPendingConcurrency,

  ExecutionStatusPendingDevice,

  ExecutionStatusProcessing,

  ExecutionStatusScheduling,

  ExecutionStatusPreparing,

  ExecutionStatusRunning,

  ExecutionStatusCompleted,

  ExecutionStatusStopping,
  ExecutionStatus'
  #-}
