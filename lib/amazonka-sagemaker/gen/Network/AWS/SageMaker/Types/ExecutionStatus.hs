{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ExecutionStatus
  ( ExecutionStatus
    ( ExecutionStatus'
    , ExecutionStatusPending
    , ExecutionStatusCompleted
    , ExecutionStatusCompletedWithViolations
    , ExecutionStatusInProgress
    , ExecutionStatusFailed
    , ExecutionStatusStopping
    , ExecutionStatusStopped
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
pattern ExecutionStatusPending = ExecutionStatus' "Pending"

pattern ExecutionStatusCompleted :: ExecutionStatus
pattern ExecutionStatusCompleted = ExecutionStatus' "Completed"

pattern ExecutionStatusCompletedWithViolations :: ExecutionStatus
pattern ExecutionStatusCompletedWithViolations = ExecutionStatus' "CompletedWithViolations"

pattern ExecutionStatusInProgress :: ExecutionStatus
pattern ExecutionStatusInProgress = ExecutionStatus' "InProgress"

pattern ExecutionStatusFailed :: ExecutionStatus
pattern ExecutionStatusFailed = ExecutionStatus' "Failed"

pattern ExecutionStatusStopping :: ExecutionStatus
pattern ExecutionStatusStopping = ExecutionStatus' "Stopping"

pattern ExecutionStatusStopped :: ExecutionStatus
pattern ExecutionStatusStopped = ExecutionStatus' "Stopped"

{-# COMPLETE 
  ExecutionStatusPending,

  ExecutionStatusCompleted,

  ExecutionStatusCompletedWithViolations,

  ExecutionStatusInProgress,

  ExecutionStatusFailed,

  ExecutionStatusStopping,

  ExecutionStatusStopped,
  ExecutionStatus'
  #-}
