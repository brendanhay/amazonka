{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskStatusType
  ( TaskStatusType
      ( TaskStatusType',
        TaskStatusTypeStarting,
        TaskStatusTypeRunning,
        TaskStatusTypeStopping,
        TaskStatusTypeStopped,
        TaskStatusTypeSucceeded,
        TaskStatusTypeFailed,
        TaskStatusTypeTimeout,
        fromTaskStatusType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TaskStatusType = TaskStatusType'
  { fromTaskStatusType ::
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

pattern TaskStatusTypeStarting :: TaskStatusType
pattern TaskStatusTypeStarting = TaskStatusType' "STARTING"

pattern TaskStatusTypeRunning :: TaskStatusType
pattern TaskStatusTypeRunning = TaskStatusType' "RUNNING"

pattern TaskStatusTypeStopping :: TaskStatusType
pattern TaskStatusTypeStopping = TaskStatusType' "STOPPING"

pattern TaskStatusTypeStopped :: TaskStatusType
pattern TaskStatusTypeStopped = TaskStatusType' "STOPPED"

pattern TaskStatusTypeSucceeded :: TaskStatusType
pattern TaskStatusTypeSucceeded = TaskStatusType' "SUCCEEDED"

pattern TaskStatusTypeFailed :: TaskStatusType
pattern TaskStatusTypeFailed = TaskStatusType' "FAILED"

pattern TaskStatusTypeTimeout :: TaskStatusType
pattern TaskStatusTypeTimeout = TaskStatusType' "TIMEOUT"

{-# COMPLETE
  TaskStatusTypeStarting,
  TaskStatusTypeRunning,
  TaskStatusTypeStopping,
  TaskStatusTypeStopped,
  TaskStatusTypeSucceeded,
  TaskStatusTypeFailed,
  TaskStatusTypeTimeout,
  TaskStatusType'
  #-}
