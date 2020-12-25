{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.NotebookExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.NotebookExecutionStatus
  ( NotebookExecutionStatus
      ( NotebookExecutionStatus',
        NotebookExecutionStatusStartPending,
        NotebookExecutionStatusStarting,
        NotebookExecutionStatusRunning,
        NotebookExecutionStatusFinishing,
        NotebookExecutionStatusFinished,
        NotebookExecutionStatusFailing,
        NotebookExecutionStatusFailed,
        NotebookExecutionStatusStopPending,
        NotebookExecutionStatusStopping,
        NotebookExecutionStatusStopped,
        fromNotebookExecutionStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype NotebookExecutionStatus = NotebookExecutionStatus'
  { fromNotebookExecutionStatus ::
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

pattern NotebookExecutionStatusStartPending :: NotebookExecutionStatus
pattern NotebookExecutionStatusStartPending = NotebookExecutionStatus' "START_PENDING"

pattern NotebookExecutionStatusStarting :: NotebookExecutionStatus
pattern NotebookExecutionStatusStarting = NotebookExecutionStatus' "STARTING"

pattern NotebookExecutionStatusRunning :: NotebookExecutionStatus
pattern NotebookExecutionStatusRunning = NotebookExecutionStatus' "RUNNING"

pattern NotebookExecutionStatusFinishing :: NotebookExecutionStatus
pattern NotebookExecutionStatusFinishing = NotebookExecutionStatus' "FINISHING"

pattern NotebookExecutionStatusFinished :: NotebookExecutionStatus
pattern NotebookExecutionStatusFinished = NotebookExecutionStatus' "FINISHED"

pattern NotebookExecutionStatusFailing :: NotebookExecutionStatus
pattern NotebookExecutionStatusFailing = NotebookExecutionStatus' "FAILING"

pattern NotebookExecutionStatusFailed :: NotebookExecutionStatus
pattern NotebookExecutionStatusFailed = NotebookExecutionStatus' "FAILED"

pattern NotebookExecutionStatusStopPending :: NotebookExecutionStatus
pattern NotebookExecutionStatusStopPending = NotebookExecutionStatus' "STOP_PENDING"

pattern NotebookExecutionStatusStopping :: NotebookExecutionStatus
pattern NotebookExecutionStatusStopping = NotebookExecutionStatus' "STOPPING"

pattern NotebookExecutionStatusStopped :: NotebookExecutionStatus
pattern NotebookExecutionStatusStopped = NotebookExecutionStatus' "STOPPED"

{-# COMPLETE
  NotebookExecutionStatusStartPending,
  NotebookExecutionStatusStarting,
  NotebookExecutionStatusRunning,
  NotebookExecutionStatusFinishing,
  NotebookExecutionStatusFinished,
  NotebookExecutionStatusFailing,
  NotebookExecutionStatusFailed,
  NotebookExecutionStatusStopPending,
  NotebookExecutionStatusStopping,
  NotebookExecutionStatusStopped,
  NotebookExecutionStatus'
  #-}
