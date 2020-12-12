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
        NESFailed,
        NESFailing,
        NESFinished,
        NESFinishing,
        NESRunning,
        NESStartPending,
        NESStarting,
        NESStopPending,
        NESStopped,
        NESStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NotebookExecutionStatus = NotebookExecutionStatus' Lude.Text
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

pattern NESFailed :: NotebookExecutionStatus
pattern NESFailed = NotebookExecutionStatus' "FAILED"

pattern NESFailing :: NotebookExecutionStatus
pattern NESFailing = NotebookExecutionStatus' "FAILING"

pattern NESFinished :: NotebookExecutionStatus
pattern NESFinished = NotebookExecutionStatus' "FINISHED"

pattern NESFinishing :: NotebookExecutionStatus
pattern NESFinishing = NotebookExecutionStatus' "FINISHING"

pattern NESRunning :: NotebookExecutionStatus
pattern NESRunning = NotebookExecutionStatus' "RUNNING"

pattern NESStartPending :: NotebookExecutionStatus
pattern NESStartPending = NotebookExecutionStatus' "START_PENDING"

pattern NESStarting :: NotebookExecutionStatus
pattern NESStarting = NotebookExecutionStatus' "STARTING"

pattern NESStopPending :: NotebookExecutionStatus
pattern NESStopPending = NotebookExecutionStatus' "STOP_PENDING"

pattern NESStopped :: NotebookExecutionStatus
pattern NESStopped = NotebookExecutionStatus' "STOPPED"

pattern NESStopping :: NotebookExecutionStatus
pattern NESStopping = NotebookExecutionStatus' "STOPPING"

{-# COMPLETE
  NESFailed,
  NESFailing,
  NESFinished,
  NESFinishing,
  NESRunning,
  NESStartPending,
  NESStarting,
  NESStopPending,
  NESStopped,
  NESStopping,
  NotebookExecutionStatus'
  #-}
