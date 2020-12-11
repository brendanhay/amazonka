-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRunStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowRunStatus
  ( WorkflowRunStatus
      ( WorkflowRunStatus',
        WRSCompleted,
        WRSError,
        WRSRunning,
        WRSStopped,
        WRSStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype WorkflowRunStatus = WorkflowRunStatus' Lude.Text
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

pattern WRSCompleted :: WorkflowRunStatus
pattern WRSCompleted = WorkflowRunStatus' "COMPLETED"

pattern WRSError :: WorkflowRunStatus
pattern WRSError = WorkflowRunStatus' "ERROR"

pattern WRSRunning :: WorkflowRunStatus
pattern WRSRunning = WorkflowRunStatus' "RUNNING"

pattern WRSStopped :: WorkflowRunStatus
pattern WRSStopped = WorkflowRunStatus' "STOPPED"

pattern WRSStopping :: WorkflowRunStatus
pattern WRSStopping = WorkflowRunStatus' "STOPPING"

{-# COMPLETE
  WRSCompleted,
  WRSError,
  WRSRunning,
  WRSStopped,
  WRSStopping,
  WorkflowRunStatus'
  #-}
