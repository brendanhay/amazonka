{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineExecutionStatus
  ( PipelineExecutionStatus
      ( PipelineExecutionStatus',
        PipelineExecutionStatusInProgress,
        PipelineExecutionStatusStopped,
        PipelineExecutionStatusStopping,
        PipelineExecutionStatusSucceeded,
        PipelineExecutionStatusSuperseded,
        PipelineExecutionStatusFailed,
        fromPipelineExecutionStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PipelineExecutionStatus = PipelineExecutionStatus'
  { fromPipelineExecutionStatus ::
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

pattern PipelineExecutionStatusInProgress :: PipelineExecutionStatus
pattern PipelineExecutionStatusInProgress = PipelineExecutionStatus' "InProgress"

pattern PipelineExecutionStatusStopped :: PipelineExecutionStatus
pattern PipelineExecutionStatusStopped = PipelineExecutionStatus' "Stopped"

pattern PipelineExecutionStatusStopping :: PipelineExecutionStatus
pattern PipelineExecutionStatusStopping = PipelineExecutionStatus' "Stopping"

pattern PipelineExecutionStatusSucceeded :: PipelineExecutionStatus
pattern PipelineExecutionStatusSucceeded = PipelineExecutionStatus' "Succeeded"

pattern PipelineExecutionStatusSuperseded :: PipelineExecutionStatus
pattern PipelineExecutionStatusSuperseded = PipelineExecutionStatus' "Superseded"

pattern PipelineExecutionStatusFailed :: PipelineExecutionStatus
pattern PipelineExecutionStatusFailed = PipelineExecutionStatus' "Failed"

{-# COMPLETE
  PipelineExecutionStatusInProgress,
  PipelineExecutionStatusStopped,
  PipelineExecutionStatusStopping,
  PipelineExecutionStatusSucceeded,
  PipelineExecutionStatusSuperseded,
  PipelineExecutionStatusFailed,
  PipelineExecutionStatus'
  #-}
