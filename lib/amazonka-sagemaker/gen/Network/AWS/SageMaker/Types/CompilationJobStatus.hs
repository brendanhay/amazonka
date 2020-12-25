{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CompilationJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CompilationJobStatus
  ( CompilationJobStatus
      ( CompilationJobStatus',
        CompilationJobStatusInprogress,
        CompilationJobStatusCompleted,
        CompilationJobStatusFailed,
        CompilationJobStatusStarting,
        CompilationJobStatusStopping,
        CompilationJobStatusStopped,
        fromCompilationJobStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CompilationJobStatus = CompilationJobStatus'
  { fromCompilationJobStatus ::
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

pattern CompilationJobStatusInprogress :: CompilationJobStatus
pattern CompilationJobStatusInprogress = CompilationJobStatus' "INPROGRESS"

pattern CompilationJobStatusCompleted :: CompilationJobStatus
pattern CompilationJobStatusCompleted = CompilationJobStatus' "COMPLETED"

pattern CompilationJobStatusFailed :: CompilationJobStatus
pattern CompilationJobStatusFailed = CompilationJobStatus' "FAILED"

pattern CompilationJobStatusStarting :: CompilationJobStatus
pattern CompilationJobStatusStarting = CompilationJobStatus' "STARTING"

pattern CompilationJobStatusStopping :: CompilationJobStatus
pattern CompilationJobStatusStopping = CompilationJobStatus' "STOPPING"

pattern CompilationJobStatusStopped :: CompilationJobStatus
pattern CompilationJobStatusStopped = CompilationJobStatus' "STOPPED"

{-# COMPLETE
  CompilationJobStatusInprogress,
  CompilationJobStatusCompleted,
  CompilationJobStatusFailed,
  CompilationJobStatusStarting,
  CompilationJobStatusStopping,
  CompilationJobStatusStopped,
  CompilationJobStatus'
  #-}
