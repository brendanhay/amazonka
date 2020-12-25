{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobStatus
  ( AutoMLJobStatus
      ( AutoMLJobStatus',
        AutoMLJobStatusCompleted,
        AutoMLJobStatusInProgress,
        AutoMLJobStatusFailed,
        AutoMLJobStatusStopped,
        AutoMLJobStatusStopping,
        fromAutoMLJobStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AutoMLJobStatus = AutoMLJobStatus'
  { fromAutoMLJobStatus ::
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

pattern AutoMLJobStatusCompleted :: AutoMLJobStatus
pattern AutoMLJobStatusCompleted = AutoMLJobStatus' "Completed"

pattern AutoMLJobStatusInProgress :: AutoMLJobStatus
pattern AutoMLJobStatusInProgress = AutoMLJobStatus' "InProgress"

pattern AutoMLJobStatusFailed :: AutoMLJobStatus
pattern AutoMLJobStatusFailed = AutoMLJobStatus' "Failed"

pattern AutoMLJobStatusStopped :: AutoMLJobStatus
pattern AutoMLJobStatusStopped = AutoMLJobStatus' "Stopped"

pattern AutoMLJobStatusStopping :: AutoMLJobStatus
pattern AutoMLJobStatusStopping = AutoMLJobStatus' "Stopping"

{-# COMPLETE
  AutoMLJobStatusCompleted,
  AutoMLJobStatusInProgress,
  AutoMLJobStatusFailed,
  AutoMLJobStatusStopped,
  AutoMLJobStatusStopping,
  AutoMLJobStatus'
  #-}
