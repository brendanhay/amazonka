{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
  ( StackSetDriftDetectionStatus
      ( StackSetDriftDetectionStatus',
        StackSetDriftDetectionStatusCompleted,
        StackSetDriftDetectionStatusFailed,
        StackSetDriftDetectionStatusPartialSuccess,
        StackSetDriftDetectionStatusInProgress,
        StackSetDriftDetectionStatusStopped,
        fromStackSetDriftDetectionStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StackSetDriftDetectionStatus = StackSetDriftDetectionStatus'
  { fromStackSetDriftDetectionStatus ::
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

pattern StackSetDriftDetectionStatusCompleted :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatusCompleted = StackSetDriftDetectionStatus' "COMPLETED"

pattern StackSetDriftDetectionStatusFailed :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatusFailed = StackSetDriftDetectionStatus' "FAILED"

pattern StackSetDriftDetectionStatusPartialSuccess :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatusPartialSuccess = StackSetDriftDetectionStatus' "PARTIAL_SUCCESS"

pattern StackSetDriftDetectionStatusInProgress :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatusInProgress = StackSetDriftDetectionStatus' "IN_PROGRESS"

pattern StackSetDriftDetectionStatusStopped :: StackSetDriftDetectionStatus
pattern StackSetDriftDetectionStatusStopped = StackSetDriftDetectionStatus' "STOPPED"

{-# COMPLETE
  StackSetDriftDetectionStatusCompleted,
  StackSetDriftDetectionStatusFailed,
  StackSetDriftDetectionStatusPartialSuccess,
  StackSetDriftDetectionStatusInProgress,
  StackSetDriftDetectionStatusStopped,
  StackSetDriftDetectionStatus'
  #-}
