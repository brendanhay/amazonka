-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackDriftDetectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftDetectionStatus
  ( StackDriftDetectionStatus
      ( StackDriftDetectionStatus',
        DetectionComplete,
        DetectionFailed,
        DetectionInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackDriftDetectionStatus = StackDriftDetectionStatus' Lude.Text
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

pattern DetectionComplete :: StackDriftDetectionStatus
pattern DetectionComplete = StackDriftDetectionStatus' "DETECTION_COMPLETE"

pattern DetectionFailed :: StackDriftDetectionStatus
pattern DetectionFailed = StackDriftDetectionStatus' "DETECTION_FAILED"

pattern DetectionInProgress :: StackDriftDetectionStatus
pattern DetectionInProgress = StackDriftDetectionStatus' "DETECTION_IN_PROGRESS"

{-# COMPLETE
  DetectionComplete,
  DetectionFailed,
  DetectionInProgress,
  StackDriftDetectionStatus'
  #-}
