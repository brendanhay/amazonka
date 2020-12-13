{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        DetectionInProgress,
        DetectionFailed,
        DetectionComplete
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

pattern DetectionInProgress :: StackDriftDetectionStatus
pattern DetectionInProgress = StackDriftDetectionStatus' "DETECTION_IN_PROGRESS"

pattern DetectionFailed :: StackDriftDetectionStatus
pattern DetectionFailed = StackDriftDetectionStatus' "DETECTION_FAILED"

pattern DetectionComplete :: StackDriftDetectionStatus
pattern DetectionComplete = StackDriftDetectionStatus' "DETECTION_COMPLETE"

{-# COMPLETE
  DetectionInProgress,
  DetectionFailed,
  DetectionComplete,
  StackDriftDetectionStatus'
  #-}
