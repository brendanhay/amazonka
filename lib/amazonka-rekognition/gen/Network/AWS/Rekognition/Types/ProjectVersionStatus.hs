{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectVersionStatus
  ( ProjectVersionStatus
      ( ProjectVersionStatus',
        TrainingInProgress,
        TrainingCompleted,
        TrainingFailed,
        Starting,
        Running,
        Failed,
        Stopping,
        Stopped,
        Deleting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProjectVersionStatus = ProjectVersionStatus' Lude.Text
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

pattern TrainingInProgress :: ProjectVersionStatus
pattern TrainingInProgress = ProjectVersionStatus' "TRAINING_IN_PROGRESS"

pattern TrainingCompleted :: ProjectVersionStatus
pattern TrainingCompleted = ProjectVersionStatus' "TRAINING_COMPLETED"

pattern TrainingFailed :: ProjectVersionStatus
pattern TrainingFailed = ProjectVersionStatus' "TRAINING_FAILED"

pattern Starting :: ProjectVersionStatus
pattern Starting = ProjectVersionStatus' "STARTING"

pattern Running :: ProjectVersionStatus
pattern Running = ProjectVersionStatus' "RUNNING"

pattern Failed :: ProjectVersionStatus
pattern Failed = ProjectVersionStatus' "FAILED"

pattern Stopping :: ProjectVersionStatus
pattern Stopping = ProjectVersionStatus' "STOPPING"

pattern Stopped :: ProjectVersionStatus
pattern Stopped = ProjectVersionStatus' "STOPPED"

pattern Deleting :: ProjectVersionStatus
pattern Deleting = ProjectVersionStatus' "DELETING"

{-# COMPLETE
  TrainingInProgress,
  TrainingCompleted,
  TrainingFailed,
  Starting,
  Running,
  Failed,
  Stopping,
  Stopped,
  Deleting,
  ProjectVersionStatus'
  #-}
