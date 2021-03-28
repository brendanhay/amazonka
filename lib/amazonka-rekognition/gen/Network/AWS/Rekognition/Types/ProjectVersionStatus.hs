{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ProjectVersionStatus
  ( ProjectVersionStatus
    ( ProjectVersionStatus'
    , ProjectVersionStatusTrainingInProgress
    , ProjectVersionStatusTrainingCompleted
    , ProjectVersionStatusTrainingFailed
    , ProjectVersionStatusStarting
    , ProjectVersionStatusRunning
    , ProjectVersionStatusFailed
    , ProjectVersionStatusStopping
    , ProjectVersionStatusStopped
    , ProjectVersionStatusDeleting
    , fromProjectVersionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProjectVersionStatus = ProjectVersionStatus'{fromProjectVersionStatus
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern ProjectVersionStatusTrainingInProgress :: ProjectVersionStatus
pattern ProjectVersionStatusTrainingInProgress = ProjectVersionStatus' "TRAINING_IN_PROGRESS"

pattern ProjectVersionStatusTrainingCompleted :: ProjectVersionStatus
pattern ProjectVersionStatusTrainingCompleted = ProjectVersionStatus' "TRAINING_COMPLETED"

pattern ProjectVersionStatusTrainingFailed :: ProjectVersionStatus
pattern ProjectVersionStatusTrainingFailed = ProjectVersionStatus' "TRAINING_FAILED"

pattern ProjectVersionStatusStarting :: ProjectVersionStatus
pattern ProjectVersionStatusStarting = ProjectVersionStatus' "STARTING"

pattern ProjectVersionStatusRunning :: ProjectVersionStatus
pattern ProjectVersionStatusRunning = ProjectVersionStatus' "RUNNING"

pattern ProjectVersionStatusFailed :: ProjectVersionStatus
pattern ProjectVersionStatusFailed = ProjectVersionStatus' "FAILED"

pattern ProjectVersionStatusStopping :: ProjectVersionStatus
pattern ProjectVersionStatusStopping = ProjectVersionStatus' "STOPPING"

pattern ProjectVersionStatusStopped :: ProjectVersionStatus
pattern ProjectVersionStatusStopped = ProjectVersionStatus' "STOPPED"

pattern ProjectVersionStatusDeleting :: ProjectVersionStatus
pattern ProjectVersionStatusDeleting = ProjectVersionStatus' "DELETING"

{-# COMPLETE 
  ProjectVersionStatusTrainingInProgress,

  ProjectVersionStatusTrainingCompleted,

  ProjectVersionStatusTrainingFailed,

  ProjectVersionStatusStarting,

  ProjectVersionStatusRunning,

  ProjectVersionStatusFailed,

  ProjectVersionStatusStopping,

  ProjectVersionStatusStopped,

  ProjectVersionStatusDeleting,
  ProjectVersionStatus'
  #-}
