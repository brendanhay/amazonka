{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectVersionStatus where

import Network.AWS.Prelude

data ProjectVersionStatus
  = Deleting
  | Failed
  | Running
  | Starting
  | Stopped
  | Stopping
  | TrainingCompleted
  | TrainingFailed
  | TrainingInProgress
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ProjectVersionStatus where
  parser =
    takeLowerText >>= \case
      "deleting" -> pure Deleting
      "failed" -> pure Failed
      "running" -> pure Running
      "starting" -> pure Starting
      "stopped" -> pure Stopped
      "stopping" -> pure Stopping
      "training_completed" -> pure TrainingCompleted
      "training_failed" -> pure TrainingFailed
      "training_in_progress" -> pure TrainingInProgress
      e ->
        fromTextError $
          "Failure parsing ProjectVersionStatus from value: '" <> e
            <> "'. Accepted values: deleting, failed, running, starting, stopped, stopping, training_completed, training_failed, training_in_progress"

instance ToText ProjectVersionStatus where
  toText = \case
    Deleting -> "DELETING"
    Failed -> "FAILED"
    Running -> "RUNNING"
    Starting -> "STARTING"
    Stopped -> "STOPPED"
    Stopping -> "STOPPING"
    TrainingCompleted -> "TRAINING_COMPLETED"
    TrainingFailed -> "TRAINING_FAILED"
    TrainingInProgress -> "TRAINING_IN_PROGRESS"

instance Hashable ProjectVersionStatus

instance NFData ProjectVersionStatus

instance ToByteString ProjectVersionStatus

instance ToQuery ProjectVersionStatus

instance ToHeader ProjectVersionStatus

instance FromJSON ProjectVersionStatus where
  parseJSON = parseJSONText "ProjectVersionStatus"
