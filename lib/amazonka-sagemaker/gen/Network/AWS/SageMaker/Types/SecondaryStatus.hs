{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SecondaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SecondaryStatus where

import Network.AWS.Prelude

data SecondaryStatus
  = SCompleted
  | SDownloading
  | SDownloadingTrainingImage
  | SFailed
  | SInterrupted
  | SLaunchingMLInstances
  | SMaxRuntimeExceeded
  | SMaxWaitTimeExceeded
  | SPreparingTrainingStack
  | SStarting
  | SStopped
  | SStopping
  | STraining
  | SUploading
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

instance FromText SecondaryStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure SCompleted
      "downloading" -> pure SDownloading
      "downloadingtrainingimage" -> pure SDownloadingTrainingImage
      "failed" -> pure SFailed
      "interrupted" -> pure SInterrupted
      "launchingmlinstances" -> pure SLaunchingMLInstances
      "maxruntimeexceeded" -> pure SMaxRuntimeExceeded
      "maxwaittimeexceeded" -> pure SMaxWaitTimeExceeded
      "preparingtrainingstack" -> pure SPreparingTrainingStack
      "starting" -> pure SStarting
      "stopped" -> pure SStopped
      "stopping" -> pure SStopping
      "training" -> pure STraining
      "uploading" -> pure SUploading
      e ->
        fromTextError $
          "Failure parsing SecondaryStatus from value: '" <> e
            <> "'. Accepted values: completed, downloading, downloadingtrainingimage, failed, interrupted, launchingmlinstances, maxruntimeexceeded, maxwaittimeexceeded, preparingtrainingstack, starting, stopped, stopping, training, uploading"

instance ToText SecondaryStatus where
  toText = \case
    SCompleted -> "Completed"
    SDownloading -> "Downloading"
    SDownloadingTrainingImage -> "DownloadingTrainingImage"
    SFailed -> "Failed"
    SInterrupted -> "Interrupted"
    SLaunchingMLInstances -> "LaunchingMLInstances"
    SMaxRuntimeExceeded -> "MaxRuntimeExceeded"
    SMaxWaitTimeExceeded -> "MaxWaitTimeExceeded"
    SPreparingTrainingStack -> "PreparingTrainingStack"
    SStarting -> "Starting"
    SStopped -> "Stopped"
    SStopping -> "Stopping"
    STraining -> "Training"
    SUploading -> "Uploading"

instance Hashable SecondaryStatus

instance NFData SecondaryStatus

instance ToByteString SecondaryStatus

instance ToQuery SecondaryStatus

instance ToHeader SecondaryStatus

instance FromJSON SecondaryStatus where
  parseJSON = parseJSONText "SecondaryStatus"
