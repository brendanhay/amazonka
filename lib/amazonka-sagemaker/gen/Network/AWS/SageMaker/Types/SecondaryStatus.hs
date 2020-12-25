{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SecondaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SecondaryStatus
  ( SecondaryStatus
      ( SecondaryStatus',
        SecondaryStatusStarting,
        SecondaryStatusLaunchingMLInstances,
        SecondaryStatusPreparingTrainingStack,
        SecondaryStatusDownloading,
        SecondaryStatusDownloadingTrainingImage,
        SecondaryStatusTraining,
        SecondaryStatusUploading,
        SecondaryStatusStopping,
        SecondaryStatusStopped,
        SecondaryStatusMaxRuntimeExceeded,
        SecondaryStatusCompleted,
        SecondaryStatusFailed,
        SecondaryStatusInterrupted,
        SecondaryStatusMaxWaitTimeExceeded,
        fromSecondaryStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SecondaryStatus = SecondaryStatus'
  { fromSecondaryStatus ::
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

pattern SecondaryStatusStarting :: SecondaryStatus
pattern SecondaryStatusStarting = SecondaryStatus' "Starting"

pattern SecondaryStatusLaunchingMLInstances :: SecondaryStatus
pattern SecondaryStatusLaunchingMLInstances = SecondaryStatus' "LaunchingMLInstances"

pattern SecondaryStatusPreparingTrainingStack :: SecondaryStatus
pattern SecondaryStatusPreparingTrainingStack = SecondaryStatus' "PreparingTrainingStack"

pattern SecondaryStatusDownloading :: SecondaryStatus
pattern SecondaryStatusDownloading = SecondaryStatus' "Downloading"

pattern SecondaryStatusDownloadingTrainingImage :: SecondaryStatus
pattern SecondaryStatusDownloadingTrainingImage = SecondaryStatus' "DownloadingTrainingImage"

pattern SecondaryStatusTraining :: SecondaryStatus
pattern SecondaryStatusTraining = SecondaryStatus' "Training"

pattern SecondaryStatusUploading :: SecondaryStatus
pattern SecondaryStatusUploading = SecondaryStatus' "Uploading"

pattern SecondaryStatusStopping :: SecondaryStatus
pattern SecondaryStatusStopping = SecondaryStatus' "Stopping"

pattern SecondaryStatusStopped :: SecondaryStatus
pattern SecondaryStatusStopped = SecondaryStatus' "Stopped"

pattern SecondaryStatusMaxRuntimeExceeded :: SecondaryStatus
pattern SecondaryStatusMaxRuntimeExceeded = SecondaryStatus' "MaxRuntimeExceeded"

pattern SecondaryStatusCompleted :: SecondaryStatus
pattern SecondaryStatusCompleted = SecondaryStatus' "Completed"

pattern SecondaryStatusFailed :: SecondaryStatus
pattern SecondaryStatusFailed = SecondaryStatus' "Failed"

pattern SecondaryStatusInterrupted :: SecondaryStatus
pattern SecondaryStatusInterrupted = SecondaryStatus' "Interrupted"

pattern SecondaryStatusMaxWaitTimeExceeded :: SecondaryStatus
pattern SecondaryStatusMaxWaitTimeExceeded = SecondaryStatus' "MaxWaitTimeExceeded"

{-# COMPLETE
  SecondaryStatusStarting,
  SecondaryStatusLaunchingMLInstances,
  SecondaryStatusPreparingTrainingStack,
  SecondaryStatusDownloading,
  SecondaryStatusDownloadingTrainingImage,
  SecondaryStatusTraining,
  SecondaryStatusUploading,
  SecondaryStatusStopping,
  SecondaryStatusStopped,
  SecondaryStatusMaxRuntimeExceeded,
  SecondaryStatusCompleted,
  SecondaryStatusFailed,
  SecondaryStatusInterrupted,
  SecondaryStatusMaxWaitTimeExceeded,
  SecondaryStatus'
  #-}
