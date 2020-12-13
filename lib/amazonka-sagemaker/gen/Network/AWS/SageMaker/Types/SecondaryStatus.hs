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
        SStarting,
        SLaunchingMLInstances,
        SPreparingTrainingStack,
        SDownloading,
        SDownloadingTrainingImage,
        STraining,
        SUploading,
        SStopping,
        SStopped,
        SMaxRuntimeExceeded,
        SCompleted,
        SFailed,
        SInterrupted,
        SMaxWaitTimeExceeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SecondaryStatus = SecondaryStatus' Lude.Text
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

pattern SStarting :: SecondaryStatus
pattern SStarting = SecondaryStatus' "Starting"

pattern SLaunchingMLInstances :: SecondaryStatus
pattern SLaunchingMLInstances = SecondaryStatus' "LaunchingMLInstances"

pattern SPreparingTrainingStack :: SecondaryStatus
pattern SPreparingTrainingStack = SecondaryStatus' "PreparingTrainingStack"

pattern SDownloading :: SecondaryStatus
pattern SDownloading = SecondaryStatus' "Downloading"

pattern SDownloadingTrainingImage :: SecondaryStatus
pattern SDownloadingTrainingImage = SecondaryStatus' "DownloadingTrainingImage"

pattern STraining :: SecondaryStatus
pattern STraining = SecondaryStatus' "Training"

pattern SUploading :: SecondaryStatus
pattern SUploading = SecondaryStatus' "Uploading"

pattern SStopping :: SecondaryStatus
pattern SStopping = SecondaryStatus' "Stopping"

pattern SStopped :: SecondaryStatus
pattern SStopped = SecondaryStatus' "Stopped"

pattern SMaxRuntimeExceeded :: SecondaryStatus
pattern SMaxRuntimeExceeded = SecondaryStatus' "MaxRuntimeExceeded"

pattern SCompleted :: SecondaryStatus
pattern SCompleted = SecondaryStatus' "Completed"

pattern SFailed :: SecondaryStatus
pattern SFailed = SecondaryStatus' "Failed"

pattern SInterrupted :: SecondaryStatus
pattern SInterrupted = SecondaryStatus' "Interrupted"

pattern SMaxWaitTimeExceeded :: SecondaryStatus
pattern SMaxWaitTimeExceeded = SecondaryStatus' "MaxWaitTimeExceeded"

{-# COMPLETE
  SStarting,
  SLaunchingMLInstances,
  SPreparingTrainingStack,
  SDownloading,
  SDownloadingTrainingImage,
  STraining,
  SUploading,
  SStopping,
  SStopped,
  SMaxRuntimeExceeded,
  SCompleted,
  SFailed,
  SInterrupted,
  SMaxWaitTimeExceeded,
  SecondaryStatus'
  #-}
