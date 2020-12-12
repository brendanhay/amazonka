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
        SCompleted,
        SDownloading,
        SDownloadingTrainingImage,
        SFailed,
        SInterrupted,
        SLaunchingMLInstances,
        SMaxRuntimeExceeded,
        SMaxWaitTimeExceeded,
        SPreparingTrainingStack,
        SStarting,
        SStopped,
        SStopping,
        STraining,
        SUploading
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

pattern SCompleted :: SecondaryStatus
pattern SCompleted = SecondaryStatus' "Completed"

pattern SDownloading :: SecondaryStatus
pattern SDownloading = SecondaryStatus' "Downloading"

pattern SDownloadingTrainingImage :: SecondaryStatus
pattern SDownloadingTrainingImage = SecondaryStatus' "DownloadingTrainingImage"

pattern SFailed :: SecondaryStatus
pattern SFailed = SecondaryStatus' "Failed"

pattern SInterrupted :: SecondaryStatus
pattern SInterrupted = SecondaryStatus' "Interrupted"

pattern SLaunchingMLInstances :: SecondaryStatus
pattern SLaunchingMLInstances = SecondaryStatus' "LaunchingMLInstances"

pattern SMaxRuntimeExceeded :: SecondaryStatus
pattern SMaxRuntimeExceeded = SecondaryStatus' "MaxRuntimeExceeded"

pattern SMaxWaitTimeExceeded :: SecondaryStatus
pattern SMaxWaitTimeExceeded = SecondaryStatus' "MaxWaitTimeExceeded"

pattern SPreparingTrainingStack :: SecondaryStatus
pattern SPreparingTrainingStack = SecondaryStatus' "PreparingTrainingStack"

pattern SStarting :: SecondaryStatus
pattern SStarting = SecondaryStatus' "Starting"

pattern SStopped :: SecondaryStatus
pattern SStopped = SecondaryStatus' "Stopped"

pattern SStopping :: SecondaryStatus
pattern SStopping = SecondaryStatus' "Stopping"

pattern STraining :: SecondaryStatus
pattern STraining = SecondaryStatus' "Training"

pattern SUploading :: SecondaryStatus
pattern SUploading = SecondaryStatus' "Uploading"

{-# COMPLETE
  SCompleted,
  SDownloading,
  SDownloadingTrainingImage,
  SFailed,
  SInterrupted,
  SLaunchingMLInstances,
  SMaxRuntimeExceeded,
  SMaxWaitTimeExceeded,
  SPreparingTrainingStack,
  SStarting,
  SStopped,
  SStopping,
  STraining,
  SUploading,
  SecondaryStatus'
  #-}
