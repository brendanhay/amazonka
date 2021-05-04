{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SecondaryStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SecondaryStatus
  ( SecondaryStatus
      ( ..,
        SecondaryStatus_Completed,
        SecondaryStatus_Downloading,
        SecondaryStatus_DownloadingTrainingImage,
        SecondaryStatus_Failed,
        SecondaryStatus_Interrupted,
        SecondaryStatus_LaunchingMLInstances,
        SecondaryStatus_MaxRuntimeExceeded,
        SecondaryStatus_MaxWaitTimeExceeded,
        SecondaryStatus_PreparingTrainingStack,
        SecondaryStatus_Starting,
        SecondaryStatus_Stopped,
        SecondaryStatus_Stopping,
        SecondaryStatus_Training,
        SecondaryStatus_Updating,
        SecondaryStatus_Uploading
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SecondaryStatus = SecondaryStatus'
  { fromSecondaryStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern SecondaryStatus_Completed :: SecondaryStatus
pattern SecondaryStatus_Completed = SecondaryStatus' "Completed"

pattern SecondaryStatus_Downloading :: SecondaryStatus
pattern SecondaryStatus_Downloading = SecondaryStatus' "Downloading"

pattern SecondaryStatus_DownloadingTrainingImage :: SecondaryStatus
pattern SecondaryStatus_DownloadingTrainingImage = SecondaryStatus' "DownloadingTrainingImage"

pattern SecondaryStatus_Failed :: SecondaryStatus
pattern SecondaryStatus_Failed = SecondaryStatus' "Failed"

pattern SecondaryStatus_Interrupted :: SecondaryStatus
pattern SecondaryStatus_Interrupted = SecondaryStatus' "Interrupted"

pattern SecondaryStatus_LaunchingMLInstances :: SecondaryStatus
pattern SecondaryStatus_LaunchingMLInstances = SecondaryStatus' "LaunchingMLInstances"

pattern SecondaryStatus_MaxRuntimeExceeded :: SecondaryStatus
pattern SecondaryStatus_MaxRuntimeExceeded = SecondaryStatus' "MaxRuntimeExceeded"

pattern SecondaryStatus_MaxWaitTimeExceeded :: SecondaryStatus
pattern SecondaryStatus_MaxWaitTimeExceeded = SecondaryStatus' "MaxWaitTimeExceeded"

pattern SecondaryStatus_PreparingTrainingStack :: SecondaryStatus
pattern SecondaryStatus_PreparingTrainingStack = SecondaryStatus' "PreparingTrainingStack"

pattern SecondaryStatus_Starting :: SecondaryStatus
pattern SecondaryStatus_Starting = SecondaryStatus' "Starting"

pattern SecondaryStatus_Stopped :: SecondaryStatus
pattern SecondaryStatus_Stopped = SecondaryStatus' "Stopped"

pattern SecondaryStatus_Stopping :: SecondaryStatus
pattern SecondaryStatus_Stopping = SecondaryStatus' "Stopping"

pattern SecondaryStatus_Training :: SecondaryStatus
pattern SecondaryStatus_Training = SecondaryStatus' "Training"

pattern SecondaryStatus_Updating :: SecondaryStatus
pattern SecondaryStatus_Updating = SecondaryStatus' "Updating"

pattern SecondaryStatus_Uploading :: SecondaryStatus
pattern SecondaryStatus_Uploading = SecondaryStatus' "Uploading"

{-# COMPLETE
  SecondaryStatus_Completed,
  SecondaryStatus_Downloading,
  SecondaryStatus_DownloadingTrainingImage,
  SecondaryStatus_Failed,
  SecondaryStatus_Interrupted,
  SecondaryStatus_LaunchingMLInstances,
  SecondaryStatus_MaxRuntimeExceeded,
  SecondaryStatus_MaxWaitTimeExceeded,
  SecondaryStatus_PreparingTrainingStack,
  SecondaryStatus_Starting,
  SecondaryStatus_Stopped,
  SecondaryStatus_Stopping,
  SecondaryStatus_Training,
  SecondaryStatus_Updating,
  SecondaryStatus_Uploading,
  SecondaryStatus'
  #-}
