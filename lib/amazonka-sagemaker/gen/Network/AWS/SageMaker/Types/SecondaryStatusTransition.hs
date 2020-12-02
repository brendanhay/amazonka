{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SecondaryStatusTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SecondaryStatusTransition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.SecondaryStatus

-- | An array element of 'DescribeTrainingJobResponse$SecondaryStatusTransitions' . It provides additional details about a status that the training job has transitioned through. A training job can be in one of several states, for example, starting, downloading, training, or uploading. Within each state, there are a number of intermediate states. For example, within the starting state, Amazon SageMaker could be starting the training job or launching the ML instances. These transitional states are referred to as the job's secondary status.
--
--
--
--
--
-- /See:/ 'secondaryStatusTransition' smart constructor.
data SecondaryStatusTransition = SecondaryStatusTransition'
  { _sstStatusMessage ::
      !(Maybe Text),
    _sstEndTime :: !(Maybe POSIX),
    _sstStatus :: !SecondaryStatus,
    _sstStartTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecondaryStatusTransition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sstStatusMessage' - A detailed description of the progress within a secondary status.  Amazon SageMaker provides secondary statuses and status messages that apply to each of them:     * Starting    *     * Starting the training job.     * Launching requested ML instances.     * Insufficient capacity error from EC2 while launching instances, retrying!     * Launched instance was unhealthy, replacing it!     * Preparing the instances for training.     * Training    *     * Downloading the training image.     * Training image download completed. Training in progress. /Important:/ Status messages are subject to change. Therefore, we recommend not including them in code that programmatically initiates actions. For examples, don't use status messages in if statements. To have an overview of your training job's progress, view @TrainingJobStatus@ and @SecondaryStatus@ in 'DescribeTrainingJob' , and @StatusMessage@ together. For example, at the start of a training job, you might see the following:     * @TrainingJobStatus@ - InProgress     * @SecondaryStatus@ - Training     * @StatusMessage@ - Downloading the training image
--
-- * 'sstEndTime' - A timestamp that shows when the training job transitioned out of this secondary status state into another secondary status state or when the training job has ended.
--
-- * 'sstStatus' - Contains a secondary status information from a training job. Status might be one of the following secondary statuses:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
--
-- * 'sstStartTime' - A timestamp that shows when the training job transitioned to the current secondary status state.
secondaryStatusTransition ::
  -- | 'sstStatus'
  SecondaryStatus ->
  -- | 'sstStartTime'
  UTCTime ->
  SecondaryStatusTransition
secondaryStatusTransition pStatus_ pStartTime_ =
  SecondaryStatusTransition'
    { _sstStatusMessage = Nothing,
      _sstEndTime = Nothing,
      _sstStatus = pStatus_,
      _sstStartTime = _Time # pStartTime_
    }

-- | A detailed description of the progress within a secondary status.  Amazon SageMaker provides secondary statuses and status messages that apply to each of them:     * Starting    *     * Starting the training job.     * Launching requested ML instances.     * Insufficient capacity error from EC2 while launching instances, retrying!     * Launched instance was unhealthy, replacing it!     * Preparing the instances for training.     * Training    *     * Downloading the training image.     * Training image download completed. Training in progress. /Important:/ Status messages are subject to change. Therefore, we recommend not including them in code that programmatically initiates actions. For examples, don't use status messages in if statements. To have an overview of your training job's progress, view @TrainingJobStatus@ and @SecondaryStatus@ in 'DescribeTrainingJob' , and @StatusMessage@ together. For example, at the start of a training job, you might see the following:     * @TrainingJobStatus@ - InProgress     * @SecondaryStatus@ - Training     * @StatusMessage@ - Downloading the training image
sstStatusMessage :: Lens' SecondaryStatusTransition (Maybe Text)
sstStatusMessage = lens _sstStatusMessage (\s a -> s {_sstStatusMessage = a})

-- | A timestamp that shows when the training job transitioned out of this secondary status state into another secondary status state or when the training job has ended.
sstEndTime :: Lens' SecondaryStatusTransition (Maybe UTCTime)
sstEndTime = lens _sstEndTime (\s a -> s {_sstEndTime = a}) . mapping _Time

-- | Contains a secondary status information from a training job. Status might be one of the following secondary statuses:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
sstStatus :: Lens' SecondaryStatusTransition SecondaryStatus
sstStatus = lens _sstStatus (\s a -> s {_sstStatus = a})

-- | A timestamp that shows when the training job transitioned to the current secondary status state.
sstStartTime :: Lens' SecondaryStatusTransition UTCTime
sstStartTime = lens _sstStartTime (\s a -> s {_sstStartTime = a}) . _Time

instance FromJSON SecondaryStatusTransition where
  parseJSON =
    withObject
      "SecondaryStatusTransition"
      ( \x ->
          SecondaryStatusTransition'
            <$> (x .:? "StatusMessage")
            <*> (x .:? "EndTime")
            <*> (x .: "Status")
            <*> (x .: "StartTime")
      )

instance Hashable SecondaryStatusTransition

instance NFData SecondaryStatusTransition
