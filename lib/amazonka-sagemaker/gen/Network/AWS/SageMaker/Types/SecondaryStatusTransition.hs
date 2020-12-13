{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SecondaryStatusTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SecondaryStatusTransition
  ( SecondaryStatusTransition (..),

    -- * Smart constructor
    mkSecondaryStatusTransition,

    -- * Lenses
    sstStatus,
    sstStartTime,
    sstStatusMessage,
    sstEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.SecondaryStatus

-- | An array element of 'DescribeTrainingJobResponse$SecondaryStatusTransitions' . It provides additional details about a status that the training job has transitioned through. A training job can be in one of several states, for example, starting, downloading, training, or uploading. Within each state, there are a number of intermediate states. For example, within the starting state, Amazon SageMaker could be starting the training job or launching the ML instances. These transitional states are referred to as the job's secondary status.
--
--
--
-- /See:/ 'mkSecondaryStatusTransition' smart constructor.
data SecondaryStatusTransition = SecondaryStatusTransition'
  { -- | Contains a secondary status information from a training job.
    --
    -- Status might be one of the following secondary statuses:
    --
    --     * InProgress
    --
    --     *
    --     * @Starting@ - Starting the training job.
    --
    --
    --     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.
    --
    --
    --     * @Training@ - Training is in progress.
    --
    --
    --     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.
    --
    --
    --
    --
    --     * Completed
    --
    --     *
    --     * @Completed@ - The training job has completed.
    --
    --
    --
    --
    --     * Failed
    --
    --     *
    --     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .
    --
    --
    --
    --
    --     * Stopped
    --
    --     *
    --     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.
    --
    --
    --     * @Stopped@ - The training job has stopped.
    --
    --
    --
    --
    --     * Stopping
    --
    --     *
    --     * @Stopping@ - Stopping the training job.
    --
    --
    --
    --
    -- We no longer support the following secondary statuses:
    --
    --     * @LaunchingMLInstances@
    --
    --
    --     * @PreparingTrainingStack@
    --
    --
    --     * @DownloadingTrainingImage@
    status :: SecondaryStatus,
    -- | A timestamp that shows when the training job transitioned to the current secondary status state.
    startTime :: Lude.Timestamp,
    -- | A detailed description of the progress within a secondary status.
    --
    -- Amazon SageMaker provides secondary statuses and status messages that apply to each of them:
    --
    --     * Starting
    --
    --     *
    --     * Starting the training job.
    --
    --
    --     * Launching requested ML instances.
    --
    --
    --     * Insufficient capacity error from EC2 while launching instances, retrying!
    --
    --
    --     * Launched instance was unhealthy, replacing it!
    --
    --
    --     * Preparing the instances for training.
    --
    --
    --
    --
    --     * Training
    --
    --     *
    --     * Downloading the training image.
    --
    --
    --     * Training image download completed. Training in progress.
    --
    --
    --
    --
    -- /Important:/ Status messages are subject to change. Therefore, we recommend not including them in code that programmatically initiates actions. For examples, don't use status messages in if statements.
    -- To have an overview of your training job's progress, view @TrainingJobStatus@ and @SecondaryStatus@ in 'DescribeTrainingJob' , and @StatusMessage@ together. For example, at the start of a training job, you might see the following:
    --
    --     * @TrainingJobStatus@ - InProgress
    --
    --
    --     * @SecondaryStatus@ - Training
    --
    --
    --     * @StatusMessage@ - Downloading the training image
    statusMessage :: Lude.Maybe Lude.Text,
    -- | A timestamp that shows when the training job transitioned out of this secondary status state into another secondary status state or when the training job has ended.
    endTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecondaryStatusTransition' with the minimum fields required to make a request.
--
-- * 'status' - Contains a secondary status information from a training job.
--
-- Status might be one of the following secondary statuses:
--
--     * InProgress
--
--     *
--     * @Starting@ - Starting the training job.
--
--
--     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.
--
--
--     * @Training@ - Training is in progress.
--
--
--     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.
--
--
--
--
--     * Completed
--
--     *
--     * @Completed@ - The training job has completed.
--
--
--
--
--     * Failed
--
--     *
--     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .
--
--
--
--
--     * Stopped
--
--     *
--     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.
--
--
--     * @Stopped@ - The training job has stopped.
--
--
--
--
--     * Stopping
--
--     *
--     * @Stopping@ - Stopping the training job.
--
--
--
--
-- We no longer support the following secondary statuses:
--
--     * @LaunchingMLInstances@
--
--
--     * @PreparingTrainingStack@
--
--
--     * @DownloadingTrainingImage@
--
--
-- * 'startTime' - A timestamp that shows when the training job transitioned to the current secondary status state.
-- * 'statusMessage' - A detailed description of the progress within a secondary status.
--
-- Amazon SageMaker provides secondary statuses and status messages that apply to each of them:
--
--     * Starting
--
--     *
--     * Starting the training job.
--
--
--     * Launching requested ML instances.
--
--
--     * Insufficient capacity error from EC2 while launching instances, retrying!
--
--
--     * Launched instance was unhealthy, replacing it!
--
--
--     * Preparing the instances for training.
--
--
--
--
--     * Training
--
--     *
--     * Downloading the training image.
--
--
--     * Training image download completed. Training in progress.
--
--
--
--
-- /Important:/ Status messages are subject to change. Therefore, we recommend not including them in code that programmatically initiates actions. For examples, don't use status messages in if statements.
-- To have an overview of your training job's progress, view @TrainingJobStatus@ and @SecondaryStatus@ in 'DescribeTrainingJob' , and @StatusMessage@ together. For example, at the start of a training job, you might see the following:
--
--     * @TrainingJobStatus@ - InProgress
--
--
--     * @SecondaryStatus@ - Training
--
--
--     * @StatusMessage@ - Downloading the training image
--
--
-- * 'endTime' - A timestamp that shows when the training job transitioned out of this secondary status state into another secondary status state or when the training job has ended.
mkSecondaryStatusTransition ::
  -- | 'status'
  SecondaryStatus ->
  -- | 'startTime'
  Lude.Timestamp ->
  SecondaryStatusTransition
mkSecondaryStatusTransition pStatus_ pStartTime_ =
  SecondaryStatusTransition'
    { status = pStatus_,
      startTime = pStartTime_,
      statusMessage = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | Contains a secondary status information from a training job.
--
-- Status might be one of the following secondary statuses:
--
--     * InProgress
--
--     *
--     * @Starting@ - Starting the training job.
--
--
--     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.
--
--
--     * @Training@ - Training is in progress.
--
--
--     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.
--
--
--
--
--     * Completed
--
--     *
--     * @Completed@ - The training job has completed.
--
--
--
--
--     * Failed
--
--     *
--     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .
--
--
--
--
--     * Stopped
--
--     *
--     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.
--
--
--     * @Stopped@ - The training job has stopped.
--
--
--
--
--     * Stopping
--
--     *
--     * @Stopping@ - Stopping the training job.
--
--
--
--
-- We no longer support the following secondary statuses:
--
--     * @LaunchingMLInstances@
--
--
--     * @PreparingTrainingStack@
--
--
--     * @DownloadingTrainingImage@
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstStatus :: Lens.Lens' SecondaryStatusTransition SecondaryStatus
sstStatus = Lens.lens (status :: SecondaryStatusTransition -> SecondaryStatus) (\s a -> s {status = a} :: SecondaryStatusTransition)
{-# DEPRECATED sstStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A timestamp that shows when the training job transitioned to the current secondary status state.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstStartTime :: Lens.Lens' SecondaryStatusTransition Lude.Timestamp
sstStartTime = Lens.lens (startTime :: SecondaryStatusTransition -> Lude.Timestamp) (\s a -> s {startTime = a} :: SecondaryStatusTransition)
{-# DEPRECATED sstStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A detailed description of the progress within a secondary status.
--
-- Amazon SageMaker provides secondary statuses and status messages that apply to each of them:
--
--     * Starting
--
--     *
--     * Starting the training job.
--
--
--     * Launching requested ML instances.
--
--
--     * Insufficient capacity error from EC2 while launching instances, retrying!
--
--
--     * Launched instance was unhealthy, replacing it!
--
--
--     * Preparing the instances for training.
--
--
--
--
--     * Training
--
--     *
--     * Downloading the training image.
--
--
--     * Training image download completed. Training in progress.
--
--
--
--
-- /Important:/ Status messages are subject to change. Therefore, we recommend not including them in code that programmatically initiates actions. For examples, don't use status messages in if statements.
-- To have an overview of your training job's progress, view @TrainingJobStatus@ and @SecondaryStatus@ in 'DescribeTrainingJob' , and @StatusMessage@ together. For example, at the start of a training job, you might see the following:
--
--     * @TrainingJobStatus@ - InProgress
--
--
--     * @SecondaryStatus@ - Training
--
--
--     * @StatusMessage@ - Downloading the training image
--
--
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstStatusMessage :: Lens.Lens' SecondaryStatusTransition (Lude.Maybe Lude.Text)
sstStatusMessage = Lens.lens (statusMessage :: SecondaryStatusTransition -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: SecondaryStatusTransition)
{-# DEPRECATED sstStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | A timestamp that shows when the training job transitioned out of this secondary status state into another secondary status state or when the training job has ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstEndTime :: Lens.Lens' SecondaryStatusTransition (Lude.Maybe Lude.Timestamp)
sstEndTime = Lens.lens (endTime :: SecondaryStatusTransition -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: SecondaryStatusTransition)
{-# DEPRECATED sstEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromJSON SecondaryStatusTransition where
  parseJSON =
    Lude.withObject
      "SecondaryStatusTransition"
      ( \x ->
          SecondaryStatusTransition'
            Lude.<$> (x Lude..: "Status")
            Lude.<*> (x Lude..: "StartTime")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "EndTime")
      )
