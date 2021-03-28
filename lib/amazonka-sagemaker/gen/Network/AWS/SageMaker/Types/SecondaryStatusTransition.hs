{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SecondaryStatusTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SecondaryStatusTransition
  ( SecondaryStatusTransition (..)
  -- * Smart constructor
  , mkSecondaryStatusTransition
  -- * Lenses
  , sstStatus
  , sstStartTime
  , sstEndTime
  , sstStatusMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.SecondaryStatus as Types
import qualified Network.AWS.SageMaker.Types.StatusMessage as Types

-- | An array element of 'DescribeTrainingJobResponse$SecondaryStatusTransitions' . It provides additional details about a status that the training job has transitioned through. A training job can be in one of several states, for example, starting, downloading, training, or uploading. Within each state, there are a number of intermediate states. For example, within the starting state, Amazon SageMaker could be starting the training job or launching the ML instances. These transitional states are referred to as the job's secondary status. 
--
--
--
-- /See:/ 'mkSecondaryStatusTransition' smart constructor.
data SecondaryStatusTransition = SecondaryStatusTransition'
  { status :: Types.SecondaryStatus
    -- ^ Contains a secondary status information from a training job.
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
  , startTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the training job transitioned to the current secondary status state.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp that shows when the training job transitioned out of this secondary status state into another secondary status state or when the training job has ended.
  , statusMessage :: Core.Maybe Types.StatusMessage
    -- ^ A detailed description of the progress within a secondary status. 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SecondaryStatusTransition' value with any optional fields omitted.
mkSecondaryStatusTransition
    :: Types.SecondaryStatus -- ^ 'status'
    -> Core.NominalDiffTime -- ^ 'startTime'
    -> SecondaryStatusTransition
mkSecondaryStatusTransition status startTime
  = SecondaryStatusTransition'{status, startTime,
                               endTime = Core.Nothing, statusMessage = Core.Nothing}

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
sstStatus :: Lens.Lens' SecondaryStatusTransition Types.SecondaryStatus
sstStatus = Lens.field @"status"
{-# INLINEABLE sstStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A timestamp that shows when the training job transitioned to the current secondary status state.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstStartTime :: Lens.Lens' SecondaryStatusTransition Core.NominalDiffTime
sstStartTime = Lens.field @"startTime"
{-# INLINEABLE sstStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | A timestamp that shows when the training job transitioned out of this secondary status state into another secondary status state or when the training job has ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstEndTime :: Lens.Lens' SecondaryStatusTransition (Core.Maybe Core.NominalDiffTime)
sstEndTime = Lens.field @"endTime"
{-# INLINEABLE sstEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

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
sstStatusMessage :: Lens.Lens' SecondaryStatusTransition (Core.Maybe Types.StatusMessage)
sstStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE sstStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

instance Core.FromJSON SecondaryStatusTransition where
        parseJSON
          = Core.withObject "SecondaryStatusTransition" Core.$
              \ x ->
                SecondaryStatusTransition' Core.<$>
                  (x Core..: "Status") Core.<*> x Core..: "StartTime" Core.<*>
                    x Core..:? "EndTime"
                    Core.<*> x Core..:? "StatusMessage"
