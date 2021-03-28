{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Job
  ( Job (..)
  -- * Smart constructor
  , mkJob
  -- * Lenses
  , jRole
  , jSettings
  , jAccelerationSettings
  , jAccelerationStatus
  , jArn
  , jBillingTagsSource
  , jCreatedAt
  , jCurrentPhase
  , jErrorCode
  , jErrorMessage
  , jHopDestinations
  , jId
  , jJobPercentComplete
  , jJobTemplate
  , jMessages
  , jOutputGroupDetails
  , jPriority
  , jQueue
  , jQueueTransitions
  , jRetryCount
  , jSimulateReservedQueue
  , jStatus
  , jStatusUpdateInterval
  , jTiming
  , jUserMetadata
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AccelerationSettings as Types
import qualified Network.AWS.MediaConvert.Types.AccelerationStatus as Types
import qualified Network.AWS.MediaConvert.Types.BillingTagsSource as Types
import qualified Network.AWS.MediaConvert.Types.HopDestination as Types
import qualified Network.AWS.MediaConvert.Types.JobMessages as Types
import qualified Network.AWS.MediaConvert.Types.JobPhase as Types
import qualified Network.AWS.MediaConvert.Types.JobSettings as Types
import qualified Network.AWS.MediaConvert.Types.JobStatus as Types
import qualified Network.AWS.MediaConvert.Types.OutputGroupDetail as Types
import qualified Network.AWS.MediaConvert.Types.QueueTransition as Types
import qualified Network.AWS.MediaConvert.Types.SimulateReservedQueue as Types
import qualified Network.AWS.MediaConvert.Types.StatusUpdateInterval as Types
import qualified Network.AWS.MediaConvert.Types.Timing as Types
import qualified Network.AWS.Prelude as Core

-- | Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { role' :: Core.Text
    -- ^ The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
  , settings :: Types.JobSettings
    -- ^ JobSettings contains all the transcode settings for a job.
  , accelerationSettings :: Core.Maybe Types.AccelerationSettings
    -- ^ Accelerated transcoding can significantly speed up jobs with long, visually complex content.
  , accelerationStatus :: Core.Maybe Types.AccelerationStatus
    -- ^ Describes whether the current job is running with accelerated transcoding. For jobs that have Acceleration (AccelerationMode) set to DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that have Acceleration (AccelerationMode) set to ENABLED or PREFERRED, AccelerationStatus is one of the other states. AccelerationStatus is IN_PROGRESS initially, while the service determines whether the input files and job settings are compatible with accelerated transcoding. If they are, AcclerationStatus is ACCELERATED. If your input files and job settings aren't compatible with accelerated transcoding, the service either fails your job or runs it without accelerated transcoding, depending on how you set Acceleration (AccelerationMode). When the service runs your job without accelerated transcoding, AccelerationStatus is NOT_ACCELERATED.
  , arn :: Core.Maybe Core.Text
    -- ^ An identifier for this resource that is unique within all of AWS.
  , billingTagsSource :: Core.Maybe Types.BillingTagsSource
    -- ^ The tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in Unix epoch format in seconds, when the job got created.
  , currentPhase :: Core.Maybe Types.JobPhase
    -- ^ A job's phase can be PROBING, TRANSCODING OR UPLOADING
  , errorCode :: Core.Maybe Core.Int
    -- ^ Error code for the job
  , errorMessage :: Core.Maybe Core.Text
    -- ^ Error message of Job
  , hopDestinations :: Core.Maybe [Types.HopDestination]
    -- ^ Optional list of hop destinations.
  , id :: Core.Maybe Core.Text
    -- ^ A portion of the job's ARN, unique within your AWS Elemental MediaConvert resources
  , jobPercentComplete :: Core.Maybe Core.Int
    -- ^ An estimate of how far your job has progressed. This estimate is shown as a percentage of the total time from when your job leaves its queue to when your output files appear in your output Amazon S3 bucket. AWS Elemental MediaConvert provides jobPercentComplete in CloudWatch STATUS_UPDATE events and in the response to GetJob and ListJobs requests. The jobPercentComplete estimate is reliable for the following input containers: Quicktime, Transport Stream, MP4, and MXF. For some jobs, the service can't provide information about job progress. In those cases, jobPercentComplete returns a null value.
  , jobTemplate :: Core.Maybe Core.Text
    -- ^ The job template that the job is created from, if it is created from a job template.
  , messages :: Core.Maybe Types.JobMessages
    -- ^ Provides messages from the service about jobs that you have already successfully submitted.
  , outputGroupDetails :: Core.Maybe [Types.OutputGroupDetail]
    -- ^ List of output group details
  , priority :: Core.Maybe Core.Int
    -- ^ Relative priority on the job.
  , queue :: Core.Maybe Core.Text
    -- ^ When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
  , queueTransitions :: Core.Maybe [Types.QueueTransition]
    -- ^ The job's queue hopping history.
  , retryCount :: Core.Maybe Core.Int
    -- ^ The number of times that the service automatically attempted to process your job after encountering an error.
  , simulateReservedQueue :: Core.Maybe Types.SimulateReservedQueue
    -- ^ Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
  , status :: Core.Maybe Types.JobStatus
    -- ^ A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
  , statusUpdateInterval :: Core.Maybe Types.StatusUpdateInterval
    -- ^ Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
  , timing :: Core.Maybe Types.Timing
    -- ^ Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
  , userMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Job' value with any optional fields omitted.
mkJob
    :: Core.Text -- ^ 'role\''
    -> Types.JobSettings -- ^ 'settings'
    -> Job
mkJob role' settings
  = Job'{role', settings, accelerationSettings = Core.Nothing,
         accelerationStatus = Core.Nothing, arn = Core.Nothing,
         billingTagsSource = Core.Nothing, createdAt = Core.Nothing,
         currentPhase = Core.Nothing, errorCode = Core.Nothing,
         errorMessage = Core.Nothing, hopDestinations = Core.Nothing,
         id = Core.Nothing, jobPercentComplete = Core.Nothing,
         jobTemplate = Core.Nothing, messages = Core.Nothing,
         outputGroupDetails = Core.Nothing, priority = Core.Nothing,
         queue = Core.Nothing, queueTransitions = Core.Nothing,
         retryCount = Core.Nothing, simulateReservedQueue = Core.Nothing,
         status = Core.Nothing, statusUpdateInterval = Core.Nothing,
         timing = Core.Nothing, userMetadata = Core.Nothing}

-- | The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jRole :: Lens.Lens' Job Core.Text
jRole = Lens.field @"role'"
{-# INLINEABLE jRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | JobSettings contains all the transcode settings for a job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jSettings :: Lens.Lens' Job Types.JobSettings
jSettings = Lens.field @"settings"
{-# INLINEABLE jSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
--
-- /Note:/ Consider using 'accelerationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAccelerationSettings :: Lens.Lens' Job (Core.Maybe Types.AccelerationSettings)
jAccelerationSettings = Lens.field @"accelerationSettings"
{-# INLINEABLE jAccelerationSettings #-}
{-# DEPRECATED accelerationSettings "Use generic-lens or generic-optics with 'accelerationSettings' instead"  #-}

-- | Describes whether the current job is running with accelerated transcoding. For jobs that have Acceleration (AccelerationMode) set to DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that have Acceleration (AccelerationMode) set to ENABLED or PREFERRED, AccelerationStatus is one of the other states. AccelerationStatus is IN_PROGRESS initially, while the service determines whether the input files and job settings are compatible with accelerated transcoding. If they are, AcclerationStatus is ACCELERATED. If your input files and job settings aren't compatible with accelerated transcoding, the service either fails your job or runs it without accelerated transcoding, depending on how you set Acceleration (AccelerationMode). When the service runs your job without accelerated transcoding, AccelerationStatus is NOT_ACCELERATED.
--
-- /Note:/ Consider using 'accelerationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAccelerationStatus :: Lens.Lens' Job (Core.Maybe Types.AccelerationStatus)
jAccelerationStatus = Lens.field @"accelerationStatus"
{-# INLINEABLE jAccelerationStatus #-}
{-# DEPRECATED accelerationStatus "Use generic-lens or generic-optics with 'accelerationStatus' instead"  #-}

-- | An identifier for this resource that is unique within all of AWS.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jArn :: Lens.Lens' Job (Core.Maybe Core.Text)
jArn = Lens.field @"arn"
{-# INLINEABLE jArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up.
--
-- /Note:/ Consider using 'billingTagsSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jBillingTagsSource :: Lens.Lens' Job (Core.Maybe Types.BillingTagsSource)
jBillingTagsSource = Lens.field @"billingTagsSource"
{-# INLINEABLE jBillingTagsSource #-}
{-# DEPRECATED billingTagsSource "Use generic-lens or generic-optics with 'billingTagsSource' instead"  #-}

-- | The time, in Unix epoch format in seconds, when the job got created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreatedAt :: Lens.Lens' Job (Core.Maybe Core.NominalDiffTime)
jCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE jCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | A job's phase can be PROBING, TRANSCODING OR UPLOADING
--
-- /Note:/ Consider using 'currentPhase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCurrentPhase :: Lens.Lens' Job (Core.Maybe Types.JobPhase)
jCurrentPhase = Lens.field @"currentPhase"
{-# INLINEABLE jCurrentPhase #-}
{-# DEPRECATED currentPhase "Use generic-lens or generic-optics with 'currentPhase' instead"  #-}

-- | Error code for the job
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jErrorCode :: Lens.Lens' Job (Core.Maybe Core.Int)
jErrorCode = Lens.field @"errorCode"
{-# INLINEABLE jErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | Error message of Job
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jErrorMessage :: Lens.Lens' Job (Core.Maybe Core.Text)
jErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE jErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | Optional list of hop destinations.
--
-- /Note:/ Consider using 'hopDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jHopDestinations :: Lens.Lens' Job (Core.Maybe [Types.HopDestination])
jHopDestinations = Lens.field @"hopDestinations"
{-# INLINEABLE jHopDestinations #-}
{-# DEPRECATED hopDestinations "Use generic-lens or generic-optics with 'hopDestinations' instead"  #-}

-- | A portion of the job's ARN, unique within your AWS Elemental MediaConvert resources
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jId :: Lens.Lens' Job (Core.Maybe Core.Text)
jId = Lens.field @"id"
{-# INLINEABLE jId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | An estimate of how far your job has progressed. This estimate is shown as a percentage of the total time from when your job leaves its queue to when your output files appear in your output Amazon S3 bucket. AWS Elemental MediaConvert provides jobPercentComplete in CloudWatch STATUS_UPDATE events and in the response to GetJob and ListJobs requests. The jobPercentComplete estimate is reliable for the following input containers: Quicktime, Transport Stream, MP4, and MXF. For some jobs, the service can't provide information about job progress. In those cases, jobPercentComplete returns a null value.
--
-- /Note:/ Consider using 'jobPercentComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobPercentComplete :: Lens.Lens' Job (Core.Maybe Core.Int)
jJobPercentComplete = Lens.field @"jobPercentComplete"
{-# INLINEABLE jJobPercentComplete #-}
{-# DEPRECATED jobPercentComplete "Use generic-lens or generic-optics with 'jobPercentComplete' instead"  #-}

-- | The job template that the job is created from, if it is created from a job template.
--
-- /Note:/ Consider using 'jobTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobTemplate :: Lens.Lens' Job (Core.Maybe Core.Text)
jJobTemplate = Lens.field @"jobTemplate"
{-# INLINEABLE jJobTemplate #-}
{-# DEPRECATED jobTemplate "Use generic-lens or generic-optics with 'jobTemplate' instead"  #-}

-- | Provides messages from the service about jobs that you have already successfully submitted.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jMessages :: Lens.Lens' Job (Core.Maybe Types.JobMessages)
jMessages = Lens.field @"messages"
{-# INLINEABLE jMessages #-}
{-# DEPRECATED messages "Use generic-lens or generic-optics with 'messages' instead"  #-}

-- | List of output group details
--
-- /Note:/ Consider using 'outputGroupDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jOutputGroupDetails :: Lens.Lens' Job (Core.Maybe [Types.OutputGroupDetail])
jOutputGroupDetails = Lens.field @"outputGroupDetails"
{-# INLINEABLE jOutputGroupDetails #-}
{-# DEPRECATED outputGroupDetails "Use generic-lens or generic-optics with 'outputGroupDetails' instead"  #-}

-- | Relative priority on the job.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jPriority :: Lens.Lens' Job (Core.Maybe Core.Int)
jPriority = Lens.field @"priority"
{-# INLINEABLE jPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jQueue :: Lens.Lens' Job (Core.Maybe Core.Text)
jQueue = Lens.field @"queue"
{-# INLINEABLE jQueue #-}
{-# DEPRECATED queue "Use generic-lens or generic-optics with 'queue' instead"  #-}

-- | The job's queue hopping history.
--
-- /Note:/ Consider using 'queueTransitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jQueueTransitions :: Lens.Lens' Job (Core.Maybe [Types.QueueTransition])
jQueueTransitions = Lens.field @"queueTransitions"
{-# INLINEABLE jQueueTransitions #-}
{-# DEPRECATED queueTransitions "Use generic-lens or generic-optics with 'queueTransitions' instead"  #-}

-- | The number of times that the service automatically attempted to process your job after encountering an error.
--
-- /Note:/ Consider using 'retryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jRetryCount :: Lens.Lens' Job (Core.Maybe Core.Int)
jRetryCount = Lens.field @"retryCount"
{-# INLINEABLE jRetryCount #-}
{-# DEPRECATED retryCount "Use generic-lens or generic-optics with 'retryCount' instead"  #-}

-- | Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
--
-- /Note:/ Consider using 'simulateReservedQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jSimulateReservedQueue :: Lens.Lens' Job (Core.Maybe Types.SimulateReservedQueue)
jSimulateReservedQueue = Lens.field @"simulateReservedQueue"
{-# INLINEABLE jSimulateReservedQueue #-}
{-# DEPRECATED simulateReservedQueue "Use generic-lens or generic-optics with 'simulateReservedQueue' instead"  #-}

-- | A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStatus :: Lens.Lens' Job (Core.Maybe Types.JobStatus)
jStatus = Lens.field @"status"
{-# INLINEABLE jStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- /Note:/ Consider using 'statusUpdateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStatusUpdateInterval :: Lens.Lens' Job (Core.Maybe Types.StatusUpdateInterval)
jStatusUpdateInterval = Lens.field @"statusUpdateInterval"
{-# INLINEABLE jStatusUpdateInterval #-}
{-# DEPRECATED statusUpdateInterval "Use generic-lens or generic-optics with 'statusUpdateInterval' instead"  #-}

-- | Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
--
-- /Note:/ Consider using 'timing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTiming :: Lens.Lens' Job (Core.Maybe Types.Timing)
jTiming = Lens.field @"timing"
{-# INLINEABLE jTiming #-}
{-# DEPRECATED timing "Use generic-lens or generic-optics with 'timing' instead"  #-}

-- | User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jUserMetadata :: Lens.Lens' Job (Core.Maybe (Core.HashMap Core.Text Core.Text))
jUserMetadata = Lens.field @"userMetadata"
{-# INLINEABLE jUserMetadata #-}
{-# DEPRECATED userMetadata "Use generic-lens or generic-optics with 'userMetadata' instead"  #-}

instance Core.FromJSON Job where
        parseJSON
          = Core.withObject "Job" Core.$
              \ x ->
                Job' Core.<$>
                  (x Core..: "role") Core.<*> x Core..: "settings" Core.<*>
                    x Core..:? "accelerationSettings"
                    Core.<*> x Core..:? "accelerationStatus"
                    Core.<*> x Core..:? "arn"
                    Core.<*> x Core..:? "billingTagsSource"
                    Core.<*> x Core..:? "createdAt"
                    Core.<*> x Core..:? "currentPhase"
                    Core.<*> x Core..:? "errorCode"
                    Core.<*> x Core..:? "errorMessage"
                    Core.<*> x Core..:? "hopDestinations"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "jobPercentComplete"
                    Core.<*> x Core..:? "jobTemplate"
                    Core.<*> x Core..:? "messages"
                    Core.<*> x Core..:? "outputGroupDetails"
                    Core.<*> x Core..:? "priority"
                    Core.<*> x Core..:? "queue"
                    Core.<*> x Core..:? "queueTransitions"
                    Core.<*> x Core..:? "retryCount"
                    Core.<*> x Core..:? "simulateReservedQueue"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "statusUpdateInterval"
                    Core.<*> x Core..:? "timing"
                    Core.<*> x Core..:? "userMetadata"
