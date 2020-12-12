{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Job
  ( Job (..),

    -- * Smart constructor
    mkJob,

    -- * Lenses
    jStatus,
    jJobTemplate,
    jAccelerationSettings,
    jPriority,
    jStatusUpdateInterval,
    jARN,
    jCreatedAt,
    jHopDestinations,
    jRetryCount,
    jSimulateReservedQueue,
    jCurrentPhase,
    jQueue,
    jUserMetadata,
    jBillingTagsSource,
    jOutputGroupDetails,
    jErrorCode,
    jQueueTransitions,
    jId,
    jJobPercentComplete,
    jTiming,
    jMessages,
    jErrorMessage,
    jAccelerationStatus,
    jRole,
    jSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AccelerationSettings
import Network.AWS.MediaConvert.Types.AccelerationStatus
import Network.AWS.MediaConvert.Types.BillingTagsSource
import Network.AWS.MediaConvert.Types.HopDestination
import Network.AWS.MediaConvert.Types.JobMessages
import Network.AWS.MediaConvert.Types.JobPhase
import Network.AWS.MediaConvert.Types.JobSettings
import Network.AWS.MediaConvert.Types.JobStatus
import Network.AWS.MediaConvert.Types.OutputGroupDetail
import Network.AWS.MediaConvert.Types.QueueTransition
import Network.AWS.MediaConvert.Types.SimulateReservedQueue
import Network.AWS.MediaConvert.Types.StatusUpdateInterval
import Network.AWS.MediaConvert.Types.Timing
import qualified Network.AWS.Prelude as Lude

-- | Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { status :: Lude.Maybe JobStatus,
    jobTemplate :: Lude.Maybe Lude.Text,
    accelerationSettings :: Lude.Maybe AccelerationSettings,
    priority :: Lude.Maybe Lude.Int,
    statusUpdateInterval :: Lude.Maybe StatusUpdateInterval,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    hopDestinations :: Lude.Maybe [HopDestination],
    retryCount :: Lude.Maybe Lude.Int,
    simulateReservedQueue :: Lude.Maybe SimulateReservedQueue,
    currentPhase :: Lude.Maybe JobPhase,
    queue :: Lude.Maybe Lude.Text,
    userMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    billingTagsSource :: Lude.Maybe BillingTagsSource,
    outputGroupDetails :: Lude.Maybe [OutputGroupDetail],
    errorCode :: Lude.Maybe Lude.Int,
    queueTransitions :: Lude.Maybe [QueueTransition],
    id :: Lude.Maybe Lude.Text,
    jobPercentComplete :: Lude.Maybe Lude.Int,
    timing :: Lude.Maybe Timing,
    messages :: Lude.Maybe JobMessages,
    errorMessage :: Lude.Maybe Lude.Text,
    accelerationStatus :: Lude.Maybe AccelerationStatus,
    role' :: Lude.Text,
    settings :: JobSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- * 'accelerationSettings' - Accelerated transcoding can significantly speed up jobs with long, visually complex content.
-- * 'accelerationStatus' - Describes whether the current job is running with accelerated transcoding. For jobs that have Acceleration (AccelerationMode) set to DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that have Acceleration (AccelerationMode) set to ENABLED or PREFERRED, AccelerationStatus is one of the other states. AccelerationStatus is IN_PROGRESS initially, while the service determines whether the input files and job settings are compatible with accelerated transcoding. If they are, AcclerationStatus is ACCELERATED. If your input files and job settings aren't compatible with accelerated transcoding, the service either fails your job or runs it without accelerated transcoding, depending on how you set Acceleration (AccelerationMode). When the service runs your job without accelerated transcoding, AccelerationStatus is NOT_ACCELERATED.
-- * 'arn' - An identifier for this resource that is unique within all of AWS.
-- * 'billingTagsSource' - The tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up.
-- * 'createdAt' - The time, in Unix epoch format in seconds, when the job got created.
-- * 'currentPhase' - A job's phase can be PROBING, TRANSCODING OR UPLOADING
-- * 'errorCode' - Error code for the job
-- * 'errorMessage' - Error message of Job
-- * 'hopDestinations' - Optional list of hop destinations.
-- * 'id' - A portion of the job's ARN, unique within your AWS Elemental MediaConvert resources
-- * 'jobPercentComplete' - An estimate of how far your job has progressed. This estimate is shown as a percentage of the total time from when your job leaves its queue to when your output files appear in your output Amazon S3 bucket. AWS Elemental MediaConvert provides jobPercentComplete in CloudWatch STATUS_UPDATE events and in the response to GetJob and ListJobs requests. The jobPercentComplete estimate is reliable for the following input containers: Quicktime, Transport Stream, MP4, and MXF. For some jobs, the service can't provide information about job progress. In those cases, jobPercentComplete returns a null value.
-- * 'jobTemplate' - The job template that the job is created from, if it is created from a job template.
-- * 'messages' - Provides messages from the service about jobs that you have already successfully submitted.
-- * 'outputGroupDetails' - List of output group details
-- * 'priority' - Relative priority on the job.
-- * 'queue' - When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
-- * 'queueTransitions' - The job's queue hopping history.
-- * 'retryCount' - The number of times that the service automatically attempted to process your job after encountering an error.
-- * 'role'' - The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
-- * 'settings' - JobSettings contains all the transcode settings for a job.
-- * 'simulateReservedQueue' - Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
-- * 'status' - A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
-- * 'statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
-- * 'timing' - Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
-- * 'userMetadata' - User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
mkJob ::
  -- | 'role''
  Lude.Text ->
  -- | 'settings'
  JobSettings ->
  Job
mkJob pRole_ pSettings_ =
  Job'
    { status = Lude.Nothing,
      jobTemplate = Lude.Nothing,
      accelerationSettings = Lude.Nothing,
      priority = Lude.Nothing,
      statusUpdateInterval = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      hopDestinations = Lude.Nothing,
      retryCount = Lude.Nothing,
      simulateReservedQueue = Lude.Nothing,
      currentPhase = Lude.Nothing,
      queue = Lude.Nothing,
      userMetadata = Lude.Nothing,
      billingTagsSource = Lude.Nothing,
      outputGroupDetails = Lude.Nothing,
      errorCode = Lude.Nothing,
      queueTransitions = Lude.Nothing,
      id = Lude.Nothing,
      jobPercentComplete = Lude.Nothing,
      timing = Lude.Nothing,
      messages = Lude.Nothing,
      errorMessage = Lude.Nothing,
      accelerationStatus = Lude.Nothing,
      role' = pRole_,
      settings = pSettings_
    }

-- | A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStatus :: Lens.Lens' Job (Lude.Maybe JobStatus)
jStatus = Lens.lens (status :: Job -> Lude.Maybe JobStatus) (\s a -> s {status = a} :: Job)
{-# DEPRECATED jStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The job template that the job is created from, if it is created from a job template.
--
-- /Note:/ Consider using 'jobTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobTemplate :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jJobTemplate = Lens.lens (jobTemplate :: Job -> Lude.Maybe Lude.Text) (\s a -> s {jobTemplate = a} :: Job)
{-# DEPRECATED jJobTemplate "Use generic-lens or generic-optics with 'jobTemplate' instead." #-}

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
--
-- /Note:/ Consider using 'accelerationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAccelerationSettings :: Lens.Lens' Job (Lude.Maybe AccelerationSettings)
jAccelerationSettings = Lens.lens (accelerationSettings :: Job -> Lude.Maybe AccelerationSettings) (\s a -> s {accelerationSettings = a} :: Job)
{-# DEPRECATED jAccelerationSettings "Use generic-lens or generic-optics with 'accelerationSettings' instead." #-}

-- | Relative priority on the job.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jPriority :: Lens.Lens' Job (Lude.Maybe Lude.Int)
jPriority = Lens.lens (priority :: Job -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: Job)
{-# DEPRECATED jPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- /Note:/ Consider using 'statusUpdateInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStatusUpdateInterval :: Lens.Lens' Job (Lude.Maybe StatusUpdateInterval)
jStatusUpdateInterval = Lens.lens (statusUpdateInterval :: Job -> Lude.Maybe StatusUpdateInterval) (\s a -> s {statusUpdateInterval = a} :: Job)
{-# DEPRECATED jStatusUpdateInterval "Use generic-lens or generic-optics with 'statusUpdateInterval' instead." #-}

-- | An identifier for this resource that is unique within all of AWS.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jARN :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jARN = Lens.lens (arn :: Job -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Job)
{-# DEPRECATED jARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in Unix epoch format in seconds, when the job got created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreatedAt :: Lens.Lens' Job (Lude.Maybe Lude.Timestamp)
jCreatedAt = Lens.lens (createdAt :: Job -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Job)
{-# DEPRECATED jCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Optional list of hop destinations.
--
-- /Note:/ Consider using 'hopDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jHopDestinations :: Lens.Lens' Job (Lude.Maybe [HopDestination])
jHopDestinations = Lens.lens (hopDestinations :: Job -> Lude.Maybe [HopDestination]) (\s a -> s {hopDestinations = a} :: Job)
{-# DEPRECATED jHopDestinations "Use generic-lens or generic-optics with 'hopDestinations' instead." #-}

-- | The number of times that the service automatically attempted to process your job after encountering an error.
--
-- /Note:/ Consider using 'retryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jRetryCount :: Lens.Lens' Job (Lude.Maybe Lude.Int)
jRetryCount = Lens.lens (retryCount :: Job -> Lude.Maybe Lude.Int) (\s a -> s {retryCount = a} :: Job)
{-# DEPRECATED jRetryCount "Use generic-lens or generic-optics with 'retryCount' instead." #-}

-- | Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
--
-- /Note:/ Consider using 'simulateReservedQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jSimulateReservedQueue :: Lens.Lens' Job (Lude.Maybe SimulateReservedQueue)
jSimulateReservedQueue = Lens.lens (simulateReservedQueue :: Job -> Lude.Maybe SimulateReservedQueue) (\s a -> s {simulateReservedQueue = a} :: Job)
{-# DEPRECATED jSimulateReservedQueue "Use generic-lens or generic-optics with 'simulateReservedQueue' instead." #-}

-- | A job's phase can be PROBING, TRANSCODING OR UPLOADING
--
-- /Note:/ Consider using 'currentPhase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCurrentPhase :: Lens.Lens' Job (Lude.Maybe JobPhase)
jCurrentPhase = Lens.lens (currentPhase :: Job -> Lude.Maybe JobPhase) (\s a -> s {currentPhase = a} :: Job)
{-# DEPRECATED jCurrentPhase "Use generic-lens or generic-optics with 'currentPhase' instead." #-}

-- | When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jQueue :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jQueue = Lens.lens (queue :: Job -> Lude.Maybe Lude.Text) (\s a -> s {queue = a} :: Job)
{-# DEPRECATED jQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jUserMetadata :: Lens.Lens' Job (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jUserMetadata = Lens.lens (userMetadata :: Job -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {userMetadata = a} :: Job)
{-# DEPRECATED jUserMetadata "Use generic-lens or generic-optics with 'userMetadata' instead." #-}

-- | The tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up.
--
-- /Note:/ Consider using 'billingTagsSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jBillingTagsSource :: Lens.Lens' Job (Lude.Maybe BillingTagsSource)
jBillingTagsSource = Lens.lens (billingTagsSource :: Job -> Lude.Maybe BillingTagsSource) (\s a -> s {billingTagsSource = a} :: Job)
{-# DEPRECATED jBillingTagsSource "Use generic-lens or generic-optics with 'billingTagsSource' instead." #-}

-- | List of output group details
--
-- /Note:/ Consider using 'outputGroupDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jOutputGroupDetails :: Lens.Lens' Job (Lude.Maybe [OutputGroupDetail])
jOutputGroupDetails = Lens.lens (outputGroupDetails :: Job -> Lude.Maybe [OutputGroupDetail]) (\s a -> s {outputGroupDetails = a} :: Job)
{-# DEPRECATED jOutputGroupDetails "Use generic-lens or generic-optics with 'outputGroupDetails' instead." #-}

-- | Error code for the job
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jErrorCode :: Lens.Lens' Job (Lude.Maybe Lude.Int)
jErrorCode = Lens.lens (errorCode :: Job -> Lude.Maybe Lude.Int) (\s a -> s {errorCode = a} :: Job)
{-# DEPRECATED jErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The job's queue hopping history.
--
-- /Note:/ Consider using 'queueTransitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jQueueTransitions :: Lens.Lens' Job (Lude.Maybe [QueueTransition])
jQueueTransitions = Lens.lens (queueTransitions :: Job -> Lude.Maybe [QueueTransition]) (\s a -> s {queueTransitions = a} :: Job)
{-# DEPRECATED jQueueTransitions "Use generic-lens or generic-optics with 'queueTransitions' instead." #-}

-- | A portion of the job's ARN, unique within your AWS Elemental MediaConvert resources
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jId :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jId = Lens.lens (id :: Job -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Job)
{-# DEPRECATED jId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | An estimate of how far your job has progressed. This estimate is shown as a percentage of the total time from when your job leaves its queue to when your output files appear in your output Amazon S3 bucket. AWS Elemental MediaConvert provides jobPercentComplete in CloudWatch STATUS_UPDATE events and in the response to GetJob and ListJobs requests. The jobPercentComplete estimate is reliable for the following input containers: Quicktime, Transport Stream, MP4, and MXF. For some jobs, the service can't provide information about job progress. In those cases, jobPercentComplete returns a null value.
--
-- /Note:/ Consider using 'jobPercentComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobPercentComplete :: Lens.Lens' Job (Lude.Maybe Lude.Int)
jJobPercentComplete = Lens.lens (jobPercentComplete :: Job -> Lude.Maybe Lude.Int) (\s a -> s {jobPercentComplete = a} :: Job)
{-# DEPRECATED jJobPercentComplete "Use generic-lens or generic-optics with 'jobPercentComplete' instead." #-}

-- | Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
--
-- /Note:/ Consider using 'timing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTiming :: Lens.Lens' Job (Lude.Maybe Timing)
jTiming = Lens.lens (timing :: Job -> Lude.Maybe Timing) (\s a -> s {timing = a} :: Job)
{-# DEPRECATED jTiming "Use generic-lens or generic-optics with 'timing' instead." #-}

-- | Provides messages from the service about jobs that you have already successfully submitted.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jMessages :: Lens.Lens' Job (Lude.Maybe JobMessages)
jMessages = Lens.lens (messages :: Job -> Lude.Maybe JobMessages) (\s a -> s {messages = a} :: Job)
{-# DEPRECATED jMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | Error message of Job
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jErrorMessage :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jErrorMessage = Lens.lens (errorMessage :: Job -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: Job)
{-# DEPRECATED jErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | Describes whether the current job is running with accelerated transcoding. For jobs that have Acceleration (AccelerationMode) set to DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that have Acceleration (AccelerationMode) set to ENABLED or PREFERRED, AccelerationStatus is one of the other states. AccelerationStatus is IN_PROGRESS initially, while the service determines whether the input files and job settings are compatible with accelerated transcoding. If they are, AcclerationStatus is ACCELERATED. If your input files and job settings aren't compatible with accelerated transcoding, the service either fails your job or runs it without accelerated transcoding, depending on how you set Acceleration (AccelerationMode). When the service runs your job without accelerated transcoding, AccelerationStatus is NOT_ACCELERATED.
--
-- /Note:/ Consider using 'accelerationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAccelerationStatus :: Lens.Lens' Job (Lude.Maybe AccelerationStatus)
jAccelerationStatus = Lens.lens (accelerationStatus :: Job -> Lude.Maybe AccelerationStatus) (\s a -> s {accelerationStatus = a} :: Job)
{-# DEPRECATED jAccelerationStatus "Use generic-lens or generic-optics with 'accelerationStatus' instead." #-}

-- | The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jRole :: Lens.Lens' Job Lude.Text
jRole = Lens.lens (role' :: Job -> Lude.Text) (\s a -> s {role' = a} :: Job)
{-# DEPRECATED jRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | JobSettings contains all the transcode settings for a job.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jSettings :: Lens.Lens' Job JobSettings
jSettings = Lens.lens (settings :: Job -> JobSettings) (\s a -> s {settings = a} :: Job)
{-# DEPRECATED jSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

instance Lude.FromJSON Job where
  parseJSON =
    Lude.withObject
      "Job"
      ( \x ->
          Job'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "jobTemplate")
            Lude.<*> (x Lude..:? "accelerationSettings")
            Lude.<*> (x Lude..:? "priority")
            Lude.<*> (x Lude..:? "statusUpdateInterval")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "hopDestinations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "retryCount")
            Lude.<*> (x Lude..:? "simulateReservedQueue")
            Lude.<*> (x Lude..:? "currentPhase")
            Lude.<*> (x Lude..:? "queue")
            Lude.<*> (x Lude..:? "userMetadata" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "billingTagsSource")
            Lude.<*> (x Lude..:? "outputGroupDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "queueTransitions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "jobPercentComplete")
            Lude.<*> (x Lude..:? "timing")
            Lude.<*> (x Lude..:? "messages")
            Lude.<*> (x Lude..:? "errorMessage")
            Lude.<*> (x Lude..:? "accelerationStatus")
            Lude.<*> (x Lude..: "role")
            Lude.<*> (x Lude..: "settings")
      )
