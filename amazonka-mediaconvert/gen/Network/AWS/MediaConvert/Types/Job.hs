{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Job where

import qualified Network.AWS.Core as Core
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

-- | Each job converts an input file into an output file or files. For more
-- information, see the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | Accelerated transcoding can significantly speed up jobs with long,
    -- visually complex content.
    accelerationSettings :: Core.Maybe AccelerationSettings,
    -- | The tag type that AWS Billing and Cost Management will use to sort your
    -- AWS Elemental MediaConvert costs on any billing report that you set up.
    billingTagsSource :: Core.Maybe BillingTagsSource,
    -- | A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or
    -- ERROR.
    status :: Core.Maybe JobStatus,
    -- | Describes whether the current job is running with accelerated
    -- transcoding. For jobs that have Acceleration (AccelerationMode) set to
    -- DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that
    -- have Acceleration (AccelerationMode) set to ENABLED or PREFERRED,
    -- AccelerationStatus is one of the other states. AccelerationStatus is
    -- IN_PROGRESS initially, while the service determines whether the input
    -- files and job settings are compatible with accelerated transcoding. If
    -- they are, AcclerationStatus is ACCELERATED. If your input files and job
    -- settings aren\'t compatible with accelerated transcoding, the service
    -- either fails your job or runs it without accelerated transcoding,
    -- depending on how you set Acceleration (AccelerationMode). When the
    -- service runs your job without accelerated transcoding,
    -- AccelerationStatus is NOT_ACCELERATED.
    accelerationStatus :: Core.Maybe AccelerationStatus,
    -- | The number of times that the service automatically attempted to process
    -- your job after encountering an error.
    retryCount :: Core.Maybe Core.Int,
    -- | The job\'s queue hopping history.
    queueTransitions :: Core.Maybe [QueueTransition],
    -- | An identifier for this resource that is unique within all of AWS.
    arn :: Core.Maybe Core.Text,
    -- | A portion of the job\'s ARN, unique within your AWS Elemental
    -- MediaConvert resources
    id :: Core.Maybe Core.Text,
    -- | An estimate of how far your job has progressed. This estimate is shown
    -- as a percentage of the total time from when your job leaves its queue to
    -- when your output files appear in your output Amazon S3 bucket. AWS
    -- Elemental MediaConvert provides jobPercentComplete in CloudWatch
    -- STATUS_UPDATE events and in the response to GetJob and ListJobs
    -- requests. The jobPercentComplete estimate is reliable for the following
    -- input containers: Quicktime, Transport Stream, MP4, and MXF. For some
    -- jobs, the service can\'t provide information about job progress. In
    -- those cases, jobPercentComplete returns a null value.
    jobPercentComplete :: Core.Maybe Core.Int,
    -- | The time, in Unix epoch format in seconds, when the job got created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | Relative priority on the job.
    priority :: Core.Maybe Core.Int,
    -- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
    -- CloudWatch Events. Set the interval, in seconds, between status updates.
    -- MediaConvert sends an update at this interval from the time the service
    -- begins processing your job to the time it completes the transcode or
    -- encounters an error.
    statusUpdateInterval :: Core.Maybe StatusUpdateInterval,
    -- | The job template that the job is created from, if it is created from a
    -- job template.
    jobTemplate :: Core.Maybe Core.Text,
    -- | User-defined metadata that you want to associate with an MediaConvert
    -- job. You specify metadata in key\/value pairs.
    userMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | When you create a job, you can specify a queue to send it to. If you
    -- don\'t specify, the job will go to the default queue. For more about
    -- queues, see the User Guide topic at
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
    queue :: Core.Maybe Core.Text,
    -- | A job\'s phase can be PROBING, TRANSCODING OR UPLOADING
    currentPhase :: Core.Maybe JobPhase,
    -- | Error message of Job
    errorMessage :: Core.Maybe Core.Text,
    -- | Enable this setting when you run a test job to estimate how many
    -- reserved transcoding slots (RTS) you need. When this is enabled,
    -- MediaConvert runs your job from an on-demand queue with similar
    -- performance to what you will see with one RTS in a reserved queue. This
    -- setting is disabled by default.
    simulateReservedQueue :: Core.Maybe SimulateReservedQueue,
    -- | Information about when jobs are submitted, started, and finished is
    -- specified in Unix epoch format in seconds.
    timing :: Core.Maybe Timing,
    -- | Optional list of hop destinations.
    hopDestinations :: Core.Maybe [HopDestination],
    -- | Provides messages from the service about jobs that you have already
    -- successfully submitted.
    messages :: Core.Maybe JobMessages,
    -- | Error code for the job
    errorCode :: Core.Maybe Core.Int,
    -- | List of output group details
    outputGroupDetails :: Core.Maybe [OutputGroupDetail],
    -- | The IAM role you use for creating this job. For details about
    -- permissions, see the User Guide topic at the User Guide at
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html
    role' :: Core.Text,
    -- | JobSettings contains all the transcode settings for a job.
    settings :: JobSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerationSettings', 'job_accelerationSettings' - Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content.
--
-- 'billingTagsSource', 'job_billingTagsSource' - The tag type that AWS Billing and Cost Management will use to sort your
-- AWS Elemental MediaConvert costs on any billing report that you set up.
--
-- 'status', 'job_status' - A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or
-- ERROR.
--
-- 'accelerationStatus', 'job_accelerationStatus' - Describes whether the current job is running with accelerated
-- transcoding. For jobs that have Acceleration (AccelerationMode) set to
-- DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that
-- have Acceleration (AccelerationMode) set to ENABLED or PREFERRED,
-- AccelerationStatus is one of the other states. AccelerationStatus is
-- IN_PROGRESS initially, while the service determines whether the input
-- files and job settings are compatible with accelerated transcoding. If
-- they are, AcclerationStatus is ACCELERATED. If your input files and job
-- settings aren\'t compatible with accelerated transcoding, the service
-- either fails your job or runs it without accelerated transcoding,
-- depending on how you set Acceleration (AccelerationMode). When the
-- service runs your job without accelerated transcoding,
-- AccelerationStatus is NOT_ACCELERATED.
--
-- 'retryCount', 'job_retryCount' - The number of times that the service automatically attempted to process
-- your job after encountering an error.
--
-- 'queueTransitions', 'job_queueTransitions' - The job\'s queue hopping history.
--
-- 'arn', 'job_arn' - An identifier for this resource that is unique within all of AWS.
--
-- 'id', 'job_id' - A portion of the job\'s ARN, unique within your AWS Elemental
-- MediaConvert resources
--
-- 'jobPercentComplete', 'job_jobPercentComplete' - An estimate of how far your job has progressed. This estimate is shown
-- as a percentage of the total time from when your job leaves its queue to
-- when your output files appear in your output Amazon S3 bucket. AWS
-- Elemental MediaConvert provides jobPercentComplete in CloudWatch
-- STATUS_UPDATE events and in the response to GetJob and ListJobs
-- requests. The jobPercentComplete estimate is reliable for the following
-- input containers: Quicktime, Transport Stream, MP4, and MXF. For some
-- jobs, the service can\'t provide information about job progress. In
-- those cases, jobPercentComplete returns a null value.
--
-- 'createdAt', 'job_createdAt' - The time, in Unix epoch format in seconds, when the job got created.
--
-- 'priority', 'job_priority' - Relative priority on the job.
--
-- 'statusUpdateInterval', 'job_statusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
--
-- 'jobTemplate', 'job_jobTemplate' - The job template that the job is created from, if it is created from a
-- job template.
--
-- 'userMetadata', 'job_userMetadata' - User-defined metadata that you want to associate with an MediaConvert
-- job. You specify metadata in key\/value pairs.
--
-- 'queue', 'job_queue' - When you create a job, you can specify a queue to send it to. If you
-- don\'t specify, the job will go to the default queue. For more about
-- queues, see the User Guide topic at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
--
-- 'currentPhase', 'job_currentPhase' - A job\'s phase can be PROBING, TRANSCODING OR UPLOADING
--
-- 'errorMessage', 'job_errorMessage' - Error message of Job
--
-- 'simulateReservedQueue', 'job_simulateReservedQueue' - Enable this setting when you run a test job to estimate how many
-- reserved transcoding slots (RTS) you need. When this is enabled,
-- MediaConvert runs your job from an on-demand queue with similar
-- performance to what you will see with one RTS in a reserved queue. This
-- setting is disabled by default.
--
-- 'timing', 'job_timing' - Information about when jobs are submitted, started, and finished is
-- specified in Unix epoch format in seconds.
--
-- 'hopDestinations', 'job_hopDestinations' - Optional list of hop destinations.
--
-- 'messages', 'job_messages' - Provides messages from the service about jobs that you have already
-- successfully submitted.
--
-- 'errorCode', 'job_errorCode' - Error code for the job
--
-- 'outputGroupDetails', 'job_outputGroupDetails' - List of output group details
--
-- 'role'', 'job_role' - The IAM role you use for creating this job. For details about
-- permissions, see the User Guide topic at the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html
--
-- 'settings', 'job_settings' - JobSettings contains all the transcode settings for a job.
newJob ::
  -- | 'role''
  Core.Text ->
  -- | 'settings'
  JobSettings ->
  Job
newJob pRole_ pSettings_ =
  Job'
    { accelerationSettings = Core.Nothing,
      billingTagsSource = Core.Nothing,
      status = Core.Nothing,
      accelerationStatus = Core.Nothing,
      retryCount = Core.Nothing,
      queueTransitions = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      jobPercentComplete = Core.Nothing,
      createdAt = Core.Nothing,
      priority = Core.Nothing,
      statusUpdateInterval = Core.Nothing,
      jobTemplate = Core.Nothing,
      userMetadata = Core.Nothing,
      queue = Core.Nothing,
      currentPhase = Core.Nothing,
      errorMessage = Core.Nothing,
      simulateReservedQueue = Core.Nothing,
      timing = Core.Nothing,
      hopDestinations = Core.Nothing,
      messages = Core.Nothing,
      errorCode = Core.Nothing,
      outputGroupDetails = Core.Nothing,
      role' = pRole_,
      settings = pSettings_
    }

-- | Accelerated transcoding can significantly speed up jobs with long,
-- visually complex content.
job_accelerationSettings :: Lens.Lens' Job (Core.Maybe AccelerationSettings)
job_accelerationSettings = Lens.lens (\Job' {accelerationSettings} -> accelerationSettings) (\s@Job' {} a -> s {accelerationSettings = a} :: Job)

-- | The tag type that AWS Billing and Cost Management will use to sort your
-- AWS Elemental MediaConvert costs on any billing report that you set up.
job_billingTagsSource :: Lens.Lens' Job (Core.Maybe BillingTagsSource)
job_billingTagsSource = Lens.lens (\Job' {billingTagsSource} -> billingTagsSource) (\s@Job' {} a -> s {billingTagsSource = a} :: Job)

-- | A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or
-- ERROR.
job_status :: Lens.Lens' Job (Core.Maybe JobStatus)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | Describes whether the current job is running with accelerated
-- transcoding. For jobs that have Acceleration (AccelerationMode) set to
-- DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that
-- have Acceleration (AccelerationMode) set to ENABLED or PREFERRED,
-- AccelerationStatus is one of the other states. AccelerationStatus is
-- IN_PROGRESS initially, while the service determines whether the input
-- files and job settings are compatible with accelerated transcoding. If
-- they are, AcclerationStatus is ACCELERATED. If your input files and job
-- settings aren\'t compatible with accelerated transcoding, the service
-- either fails your job or runs it without accelerated transcoding,
-- depending on how you set Acceleration (AccelerationMode). When the
-- service runs your job without accelerated transcoding,
-- AccelerationStatus is NOT_ACCELERATED.
job_accelerationStatus :: Lens.Lens' Job (Core.Maybe AccelerationStatus)
job_accelerationStatus = Lens.lens (\Job' {accelerationStatus} -> accelerationStatus) (\s@Job' {} a -> s {accelerationStatus = a} :: Job)

-- | The number of times that the service automatically attempted to process
-- your job after encountering an error.
job_retryCount :: Lens.Lens' Job (Core.Maybe Core.Int)
job_retryCount = Lens.lens (\Job' {retryCount} -> retryCount) (\s@Job' {} a -> s {retryCount = a} :: Job)

-- | The job\'s queue hopping history.
job_queueTransitions :: Lens.Lens' Job (Core.Maybe [QueueTransition])
job_queueTransitions = Lens.lens (\Job' {queueTransitions} -> queueTransitions) (\s@Job' {} a -> s {queueTransitions = a} :: Job) Core.. Lens.mapping Lens._Coerce

-- | An identifier for this resource that is unique within all of AWS.
job_arn :: Lens.Lens' Job (Core.Maybe Core.Text)
job_arn = Lens.lens (\Job' {arn} -> arn) (\s@Job' {} a -> s {arn = a} :: Job)

-- | A portion of the job\'s ARN, unique within your AWS Elemental
-- MediaConvert resources
job_id :: Lens.Lens' Job (Core.Maybe Core.Text)
job_id = Lens.lens (\Job' {id} -> id) (\s@Job' {} a -> s {id = a} :: Job)

-- | An estimate of how far your job has progressed. This estimate is shown
-- as a percentage of the total time from when your job leaves its queue to
-- when your output files appear in your output Amazon S3 bucket. AWS
-- Elemental MediaConvert provides jobPercentComplete in CloudWatch
-- STATUS_UPDATE events and in the response to GetJob and ListJobs
-- requests. The jobPercentComplete estimate is reliable for the following
-- input containers: Quicktime, Transport Stream, MP4, and MXF. For some
-- jobs, the service can\'t provide information about job progress. In
-- those cases, jobPercentComplete returns a null value.
job_jobPercentComplete :: Lens.Lens' Job (Core.Maybe Core.Int)
job_jobPercentComplete = Lens.lens (\Job' {jobPercentComplete} -> jobPercentComplete) (\s@Job' {} a -> s {jobPercentComplete = a} :: Job)

-- | The time, in Unix epoch format in seconds, when the job got created.
job_createdAt :: Lens.Lens' Job (Core.Maybe Core.UTCTime)
job_createdAt = Lens.lens (\Job' {createdAt} -> createdAt) (\s@Job' {} a -> s {createdAt = a} :: Job) Core.. Lens.mapping Core._Time

-- | Relative priority on the job.
job_priority :: Lens.Lens' Job (Core.Maybe Core.Int)
job_priority = Lens.lens (\Job' {priority} -> priority) (\s@Job' {} a -> s {priority = a} :: Job)

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
job_statusUpdateInterval :: Lens.Lens' Job (Core.Maybe StatusUpdateInterval)
job_statusUpdateInterval = Lens.lens (\Job' {statusUpdateInterval} -> statusUpdateInterval) (\s@Job' {} a -> s {statusUpdateInterval = a} :: Job)

-- | The job template that the job is created from, if it is created from a
-- job template.
job_jobTemplate :: Lens.Lens' Job (Core.Maybe Core.Text)
job_jobTemplate = Lens.lens (\Job' {jobTemplate} -> jobTemplate) (\s@Job' {} a -> s {jobTemplate = a} :: Job)

-- | User-defined metadata that you want to associate with an MediaConvert
-- job. You specify metadata in key\/value pairs.
job_userMetadata :: Lens.Lens' Job (Core.Maybe (Core.HashMap Core.Text Core.Text))
job_userMetadata = Lens.lens (\Job' {userMetadata} -> userMetadata) (\s@Job' {} a -> s {userMetadata = a} :: Job) Core.. Lens.mapping Lens._Coerce

-- | When you create a job, you can specify a queue to send it to. If you
-- don\'t specify, the job will go to the default queue. For more about
-- queues, see the User Guide topic at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
job_queue :: Lens.Lens' Job (Core.Maybe Core.Text)
job_queue = Lens.lens (\Job' {queue} -> queue) (\s@Job' {} a -> s {queue = a} :: Job)

-- | A job\'s phase can be PROBING, TRANSCODING OR UPLOADING
job_currentPhase :: Lens.Lens' Job (Core.Maybe JobPhase)
job_currentPhase = Lens.lens (\Job' {currentPhase} -> currentPhase) (\s@Job' {} a -> s {currentPhase = a} :: Job)

-- | Error message of Job
job_errorMessage :: Lens.Lens' Job (Core.Maybe Core.Text)
job_errorMessage = Lens.lens (\Job' {errorMessage} -> errorMessage) (\s@Job' {} a -> s {errorMessage = a} :: Job)

-- | Enable this setting when you run a test job to estimate how many
-- reserved transcoding slots (RTS) you need. When this is enabled,
-- MediaConvert runs your job from an on-demand queue with similar
-- performance to what you will see with one RTS in a reserved queue. This
-- setting is disabled by default.
job_simulateReservedQueue :: Lens.Lens' Job (Core.Maybe SimulateReservedQueue)
job_simulateReservedQueue = Lens.lens (\Job' {simulateReservedQueue} -> simulateReservedQueue) (\s@Job' {} a -> s {simulateReservedQueue = a} :: Job)

-- | Information about when jobs are submitted, started, and finished is
-- specified in Unix epoch format in seconds.
job_timing :: Lens.Lens' Job (Core.Maybe Timing)
job_timing = Lens.lens (\Job' {timing} -> timing) (\s@Job' {} a -> s {timing = a} :: Job)

-- | Optional list of hop destinations.
job_hopDestinations :: Lens.Lens' Job (Core.Maybe [HopDestination])
job_hopDestinations = Lens.lens (\Job' {hopDestinations} -> hopDestinations) (\s@Job' {} a -> s {hopDestinations = a} :: Job) Core.. Lens.mapping Lens._Coerce

-- | Provides messages from the service about jobs that you have already
-- successfully submitted.
job_messages :: Lens.Lens' Job (Core.Maybe JobMessages)
job_messages = Lens.lens (\Job' {messages} -> messages) (\s@Job' {} a -> s {messages = a} :: Job)

-- | Error code for the job
job_errorCode :: Lens.Lens' Job (Core.Maybe Core.Int)
job_errorCode = Lens.lens (\Job' {errorCode} -> errorCode) (\s@Job' {} a -> s {errorCode = a} :: Job)

-- | List of output group details
job_outputGroupDetails :: Lens.Lens' Job (Core.Maybe [OutputGroupDetail])
job_outputGroupDetails = Lens.lens (\Job' {outputGroupDetails} -> outputGroupDetails) (\s@Job' {} a -> s {outputGroupDetails = a} :: Job) Core.. Lens.mapping Lens._Coerce

-- | The IAM role you use for creating this job. For details about
-- permissions, see the User Guide topic at the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html
job_role :: Lens.Lens' Job Core.Text
job_role = Lens.lens (\Job' {role'} -> role') (\s@Job' {} a -> s {role' = a} :: Job)

-- | JobSettings contains all the transcode settings for a job.
job_settings :: Lens.Lens' Job JobSettings
job_settings = Lens.lens (\Job' {settings} -> settings) (\s@Job' {} a -> s {settings = a} :: Job)

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject
      "Job"
      ( \x ->
          Job'
            Core.<$> (x Core..:? "accelerationSettings")
            Core.<*> (x Core..:? "billingTagsSource")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "accelerationStatus")
            Core.<*> (x Core..:? "retryCount")
            Core.<*> (x Core..:? "queueTransitions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "jobPercentComplete")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "priority")
            Core.<*> (x Core..:? "statusUpdateInterval")
            Core.<*> (x Core..:? "jobTemplate")
            Core.<*> (x Core..:? "userMetadata" Core..!= Core.mempty)
            Core.<*> (x Core..:? "queue")
            Core.<*> (x Core..:? "currentPhase")
            Core.<*> (x Core..:? "errorMessage")
            Core.<*> (x Core..:? "simulateReservedQueue")
            Core.<*> (x Core..:? "timing")
            Core.<*> (x Core..:? "hopDestinations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "messages")
            Core.<*> (x Core..:? "errorCode")
            Core.<*> ( x Core..:? "outputGroupDetails"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..: "role")
            Core.<*> (x Core..: "settings")
      )

instance Core.Hashable Job

instance Core.NFData Job
