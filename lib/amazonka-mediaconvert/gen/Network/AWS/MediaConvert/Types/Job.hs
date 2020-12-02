{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Job where

import Network.AWS.Lens
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
import Network.AWS.Prelude

-- | Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jStatus :: !(Maybe JobStatus),
    _jJobTemplate :: !(Maybe Text),
    _jAccelerationSettings :: !(Maybe AccelerationSettings),
    _jPriority :: !(Maybe Int),
    _jStatusUpdateInterval :: !(Maybe StatusUpdateInterval),
    _jARN :: !(Maybe Text),
    _jCreatedAt :: !(Maybe POSIX),
    _jHopDestinations :: !(Maybe [HopDestination]),
    _jRetryCount :: !(Maybe Int),
    _jSimulateReservedQueue :: !(Maybe SimulateReservedQueue),
    _jCurrentPhase :: !(Maybe JobPhase),
    _jQueue :: !(Maybe Text),
    _jUserMetadata :: !(Maybe (Map Text (Text))),
    _jBillingTagsSource :: !(Maybe BillingTagsSource),
    _jOutputGroupDetails :: !(Maybe [OutputGroupDetail]),
    _jErrorCode :: !(Maybe Int),
    _jQueueTransitions :: !(Maybe [QueueTransition]),
    _jId :: !(Maybe Text),
    _jJobPercentComplete :: !(Maybe Int),
    _jTiming :: !(Maybe Timing),
    _jMessages :: !(Maybe JobMessages),
    _jErrorMessage :: !(Maybe Text),
    _jAccelerationStatus :: !(Maybe AccelerationStatus),
    _jRole :: !Text,
    _jSettings :: !JobSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jStatus' - A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
--
-- * 'jJobTemplate' - The job template that the job is created from, if it is created from a job template.
--
-- * 'jAccelerationSettings' - Accelerated transcoding can significantly speed up jobs with long, visually complex content.
--
-- * 'jPriority' - Relative priority on the job.
--
-- * 'jStatusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- * 'jARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'jCreatedAt' - The time, in Unix epoch format in seconds, when the job got created.
--
-- * 'jHopDestinations' - Optional list of hop destinations.
--
-- * 'jRetryCount' - The number of times that the service automatically attempted to process your job after encountering an error.
--
-- * 'jSimulateReservedQueue' - Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
--
-- * 'jCurrentPhase' - A job's phase can be PROBING, TRANSCODING OR UPLOADING
--
-- * 'jQueue' - When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- * 'jUserMetadata' - User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
--
-- * 'jBillingTagsSource' - The tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up.
--
-- * 'jOutputGroupDetails' - List of output group details
--
-- * 'jErrorCode' - Error code for the job
--
-- * 'jQueueTransitions' - The job's queue hopping history.
--
-- * 'jId' - A portion of the job's ARN, unique within your AWS Elemental MediaConvert resources
--
-- * 'jJobPercentComplete' - An estimate of how far your job has progressed. This estimate is shown as a percentage of the total time from when your job leaves its queue to when your output files appear in your output Amazon S3 bucket. AWS Elemental MediaConvert provides jobPercentComplete in CloudWatch STATUS_UPDATE events and in the response to GetJob and ListJobs requests. The jobPercentComplete estimate is reliable for the following input containers: Quicktime, Transport Stream, MP4, and MXF. For some jobs, the service can't provide information about job progress. In those cases, jobPercentComplete returns a null value.
--
-- * 'jTiming' - Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
--
-- * 'jMessages' - Provides messages from the service about jobs that you have already successfully submitted.
--
-- * 'jErrorMessage' - Error message of Job
--
-- * 'jAccelerationStatus' - Describes whether the current job is running with accelerated transcoding. For jobs that have Acceleration (AccelerationMode) set to DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that have Acceleration (AccelerationMode) set to ENABLED or PREFERRED, AccelerationStatus is one of the other states. AccelerationStatus is IN_PROGRESS initially, while the service determines whether the input files and job settings are compatible with accelerated transcoding. If they are, AcclerationStatus is ACCELERATED. If your input files and job settings aren't compatible with accelerated transcoding, the service either fails your job or runs it without accelerated transcoding, depending on how you set Acceleration (AccelerationMode). When the service runs your job without accelerated transcoding, AccelerationStatus is NOT_ACCELERATED.
--
-- * 'jRole' - The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
--
-- * 'jSettings' - JobSettings contains all the transcode settings for a job.
job ::
  -- | 'jRole'
  Text ->
  -- | 'jSettings'
  JobSettings ->
  Job
job pRole_ pSettings_ =
  Job'
    { _jStatus = Nothing,
      _jJobTemplate = Nothing,
      _jAccelerationSettings = Nothing,
      _jPriority = Nothing,
      _jStatusUpdateInterval = Nothing,
      _jARN = Nothing,
      _jCreatedAt = Nothing,
      _jHopDestinations = Nothing,
      _jRetryCount = Nothing,
      _jSimulateReservedQueue = Nothing,
      _jCurrentPhase = Nothing,
      _jQueue = Nothing,
      _jUserMetadata = Nothing,
      _jBillingTagsSource = Nothing,
      _jOutputGroupDetails = Nothing,
      _jErrorCode = Nothing,
      _jQueueTransitions = Nothing,
      _jId = Nothing,
      _jJobPercentComplete = Nothing,
      _jTiming = Nothing,
      _jMessages = Nothing,
      _jErrorMessage = Nothing,
      _jAccelerationStatus = Nothing,
      _jRole = pRole_,
      _jSettings = pSettings_
    }

-- | A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
jStatus :: Lens' Job (Maybe JobStatus)
jStatus = lens _jStatus (\s a -> s {_jStatus = a})

-- | The job template that the job is created from, if it is created from a job template.
jJobTemplate :: Lens' Job (Maybe Text)
jJobTemplate = lens _jJobTemplate (\s a -> s {_jJobTemplate = a})

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
jAccelerationSettings :: Lens' Job (Maybe AccelerationSettings)
jAccelerationSettings = lens _jAccelerationSettings (\s a -> s {_jAccelerationSettings = a})

-- | Relative priority on the job.
jPriority :: Lens' Job (Maybe Int)
jPriority = lens _jPriority (\s a -> s {_jPriority = a})

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
jStatusUpdateInterval :: Lens' Job (Maybe StatusUpdateInterval)
jStatusUpdateInterval = lens _jStatusUpdateInterval (\s a -> s {_jStatusUpdateInterval = a})

-- | An identifier for this resource that is unique within all of AWS.
jARN :: Lens' Job (Maybe Text)
jARN = lens _jARN (\s a -> s {_jARN = a})

-- | The time, in Unix epoch format in seconds, when the job got created.
jCreatedAt :: Lens' Job (Maybe UTCTime)
jCreatedAt = lens _jCreatedAt (\s a -> s {_jCreatedAt = a}) . mapping _Time

-- | Optional list of hop destinations.
jHopDestinations :: Lens' Job [HopDestination]
jHopDestinations = lens _jHopDestinations (\s a -> s {_jHopDestinations = a}) . _Default . _Coerce

-- | The number of times that the service automatically attempted to process your job after encountering an error.
jRetryCount :: Lens' Job (Maybe Int)
jRetryCount = lens _jRetryCount (\s a -> s {_jRetryCount = a})

-- | Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
jSimulateReservedQueue :: Lens' Job (Maybe SimulateReservedQueue)
jSimulateReservedQueue = lens _jSimulateReservedQueue (\s a -> s {_jSimulateReservedQueue = a})

-- | A job's phase can be PROBING, TRANSCODING OR UPLOADING
jCurrentPhase :: Lens' Job (Maybe JobPhase)
jCurrentPhase = lens _jCurrentPhase (\s a -> s {_jCurrentPhase = a})

-- | When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
jQueue :: Lens' Job (Maybe Text)
jQueue = lens _jQueue (\s a -> s {_jQueue = a})

-- | User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
jUserMetadata :: Lens' Job (HashMap Text (Text))
jUserMetadata = lens _jUserMetadata (\s a -> s {_jUserMetadata = a}) . _Default . _Map

-- | The tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up.
jBillingTagsSource :: Lens' Job (Maybe BillingTagsSource)
jBillingTagsSource = lens _jBillingTagsSource (\s a -> s {_jBillingTagsSource = a})

-- | List of output group details
jOutputGroupDetails :: Lens' Job [OutputGroupDetail]
jOutputGroupDetails = lens _jOutputGroupDetails (\s a -> s {_jOutputGroupDetails = a}) . _Default . _Coerce

-- | Error code for the job
jErrorCode :: Lens' Job (Maybe Int)
jErrorCode = lens _jErrorCode (\s a -> s {_jErrorCode = a})

-- | The job's queue hopping history.
jQueueTransitions :: Lens' Job [QueueTransition]
jQueueTransitions = lens _jQueueTransitions (\s a -> s {_jQueueTransitions = a}) . _Default . _Coerce

-- | A portion of the job's ARN, unique within your AWS Elemental MediaConvert resources
jId :: Lens' Job (Maybe Text)
jId = lens _jId (\s a -> s {_jId = a})

-- | An estimate of how far your job has progressed. This estimate is shown as a percentage of the total time from when your job leaves its queue to when your output files appear in your output Amazon S3 bucket. AWS Elemental MediaConvert provides jobPercentComplete in CloudWatch STATUS_UPDATE events and in the response to GetJob and ListJobs requests. The jobPercentComplete estimate is reliable for the following input containers: Quicktime, Transport Stream, MP4, and MXF. For some jobs, the service can't provide information about job progress. In those cases, jobPercentComplete returns a null value.
jJobPercentComplete :: Lens' Job (Maybe Int)
jJobPercentComplete = lens _jJobPercentComplete (\s a -> s {_jJobPercentComplete = a})

-- | Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
jTiming :: Lens' Job (Maybe Timing)
jTiming = lens _jTiming (\s a -> s {_jTiming = a})

-- | Provides messages from the service about jobs that you have already successfully submitted.
jMessages :: Lens' Job (Maybe JobMessages)
jMessages = lens _jMessages (\s a -> s {_jMessages = a})

-- | Error message of Job
jErrorMessage :: Lens' Job (Maybe Text)
jErrorMessage = lens _jErrorMessage (\s a -> s {_jErrorMessage = a})

-- | Describes whether the current job is running with accelerated transcoding. For jobs that have Acceleration (AccelerationMode) set to DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that have Acceleration (AccelerationMode) set to ENABLED or PREFERRED, AccelerationStatus is one of the other states. AccelerationStatus is IN_PROGRESS initially, while the service determines whether the input files and job settings are compatible with accelerated transcoding. If they are, AcclerationStatus is ACCELERATED. If your input files and job settings aren't compatible with accelerated transcoding, the service either fails your job or runs it without accelerated transcoding, depending on how you set Acceleration (AccelerationMode). When the service runs your job without accelerated transcoding, AccelerationStatus is NOT_ACCELERATED.
jAccelerationStatus :: Lens' Job (Maybe AccelerationStatus)
jAccelerationStatus = lens _jAccelerationStatus (\s a -> s {_jAccelerationStatus = a})

-- | The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
jRole :: Lens' Job Text
jRole = lens _jRole (\s a -> s {_jRole = a})

-- | JobSettings contains all the transcode settings for a job.
jSettings :: Lens' Job JobSettings
jSettings = lens _jSettings (\s a -> s {_jSettings = a})

instance FromJSON Job where
  parseJSON =
    withObject
      "Job"
      ( \x ->
          Job'
            <$> (x .:? "status")
            <*> (x .:? "jobTemplate")
            <*> (x .:? "accelerationSettings")
            <*> (x .:? "priority")
            <*> (x .:? "statusUpdateInterval")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "hopDestinations" .!= mempty)
            <*> (x .:? "retryCount")
            <*> (x .:? "simulateReservedQueue")
            <*> (x .:? "currentPhase")
            <*> (x .:? "queue")
            <*> (x .:? "userMetadata" .!= mempty)
            <*> (x .:? "billingTagsSource")
            <*> (x .:? "outputGroupDetails" .!= mempty)
            <*> (x .:? "errorCode")
            <*> (x .:? "queueTransitions" .!= mempty)
            <*> (x .:? "id")
            <*> (x .:? "jobPercentComplete")
            <*> (x .:? "timing")
            <*> (x .:? "messages")
            <*> (x .:? "errorMessage")
            <*> (x .:? "accelerationStatus")
            <*> (x .: "role")
            <*> (x .: "settings")
      )

instance Hashable Job

instance NFData Job
