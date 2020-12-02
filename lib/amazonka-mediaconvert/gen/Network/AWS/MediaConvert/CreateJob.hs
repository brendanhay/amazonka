{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.CreateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new transcoding job. For information about jobs and job settings, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
module Network.AWS.MediaConvert.CreateJob
  ( -- * Creating a Request
    createJob,
    CreateJob,

    -- * Request Lenses
    cjJobTemplate,
    cjAccelerationSettings,
    cjPriority,
    cjStatusUpdateInterval,
    cjHopDestinations,
    cjSimulateReservedQueue,
    cjQueue,
    cjUserMetadata,
    cjBillingTagsSource,
    cjClientRequestToken,
    cjTags,
    cjRole,
    cjSettings,

    -- * Destructuring the Response
    createJobResponse,
    CreateJobResponse,

    -- * Response Lenses
    cjrsJob,
    cjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createJob' smart constructor.
data CreateJob = CreateJob'
  { _cjJobTemplate :: !(Maybe Text),
    _cjAccelerationSettings :: !(Maybe AccelerationSettings),
    _cjPriority :: !(Maybe Int),
    _cjStatusUpdateInterval :: !(Maybe StatusUpdateInterval),
    _cjHopDestinations :: !(Maybe [HopDestination]),
    _cjSimulateReservedQueue :: !(Maybe SimulateReservedQueue),
    _cjQueue :: !(Maybe Text),
    _cjUserMetadata :: !(Maybe (Map Text (Text))),
    _cjBillingTagsSource :: !(Maybe BillingTagsSource),
    _cjClientRequestToken :: !(Maybe Text),
    _cjTags :: !(Maybe (Map Text (Text))),
    _cjRole :: !Text,
    _cjSettings :: !JobSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjJobTemplate' - Optional. When you create a job, you can either specify a job template or specify the transcoding settings individually.
--
-- * 'cjAccelerationSettings' - Optional. Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
--
-- * 'cjPriority' - Optional. Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
--
-- * 'cjStatusUpdateInterval' - Optional. Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- * 'cjHopDestinations' - Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
--
-- * 'cjSimulateReservedQueue' - Optional. Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
--
-- * 'cjQueue' - Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html.
--
-- * 'cjUserMetadata' - Optional. User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.  Use only for existing integrations or workflows that rely on job metadata tags. Otherwise, we recommend that you use standard AWS tags.
--
-- * 'cjBillingTagsSource' - Optional. Choose a tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up. Any transcoding outputs that don't have an associated tag will appear in your billing report unsorted. If you don't choose a valid value for this field, your job outputs will appear on the billing report unsorted.
--
-- * 'cjClientRequestToken' - Optional. Idempotency token for CreateJob operation.
--
-- * 'cjTags' - Optional. The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.  Use standard AWS tags on your job for automatic integration with AWS services and for custom integrations and workflows.
--
-- * 'cjRole' - Required. The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html.
--
-- * 'cjSettings' - JobSettings contains all the transcode settings for a job.
createJob ::
  -- | 'cjRole'
  Text ->
  -- | 'cjSettings'
  JobSettings ->
  CreateJob
createJob pRole_ pSettings_ =
  CreateJob'
    { _cjJobTemplate = Nothing,
      _cjAccelerationSettings = Nothing,
      _cjPriority = Nothing,
      _cjStatusUpdateInterval = Nothing,
      _cjHopDestinations = Nothing,
      _cjSimulateReservedQueue = Nothing,
      _cjQueue = Nothing,
      _cjUserMetadata = Nothing,
      _cjBillingTagsSource = Nothing,
      _cjClientRequestToken = Nothing,
      _cjTags = Nothing,
      _cjRole = pRole_,
      _cjSettings = pSettings_
    }

-- | Optional. When you create a job, you can either specify a job template or specify the transcoding settings individually.
cjJobTemplate :: Lens' CreateJob (Maybe Text)
cjJobTemplate = lens _cjJobTemplate (\s a -> s {_cjJobTemplate = a})

-- | Optional. Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
cjAccelerationSettings :: Lens' CreateJob (Maybe AccelerationSettings)
cjAccelerationSettings = lens _cjAccelerationSettings (\s a -> s {_cjAccelerationSettings = a})

-- | Optional. Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
cjPriority :: Lens' CreateJob (Maybe Int)
cjPriority = lens _cjPriority (\s a -> s {_cjPriority = a})

-- | Optional. Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
cjStatusUpdateInterval :: Lens' CreateJob (Maybe StatusUpdateInterval)
cjStatusUpdateInterval = lens _cjStatusUpdateInterval (\s a -> s {_cjStatusUpdateInterval = a})

-- | Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
cjHopDestinations :: Lens' CreateJob [HopDestination]
cjHopDestinations = lens _cjHopDestinations (\s a -> s {_cjHopDestinations = a}) . _Default . _Coerce

-- | Optional. Enable this setting when you run a test job to estimate how many reserved transcoding slots (RTS) you need. When this is enabled, MediaConvert runs your job from an on-demand queue with similar performance to what you will see with one RTS in a reserved queue. This setting is disabled by default.
cjSimulateReservedQueue :: Lens' CreateJob (Maybe SimulateReservedQueue)
cjSimulateReservedQueue = lens _cjSimulateReservedQueue (\s a -> s {_cjSimulateReservedQueue = a})

-- | Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html.
cjQueue :: Lens' CreateJob (Maybe Text)
cjQueue = lens _cjQueue (\s a -> s {_cjQueue = a})

-- | Optional. User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.  Use only for existing integrations or workflows that rely on job metadata tags. Otherwise, we recommend that you use standard AWS tags.
cjUserMetadata :: Lens' CreateJob (HashMap Text (Text))
cjUserMetadata = lens _cjUserMetadata (\s a -> s {_cjUserMetadata = a}) . _Default . _Map

-- | Optional. Choose a tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up. Any transcoding outputs that don't have an associated tag will appear in your billing report unsorted. If you don't choose a valid value for this field, your job outputs will appear on the billing report unsorted.
cjBillingTagsSource :: Lens' CreateJob (Maybe BillingTagsSource)
cjBillingTagsSource = lens _cjBillingTagsSource (\s a -> s {_cjBillingTagsSource = a})

-- | Optional. Idempotency token for CreateJob operation.
cjClientRequestToken :: Lens' CreateJob (Maybe Text)
cjClientRequestToken = lens _cjClientRequestToken (\s a -> s {_cjClientRequestToken = a})

-- | Optional. The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.  Use standard AWS tags on your job for automatic integration with AWS services and for custom integrations and workflows.
cjTags :: Lens' CreateJob (HashMap Text (Text))
cjTags = lens _cjTags (\s a -> s {_cjTags = a}) . _Default . _Map

-- | Required. The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html.
cjRole :: Lens' CreateJob Text
cjRole = lens _cjRole (\s a -> s {_cjRole = a})

-- | JobSettings contains all the transcode settings for a job.
cjSettings :: Lens' CreateJob JobSettings
cjSettings = lens _cjSettings (\s a -> s {_cjSettings = a})

instance AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request = postJSON mediaConvert
  response =
    receiveJSON
      ( \s h x ->
          CreateJobResponse' <$> (x .?> "job") <*> (pure (fromEnum s))
      )

instance Hashable CreateJob

instance NFData CreateJob

instance ToHeaders CreateJob where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateJob where
  toJSON CreateJob' {..} =
    object
      ( catMaybes
          [ ("jobTemplate" .=) <$> _cjJobTemplate,
            ("accelerationSettings" .=) <$> _cjAccelerationSettings,
            ("priority" .=) <$> _cjPriority,
            ("statusUpdateInterval" .=) <$> _cjStatusUpdateInterval,
            ("hopDestinations" .=) <$> _cjHopDestinations,
            ("simulateReservedQueue" .=) <$> _cjSimulateReservedQueue,
            ("queue" .=) <$> _cjQueue,
            ("userMetadata" .=) <$> _cjUserMetadata,
            ("billingTagsSource" .=) <$> _cjBillingTagsSource,
            ("clientRequestToken" .=) <$> _cjClientRequestToken,
            ("tags" .=) <$> _cjTags,
            Just ("role" .= _cjRole),
            Just ("settings" .= _cjSettings)
          ]
      )

instance ToPath CreateJob where
  toPath = const "/2017-08-29/jobs"

instance ToQuery CreateJob where
  toQuery = const mempty

-- | /See:/ 'createJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { _cjrsJob ::
      !(Maybe Job),
    _cjrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjrsJob' - Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- * 'cjrsResponseStatus' - -- | The response status code.
createJobResponse ::
  -- | 'cjrsResponseStatus'
  Int ->
  CreateJobResponse
createJobResponse pResponseStatus_ =
  CreateJobResponse'
    { _cjrsJob = Nothing,
      _cjrsResponseStatus = pResponseStatus_
    }

-- | Each job converts an input file into an output file or files. For more information, see the User Guide at https://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
cjrsJob :: Lens' CreateJobResponse (Maybe Job)
cjrsJob = lens _cjrsJob (\s a -> s {_cjrsJob = a})

-- | -- | The response status code.
cjrsResponseStatus :: Lens' CreateJobResponse Int
cjrsResponseStatus = lens _cjrsResponseStatus (\s a -> s {_cjrsResponseStatus = a})

instance NFData CreateJobResponse
