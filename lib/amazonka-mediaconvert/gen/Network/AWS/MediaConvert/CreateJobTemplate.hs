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
-- Module      : Network.AWS.MediaConvert.CreateJobTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new job template. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
module Network.AWS.MediaConvert.CreateJobTemplate
  ( -- * Creating a Request
    createJobTemplate,
    CreateJobTemplate,

    -- * Request Lenses
    cjtAccelerationSettings,
    cjtPriority,
    cjtStatusUpdateInterval,
    cjtCategory,
    cjtHopDestinations,
    cjtQueue,
    cjtDescription,
    cjtTags,
    cjtSettings,
    cjtName,

    -- * Destructuring the Response
    createJobTemplateResponse,
    CreateJobTemplateResponse,

    -- * Response Lenses
    cjtrsJobTemplate,
    cjtrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createJobTemplate' smart constructor.
data CreateJobTemplate = CreateJobTemplate'
  { _cjtAccelerationSettings ::
      !(Maybe AccelerationSettings),
    _cjtPriority :: !(Maybe Int),
    _cjtStatusUpdateInterval ::
      !(Maybe StatusUpdateInterval),
    _cjtCategory :: !(Maybe Text),
    _cjtHopDestinations :: !(Maybe [HopDestination]),
    _cjtQueue :: !(Maybe Text),
    _cjtDescription :: !(Maybe Text),
    _cjtTags :: !(Maybe (Map Text (Text))),
    _cjtSettings :: !JobTemplateSettings,
    _cjtName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateJobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjtAccelerationSettings' - Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
--
-- * 'cjtPriority' - Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
--
-- * 'cjtStatusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- * 'cjtCategory' - Optional. A category for the job template you are creating
--
-- * 'cjtHopDestinations' - Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
--
-- * 'cjtQueue' - Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- * 'cjtDescription' - Optional. A description of the job template you are creating.
--
-- * 'cjtTags' - The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
--
-- * 'cjtSettings' - JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- * 'cjtName' - The name of the job template you are creating.
createJobTemplate ::
  -- | 'cjtSettings'
  JobTemplateSettings ->
  -- | 'cjtName'
  Text ->
  CreateJobTemplate
createJobTemplate pSettings_ pName_ =
  CreateJobTemplate'
    { _cjtAccelerationSettings = Nothing,
      _cjtPriority = Nothing,
      _cjtStatusUpdateInterval = Nothing,
      _cjtCategory = Nothing,
      _cjtHopDestinations = Nothing,
      _cjtQueue = Nothing,
      _cjtDescription = Nothing,
      _cjtTags = Nothing,
      _cjtSettings = pSettings_,
      _cjtName = pName_
    }

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
cjtAccelerationSettings :: Lens' CreateJobTemplate (Maybe AccelerationSettings)
cjtAccelerationSettings = lens _cjtAccelerationSettings (\s a -> s {_cjtAccelerationSettings = a})

-- | Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
cjtPriority :: Lens' CreateJobTemplate (Maybe Int)
cjtPriority = lens _cjtPriority (\s a -> s {_cjtPriority = a})

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
cjtStatusUpdateInterval :: Lens' CreateJobTemplate (Maybe StatusUpdateInterval)
cjtStatusUpdateInterval = lens _cjtStatusUpdateInterval (\s a -> s {_cjtStatusUpdateInterval = a})

-- | Optional. A category for the job template you are creating
cjtCategory :: Lens' CreateJobTemplate (Maybe Text)
cjtCategory = lens _cjtCategory (\s a -> s {_cjtCategory = a})

-- | Optional. Use queue hopping to avoid overly long waits in the backlog of the queue that you submit your job to. Specify an alternate queue and the maximum time that your job will wait in the initial queue before hopping. For more information about this feature, see the AWS Elemental MediaConvert User Guide.
cjtHopDestinations :: Lens' CreateJobTemplate [HopDestination]
cjtHopDestinations = lens _cjtHopDestinations (\s a -> s {_cjtHopDestinations = a}) . _Default . _Coerce

-- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
cjtQueue :: Lens' CreateJobTemplate (Maybe Text)
cjtQueue = lens _cjtQueue (\s a -> s {_cjtQueue = a})

-- | Optional. A description of the job template you are creating.
cjtDescription :: Lens' CreateJobTemplate (Maybe Text)
cjtDescription = lens _cjtDescription (\s a -> s {_cjtDescription = a})

-- | The tags that you want to add to the resource. You can tag resources with a key-value pair or with only a key.
cjtTags :: Lens' CreateJobTemplate (HashMap Text (Text))
cjtTags = lens _cjtTags (\s a -> s {_cjtTags = a}) . _Default . _Map

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
cjtSettings :: Lens' CreateJobTemplate JobTemplateSettings
cjtSettings = lens _cjtSettings (\s a -> s {_cjtSettings = a})

-- | The name of the job template you are creating.
cjtName :: Lens' CreateJobTemplate Text
cjtName = lens _cjtName (\s a -> s {_cjtName = a})

instance AWSRequest CreateJobTemplate where
  type Rs CreateJobTemplate = CreateJobTemplateResponse
  request = postJSON mediaConvert
  response =
    receiveJSON
      ( \s h x ->
          CreateJobTemplateResponse'
            <$> (x .?> "jobTemplate") <*> (pure (fromEnum s))
      )

instance Hashable CreateJobTemplate

instance NFData CreateJobTemplate

instance ToHeaders CreateJobTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateJobTemplate where
  toJSON CreateJobTemplate' {..} =
    object
      ( catMaybes
          [ ("accelerationSettings" .=) <$> _cjtAccelerationSettings,
            ("priority" .=) <$> _cjtPriority,
            ("statusUpdateInterval" .=) <$> _cjtStatusUpdateInterval,
            ("category" .=) <$> _cjtCategory,
            ("hopDestinations" .=) <$> _cjtHopDestinations,
            ("queue" .=) <$> _cjtQueue,
            ("description" .=) <$> _cjtDescription,
            ("tags" .=) <$> _cjtTags,
            Just ("settings" .= _cjtSettings),
            Just ("name" .= _cjtName)
          ]
      )

instance ToPath CreateJobTemplate where
  toPath = const "/2017-08-29/jobTemplates"

instance ToQuery CreateJobTemplate where
  toQuery = const mempty

-- | /See:/ 'createJobTemplateResponse' smart constructor.
data CreateJobTemplateResponse = CreateJobTemplateResponse'
  { _cjtrsJobTemplate ::
      !(Maybe JobTemplate),
    _cjtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateJobTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjtrsJobTemplate' - A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- * 'cjtrsResponseStatus' - -- | The response status code.
createJobTemplateResponse ::
  -- | 'cjtrsResponseStatus'
  Int ->
  CreateJobTemplateResponse
createJobTemplateResponse pResponseStatus_ =
  CreateJobTemplateResponse'
    { _cjtrsJobTemplate = Nothing,
      _cjtrsResponseStatus = pResponseStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
cjtrsJobTemplate :: Lens' CreateJobTemplateResponse (Maybe JobTemplate)
cjtrsJobTemplate = lens _cjtrsJobTemplate (\s a -> s {_cjtrsJobTemplate = a})

-- | -- | The response status code.
cjtrsResponseStatus :: Lens' CreateJobTemplateResponse Int
cjtrsResponseStatus = lens _cjtrsResponseStatus (\s a -> s {_cjtrsResponseStatus = a})

instance NFData CreateJobTemplateResponse
