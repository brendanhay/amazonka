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
-- Module      : Network.AWS.MediaConvert.UpdateJobTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing job templates.
module Network.AWS.MediaConvert.UpdateJobTemplate
  ( -- * Creating a Request
    updateJobTemplate,
    UpdateJobTemplate,

    -- * Request Lenses
    ujtAccelerationSettings,
    ujtPriority,
    ujtStatusUpdateInterval,
    ujtSettings,
    ujtCategory,
    ujtHopDestinations,
    ujtQueue,
    ujtDescription,
    ujtName,

    -- * Destructuring the Response
    updateJobTemplateResponse,
    UpdateJobTemplateResponse,

    -- * Response Lenses
    ujtrsJobTemplate,
    ujtrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateJobTemplate' smart constructor.
data UpdateJobTemplate = UpdateJobTemplate'
  { _ujtAccelerationSettings ::
      !(Maybe AccelerationSettings),
    _ujtPriority :: !(Maybe Int),
    _ujtStatusUpdateInterval ::
      !(Maybe StatusUpdateInterval),
    _ujtSettings :: !(Maybe JobTemplateSettings),
    _ujtCategory :: !(Maybe Text),
    _ujtHopDestinations :: !(Maybe [HopDestination]),
    _ujtQueue :: !(Maybe Text),
    _ujtDescription :: !(Maybe Text),
    _ujtName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujtAccelerationSettings' - Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
--
-- * 'ujtPriority' - Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
--
-- * 'ujtStatusUpdateInterval' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- * 'ujtSettings' - JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- * 'ujtCategory' - The new category for the job template, if you are changing it.
--
-- * 'ujtHopDestinations' - Optional list of hop destinations.
--
-- * 'ujtQueue' - The new queue for the job template, if you are changing it.
--
-- * 'ujtDescription' - The new description for the job template, if you are changing it.
--
-- * 'ujtName' - The name of the job template you are modifying
updateJobTemplate ::
  -- | 'ujtName'
  Text ->
  UpdateJobTemplate
updateJobTemplate pName_ =
  UpdateJobTemplate'
    { _ujtAccelerationSettings = Nothing,
      _ujtPriority = Nothing,
      _ujtStatusUpdateInterval = Nothing,
      _ujtSettings = Nothing,
      _ujtCategory = Nothing,
      _ujtHopDestinations = Nothing,
      _ujtQueue = Nothing,
      _ujtDescription = Nothing,
      _ujtName = pName_
    }

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content. Outputs that use this feature incur pro-tier pricing. For information about feature limitations, see the AWS Elemental MediaConvert User Guide.
ujtAccelerationSettings :: Lens' UpdateJobTemplate (Maybe AccelerationSettings)
ujtAccelerationSettings = lens _ujtAccelerationSettings (\s a -> s {_ujtAccelerationSettings = a})

-- | Specify the relative priority for this job. In any given queue, the service begins processing the job with the highest value first. When more than one job has the same priority, the service begins processing the job that you submitted first. If you don't specify a priority, the service uses the default value 0.
ujtPriority :: Lens' UpdateJobTemplate (Maybe Int)
ujtPriority = lens _ujtPriority (\s a -> s {_ujtPriority = a})

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
ujtStatusUpdateInterval :: Lens' UpdateJobTemplate (Maybe StatusUpdateInterval)
ujtStatusUpdateInterval = lens _ujtStatusUpdateInterval (\s a -> s {_ujtStatusUpdateInterval = a})

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
ujtSettings :: Lens' UpdateJobTemplate (Maybe JobTemplateSettings)
ujtSettings = lens _ujtSettings (\s a -> s {_ujtSettings = a})

-- | The new category for the job template, if you are changing it.
ujtCategory :: Lens' UpdateJobTemplate (Maybe Text)
ujtCategory = lens _ujtCategory (\s a -> s {_ujtCategory = a})

-- | Optional list of hop destinations.
ujtHopDestinations :: Lens' UpdateJobTemplate [HopDestination]
ujtHopDestinations = lens _ujtHopDestinations (\s a -> s {_ujtHopDestinations = a}) . _Default . _Coerce

-- | The new queue for the job template, if you are changing it.
ujtQueue :: Lens' UpdateJobTemplate (Maybe Text)
ujtQueue = lens _ujtQueue (\s a -> s {_ujtQueue = a})

-- | The new description for the job template, if you are changing it.
ujtDescription :: Lens' UpdateJobTemplate (Maybe Text)
ujtDescription = lens _ujtDescription (\s a -> s {_ujtDescription = a})

-- | The name of the job template you are modifying
ujtName :: Lens' UpdateJobTemplate Text
ujtName = lens _ujtName (\s a -> s {_ujtName = a})

instance AWSRequest UpdateJobTemplate where
  type Rs UpdateJobTemplate = UpdateJobTemplateResponse
  request = putJSON mediaConvert
  response =
    receiveJSON
      ( \s h x ->
          UpdateJobTemplateResponse'
            <$> (x .?> "jobTemplate") <*> (pure (fromEnum s))
      )

instance Hashable UpdateJobTemplate

instance NFData UpdateJobTemplate

instance ToHeaders UpdateJobTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateJobTemplate where
  toJSON UpdateJobTemplate' {..} =
    object
      ( catMaybes
          [ ("accelerationSettings" .=) <$> _ujtAccelerationSettings,
            ("priority" .=) <$> _ujtPriority,
            ("statusUpdateInterval" .=) <$> _ujtStatusUpdateInterval,
            ("settings" .=) <$> _ujtSettings,
            ("category" .=) <$> _ujtCategory,
            ("hopDestinations" .=) <$> _ujtHopDestinations,
            ("queue" .=) <$> _ujtQueue,
            ("description" .=) <$> _ujtDescription
          ]
      )

instance ToPath UpdateJobTemplate where
  toPath UpdateJobTemplate' {..} =
    mconcat ["/2017-08-29/jobTemplates/", toBS _ujtName]

instance ToQuery UpdateJobTemplate where
  toQuery = const mempty

-- | /See:/ 'updateJobTemplateResponse' smart constructor.
data UpdateJobTemplateResponse = UpdateJobTemplateResponse'
  { _ujtrsJobTemplate ::
      !(Maybe JobTemplate),
    _ujtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJobTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujtrsJobTemplate' - A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- * 'ujtrsResponseStatus' - -- | The response status code.
updateJobTemplateResponse ::
  -- | 'ujtrsResponseStatus'
  Int ->
  UpdateJobTemplateResponse
updateJobTemplateResponse pResponseStatus_ =
  UpdateJobTemplateResponse'
    { _ujtrsJobTemplate = Nothing,
      _ujtrsResponseStatus = pResponseStatus_
    }

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
ujtrsJobTemplate :: Lens' UpdateJobTemplateResponse (Maybe JobTemplate)
ujtrsJobTemplate = lens _ujtrsJobTemplate (\s a -> s {_ujtrsJobTemplate = a})

-- | -- | The response status code.
ujtrsResponseStatus :: Lens' UpdateJobTemplateResponse Int
ujtrsResponseStatus = lens _ujtrsResponseStatus (\s a -> s {_ujtrsResponseStatus = a})

instance NFData UpdateJobTemplateResponse
