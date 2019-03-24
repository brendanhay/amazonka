{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.CreateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new transcoding job. For information about jobs and job settings, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
module Network.AWS.MediaConvert.CreateJob
    (
    -- * Creating a Request
      createJob
    , CreateJob
    -- * Request Lenses
    , cjJobTemplate
    , cjAccelerationSettings
    , cjQueue
    , cjUserMetadata
    , cjBillingTagsSource
    , cjStatusUpdateIntervalInSecs
    , cjClientRequestToken
    , cjRole
    , cjSettings

    -- * Destructuring the Response
    , createJobResponse
    , CreateJobResponse
    -- * Response Lenses
    , cjrsJob
    , cjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createJob' smart constructor.
data CreateJob = CreateJob'
  { _cjJobTemplate                :: !(Maybe Text)
  , _cjAccelerationSettings       :: !(Maybe AccelerationSettings)
  , _cjQueue                      :: !(Maybe Text)
  , _cjUserMetadata               :: !(Maybe (Map Text Text))
  , _cjBillingTagsSource          :: !(Maybe BillingTagsSource)
  , _cjStatusUpdateIntervalInSecs :: !(Maybe Nat)
  , _cjClientRequestToken         :: !(Maybe Text)
  , _cjRole                       :: !Text
  , _cjSettings                   :: !JobSettings
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjJobTemplate' - When you create a job, you can either specify a job template or specify the transcoding settings individually
--
-- * 'cjAccelerationSettings' - This is a beta feature. If you are interested in using this feature, please contact AWS customer support.
--
-- * 'cjQueue' - Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html.
--
-- * 'cjUserMetadata' - User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
--
-- * 'cjBillingTagsSource' - Undocumented member.
--
-- * 'cjStatusUpdateIntervalInSecs' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- * 'cjClientRequestToken' - Idempotency token for CreateJob operation.
--
-- * 'cjRole' - Required. The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html.
--
-- * 'cjSettings' - Undocumented member.
createJob
    :: Text -- ^ 'cjRole'
    -> JobSettings -- ^ 'cjSettings'
    -> CreateJob
createJob pRole_ pSettings_ =
  CreateJob'
    { _cjJobTemplate = Nothing
    , _cjAccelerationSettings = Nothing
    , _cjQueue = Nothing
    , _cjUserMetadata = Nothing
    , _cjBillingTagsSource = Nothing
    , _cjStatusUpdateIntervalInSecs = Nothing
    , _cjClientRequestToken = Nothing
    , _cjRole = pRole_
    , _cjSettings = pSettings_
    }


-- | When you create a job, you can either specify a job template or specify the transcoding settings individually
cjJobTemplate :: Lens' CreateJob (Maybe Text)
cjJobTemplate = lens _cjJobTemplate (\ s a -> s{_cjJobTemplate = a})

-- | This is a beta feature. If you are interested in using this feature, please contact AWS customer support.
cjAccelerationSettings :: Lens' CreateJob (Maybe AccelerationSettings)
cjAccelerationSettings = lens _cjAccelerationSettings (\ s a -> s{_cjAccelerationSettings = a})

-- | Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html.
cjQueue :: Lens' CreateJob (Maybe Text)
cjQueue = lens _cjQueue (\ s a -> s{_cjQueue = a})

-- | User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
cjUserMetadata :: Lens' CreateJob (HashMap Text Text)
cjUserMetadata = lens _cjUserMetadata (\ s a -> s{_cjUserMetadata = a}) . _Default . _Map

-- | Undocumented member.
cjBillingTagsSource :: Lens' CreateJob (Maybe BillingTagsSource)
cjBillingTagsSource = lens _cjBillingTagsSource (\ s a -> s{_cjBillingTagsSource = a})

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
cjStatusUpdateIntervalInSecs :: Lens' CreateJob (Maybe Natural)
cjStatusUpdateIntervalInSecs = lens _cjStatusUpdateIntervalInSecs (\ s a -> s{_cjStatusUpdateIntervalInSecs = a}) . mapping _Nat

-- | Idempotency token for CreateJob operation.
cjClientRequestToken :: Lens' CreateJob (Maybe Text)
cjClientRequestToken = lens _cjClientRequestToken (\ s a -> s{_cjClientRequestToken = a})

-- | Required. The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html.
cjRole :: Lens' CreateJob Text
cjRole = lens _cjRole (\ s a -> s{_cjRole = a})

-- | Undocumented member.
cjSettings :: Lens' CreateJob JobSettings
cjSettings = lens _cjSettings (\ s a -> s{_cjSettings = a})

instance AWSRequest CreateJob where
        type Rs CreateJob = CreateJobResponse
        request = postJSON mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 CreateJobResponse' <$>
                   (x .?> "job") <*> (pure (fromEnum s)))

instance Hashable CreateJob where

instance NFData CreateJob where

instance ToHeaders CreateJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateJob where
        toJSON CreateJob'{..}
          = object
              (catMaybes
                 [("jobTemplate" .=) <$> _cjJobTemplate,
                  ("accelerationSettings" .=) <$>
                    _cjAccelerationSettings,
                  ("queue" .=) <$> _cjQueue,
                  ("userMetadata" .=) <$> _cjUserMetadata,
                  ("billingTagsSource" .=) <$> _cjBillingTagsSource,
                  ("statusUpdateIntervalInSecs" .=) <$>
                    _cjStatusUpdateIntervalInSecs,
                  ("clientRequestToken" .=) <$> _cjClientRequestToken,
                  Just ("role" .= _cjRole),
                  Just ("settings" .= _cjSettings)])

instance ToPath CreateJob where
        toPath = const "/2017-08-29/jobs"

instance ToQuery CreateJob where
        toQuery = const mempty

-- | /See:/ 'createJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { _cjrsJob            :: !(Maybe Job)
  , _cjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjrsJob' - Undocumented member.
--
-- * 'cjrsResponseStatus' - -- | The response status code.
createJobResponse
    :: Int -- ^ 'cjrsResponseStatus'
    -> CreateJobResponse
createJobResponse pResponseStatus_ =
  CreateJobResponse'
    {_cjrsJob = Nothing, _cjrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cjrsJob :: Lens' CreateJobResponse (Maybe Job)
cjrsJob = lens _cjrsJob (\ s a -> s{_cjrsJob = a})

-- | -- | The response status code.
cjrsResponseStatus :: Lens' CreateJobResponse Int
cjrsResponseStatus = lens _cjrsResponseStatus (\ s a -> s{_cjrsResponseStatus = a})

instance NFData CreateJobResponse where
