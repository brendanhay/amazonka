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
-- Module      : Network.AWS.Rekognition.StartPersonTracking
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous tracking of persons in a stored video.
--
--
-- Rekognition Video can track persons in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartPersonTracking@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When label detection is finished, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
--
-- To get the results of the person detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call and pass the job identifier (@JobId@ ) from the initial call to @StartPersonTracking@ .
--
module Network.AWS.Rekognition.StartPersonTracking
    (
    -- * Creating a Request
      startPersonTracking
    , StartPersonTracking
    -- * Request Lenses
    , sptJobTag
    , sptNotificationChannel
    , sptClientRequestToken
    , sptVideo

    -- * Destructuring the Response
    , startPersonTrackingResponse
    , StartPersonTrackingResponse
    -- * Response Lenses
    , sptrsJobId
    , sptrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startPersonTracking' smart constructor.
data StartPersonTracking = StartPersonTracking'
  { _sptJobTag              :: !(Maybe Text)
  , _sptNotificationChannel :: !(Maybe NotificationChannel)
  , _sptClientRequestToken  :: !(Maybe Text)
  , _sptVideo               :: !Video
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartPersonTracking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sptJobTag' - Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
--
-- * 'sptNotificationChannel' - The Amazon SNS topic ARN you want Rekognition Video to publish the completion status of the people detection operation to.
--
-- * 'sptClientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartPersonTracking@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- * 'sptVideo' - The video in which you want to detect people. The video must be stored in an Amazon S3 bucket.
startPersonTracking
    :: Video -- ^ 'sptVideo'
    -> StartPersonTracking
startPersonTracking pVideo_ =
  StartPersonTracking'
    { _sptJobTag = Nothing
    , _sptNotificationChannel = Nothing
    , _sptClientRequestToken = Nothing
    , _sptVideo = pVideo_
    }


-- | Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
sptJobTag :: Lens' StartPersonTracking (Maybe Text)
sptJobTag = lens _sptJobTag (\ s a -> s{_sptJobTag = a})

-- | The Amazon SNS topic ARN you want Rekognition Video to publish the completion status of the people detection operation to.
sptNotificationChannel :: Lens' StartPersonTracking (Maybe NotificationChannel)
sptNotificationChannel = lens _sptNotificationChannel (\ s a -> s{_sptNotificationChannel = a})

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartPersonTracking@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
sptClientRequestToken :: Lens' StartPersonTracking (Maybe Text)
sptClientRequestToken = lens _sptClientRequestToken (\ s a -> s{_sptClientRequestToken = a})

-- | The video in which you want to detect people. The video must be stored in an Amazon S3 bucket.
sptVideo :: Lens' StartPersonTracking Video
sptVideo = lens _sptVideo (\ s a -> s{_sptVideo = a})

instance AWSRequest StartPersonTracking where
        type Rs StartPersonTracking =
             StartPersonTrackingResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 StartPersonTrackingResponse' <$>
                   (x .?> "JobId") <*> (pure (fromEnum s)))

instance Hashable StartPersonTracking where

instance NFData StartPersonTracking where

instance ToHeaders StartPersonTracking where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.StartPersonTracking" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartPersonTracking where
        toJSON StartPersonTracking'{..}
          = object
              (catMaybes
                 [("JobTag" .=) <$> _sptJobTag,
                  ("NotificationChannel" .=) <$>
                    _sptNotificationChannel,
                  ("ClientRequestToken" .=) <$> _sptClientRequestToken,
                  Just ("Video" .= _sptVideo)])

instance ToPath StartPersonTracking where
        toPath = const "/"

instance ToQuery StartPersonTracking where
        toQuery = const mempty

-- | /See:/ 'startPersonTrackingResponse' smart constructor.
data StartPersonTrackingResponse = StartPersonTrackingResponse'
  { _sptrsJobId          :: !(Maybe Text)
  , _sptrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartPersonTrackingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sptrsJobId' - The identifier for the person detection job. Use @JobId@ to identify the job in a subsequent call to @GetPersonTracking@ .
--
-- * 'sptrsResponseStatus' - -- | The response status code.
startPersonTrackingResponse
    :: Int -- ^ 'sptrsResponseStatus'
    -> StartPersonTrackingResponse
startPersonTrackingResponse pResponseStatus_ =
  StartPersonTrackingResponse'
    {_sptrsJobId = Nothing, _sptrsResponseStatus = pResponseStatus_}


-- | The identifier for the person detection job. Use @JobId@ to identify the job in a subsequent call to @GetPersonTracking@ .
sptrsJobId :: Lens' StartPersonTrackingResponse (Maybe Text)
sptrsJobId = lens _sptrsJobId (\ s a -> s{_sptrsJobId = a})

-- | -- | The response status code.
sptrsResponseStatus :: Lens' StartPersonTrackingResponse Int
sptrsResponseStatus = lens _sptrsResponseStatus (\ s a -> s{_sptrsResponseStatus = a})

instance NFData StartPersonTrackingResponse where
