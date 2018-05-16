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
-- Module      : Network.AWS.Rekognition.StartContentModeration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of explicit or suggestive adult content in a stored video.
--
--
-- Rekognition Video can moderate content in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartContentModeration@ returns a job identifier (@JobId@ ) which you use to get the results of the analysis. When content moderation analysis is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
--
-- To get the results of the content moderation analysis, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call and pass the job identifier (@JobId@ ) from the initial call to @StartContentModeration@ . For more information, see 'moderation' .
--
module Network.AWS.Rekognition.StartContentModeration
    (
    -- * Creating a Request
      startContentModeration
    , StartContentModeration
    -- * Request Lenses
    , scmJobTag
    , scmNotificationChannel
    , scmClientRequestToken
    , scmMinConfidence
    , scmVideo

    -- * Destructuring the Response
    , startContentModerationResponse
    , StartContentModerationResponse
    -- * Response Lenses
    , scmrsJobId
    , scmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startContentModeration' smart constructor.
data StartContentModeration = StartContentModeration'
  { _scmJobTag              :: !(Maybe Text)
  , _scmNotificationChannel :: !(Maybe NotificationChannel)
  , _scmClientRequestToken  :: !(Maybe Text)
  , _scmMinConfidence       :: !(Maybe Double)
  , _scmVideo               :: !Video
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartContentModeration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scmJobTag' - Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
--
-- * 'scmNotificationChannel' - The Amazon SNS topic ARN that you want Rekognition Video to publish the completion status of the content moderation analysis to.
--
-- * 'scmClientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartContentModeration@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- * 'scmMinConfidence' - Specifies the minimum confidence that Amazon Rekognition must have in order to return a moderated content label. Confidence represents how certain Amazon Rekognition is that the moderated content is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition doesn't return any moderated content labels with a confidence level lower than this specified value.
--
-- * 'scmVideo' - The video in which you want to moderate content. The video must be stored in an Amazon S3 bucket.
startContentModeration
    :: Video -- ^ 'scmVideo'
    -> StartContentModeration
startContentModeration pVideo_ =
  StartContentModeration'
    { _scmJobTag = Nothing
    , _scmNotificationChannel = Nothing
    , _scmClientRequestToken = Nothing
    , _scmMinConfidence = Nothing
    , _scmVideo = pVideo_
    }


-- | Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
scmJobTag :: Lens' StartContentModeration (Maybe Text)
scmJobTag = lens _scmJobTag (\ s a -> s{_scmJobTag = a})

-- | The Amazon SNS topic ARN that you want Rekognition Video to publish the completion status of the content moderation analysis to.
scmNotificationChannel :: Lens' StartContentModeration (Maybe NotificationChannel)
scmNotificationChannel = lens _scmNotificationChannel (\ s a -> s{_scmNotificationChannel = a})

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartContentModeration@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
scmClientRequestToken :: Lens' StartContentModeration (Maybe Text)
scmClientRequestToken = lens _scmClientRequestToken (\ s a -> s{_scmClientRequestToken = a})

-- | Specifies the minimum confidence that Amazon Rekognition must have in order to return a moderated content label. Confidence represents how certain Amazon Rekognition is that the moderated content is correctly identified. 0 is the lowest confidence. 100 is the highest confidence. Amazon Rekognition doesn't return any moderated content labels with a confidence level lower than this specified value.
scmMinConfidence :: Lens' StartContentModeration (Maybe Double)
scmMinConfidence = lens _scmMinConfidence (\ s a -> s{_scmMinConfidence = a})

-- | The video in which you want to moderate content. The video must be stored in an Amazon S3 bucket.
scmVideo :: Lens' StartContentModeration Video
scmVideo = lens _scmVideo (\ s a -> s{_scmVideo = a})

instance AWSRequest StartContentModeration where
        type Rs StartContentModeration =
             StartContentModerationResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 StartContentModerationResponse' <$>
                   (x .?> "JobId") <*> (pure (fromEnum s)))

instance Hashable StartContentModeration where

instance NFData StartContentModeration where

instance ToHeaders StartContentModeration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.StartContentModeration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartContentModeration where
        toJSON StartContentModeration'{..}
          = object
              (catMaybes
                 [("JobTag" .=) <$> _scmJobTag,
                  ("NotificationChannel" .=) <$>
                    _scmNotificationChannel,
                  ("ClientRequestToken" .=) <$> _scmClientRequestToken,
                  ("MinConfidence" .=) <$> _scmMinConfidence,
                  Just ("Video" .= _scmVideo)])

instance ToPath StartContentModeration where
        toPath = const "/"

instance ToQuery StartContentModeration where
        toQuery = const mempty

-- | /See:/ 'startContentModerationResponse' smart constructor.
data StartContentModerationResponse = StartContentModerationResponse'
  { _scmrsJobId          :: !(Maybe Text)
  , _scmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartContentModerationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scmrsJobId' - The identifier for the content moderation analysis job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
--
-- * 'scmrsResponseStatus' - -- | The response status code.
startContentModerationResponse
    :: Int -- ^ 'scmrsResponseStatus'
    -> StartContentModerationResponse
startContentModerationResponse pResponseStatus_ =
  StartContentModerationResponse'
    {_scmrsJobId = Nothing, _scmrsResponseStatus = pResponseStatus_}


-- | The identifier for the content moderation analysis job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
scmrsJobId :: Lens' StartContentModerationResponse (Maybe Text)
scmrsJobId = lens _scmrsJobId (\ s a -> s{_scmrsJobId = a})

-- | -- | The response status code.
scmrsResponseStatus :: Lens' StartContentModerationResponse Int
scmrsResponseStatus = lens _scmrsResponseStatus (\ s a -> s{_scmrsResponseStatus = a})

instance NFData StartContentModerationResponse where
