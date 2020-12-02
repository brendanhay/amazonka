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
-- Module      : Network.AWS.Rekognition.StartCelebrityRecognition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous recognition of celebrities in a stored video.
--
--
-- Rekognition Video can detect celebrities in a video must be stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartCelebrityRecognition@ returns a job identifier (@JobId@ ) which you use to get the results of the analysis. When celebrity recognition analysis is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ . To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call and pass the job identifier (@JobId@ ) from the initial call to @StartCelebrityRecognition@ . For more information, see 'celebrities' .
--
module Network.AWS.Rekognition.StartCelebrityRecognition
    (
    -- * Creating a Request
      startCelebrityRecognition
    , StartCelebrityRecognition
    -- * Request Lenses
    , scrJobTag
    , scrNotificationChannel
    , scrClientRequestToken
    , scrVideo

    -- * Destructuring the Response
    , startCelebrityRecognitionResponse
    , StartCelebrityRecognitionResponse
    -- * Response Lenses
    , scrrsJobId
    , scrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startCelebrityRecognition' smart constructor.
data StartCelebrityRecognition = StartCelebrityRecognition'
  { _scrJobTag              :: !(Maybe Text)
  , _scrNotificationChannel :: !(Maybe NotificationChannel)
  , _scrClientRequestToken  :: !(Maybe Text)
  , _scrVideo               :: !Video
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartCelebrityRecognition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrJobTag' - Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
--
-- * 'scrNotificationChannel' - The Amazon SNS topic ARN that you want Rekognition Video to publish the completion status of the celebrity recognition analysis to.
--
-- * 'scrClientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartCelebrityRecognition@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- * 'scrVideo' - The video in which you want to recognize celebrities. The video must be stored in an Amazon S3 bucket.
startCelebrityRecognition
    :: Video -- ^ 'scrVideo'
    -> StartCelebrityRecognition
startCelebrityRecognition pVideo_ =
  StartCelebrityRecognition'
    { _scrJobTag = Nothing
    , _scrNotificationChannel = Nothing
    , _scrClientRequestToken = Nothing
    , _scrVideo = pVideo_
    }


-- | Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
scrJobTag :: Lens' StartCelebrityRecognition (Maybe Text)
scrJobTag = lens _scrJobTag (\ s a -> s{_scrJobTag = a})

-- | The Amazon SNS topic ARN that you want Rekognition Video to publish the completion status of the celebrity recognition analysis to.
scrNotificationChannel :: Lens' StartCelebrityRecognition (Maybe NotificationChannel)
scrNotificationChannel = lens _scrNotificationChannel (\ s a -> s{_scrNotificationChannel = a})

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartCelebrityRecognition@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
scrClientRequestToken :: Lens' StartCelebrityRecognition (Maybe Text)
scrClientRequestToken = lens _scrClientRequestToken (\ s a -> s{_scrClientRequestToken = a})

-- | The video in which you want to recognize celebrities. The video must be stored in an Amazon S3 bucket.
scrVideo :: Lens' StartCelebrityRecognition Video
scrVideo = lens _scrVideo (\ s a -> s{_scrVideo = a})

instance AWSRequest StartCelebrityRecognition where
        type Rs StartCelebrityRecognition =
             StartCelebrityRecognitionResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 StartCelebrityRecognitionResponse' <$>
                   (x .?> "JobId") <*> (pure (fromEnum s)))

instance Hashable StartCelebrityRecognition where

instance NFData StartCelebrityRecognition where

instance ToHeaders StartCelebrityRecognition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.StartCelebrityRecognition" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartCelebrityRecognition where
        toJSON StartCelebrityRecognition'{..}
          = object
              (catMaybes
                 [("JobTag" .=) <$> _scrJobTag,
                  ("NotificationChannel" .=) <$>
                    _scrNotificationChannel,
                  ("ClientRequestToken" .=) <$> _scrClientRequestToken,
                  Just ("Video" .= _scrVideo)])

instance ToPath StartCelebrityRecognition where
        toPath = const "/"

instance ToQuery StartCelebrityRecognition where
        toQuery = const mempty

-- | /See:/ 'startCelebrityRecognitionResponse' smart constructor.
data StartCelebrityRecognitionResponse = StartCelebrityRecognitionResponse'
  { _scrrsJobId          :: !(Maybe Text)
  , _scrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartCelebrityRecognitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrrsJobId' - The identifier for the celebrity recognition analysis job. Use @JobId@ to identify the job in a subsequent call to @GetCelebrityRecognition@ .
--
-- * 'scrrsResponseStatus' - -- | The response status code.
startCelebrityRecognitionResponse
    :: Int -- ^ 'scrrsResponseStatus'
    -> StartCelebrityRecognitionResponse
startCelebrityRecognitionResponse pResponseStatus_ =
  StartCelebrityRecognitionResponse'
    {_scrrsJobId = Nothing, _scrrsResponseStatus = pResponseStatus_}


-- | The identifier for the celebrity recognition analysis job. Use @JobId@ to identify the job in a subsequent call to @GetCelebrityRecognition@ .
scrrsJobId :: Lens' StartCelebrityRecognitionResponse (Maybe Text)
scrrsJobId = lens _scrrsJobId (\ s a -> s{_scrrsJobId = a})

-- | -- | The response status code.
scrrsResponseStatus :: Lens' StartCelebrityRecognitionResponse Int
scrrsResponseStatus = lens _scrrsResponseStatus (\ s a -> s{_scrrsResponseStatus = a})

instance NFData StartCelebrityRecognitionResponse
         where
