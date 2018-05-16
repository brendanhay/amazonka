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
-- Module      : Network.AWS.Rekognition.StartLabelDetection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of labels in a stored video.
--
--
-- Rekognition Video can detect labels in a video. Labels are instances of real-world entities. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; concepts like landscape, evening, and nature; and activities like a person getting out of a car or a person skiing.
--
-- The video must be stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartLabelDetection@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When label detection is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
--
-- To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call and pass the job identifier (@JobId@ ) from the initial call to @StartLabelDetection@ .
--
--
--
module Network.AWS.Rekognition.StartLabelDetection
    (
    -- * Creating a Request
      startLabelDetection
    , StartLabelDetection
    -- * Request Lenses
    , sldJobTag
    , sldNotificationChannel
    , sldClientRequestToken
    , sldMinConfidence
    , sldVideo

    -- * Destructuring the Response
    , startLabelDetectionResponse
    , StartLabelDetectionResponse
    -- * Response Lenses
    , sldrsJobId
    , sldrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startLabelDetection' smart constructor.
data StartLabelDetection = StartLabelDetection'
  { _sldJobTag              :: !(Maybe Text)
  , _sldNotificationChannel :: !(Maybe NotificationChannel)
  , _sldClientRequestToken  :: !(Maybe Text)
  , _sldMinConfidence       :: !(Maybe Double)
  , _sldVideo               :: !Video
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartLabelDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sldJobTag' - Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
--
-- * 'sldNotificationChannel' - The Amazon SNS topic ARN you want Rekognition Video to publish the completion status of the label detection operation to.
--
-- * 'sldClientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartLabelDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- * 'sldMinConfidence' - Specifies the minimum confidence that Rekognition Video must have in order to return a detected label. Confidence represents how certain Amazon Rekognition is that a label is correctly identified.0 is the lowest confidence. 100 is the highest confidence. Rekognition Video doesn't return any labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
--
-- * 'sldVideo' - The video in which you want to detect labels. The video must be stored in an Amazon S3 bucket.
startLabelDetection
    :: Video -- ^ 'sldVideo'
    -> StartLabelDetection
startLabelDetection pVideo_ =
  StartLabelDetection'
    { _sldJobTag = Nothing
    , _sldNotificationChannel = Nothing
    , _sldClientRequestToken = Nothing
    , _sldMinConfidence = Nothing
    , _sldVideo = pVideo_
    }


-- | Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
sldJobTag :: Lens' StartLabelDetection (Maybe Text)
sldJobTag = lens _sldJobTag (\ s a -> s{_sldJobTag = a})

-- | The Amazon SNS topic ARN you want Rekognition Video to publish the completion status of the label detection operation to.
sldNotificationChannel :: Lens' StartLabelDetection (Maybe NotificationChannel)
sldNotificationChannel = lens _sldNotificationChannel (\ s a -> s{_sldNotificationChannel = a})

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartLabelDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
sldClientRequestToken :: Lens' StartLabelDetection (Maybe Text)
sldClientRequestToken = lens _sldClientRequestToken (\ s a -> s{_sldClientRequestToken = a})

-- | Specifies the minimum confidence that Rekognition Video must have in order to return a detected label. Confidence represents how certain Amazon Rekognition is that a label is correctly identified.0 is the lowest confidence. 100 is the highest confidence. Rekognition Video doesn't return any labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
sldMinConfidence :: Lens' StartLabelDetection (Maybe Double)
sldMinConfidence = lens _sldMinConfidence (\ s a -> s{_sldMinConfidence = a})

-- | The video in which you want to detect labels. The video must be stored in an Amazon S3 bucket.
sldVideo :: Lens' StartLabelDetection Video
sldVideo = lens _sldVideo (\ s a -> s{_sldVideo = a})

instance AWSRequest StartLabelDetection where
        type Rs StartLabelDetection =
             StartLabelDetectionResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 StartLabelDetectionResponse' <$>
                   (x .?> "JobId") <*> (pure (fromEnum s)))

instance Hashable StartLabelDetection where

instance NFData StartLabelDetection where

instance ToHeaders StartLabelDetection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.StartLabelDetection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartLabelDetection where
        toJSON StartLabelDetection'{..}
          = object
              (catMaybes
                 [("JobTag" .=) <$> _sldJobTag,
                  ("NotificationChannel" .=) <$>
                    _sldNotificationChannel,
                  ("ClientRequestToken" .=) <$> _sldClientRequestToken,
                  ("MinConfidence" .=) <$> _sldMinConfidence,
                  Just ("Video" .= _sldVideo)])

instance ToPath StartLabelDetection where
        toPath = const "/"

instance ToQuery StartLabelDetection where
        toQuery = const mempty

-- | /See:/ 'startLabelDetectionResponse' smart constructor.
data StartLabelDetectionResponse = StartLabelDetectionResponse'
  { _sldrsJobId          :: !(Maybe Text)
  , _sldrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartLabelDetectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sldrsJobId' - The identifier for the label detection job. Use @JobId@ to identify the job in a subsequent call to @GetLabelDetection@ .
--
-- * 'sldrsResponseStatus' - -- | The response status code.
startLabelDetectionResponse
    :: Int -- ^ 'sldrsResponseStatus'
    -> StartLabelDetectionResponse
startLabelDetectionResponse pResponseStatus_ =
  StartLabelDetectionResponse'
    {_sldrsJobId = Nothing, _sldrsResponseStatus = pResponseStatus_}


-- | The identifier for the label detection job. Use @JobId@ to identify the job in a subsequent call to @GetLabelDetection@ .
sldrsJobId :: Lens' StartLabelDetectionResponse (Maybe Text)
sldrsJobId = lens _sldrsJobId (\ s a -> s{_sldrsJobId = a})

-- | -- | The response status code.
sldrsResponseStatus :: Lens' StartLabelDetectionResponse Int
sldrsResponseStatus = lens _sldrsResponseStatus (\ s a -> s{_sldrsResponseStatus = a})

instance NFData StartLabelDetectionResponse where
