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
-- Module      : Network.AWS.Rekognition.StartFaceDetection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of faces in a stored video.
--
--
-- Rekognition Video can detect faces in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartFaceDetection@ returns a job identifier (@JobId@ ) that you use to get the results of the operation. When face detection is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ . To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call and pass the job identifier (@JobId@ ) from the initial call to @StartFaceDetection@ . For more information, see 'faces-video' .
--
module Network.AWS.Rekognition.StartFaceDetection
    (
    -- * Creating a Request
      startFaceDetection
    , StartFaceDetection
    -- * Request Lenses
    , sfdJobTag
    , sfdNotificationChannel
    , sfdClientRequestToken
    , sfdFaceAttributes
    , sfdVideo

    -- * Destructuring the Response
    , startFaceDetectionResponse
    , StartFaceDetectionResponse
    -- * Response Lenses
    , sfdrsJobId
    , sfdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startFaceDetection' smart constructor.
data StartFaceDetection = StartFaceDetection'
  { _sfdJobTag              :: !(Maybe Text)
  , _sfdNotificationChannel :: !(Maybe NotificationChannel)
  , _sfdClientRequestToken  :: !(Maybe Text)
  , _sfdFaceAttributes      :: !(Maybe FaceAttributes)
  , _sfdVideo               :: !Video
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartFaceDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfdJobTag' - Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
--
-- * 'sfdNotificationChannel' - The ARN of the Amazon SNS topic to which you want Rekognition Video to publish the completion status of the face detection operation.
--
-- * 'sfdClientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- * 'sfdFaceAttributes' - The face attributes you want returned. @DEFAULT@ - The following subset of facial attributes are returned: BoundingBox, Confidence, Pose, Quality and Landmarks.  @ALL@ - All facial attributes are returned.
--
-- * 'sfdVideo' - The video in which you want to detect faces. The video must be stored in an Amazon S3 bucket.
startFaceDetection
    :: Video -- ^ 'sfdVideo'
    -> StartFaceDetection
startFaceDetection pVideo_ =
  StartFaceDetection'
    { _sfdJobTag = Nothing
    , _sfdNotificationChannel = Nothing
    , _sfdClientRequestToken = Nothing
    , _sfdFaceAttributes = Nothing
    , _sfdVideo = pVideo_
    }


-- | Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
sfdJobTag :: Lens' StartFaceDetection (Maybe Text)
sfdJobTag = lens _sfdJobTag (\ s a -> s{_sfdJobTag = a})

-- | The ARN of the Amazon SNS topic to which you want Rekognition Video to publish the completion status of the face detection operation.
sfdNotificationChannel :: Lens' StartFaceDetection (Maybe NotificationChannel)
sfdNotificationChannel = lens _sfdNotificationChannel (\ s a -> s{_sfdNotificationChannel = a})

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
sfdClientRequestToken :: Lens' StartFaceDetection (Maybe Text)
sfdClientRequestToken = lens _sfdClientRequestToken (\ s a -> s{_sfdClientRequestToken = a})

-- | The face attributes you want returned. @DEFAULT@ - The following subset of facial attributes are returned: BoundingBox, Confidence, Pose, Quality and Landmarks.  @ALL@ - All facial attributes are returned.
sfdFaceAttributes :: Lens' StartFaceDetection (Maybe FaceAttributes)
sfdFaceAttributes = lens _sfdFaceAttributes (\ s a -> s{_sfdFaceAttributes = a})

-- | The video in which you want to detect faces. The video must be stored in an Amazon S3 bucket.
sfdVideo :: Lens' StartFaceDetection Video
sfdVideo = lens _sfdVideo (\ s a -> s{_sfdVideo = a})

instance AWSRequest StartFaceDetection where
        type Rs StartFaceDetection =
             StartFaceDetectionResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 StartFaceDetectionResponse' <$>
                   (x .?> "JobId") <*> (pure (fromEnum s)))

instance Hashable StartFaceDetection where

instance NFData StartFaceDetection where

instance ToHeaders StartFaceDetection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.StartFaceDetection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartFaceDetection where
        toJSON StartFaceDetection'{..}
          = object
              (catMaybes
                 [("JobTag" .=) <$> _sfdJobTag,
                  ("NotificationChannel" .=) <$>
                    _sfdNotificationChannel,
                  ("ClientRequestToken" .=) <$> _sfdClientRequestToken,
                  ("FaceAttributes" .=) <$> _sfdFaceAttributes,
                  Just ("Video" .= _sfdVideo)])

instance ToPath StartFaceDetection where
        toPath = const "/"

instance ToQuery StartFaceDetection where
        toQuery = const mempty

-- | /See:/ 'startFaceDetectionResponse' smart constructor.
data StartFaceDetectionResponse = StartFaceDetectionResponse'
  { _sfdrsJobId          :: !(Maybe Text)
  , _sfdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartFaceDetectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfdrsJobId' - The identifier for the face detection job. Use @JobId@ to identify the job in a subsequent call to @GetFaceDetection@ .
--
-- * 'sfdrsResponseStatus' - -- | The response status code.
startFaceDetectionResponse
    :: Int -- ^ 'sfdrsResponseStatus'
    -> StartFaceDetectionResponse
startFaceDetectionResponse pResponseStatus_ =
  StartFaceDetectionResponse'
    {_sfdrsJobId = Nothing, _sfdrsResponseStatus = pResponseStatus_}


-- | The identifier for the face detection job. Use @JobId@ to identify the job in a subsequent call to @GetFaceDetection@ .
sfdrsJobId :: Lens' StartFaceDetectionResponse (Maybe Text)
sfdrsJobId = lens _sfdrsJobId (\ s a -> s{_sfdrsJobId = a})

-- | -- | The response status code.
sfdrsResponseStatus :: Lens' StartFaceDetectionResponse Int
sfdrsResponseStatus = lens _sfdrsResponseStatus (\ s a -> s{_sfdrsResponseStatus = a})

instance NFData StartFaceDetectionResponse where
