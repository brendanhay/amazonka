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
-- Module      : Network.AWS.Rekognition.StartTextDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts asynchronous detection of text in a stored video.
--
--
-- Amazon Rekognition Video can detect text in a video stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartTextDetection@ returns a job identifier (@JobId@ ) which you use to get the results of the operation. When text detection is finished, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ .
--
-- To get the results of the text detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call 'GetTextDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartTextDetection@ .
module Network.AWS.Rekognition.StartTextDetection
  ( -- * Creating a Request
    startTextDetection,
    StartTextDetection,

    -- * Request Lenses
    stdJobTag,
    stdFilters,
    stdNotificationChannel,
    stdClientRequestToken,
    stdVideo,

    -- * Destructuring the Response
    startTextDetectionResponse,
    StartTextDetectionResponse,

    -- * Response Lenses
    stdrsJobId,
    stdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startTextDetection' smart constructor.
data StartTextDetection = StartTextDetection'
  { _stdJobTag ::
      !(Maybe Text),
    _stdFilters :: !(Maybe StartTextDetectionFilters),
    _stdNotificationChannel ::
      !(Maybe NotificationChannel),
    _stdClientRequestToken :: !(Maybe Text),
    _stdVideo :: !Video
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTextDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdJobTag' - An identifier returned in the completion status published by your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
--
-- * 'stdFilters' - Optional parameters that let you set criteria the text must meet to be included in your response.
--
-- * 'stdNotificationChannel' - Undocumented member.
--
-- * 'stdClientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartTextDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentaly started more than once.
--
-- * 'stdVideo' - Undocumented member.
startTextDetection ::
  -- | 'stdVideo'
  Video ->
  StartTextDetection
startTextDetection pVideo_ =
  StartTextDetection'
    { _stdJobTag = Nothing,
      _stdFilters = Nothing,
      _stdNotificationChannel = Nothing,
      _stdClientRequestToken = Nothing,
      _stdVideo = pVideo_
    }

-- | An identifier returned in the completion status published by your Amazon Simple Notification Service topic. For example, you can use @JobTag@ to group related jobs and identify them in the completion notification.
stdJobTag :: Lens' StartTextDetection (Maybe Text)
stdJobTag = lens _stdJobTag (\s a -> s {_stdJobTag = a})

-- | Optional parameters that let you set criteria the text must meet to be included in your response.
stdFilters :: Lens' StartTextDetection (Maybe StartTextDetectionFilters)
stdFilters = lens _stdFilters (\s a -> s {_stdFilters = a})

-- | Undocumented member.
stdNotificationChannel :: Lens' StartTextDetection (Maybe NotificationChannel)
stdNotificationChannel = lens _stdNotificationChannel (\s a -> s {_stdNotificationChannel = a})

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartTextDetection@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidentaly started more than once.
stdClientRequestToken :: Lens' StartTextDetection (Maybe Text)
stdClientRequestToken = lens _stdClientRequestToken (\s a -> s {_stdClientRequestToken = a})

-- | Undocumented member.
stdVideo :: Lens' StartTextDetection Video
stdVideo = lens _stdVideo (\s a -> s {_stdVideo = a})

instance AWSRequest StartTextDetection where
  type Rs StartTextDetection = StartTextDetectionResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          StartTextDetectionResponse'
            <$> (x .?> "JobId") <*> (pure (fromEnum s))
      )

instance Hashable StartTextDetection

instance NFData StartTextDetection

instance ToHeaders StartTextDetection where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.StartTextDetection" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartTextDetection where
  toJSON StartTextDetection' {..} =
    object
      ( catMaybes
          [ ("JobTag" .=) <$> _stdJobTag,
            ("Filters" .=) <$> _stdFilters,
            ("NotificationChannel" .=) <$> _stdNotificationChannel,
            ("ClientRequestToken" .=) <$> _stdClientRequestToken,
            Just ("Video" .= _stdVideo)
          ]
      )

instance ToPath StartTextDetection where
  toPath = const "/"

instance ToQuery StartTextDetection where
  toQuery = const mempty

-- | /See:/ 'startTextDetectionResponse' smart constructor.
data StartTextDetectionResponse = StartTextDetectionResponse'
  { _stdrsJobId ::
      !(Maybe Text),
    _stdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTextDetectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdrsJobId' - Identifier for the text detection job. Use @JobId@ to identify the job in a subsequent call to @GetTextDetection@ .
--
-- * 'stdrsResponseStatus' - -- | The response status code.
startTextDetectionResponse ::
  -- | 'stdrsResponseStatus'
  Int ->
  StartTextDetectionResponse
startTextDetectionResponse pResponseStatus_ =
  StartTextDetectionResponse'
    { _stdrsJobId = Nothing,
      _stdrsResponseStatus = pResponseStatus_
    }

-- | Identifier for the text detection job. Use @JobId@ to identify the job in a subsequent call to @GetTextDetection@ .
stdrsJobId :: Lens' StartTextDetectionResponse (Maybe Text)
stdrsJobId = lens _stdrsJobId (\s a -> s {_stdrsJobId = a})

-- | -- | The response status code.
stdrsResponseStatus :: Lens' StartTextDetectionResponse Int
stdrsResponseStatus = lens _stdrsResponseStatus (\s a -> s {_stdrsResponseStatus = a})

instance NFData StartTextDetectionResponse
