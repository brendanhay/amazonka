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
-- Module      : Network.AWS.Rekognition.GetTextDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the text detection results of a Amazon Rekognition Video analysis started by 'StartTextDetection' .
--
--
-- Text detection with Amazon Rekognition Video is an asynchronous operation. You start text detection by calling 'StartTextDetection' which returns a job identifier (@JobId@ ) When the text detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartTextDetection@ . To get the results of the text detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call @GetTextDetection@ and pass the job identifier (@JobId@ ) from the initial call of @StartLabelDetection@ .
--
-- @GetTextDetection@ returns an array of detected text (@TextDetections@ ) sorted by the time the text was detected, up to 50 words per frame of video.
--
-- Each element of the array includes the detected text, the precentage confidence in the acuracy of the detected text, the time the text was detected, bounding box information for where the text was located, and unique identifiers for words and their lines.
--
-- Use MaxResults parameter to limit the number of text detections returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetTextDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetTextDetection@ .
module Network.AWS.Rekognition.GetTextDetection
  ( -- * Creating a Request
    getTextDetection,
    GetTextDetection,

    -- * Request Lenses
    gtdNextToken,
    gtdMaxResults,
    gtdJobId,

    -- * Destructuring the Response
    getTextDetectionResponse,
    GetTextDetectionResponse,

    -- * Response Lenses
    gtdrsTextDetections,
    gtdrsNextToken,
    gtdrsVideoMetadata,
    gtdrsStatusMessage,
    gtdrsTextModelVersion,
    gtdrsJobStatus,
    gtdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTextDetection' smart constructor.
data GetTextDetection = GetTextDetection'
  { _gtdNextToken ::
      !(Maybe Text),
    _gtdMaxResults :: !(Maybe Nat),
    _gtdJobId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTextDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtdNextToken' - If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
--
-- * 'gtdMaxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000.
--
-- * 'gtdJobId' - Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartTextDetection@ .
getTextDetection ::
  -- | 'gtdJobId'
  Text ->
  GetTextDetection
getTextDetection pJobId_ =
  GetTextDetection'
    { _gtdNextToken = Nothing,
      _gtdMaxResults = Nothing,
      _gtdJobId = pJobId_
    }

-- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
gtdNextToken :: Lens' GetTextDetection (Maybe Text)
gtdNextToken = lens _gtdNextToken (\s a -> s {_gtdNextToken = a})

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
gtdMaxResults :: Lens' GetTextDetection (Maybe Natural)
gtdMaxResults = lens _gtdMaxResults (\s a -> s {_gtdMaxResults = a}) . mapping _Nat

-- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartTextDetection@ .
gtdJobId :: Lens' GetTextDetection Text
gtdJobId = lens _gtdJobId (\s a -> s {_gtdJobId = a})

instance AWSRequest GetTextDetection where
  type Rs GetTextDetection = GetTextDetectionResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          GetTextDetectionResponse'
            <$> (x .?> "TextDetections" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (x .?> "VideoMetadata")
            <*> (x .?> "StatusMessage")
            <*> (x .?> "TextModelVersion")
            <*> (x .?> "JobStatus")
            <*> (pure (fromEnum s))
      )

instance Hashable GetTextDetection

instance NFData GetTextDetection

instance ToHeaders GetTextDetection where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.GetTextDetection" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetTextDetection where
  toJSON GetTextDetection' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _gtdNextToken,
            ("MaxResults" .=) <$> _gtdMaxResults,
            Just ("JobId" .= _gtdJobId)
          ]
      )

instance ToPath GetTextDetection where
  toPath = const "/"

instance ToQuery GetTextDetection where
  toQuery = const mempty

-- | /See:/ 'getTextDetectionResponse' smart constructor.
data GetTextDetectionResponse = GetTextDetectionResponse'
  { _gtdrsTextDetections ::
      !(Maybe [TextDetectionResult]),
    _gtdrsNextToken :: !(Maybe Text),
    _gtdrsVideoMetadata ::
      !(Maybe VideoMetadata),
    _gtdrsStatusMessage :: !(Maybe Text),
    _gtdrsTextModelVersion :: !(Maybe Text),
    _gtdrsJobStatus ::
      !(Maybe VideoJobStatus),
    _gtdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTextDetectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtdrsTextDetections' - An array of text detected in the video. Each element contains the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
--
-- * 'gtdrsNextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
--
-- * 'gtdrsVideoMetadata' - Undocumented member.
--
-- * 'gtdrsStatusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- * 'gtdrsTextModelVersion' - Version number of the text detection model that was used to detect text.
--
-- * 'gtdrsJobStatus' - Current status of the text detection job.
--
-- * 'gtdrsResponseStatus' - -- | The response status code.
getTextDetectionResponse ::
  -- | 'gtdrsResponseStatus'
  Int ->
  GetTextDetectionResponse
getTextDetectionResponse pResponseStatus_ =
  GetTextDetectionResponse'
    { _gtdrsTextDetections = Nothing,
      _gtdrsNextToken = Nothing,
      _gtdrsVideoMetadata = Nothing,
      _gtdrsStatusMessage = Nothing,
      _gtdrsTextModelVersion = Nothing,
      _gtdrsJobStatus = Nothing,
      _gtdrsResponseStatus = pResponseStatus_
    }

-- | An array of text detected in the video. Each element contains the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
gtdrsTextDetections :: Lens' GetTextDetectionResponse [TextDetectionResult]
gtdrsTextDetections = lens _gtdrsTextDetections (\s a -> s {_gtdrsTextDetections = a}) . _Default . _Coerce

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
gtdrsNextToken :: Lens' GetTextDetectionResponse (Maybe Text)
gtdrsNextToken = lens _gtdrsNextToken (\s a -> s {_gtdrsNextToken = a})

-- | Undocumented member.
gtdrsVideoMetadata :: Lens' GetTextDetectionResponse (Maybe VideoMetadata)
gtdrsVideoMetadata = lens _gtdrsVideoMetadata (\s a -> s {_gtdrsVideoMetadata = a})

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
gtdrsStatusMessage :: Lens' GetTextDetectionResponse (Maybe Text)
gtdrsStatusMessage = lens _gtdrsStatusMessage (\s a -> s {_gtdrsStatusMessage = a})

-- | Version number of the text detection model that was used to detect text.
gtdrsTextModelVersion :: Lens' GetTextDetectionResponse (Maybe Text)
gtdrsTextModelVersion = lens _gtdrsTextModelVersion (\s a -> s {_gtdrsTextModelVersion = a})

-- | Current status of the text detection job.
gtdrsJobStatus :: Lens' GetTextDetectionResponse (Maybe VideoJobStatus)
gtdrsJobStatus = lens _gtdrsJobStatus (\s a -> s {_gtdrsJobStatus = a})

-- | -- | The response status code.
gtdrsResponseStatus :: Lens' GetTextDetectionResponse Int
gtdrsResponseStatus = lens _gtdrsResponseStatus (\s a -> s {_gtdrsResponseStatus = a})

instance NFData GetTextDetectionResponse
