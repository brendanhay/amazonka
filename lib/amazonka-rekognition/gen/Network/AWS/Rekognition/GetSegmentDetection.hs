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
-- Module      : Network.AWS.Rekognition.GetSegmentDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the segment detection results of a Amazon Rekognition Video analysis started by 'StartSegmentDetection' .
--
--
-- Segment detection with Amazon Rekognition Video is an asynchronous operation. You start segment detection by calling 'StartSegmentDetection' which returns a job identifier (@JobId@ ). When the segment detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartSegmentDetection@ . To get the results of the segment detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call @GetSegmentDetection@ and pass the job identifier (@JobId@ ) from the initial call of @StartSegmentDetection@ .
--
-- @GetSegmentDetection@ returns detected segments in an array (@Segments@ ) of 'SegmentDetection' objects. @Segments@ is sorted by the segment types specified in the @SegmentTypes@ input parameter of @StartSegmentDetection@ . Each element of the array includes the detected segment, the precentage confidence in the acuracy of the detected segment, the type of the segment, and the frame in which the segment was detected.
--
-- Use @SelectedSegmentTypes@ to find out the type of segment detection requested in the call to @StartSegmentDetection@ .
--
-- Use the @MaxResults@ parameter to limit the number of segment detections returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetSegmentDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetSegmentDetection@ .
--
-- For more information, see Detecting Video Segments in Stored Video in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.GetSegmentDetection
  ( -- * Creating a Request
    getSegmentDetection,
    GetSegmentDetection,

    -- * Request Lenses
    gsdNextToken,
    gsdMaxResults,
    gsdJobId,

    -- * Destructuring the Response
    getSegmentDetectionResponse,
    GetSegmentDetectionResponse,

    -- * Response Lenses
    gsdrsSelectedSegmentTypes,
    gsdrsNextToken,
    gsdrsVideoMetadata,
    gsdrsStatusMessage,
    gsdrsSegments,
    gsdrsJobStatus,
    gsdrsAudioMetadata,
    gsdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSegmentDetection' smart constructor.
data GetSegmentDetection = GetSegmentDetection'
  { _gsdNextToken ::
      !(Maybe Text),
    _gsdMaxResults :: !(Maybe Nat),
    _gsdJobId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegmentDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdNextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
--
-- * 'gsdMaxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000.
--
-- * 'gsdJobId' - Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartSegmentDetection@ .
getSegmentDetection ::
  -- | 'gsdJobId'
  Text ->
  GetSegmentDetection
getSegmentDetection pJobId_ =
  GetSegmentDetection'
    { _gsdNextToken = Nothing,
      _gsdMaxResults = Nothing,
      _gsdJobId = pJobId_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
gsdNextToken :: Lens' GetSegmentDetection (Maybe Text)
gsdNextToken = lens _gsdNextToken (\s a -> s {_gsdNextToken = a})

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
gsdMaxResults :: Lens' GetSegmentDetection (Maybe Natural)
gsdMaxResults = lens _gsdMaxResults (\s a -> s {_gsdMaxResults = a}) . mapping _Nat

-- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartSegmentDetection@ .
gsdJobId :: Lens' GetSegmentDetection Text
gsdJobId = lens _gsdJobId (\s a -> s {_gsdJobId = a})

instance AWSRequest GetSegmentDetection where
  type Rs GetSegmentDetection = GetSegmentDetectionResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          GetSegmentDetectionResponse'
            <$> (x .?> "SelectedSegmentTypes" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (x .?> "VideoMetadata" .!@ mempty)
            <*> (x .?> "StatusMessage")
            <*> (x .?> "Segments" .!@ mempty)
            <*> (x .?> "JobStatus")
            <*> (x .?> "AudioMetadata" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetSegmentDetection

instance NFData GetSegmentDetection

instance ToHeaders GetSegmentDetection where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.GetSegmentDetection" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSegmentDetection where
  toJSON GetSegmentDetection' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _gsdNextToken,
            ("MaxResults" .=) <$> _gsdMaxResults,
            Just ("JobId" .= _gsdJobId)
          ]
      )

instance ToPath GetSegmentDetection where
  toPath = const "/"

instance ToQuery GetSegmentDetection where
  toQuery = const mempty

-- | /See:/ 'getSegmentDetectionResponse' smart constructor.
data GetSegmentDetectionResponse = GetSegmentDetectionResponse'
  { _gsdrsSelectedSegmentTypes ::
      !(Maybe [SegmentTypeInfo]),
    _gsdrsNextToken :: !(Maybe Text),
    _gsdrsVideoMetadata ::
      !(Maybe [VideoMetadata]),
    _gsdrsStatusMessage ::
      !(Maybe Text),
    _gsdrsSegments ::
      !(Maybe [SegmentDetection]),
    _gsdrsJobStatus ::
      !(Maybe VideoJobStatus),
    _gsdrsAudioMetadata ::
      !(Maybe [AudioMetadata]),
    _gsdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSegmentDetectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdrsSelectedSegmentTypes' - An array containing the segment types requested in the call to @StartSegmentDetection@ .
--
-- * 'gsdrsNextToken' - If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
--
-- * 'gsdrsVideoMetadata' - Currently, Amazon Rekognition Video returns a single object in the @VideoMetadata@ array. The object contains information about the video stream in the input file that Amazon Rekognition Video chose to analyze. The @VideoMetadata@ object includes the video codec, video format and other information. Video metadata is returned in each page of information returned by @GetSegmentDetection@ .
--
-- * 'gsdrsStatusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- * 'gsdrsSegments' - An array of segments detected in a video. The array is sorted by the segment types (TECHNICAL_CUE or SHOT) specified in the @SegmentTypes@ input parameter of @StartSegmentDetection@ . Within each segment type the array is sorted by timestamp values.
--
-- * 'gsdrsJobStatus' - Current status of the segment detection job.
--
-- * 'gsdrsAudioMetadata' - An array of objects. There can be multiple audio streams. Each @AudioMetadata@ object contains metadata for a single audio stream. Audio information in an @AudioMetadata@ objects includes the audio codec, the number of audio channels, the duration of the audio stream, and the sample rate. Audio metadata is returned in each page of information returned by @GetSegmentDetection@ .
--
-- * 'gsdrsResponseStatus' - -- | The response status code.
getSegmentDetectionResponse ::
  -- | 'gsdrsResponseStatus'
  Int ->
  GetSegmentDetectionResponse
getSegmentDetectionResponse pResponseStatus_ =
  GetSegmentDetectionResponse'
    { _gsdrsSelectedSegmentTypes =
        Nothing,
      _gsdrsNextToken = Nothing,
      _gsdrsVideoMetadata = Nothing,
      _gsdrsStatusMessage = Nothing,
      _gsdrsSegments = Nothing,
      _gsdrsJobStatus = Nothing,
      _gsdrsAudioMetadata = Nothing,
      _gsdrsResponseStatus = pResponseStatus_
    }

-- | An array containing the segment types requested in the call to @StartSegmentDetection@ .
gsdrsSelectedSegmentTypes :: Lens' GetSegmentDetectionResponse [SegmentTypeInfo]
gsdrsSelectedSegmentTypes = lens _gsdrsSelectedSegmentTypes (\s a -> s {_gsdrsSelectedSegmentTypes = a}) . _Default . _Coerce

-- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
gsdrsNextToken :: Lens' GetSegmentDetectionResponse (Maybe Text)
gsdrsNextToken = lens _gsdrsNextToken (\s a -> s {_gsdrsNextToken = a})

-- | Currently, Amazon Rekognition Video returns a single object in the @VideoMetadata@ array. The object contains information about the video stream in the input file that Amazon Rekognition Video chose to analyze. The @VideoMetadata@ object includes the video codec, video format and other information. Video metadata is returned in each page of information returned by @GetSegmentDetection@ .
gsdrsVideoMetadata :: Lens' GetSegmentDetectionResponse [VideoMetadata]
gsdrsVideoMetadata = lens _gsdrsVideoMetadata (\s a -> s {_gsdrsVideoMetadata = a}) . _Default . _Coerce

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
gsdrsStatusMessage :: Lens' GetSegmentDetectionResponse (Maybe Text)
gsdrsStatusMessage = lens _gsdrsStatusMessage (\s a -> s {_gsdrsStatusMessage = a})

-- | An array of segments detected in a video. The array is sorted by the segment types (TECHNICAL_CUE or SHOT) specified in the @SegmentTypes@ input parameter of @StartSegmentDetection@ . Within each segment type the array is sorted by timestamp values.
gsdrsSegments :: Lens' GetSegmentDetectionResponse [SegmentDetection]
gsdrsSegments = lens _gsdrsSegments (\s a -> s {_gsdrsSegments = a}) . _Default . _Coerce

-- | Current status of the segment detection job.
gsdrsJobStatus :: Lens' GetSegmentDetectionResponse (Maybe VideoJobStatus)
gsdrsJobStatus = lens _gsdrsJobStatus (\s a -> s {_gsdrsJobStatus = a})

-- | An array of objects. There can be multiple audio streams. Each @AudioMetadata@ object contains metadata for a single audio stream. Audio information in an @AudioMetadata@ objects includes the audio codec, the number of audio channels, the duration of the audio stream, and the sample rate. Audio metadata is returned in each page of information returned by @GetSegmentDetection@ .
gsdrsAudioMetadata :: Lens' GetSegmentDetectionResponse [AudioMetadata]
gsdrsAudioMetadata = lens _gsdrsAudioMetadata (\s a -> s {_gsdrsAudioMetadata = a}) . _Default . _Coerce

-- | -- | The response status code.
gsdrsResponseStatus :: Lens' GetSegmentDetectionResponse Int
gsdrsResponseStatus = lens _gsdrsResponseStatus (\s a -> s {_gsdrsResponseStatus = a})

instance NFData GetSegmentDetectionResponse
