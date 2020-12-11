{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- Segment detection with Amazon Rekognition Video is an asynchronous operation. You start segment detection by calling 'StartSegmentDetection' which returns a job identifier (@JobId@ ). When the segment detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartSegmentDetection@ . To get the results of the segment detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call @GetSegmentDetection@ and pass the job identifier (@JobId@ ) from the initial call of @StartSegmentDetection@ .
-- @GetSegmentDetection@ returns detected segments in an array (@Segments@ ) of 'SegmentDetection' objects. @Segments@ is sorted by the segment types specified in the @SegmentTypes@ input parameter of @StartSegmentDetection@ . Each element of the array includes the detected segment, the precentage confidence in the acuracy of the detected segment, the type of the segment, and the frame in which the segment was detected.
-- Use @SelectedSegmentTypes@ to find out the type of segment detection requested in the call to @StartSegmentDetection@ .
-- Use the @MaxResults@ parameter to limit the number of segment detections returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetSegmentDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetSegmentDetection@ .
-- For more information, see Detecting Video Segments in Stored Video in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.GetSegmentDetection
  ( -- * Creating a request
    GetSegmentDetection (..),
    mkGetSegmentDetection,

    -- ** Request lenses
    gsdNextToken,
    gsdMaxResults,
    gsdJobId,

    -- * Destructuring the response
    GetSegmentDetectionResponse (..),
    mkGetSegmentDetectionResponse,

    -- ** Response lenses
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSegmentDetection' smart constructor.
data GetSegmentDetection = GetSegmentDetection'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    jobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentDetection' with the minimum fields required to make a request.
--
-- * 'jobId' - Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartSegmentDetection@ .
-- * 'maxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000.
-- * 'nextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
mkGetSegmentDetection ::
  -- | 'jobId'
  Lude.Text ->
  GetSegmentDetection
mkGetSegmentDetection pJobId_ =
  GetSegmentDetection'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      jobId = pJobId_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdNextToken :: Lens.Lens' GetSegmentDetection (Lude.Maybe Lude.Text)
gsdNextToken = Lens.lens (nextToken :: GetSegmentDetection -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSegmentDetection)
{-# DEPRECATED gsdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdMaxResults :: Lens.Lens' GetSegmentDetection (Lude.Maybe Lude.Natural)
gsdMaxResults = Lens.lens (maxResults :: GetSegmentDetection -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetSegmentDetection)
{-# DEPRECATED gsdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartSegmentDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdJobId :: Lens.Lens' GetSegmentDetection Lude.Text
gsdJobId = Lens.lens (jobId :: GetSegmentDetection -> Lude.Text) (\s a -> s {jobId = a} :: GetSegmentDetection)
{-# DEPRECATED gsdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetSegmentDetection where
  type Rs GetSegmentDetection = GetSegmentDetectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSegmentDetectionResponse'
            Lude.<$> (x Lude..?> "SelectedSegmentTypes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "VideoMetadata" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "StatusMessage")
            Lude.<*> (x Lude..?> "Segments" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (x Lude..?> "AudioMetadata" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSegmentDetection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.GetSegmentDetection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSegmentDetection where
  toJSON GetSegmentDetection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("JobId" Lude..= jobId)
          ]
      )

instance Lude.ToPath GetSegmentDetection where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSegmentDetection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSegmentDetectionResponse' smart constructor.
data GetSegmentDetectionResponse = GetSegmentDetectionResponse'
  { selectedSegmentTypes ::
      Lude.Maybe [SegmentTypeInfo],
    nextToken :: Lude.Maybe Lude.Text,
    videoMetadata ::
      Lude.Maybe [VideoMetadata],
    statusMessage ::
      Lude.Maybe Lude.Text,
    segments ::
      Lude.Maybe [SegmentDetection],
    jobStatus ::
      Lude.Maybe VideoJobStatus,
    audioMetadata ::
      Lude.Maybe [AudioMetadata],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentDetectionResponse' with the minimum fields required to make a request.
--
-- * 'audioMetadata' - An array of objects. There can be multiple audio streams. Each @AudioMetadata@ object contains metadata for a single audio stream. Audio information in an @AudioMetadata@ objects includes the audio codec, the number of audio channels, the duration of the audio stream, and the sample rate. Audio metadata is returned in each page of information returned by @GetSegmentDetection@ .
-- * 'jobStatus' - Current status of the segment detection job.
-- * 'nextToken' - If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
-- * 'responseStatus' - The response status code.
-- * 'segments' - An array of segments detected in a video. The array is sorted by the segment types (TECHNICAL_CUE or SHOT) specified in the @SegmentTypes@ input parameter of @StartSegmentDetection@ . Within each segment type the array is sorted by timestamp values.
-- * 'selectedSegmentTypes' - An array containing the segment types requested in the call to @StartSegmentDetection@ .
-- * 'statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
-- * 'videoMetadata' - Currently, Amazon Rekognition Video returns a single object in the @VideoMetadata@ array. The object contains information about the video stream in the input file that Amazon Rekognition Video chose to analyze. The @VideoMetadata@ object includes the video codec, video format and other information. Video metadata is returned in each page of information returned by @GetSegmentDetection@ .
mkGetSegmentDetectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSegmentDetectionResponse
mkGetSegmentDetectionResponse pResponseStatus_ =
  GetSegmentDetectionResponse'
    { selectedSegmentTypes = Lude.Nothing,
      nextToken = Lude.Nothing,
      videoMetadata = Lude.Nothing,
      statusMessage = Lude.Nothing,
      segments = Lude.Nothing,
      jobStatus = Lude.Nothing,
      audioMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array containing the segment types requested in the call to @StartSegmentDetection@ .
--
-- /Note:/ Consider using 'selectedSegmentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsSelectedSegmentTypes :: Lens.Lens' GetSegmentDetectionResponse (Lude.Maybe [SegmentTypeInfo])
gsdrsSelectedSegmentTypes = Lens.lens (selectedSegmentTypes :: GetSegmentDetectionResponse -> Lude.Maybe [SegmentTypeInfo]) (\s a -> s {selectedSegmentTypes = a} :: GetSegmentDetectionResponse)
{-# DEPRECATED gsdrsSelectedSegmentTypes "Use generic-lens or generic-optics with 'selectedSegmentTypes' instead." #-}

-- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsNextToken :: Lens.Lens' GetSegmentDetectionResponse (Lude.Maybe Lude.Text)
gsdrsNextToken = Lens.lens (nextToken :: GetSegmentDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSegmentDetectionResponse)
{-# DEPRECATED gsdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Currently, Amazon Rekognition Video returns a single object in the @VideoMetadata@ array. The object contains information about the video stream in the input file that Amazon Rekognition Video chose to analyze. The @VideoMetadata@ object includes the video codec, video format and other information. Video metadata is returned in each page of information returned by @GetSegmentDetection@ .
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsVideoMetadata :: Lens.Lens' GetSegmentDetectionResponse (Lude.Maybe [VideoMetadata])
gsdrsVideoMetadata = Lens.lens (videoMetadata :: GetSegmentDetectionResponse -> Lude.Maybe [VideoMetadata]) (\s a -> s {videoMetadata = a} :: GetSegmentDetectionResponse)
{-# DEPRECATED gsdrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsStatusMessage :: Lens.Lens' GetSegmentDetectionResponse (Lude.Maybe Lude.Text)
gsdrsStatusMessage = Lens.lens (statusMessage :: GetSegmentDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: GetSegmentDetectionResponse)
{-# DEPRECATED gsdrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | An array of segments detected in a video. The array is sorted by the segment types (TECHNICAL_CUE or SHOT) specified in the @SegmentTypes@ input parameter of @StartSegmentDetection@ . Within each segment type the array is sorted by timestamp values.
--
-- /Note:/ Consider using 'segments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsSegments :: Lens.Lens' GetSegmentDetectionResponse (Lude.Maybe [SegmentDetection])
gsdrsSegments = Lens.lens (segments :: GetSegmentDetectionResponse -> Lude.Maybe [SegmentDetection]) (\s a -> s {segments = a} :: GetSegmentDetectionResponse)
{-# DEPRECATED gsdrsSegments "Use generic-lens or generic-optics with 'segments' instead." #-}

-- | Current status of the segment detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsJobStatus :: Lens.Lens' GetSegmentDetectionResponse (Lude.Maybe VideoJobStatus)
gsdrsJobStatus = Lens.lens (jobStatus :: GetSegmentDetectionResponse -> Lude.Maybe VideoJobStatus) (\s a -> s {jobStatus = a} :: GetSegmentDetectionResponse)
{-# DEPRECATED gsdrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | An array of objects. There can be multiple audio streams. Each @AudioMetadata@ object contains metadata for a single audio stream. Audio information in an @AudioMetadata@ objects includes the audio codec, the number of audio channels, the duration of the audio stream, and the sample rate. Audio metadata is returned in each page of information returned by @GetSegmentDetection@ .
--
-- /Note:/ Consider using 'audioMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsAudioMetadata :: Lens.Lens' GetSegmentDetectionResponse (Lude.Maybe [AudioMetadata])
gsdrsAudioMetadata = Lens.lens (audioMetadata :: GetSegmentDetectionResponse -> Lude.Maybe [AudioMetadata]) (\s a -> s {audioMetadata = a} :: GetSegmentDetectionResponse)
{-# DEPRECATED gsdrsAudioMetadata "Use generic-lens or generic-optics with 'audioMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsResponseStatus :: Lens.Lens' GetSegmentDetectionResponse Lude.Int
gsdrsResponseStatus = Lens.lens (responseStatus :: GetSegmentDetectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSegmentDetectionResponse)
{-# DEPRECATED gsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
