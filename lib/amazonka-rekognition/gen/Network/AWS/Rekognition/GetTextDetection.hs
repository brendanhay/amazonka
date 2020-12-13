{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- Text detection with Amazon Rekognition Video is an asynchronous operation. You start text detection by calling 'StartTextDetection' which returns a job identifier (@JobId@ ) When the text detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartTextDetection@ . To get the results of the text detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . if so, call @GetTextDetection@ and pass the job identifier (@JobId@ ) from the initial call of @StartLabelDetection@ .
-- @GetTextDetection@ returns an array of detected text (@TextDetections@ ) sorted by the time the text was detected, up to 50 words per frame of video.
-- Each element of the array includes the detected text, the precentage confidence in the acuracy of the detected text, the time the text was detected, bounding box information for where the text was located, and unique identifiers for words and their lines.
-- Use MaxResults parameter to limit the number of text detections returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetTextDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetTextDetection@ .
module Network.AWS.Rekognition.GetTextDetection
  ( -- * Creating a request
    GetTextDetection (..),
    mkGetTextDetection,

    -- ** Request lenses
    gtdJobId,
    gtdNextToken,
    gtdMaxResults,

    -- * Destructuring the response
    GetTextDetectionResponse (..),
    mkGetTextDetectionResponse,

    -- ** Response lenses
    gtdrsTextDetections,
    gtdrsNextToken,
    gtdrsVideoMetadata,
    gtdrsStatusMessage,
    gtdrsTextModelVersion,
    gtdrsJobStatus,
    gtdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTextDetection' smart constructor.
data GetTextDetection = GetTextDetection'
  { -- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartTextDetection@ .
    jobId :: Lude.Text,
    -- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTextDetection' with the minimum fields required to make a request.
--
-- * 'jobId' - Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartTextDetection@ .
-- * 'nextToken' - If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
-- * 'maxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000.
mkGetTextDetection ::
  -- | 'jobId'
  Lude.Text ->
  GetTextDetection
mkGetTextDetection pJobId_ =
  GetTextDetection'
    { jobId = pJobId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartTextDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdJobId :: Lens.Lens' GetTextDetection Lude.Text
gtdJobId = Lens.lens (jobId :: GetTextDetection -> Lude.Text) (\s a -> s {jobId = a} :: GetTextDetection)
{-# DEPRECATED gtdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdNextToken :: Lens.Lens' GetTextDetection (Lude.Maybe Lude.Text)
gtdNextToken = Lens.lens (nextToken :: GetTextDetection -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTextDetection)
{-# DEPRECATED gtdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdMaxResults :: Lens.Lens' GetTextDetection (Lude.Maybe Lude.Natural)
gtdMaxResults = Lens.lens (maxResults :: GetTextDetection -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTextDetection)
{-# DEPRECATED gtdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest GetTextDetection where
  type Rs GetTextDetection = GetTextDetectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTextDetectionResponse'
            Lude.<$> (x Lude..?> "TextDetections" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "VideoMetadata")
            Lude.<*> (x Lude..?> "StatusMessage")
            Lude.<*> (x Lude..?> "TextModelVersion")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTextDetection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.GetTextDetection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTextDetection where
  toJSON GetTextDetection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobId" Lude..= jobId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetTextDetection where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTextDetection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTextDetectionResponse' smart constructor.
data GetTextDetectionResponse = GetTextDetectionResponse'
  { -- | An array of text detected in the video. Each element contains the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
    textDetections :: Lude.Maybe [TextDetectionResult],
    -- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
    nextToken :: Lude.Maybe Lude.Text,
    videoMetadata :: Lude.Maybe VideoMetadata,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | Version number of the text detection model that was used to detect text.
    textModelVersion :: Lude.Maybe Lude.Text,
    -- | Current status of the text detection job.
    jobStatus :: Lude.Maybe VideoJobStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTextDetectionResponse' with the minimum fields required to make a request.
--
-- * 'textDetections' - An array of text detected in the video. Each element contains the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
-- * 'nextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
-- * 'videoMetadata' -
-- * 'statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
-- * 'textModelVersion' - Version number of the text detection model that was used to detect text.
-- * 'jobStatus' - Current status of the text detection job.
-- * 'responseStatus' - The response status code.
mkGetTextDetectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTextDetectionResponse
mkGetTextDetectionResponse pResponseStatus_ =
  GetTextDetectionResponse'
    { textDetections = Lude.Nothing,
      nextToken = Lude.Nothing,
      videoMetadata = Lude.Nothing,
      statusMessage = Lude.Nothing,
      textModelVersion = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of text detected in the video. Each element contains the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
--
-- /Note:/ Consider using 'textDetections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrsTextDetections :: Lens.Lens' GetTextDetectionResponse (Lude.Maybe [TextDetectionResult])
gtdrsTextDetections = Lens.lens (textDetections :: GetTextDetectionResponse -> Lude.Maybe [TextDetectionResult]) (\s a -> s {textDetections = a} :: GetTextDetectionResponse)
{-# DEPRECATED gtdrsTextDetections "Use generic-lens or generic-optics with 'textDetections' instead." #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrsNextToken :: Lens.Lens' GetTextDetectionResponse (Lude.Maybe Lude.Text)
gtdrsNextToken = Lens.lens (nextToken :: GetTextDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTextDetectionResponse)
{-# DEPRECATED gtdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrsVideoMetadata :: Lens.Lens' GetTextDetectionResponse (Lude.Maybe VideoMetadata)
gtdrsVideoMetadata = Lens.lens (videoMetadata :: GetTextDetectionResponse -> Lude.Maybe VideoMetadata) (\s a -> s {videoMetadata = a} :: GetTextDetectionResponse)
{-# DEPRECATED gtdrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrsStatusMessage :: Lens.Lens' GetTextDetectionResponse (Lude.Maybe Lude.Text)
gtdrsStatusMessage = Lens.lens (statusMessage :: GetTextDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: GetTextDetectionResponse)
{-# DEPRECATED gtdrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Version number of the text detection model that was used to detect text.
--
-- /Note:/ Consider using 'textModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrsTextModelVersion :: Lens.Lens' GetTextDetectionResponse (Lude.Maybe Lude.Text)
gtdrsTextModelVersion = Lens.lens (textModelVersion :: GetTextDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {textModelVersion = a} :: GetTextDetectionResponse)
{-# DEPRECATED gtdrsTextModelVersion "Use generic-lens or generic-optics with 'textModelVersion' instead." #-}

-- | Current status of the text detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrsJobStatus :: Lens.Lens' GetTextDetectionResponse (Lude.Maybe VideoJobStatus)
gtdrsJobStatus = Lens.lens (jobStatus :: GetTextDetectionResponse -> Lude.Maybe VideoJobStatus) (\s a -> s {jobStatus = a} :: GetTextDetectionResponse)
{-# DEPRECATED gtdrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrsResponseStatus :: Lens.Lens' GetTextDetectionResponse Lude.Int
gtdrsResponseStatus = Lens.lens (responseStatus :: GetTextDetectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTextDetectionResponse)
{-# DEPRECATED gtdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
