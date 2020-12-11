{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.GetFaceDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets face detection results for a Amazon Rekognition Video analysis started by 'StartFaceDetection' .
--
-- Face detection with Amazon Rekognition Video is an asynchronous operation. You start face detection by calling 'StartFaceDetection' which returns a job identifier (@JobId@ ). When the face detection operation finishes, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartFaceDetection@ . To get the results of the face detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetFaceDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartFaceDetection@ .
-- @GetFaceDetection@ returns an array of detected faces (@Faces@ ) sorted by the time the faces were detected.
-- Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetFaceDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetFaceDetection@ .
module Network.AWS.Rekognition.GetFaceDetection
  ( -- * Creating a request
    GetFaceDetection (..),
    mkGetFaceDetection,

    -- ** Request lenses
    gfdNextToken,
    gfdMaxResults,
    gfdJobId,

    -- * Destructuring the response
    GetFaceDetectionResponse (..),
    mkGetFaceDetectionResponse,

    -- ** Response lenses
    gfdrsNextToken,
    gfdrsVideoMetadata,
    gfdrsStatusMessage,
    gfdrsFaces,
    gfdrsJobStatus,
    gfdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFaceDetection' smart constructor.
data GetFaceDetection = GetFaceDetection'
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

-- | Creates a value of 'GetFaceDetection' with the minimum fields required to make a request.
--
-- * 'jobId' - Unique identifier for the face detection job. The @JobId@ is returned from @StartFaceDetection@ .
-- * 'maxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
-- * 'nextToken' - If the previous response was incomplete (because there are more faces to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of faces.
mkGetFaceDetection ::
  -- | 'jobId'
  Lude.Text ->
  GetFaceDetection
mkGetFaceDetection pJobId_ =
  GetFaceDetection'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there are more faces to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of faces.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdNextToken :: Lens.Lens' GetFaceDetection (Lude.Maybe Lude.Text)
gfdNextToken = Lens.lens (nextToken :: GetFaceDetection -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetFaceDetection)
{-# DEPRECATED gfdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdMaxResults :: Lens.Lens' GetFaceDetection (Lude.Maybe Lude.Natural)
gfdMaxResults = Lens.lens (maxResults :: GetFaceDetection -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetFaceDetection)
{-# DEPRECATED gfdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Unique identifier for the face detection job. The @JobId@ is returned from @StartFaceDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdJobId :: Lens.Lens' GetFaceDetection Lude.Text
gfdJobId = Lens.lens (jobId :: GetFaceDetection -> Lude.Text) (\s a -> s {jobId = a} :: GetFaceDetection)
{-# DEPRECATED gfdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetFaceDetection where
  type Rs GetFaceDetection = GetFaceDetectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFaceDetectionResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "VideoMetadata")
            Lude.<*> (x Lude..?> "StatusMessage")
            Lude.<*> (x Lude..?> "Faces" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFaceDetection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.GetFaceDetection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetFaceDetection where
  toJSON GetFaceDetection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("JobId" Lude..= jobId)
          ]
      )

instance Lude.ToPath GetFaceDetection where
  toPath = Lude.const "/"

instance Lude.ToQuery GetFaceDetection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFaceDetectionResponse' smart constructor.
data GetFaceDetectionResponse = GetFaceDetectionResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    videoMetadata :: Lude.Maybe VideoMetadata,
    statusMessage :: Lude.Maybe Lude.Text,
    faces :: Lude.Maybe [FaceDetection],
    jobStatus :: Lude.Maybe VideoJobStatus,
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

-- | Creates a value of 'GetFaceDetectionResponse' with the minimum fields required to make a request.
--
-- * 'faces' - An array of faces detected in the video. Each element contains a detected face's details and the time, in milliseconds from the start of the video, the face was detected.
-- * 'jobStatus' - The current status of the face detection job.
-- * 'nextToken' - If the response is truncated, Amazon Rekognition returns this token that you can use in the subsequent request to retrieve the next set of faces.
-- * 'responseStatus' - The response status code.
-- * 'statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
-- * 'videoMetadata' - Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
mkGetFaceDetectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFaceDetectionResponse
mkGetFaceDetectionResponse pResponseStatus_ =
  GetFaceDetectionResponse'
    { nextToken = Lude.Nothing,
      videoMetadata = Lude.Nothing,
      statusMessage = Lude.Nothing,
      faces = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon Rekognition returns this token that you can use in the subsequent request to retrieve the next set of faces.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsNextToken :: Lens.Lens' GetFaceDetectionResponse (Lude.Maybe Lude.Text)
gfdrsNextToken = Lens.lens (nextToken :: GetFaceDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetFaceDetectionResponse)
{-# DEPRECATED gfdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsVideoMetadata :: Lens.Lens' GetFaceDetectionResponse (Lude.Maybe VideoMetadata)
gfdrsVideoMetadata = Lens.lens (videoMetadata :: GetFaceDetectionResponse -> Lude.Maybe VideoMetadata) (\s a -> s {videoMetadata = a} :: GetFaceDetectionResponse)
{-# DEPRECATED gfdrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsStatusMessage :: Lens.Lens' GetFaceDetectionResponse (Lude.Maybe Lude.Text)
gfdrsStatusMessage = Lens.lens (statusMessage :: GetFaceDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: GetFaceDetectionResponse)
{-# DEPRECATED gfdrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | An array of faces detected in the video. Each element contains a detected face's details and the time, in milliseconds from the start of the video, the face was detected.
--
-- /Note:/ Consider using 'faces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsFaces :: Lens.Lens' GetFaceDetectionResponse (Lude.Maybe [FaceDetection])
gfdrsFaces = Lens.lens (faces :: GetFaceDetectionResponse -> Lude.Maybe [FaceDetection]) (\s a -> s {faces = a} :: GetFaceDetectionResponse)
{-# DEPRECATED gfdrsFaces "Use generic-lens or generic-optics with 'faces' instead." #-}

-- | The current status of the face detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsJobStatus :: Lens.Lens' GetFaceDetectionResponse (Lude.Maybe VideoJobStatus)
gfdrsJobStatus = Lens.lens (jobStatus :: GetFaceDetectionResponse -> Lude.Maybe VideoJobStatus) (\s a -> s {jobStatus = a} :: GetFaceDetectionResponse)
{-# DEPRECATED gfdrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfdrsResponseStatus :: Lens.Lens' GetFaceDetectionResponse Lude.Int
gfdrsResponseStatus = Lens.lens (responseStatus :: GetFaceDetectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFaceDetectionResponse)
{-# DEPRECATED gfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
