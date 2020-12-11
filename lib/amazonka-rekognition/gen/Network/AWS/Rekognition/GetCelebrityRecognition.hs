{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.GetCelebrityRecognition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the celebrity recognition results for a Amazon Rekognition Video analysis started by 'StartCelebrityRecognition' .
--
-- Celebrity recognition in a video is an asynchronous operation. Analysis is started by a call to 'StartCelebrityRecognition' which returns a job identifier (@JobId@ ). When the celebrity recognition operation finishes, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartCelebrityRecognition@ . To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call @GetCelebrityDetection@ and pass the job identifier (@JobId@ ) from the initial call to @StartCelebrityDetection@ .
-- For more information, see Working With Stored Videos in the Amazon Rekognition Developer Guide.
-- @GetCelebrityRecognition@ returns detected celebrities and the time(s) they are detected in an array (@Celebrities@ ) of 'CelebrityRecognition' objects. Each @CelebrityRecognition@ contains information about the celebrity in a 'CelebrityDetail' object and the time, @Timestamp@ , the celebrity was detected.
-- By default, the @Celebrities@ array is sorted by time (milliseconds from the start of the video). You can also sort the array by celebrity by specifying the value @ID@ in the @SortBy@ input parameter.
-- The @CelebrityDetail@ object includes the celebrity identifer and additional information urls. If you don't store the additional information urls, you can get them later by calling 'GetCelebrityInfo' with the celebrity identifer.
-- No information is returned for faces not recognized as celebrities.
-- Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetCelebrityDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetCelebrityRecognition@ .
module Network.AWS.Rekognition.GetCelebrityRecognition
  ( -- * Creating a request
    GetCelebrityRecognition (..),
    mkGetCelebrityRecognition,

    -- ** Request lenses
    gcrNextToken,
    gcrMaxResults,
    gcrSortBy,
    gcrJobId,

    -- * Destructuring the response
    GetCelebrityRecognitionResponse (..),
    mkGetCelebrityRecognitionResponse,

    -- ** Response lenses
    gcrrsNextToken,
    gcrrsVideoMetadata,
    gcrrsStatusMessage,
    gcrrsCelebrities,
    gcrrsJobStatus,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCelebrityRecognition' smart constructor.
data GetCelebrityRecognition = GetCelebrityRecognition'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy ::
      Lude.Maybe CelebrityRecognitionSortBy,
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

-- | Creates a value of 'GetCelebrityRecognition' with the minimum fields required to make a request.
--
-- * 'jobId' - Job identifier for the required celebrity recognition analysis. You can get the job identifer from a call to @StartCelebrityRecognition@ .
-- * 'maxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
-- * 'nextToken' - If the previous response was incomplete (because there is more recognized celebrities to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of celebrities.
-- * 'sortBy' - Sort to use for celebrities returned in @Celebrities@ field. Specify @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by the time the celebrity was recognized.
mkGetCelebrityRecognition ::
  -- | 'jobId'
  Lude.Text ->
  GetCelebrityRecognition
mkGetCelebrityRecognition pJobId_ =
  GetCelebrityRecognition'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing,
      jobId = pJobId_
    }

-- | If the previous response was incomplete (because there is more recognized celebrities to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of celebrities.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrNextToken :: Lens.Lens' GetCelebrityRecognition (Lude.Maybe Lude.Text)
gcrNextToken = Lens.lens (nextToken :: GetCelebrityRecognition -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCelebrityRecognition)
{-# DEPRECATED gcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrMaxResults :: Lens.Lens' GetCelebrityRecognition (Lude.Maybe Lude.Natural)
gcrMaxResults = Lens.lens (maxResults :: GetCelebrityRecognition -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetCelebrityRecognition)
{-# DEPRECATED gcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sort to use for celebrities returned in @Celebrities@ field. Specify @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by the time the celebrity was recognized.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrSortBy :: Lens.Lens' GetCelebrityRecognition (Lude.Maybe CelebrityRecognitionSortBy)
gcrSortBy = Lens.lens (sortBy :: GetCelebrityRecognition -> Lude.Maybe CelebrityRecognitionSortBy) (\s a -> s {sortBy = a} :: GetCelebrityRecognition)
{-# DEPRECATED gcrSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | Job identifier for the required celebrity recognition analysis. You can get the job identifer from a call to @StartCelebrityRecognition@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrJobId :: Lens.Lens' GetCelebrityRecognition Lude.Text
gcrJobId = Lens.lens (jobId :: GetCelebrityRecognition -> Lude.Text) (\s a -> s {jobId = a} :: GetCelebrityRecognition)
{-# DEPRECATED gcrJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetCelebrityRecognition where
  type Rs GetCelebrityRecognition = GetCelebrityRecognitionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCelebrityRecognitionResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "VideoMetadata")
            Lude.<*> (x Lude..?> "StatusMessage")
            Lude.<*> (x Lude..?> "Celebrities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCelebrityRecognition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.GetCelebrityRecognition" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCelebrityRecognition where
  toJSON GetCelebrityRecognition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy,
            Lude.Just ("JobId" Lude..= jobId)
          ]
      )

instance Lude.ToPath GetCelebrityRecognition where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCelebrityRecognition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCelebrityRecognitionResponse' smart constructor.
data GetCelebrityRecognitionResponse = GetCelebrityRecognitionResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    videoMetadata ::
      Lude.Maybe VideoMetadata,
    statusMessage ::
      Lude.Maybe Lude.Text,
    celebrities ::
      Lude.Maybe
        [CelebrityRecognition],
    jobStatus ::
      Lude.Maybe VideoJobStatus,
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

-- | Creates a value of 'GetCelebrityRecognitionResponse' with the minimum fields required to make a request.
--
-- * 'celebrities' - Array of celebrities recognized in the video.
-- * 'jobStatus' - The current status of the celebrity recognition job.
-- * 'nextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of celebrities.
-- * 'responseStatus' - The response status code.
-- * 'statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
-- * 'videoMetadata' - Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
mkGetCelebrityRecognitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCelebrityRecognitionResponse
mkGetCelebrityRecognitionResponse pResponseStatus_ =
  GetCelebrityRecognitionResponse'
    { nextToken = Lude.Nothing,
      videoMetadata = Lude.Nothing,
      statusMessage = Lude.Nothing,
      celebrities = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of celebrities.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsNextToken :: Lens.Lens' GetCelebrityRecognitionResponse (Lude.Maybe Lude.Text)
gcrrsNextToken = Lens.lens (nextToken :: GetCelebrityRecognitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCelebrityRecognitionResponse)
{-# DEPRECATED gcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsVideoMetadata :: Lens.Lens' GetCelebrityRecognitionResponse (Lude.Maybe VideoMetadata)
gcrrsVideoMetadata = Lens.lens (videoMetadata :: GetCelebrityRecognitionResponse -> Lude.Maybe VideoMetadata) (\s a -> s {videoMetadata = a} :: GetCelebrityRecognitionResponse)
{-# DEPRECATED gcrrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsStatusMessage :: Lens.Lens' GetCelebrityRecognitionResponse (Lude.Maybe Lude.Text)
gcrrsStatusMessage = Lens.lens (statusMessage :: GetCelebrityRecognitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: GetCelebrityRecognitionResponse)
{-# DEPRECATED gcrrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Array of celebrities recognized in the video.
--
-- /Note:/ Consider using 'celebrities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCelebrities :: Lens.Lens' GetCelebrityRecognitionResponse (Lude.Maybe [CelebrityRecognition])
gcrrsCelebrities = Lens.lens (celebrities :: GetCelebrityRecognitionResponse -> Lude.Maybe [CelebrityRecognition]) (\s a -> s {celebrities = a} :: GetCelebrityRecognitionResponse)
{-# DEPRECATED gcrrsCelebrities "Use generic-lens or generic-optics with 'celebrities' instead." #-}

-- | The current status of the celebrity recognition job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsJobStatus :: Lens.Lens' GetCelebrityRecognitionResponse (Lude.Maybe VideoJobStatus)
gcrrsJobStatus = Lens.lens (jobStatus :: GetCelebrityRecognitionResponse -> Lude.Maybe VideoJobStatus) (\s a -> s {jobStatus = a} :: GetCelebrityRecognitionResponse)
{-# DEPRECATED gcrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetCelebrityRecognitionResponse Lude.Int
gcrrsResponseStatus = Lens.lens (responseStatus :: GetCelebrityRecognitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCelebrityRecognitionResponse)
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
