{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.GetPersonTracking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the path tracking results of a Amazon Rekognition Video analysis started by 'StartPersonTracking' .
--
-- The person path tracking operation is started by a call to @StartPersonTracking@ which returns a job identifier (@JobId@ ). When the operation finishes, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartPersonTracking@ .
-- To get the results of the person path tracking operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetPersonTracking' and pass the job identifier (@JobId@ ) from the initial call to @StartPersonTracking@ .
-- @GetPersonTracking@ returns an array, @Persons@ , of tracked persons and the time(s) their paths were tracked in the video.
-- By default, the array is sorted by the time(s) a person's path is tracked in the video. You can sort by tracked persons by specifying @INDEX@ for the @SortBy@ input parameter.
-- Use the @MaxResults@ parameter to limit the number of items returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetPersonTracking@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetPersonTracking@ .
module Network.AWS.Rekognition.GetPersonTracking
  ( -- * Creating a request
    GetPersonTracking (..),
    mkGetPersonTracking,

    -- ** Request lenses
    gptJobId,
    gptNextToken,
    gptMaxResults,
    gptSortBy,

    -- * Destructuring the response
    GetPersonTrackingResponse (..),
    mkGetPersonTrackingResponse,

    -- ** Response lenses
    gptrsNextToken,
    gptrsVideoMetadata,
    gptrsStatusMessage,
    gptrsJobStatus,
    gptrsPersons,
    gptrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPersonTracking' smart constructor.
data GetPersonTracking = GetPersonTracking'
  { -- | The identifier for a job that tracks persons in a video. You get the @JobId@ from a call to @StartPersonTracking@ .
    jobId :: Lude.Text,
    -- | If the previous response was incomplete (because there are more persons to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of persons.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort array elements by the time persons are detected. Use @INDEX@ to sort by the tracked persons. If you sort by @INDEX@ , the array elements for each person are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
    sortBy :: Lude.Maybe PersonTrackingSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPersonTracking' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier for a job that tracks persons in a video. You get the @JobId@ from a call to @StartPersonTracking@ .
-- * 'nextToken' - If the previous response was incomplete (because there are more persons to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of persons.
-- * 'maxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
-- * 'sortBy' - Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort array elements by the time persons are detected. Use @INDEX@ to sort by the tracked persons. If you sort by @INDEX@ , the array elements for each person are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
mkGetPersonTracking ::
  -- | 'jobId'
  Lude.Text ->
  GetPersonTracking
mkGetPersonTracking pJobId_ =
  GetPersonTracking'
    { jobId = pJobId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | The identifier for a job that tracks persons in a video. You get the @JobId@ from a call to @StartPersonTracking@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptJobId :: Lens.Lens' GetPersonTracking Lude.Text
gptJobId = Lens.lens (jobId :: GetPersonTracking -> Lude.Text) (\s a -> s {jobId = a} :: GetPersonTracking)
{-# DEPRECATED gptJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | If the previous response was incomplete (because there are more persons to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of persons.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptNextToken :: Lens.Lens' GetPersonTracking (Lude.Maybe Lude.Text)
gptNextToken = Lens.lens (nextToken :: GetPersonTracking -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetPersonTracking)
{-# DEPRECATED gptNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptMaxResults :: Lens.Lens' GetPersonTracking (Lude.Maybe Lude.Natural)
gptMaxResults = Lens.lens (maxResults :: GetPersonTracking -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetPersonTracking)
{-# DEPRECATED gptMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort array elements by the time persons are detected. Use @INDEX@ to sort by the tracked persons. If you sort by @INDEX@ , the array elements for each person are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptSortBy :: Lens.Lens' GetPersonTracking (Lude.Maybe PersonTrackingSortBy)
gptSortBy = Lens.lens (sortBy :: GetPersonTracking -> Lude.Maybe PersonTrackingSortBy) (\s a -> s {sortBy = a} :: GetPersonTracking)
{-# DEPRECATED gptSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Lude.AWSRequest GetPersonTracking where
  type Rs GetPersonTracking = GetPersonTrackingResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPersonTrackingResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "VideoMetadata")
            Lude.<*> (x Lude..?> "StatusMessage")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (x Lude..?> "Persons" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPersonTracking where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.GetPersonTracking" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPersonTracking where
  toJSON GetPersonTracking' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobId" Lude..= jobId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath GetPersonTracking where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPersonTracking where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPersonTrackingResponse' smart constructor.
data GetPersonTrackingResponse = GetPersonTrackingResponse'
  { -- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of persons.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
    videoMetadata :: Lude.Maybe VideoMetadata,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The current status of the person tracking job.
    jobStatus :: Lude.Maybe VideoJobStatus,
    -- | An array of the persons detected in the video and the time(s) their path was tracked throughout the video. An array element will exist for each time a person's path is tracked.
    persons :: Lude.Maybe [PersonDetection],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPersonTrackingResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of persons.
-- * 'videoMetadata' - Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
-- * 'statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
-- * 'jobStatus' - The current status of the person tracking job.
-- * 'persons' - An array of the persons detected in the video and the time(s) their path was tracked throughout the video. An array element will exist for each time a person's path is tracked.
-- * 'responseStatus' - The response status code.
mkGetPersonTrackingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPersonTrackingResponse
mkGetPersonTrackingResponse pResponseStatus_ =
  GetPersonTrackingResponse'
    { nextToken = Lude.Nothing,
      videoMetadata = Lude.Nothing,
      statusMessage = Lude.Nothing,
      jobStatus = Lude.Nothing,
      persons = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of persons.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrsNextToken :: Lens.Lens' GetPersonTrackingResponse (Lude.Maybe Lude.Text)
gptrsNextToken = Lens.lens (nextToken :: GetPersonTrackingResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetPersonTrackingResponse)
{-# DEPRECATED gptrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrsVideoMetadata :: Lens.Lens' GetPersonTrackingResponse (Lude.Maybe VideoMetadata)
gptrsVideoMetadata = Lens.lens (videoMetadata :: GetPersonTrackingResponse -> Lude.Maybe VideoMetadata) (\s a -> s {videoMetadata = a} :: GetPersonTrackingResponse)
{-# DEPRECATED gptrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrsStatusMessage :: Lens.Lens' GetPersonTrackingResponse (Lude.Maybe Lude.Text)
gptrsStatusMessage = Lens.lens (statusMessage :: GetPersonTrackingResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: GetPersonTrackingResponse)
{-# DEPRECATED gptrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The current status of the person tracking job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrsJobStatus :: Lens.Lens' GetPersonTrackingResponse (Lude.Maybe VideoJobStatus)
gptrsJobStatus = Lens.lens (jobStatus :: GetPersonTrackingResponse -> Lude.Maybe VideoJobStatus) (\s a -> s {jobStatus = a} :: GetPersonTrackingResponse)
{-# DEPRECATED gptrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | An array of the persons detected in the video and the time(s) their path was tracked throughout the video. An array element will exist for each time a person's path is tracked.
--
-- /Note:/ Consider using 'persons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrsPersons :: Lens.Lens' GetPersonTrackingResponse (Lude.Maybe [PersonDetection])
gptrsPersons = Lens.lens (persons :: GetPersonTrackingResponse -> Lude.Maybe [PersonDetection]) (\s a -> s {persons = a} :: GetPersonTrackingResponse)
{-# DEPRECATED gptrsPersons "Use generic-lens or generic-optics with 'persons' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrsResponseStatus :: Lens.Lens' GetPersonTrackingResponse Lude.Int
gptrsResponseStatus = Lens.lens (responseStatus :: GetPersonTrackingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPersonTrackingResponse)
{-# DEPRECATED gptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
