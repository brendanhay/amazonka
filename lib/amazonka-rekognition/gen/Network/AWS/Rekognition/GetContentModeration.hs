{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.GetContentModeration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the unsafe content analysis results for a Amazon Rekognition Video analysis started by 'StartContentModeration' .
--
-- Unsafe content analysis of a video is an asynchronous operation. You start analysis by calling 'StartContentModeration' which returns a job identifier (@JobId@ ). When analysis finishes, Amazon Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartContentModeration@ . To get the results of the unsafe content analysis, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call @GetContentModeration@ and pass the job identifier (@JobId@ ) from the initial call to @StartContentModeration@ .
-- For more information, see Working with Stored Videos in the Amazon Rekognition Devlopers Guide.
-- @GetContentModeration@ returns detected unsafe content labels, and the time they are detected, in an array, @ModerationLabels@ , of 'ContentModerationDetection' objects.
-- By default, the moderated labels are returned sorted by time, in milliseconds from the start of the video. You can also sort them by moderated label by specifying @NAME@ for the @SortBy@ input parameter.
-- Since video analysis can return a large number of results, use the @MaxResults@ parameter to limit the number of labels returned in a single call to @GetContentModeration@ . If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetContentModeration@ and populate the @NextToken@ request parameter with the value of @NextToken@ returned from the previous call to @GetContentModeration@ .
-- For more information, see Detecting Unsafe Content in the Amazon Rekognition Developer Guide.
module Network.AWS.Rekognition.GetContentModeration
  ( -- * Creating a request
    GetContentModeration (..),
    mkGetContentModeration,

    -- ** Request lenses
    gcmJobId,
    gcmNextToken,
    gcmMaxResults,
    gcmSortBy,

    -- * Destructuring the response
    GetContentModerationResponse (..),
    mkGetContentModerationResponse,

    -- ** Response lenses
    gcmrsNextToken,
    gcmrsVideoMetadata,
    gcmrsStatusMessage,
    gcmrsJobStatus,
    gcmrsModerationModelVersion,
    gcmrsModerationLabels,
    gcmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContentModeration' smart constructor.
data GetContentModeration = GetContentModeration'
  { -- | The identifier for the unsafe content job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
    jobId :: Lude.Text,
    -- | If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of unsafe content labels.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Sort to use for elements in the @ModerationLabelDetections@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
    sortBy :: Lude.Maybe ContentModerationSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContentModeration' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier for the unsafe content job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
-- * 'nextToken' - If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of unsafe content labels.
-- * 'maxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
-- * 'sortBy' - Sort to use for elements in the @ModerationLabelDetections@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
mkGetContentModeration ::
  -- | 'jobId'
  Lude.Text ->
  GetContentModeration
mkGetContentModeration pJobId_ =
  GetContentModeration'
    { jobId = pJobId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | The identifier for the unsafe content job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmJobId :: Lens.Lens' GetContentModeration Lude.Text
gcmJobId = Lens.lens (jobId :: GetContentModeration -> Lude.Text) (\s a -> s {jobId = a} :: GetContentModeration)
{-# DEPRECATED gcmJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of unsafe content labels.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmNextToken :: Lens.Lens' GetContentModeration (Lude.Maybe Lude.Text)
gcmNextToken = Lens.lens (nextToken :: GetContentModeration -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetContentModeration)
{-# DEPRECATED gcmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmMaxResults :: Lens.Lens' GetContentModeration (Lude.Maybe Lude.Natural)
gcmMaxResults = Lens.lens (maxResults :: GetContentModeration -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetContentModeration)
{-# DEPRECATED gcmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sort to use for elements in the @ModerationLabelDetections@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmSortBy :: Lens.Lens' GetContentModeration (Lude.Maybe ContentModerationSortBy)
gcmSortBy = Lens.lens (sortBy :: GetContentModeration -> Lude.Maybe ContentModerationSortBy) (\s a -> s {sortBy = a} :: GetContentModeration)
{-# DEPRECATED gcmSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Lude.AWSRequest GetContentModeration where
  type Rs GetContentModeration = GetContentModerationResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContentModerationResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "VideoMetadata")
            Lude.<*> (x Lude..?> "StatusMessage")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (x Lude..?> "ModerationModelVersion")
            Lude.<*> (x Lude..?> "ModerationLabels" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContentModeration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.GetContentModeration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContentModeration where
  toJSON GetContentModeration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobId" Lude..= jobId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath GetContentModeration where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContentModeration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContentModerationResponse' smart constructor.
data GetContentModerationResponse = GetContentModerationResponse'
  { -- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of unsafe content labels.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from @GetContentModeration@ .
    videoMetadata :: Lude.Maybe VideoMetadata,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The current status of the unsafe content analysis job.
    jobStatus :: Lude.Maybe VideoJobStatus,
    -- | Version number of the moderation detection model that was used to detect unsafe content.
    moderationModelVersion :: Lude.Maybe Lude.Text,
    -- | The detected unsafe content labels and the time(s) they were detected.
    moderationLabels :: Lude.Maybe [ContentModerationDetection],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContentModerationResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of unsafe content labels.
-- * 'videoMetadata' - Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from @GetContentModeration@ .
-- * 'statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
-- * 'jobStatus' - The current status of the unsafe content analysis job.
-- * 'moderationModelVersion' - Version number of the moderation detection model that was used to detect unsafe content.
-- * 'moderationLabels' - The detected unsafe content labels and the time(s) they were detected.
-- * 'responseStatus' - The response status code.
mkGetContentModerationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContentModerationResponse
mkGetContentModerationResponse pResponseStatus_ =
  GetContentModerationResponse'
    { nextToken = Lude.Nothing,
      videoMetadata = Lude.Nothing,
      statusMessage = Lude.Nothing,
      jobStatus = Lude.Nothing,
      moderationModelVersion = Lude.Nothing,
      moderationLabels = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of unsafe content labels.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsNextToken :: Lens.Lens' GetContentModerationResponse (Lude.Maybe Lude.Text)
gcmrsNextToken = Lens.lens (nextToken :: GetContentModerationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetContentModerationResponse)
{-# DEPRECATED gcmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from @GetContentModeration@ .
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsVideoMetadata :: Lens.Lens' GetContentModerationResponse (Lude.Maybe VideoMetadata)
gcmrsVideoMetadata = Lens.lens (videoMetadata :: GetContentModerationResponse -> Lude.Maybe VideoMetadata) (\s a -> s {videoMetadata = a} :: GetContentModerationResponse)
{-# DEPRECATED gcmrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsStatusMessage :: Lens.Lens' GetContentModerationResponse (Lude.Maybe Lude.Text)
gcmrsStatusMessage = Lens.lens (statusMessage :: GetContentModerationResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: GetContentModerationResponse)
{-# DEPRECATED gcmrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The current status of the unsafe content analysis job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsJobStatus :: Lens.Lens' GetContentModerationResponse (Lude.Maybe VideoJobStatus)
gcmrsJobStatus = Lens.lens (jobStatus :: GetContentModerationResponse -> Lude.Maybe VideoJobStatus) (\s a -> s {jobStatus = a} :: GetContentModerationResponse)
{-# DEPRECATED gcmrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | Version number of the moderation detection model that was used to detect unsafe content.
--
-- /Note:/ Consider using 'moderationModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsModerationModelVersion :: Lens.Lens' GetContentModerationResponse (Lude.Maybe Lude.Text)
gcmrsModerationModelVersion = Lens.lens (moderationModelVersion :: GetContentModerationResponse -> Lude.Maybe Lude.Text) (\s a -> s {moderationModelVersion = a} :: GetContentModerationResponse)
{-# DEPRECATED gcmrsModerationModelVersion "Use generic-lens or generic-optics with 'moderationModelVersion' instead." #-}

-- | The detected unsafe content labels and the time(s) they were detected.
--
-- /Note:/ Consider using 'moderationLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsModerationLabels :: Lens.Lens' GetContentModerationResponse (Lude.Maybe [ContentModerationDetection])
gcmrsModerationLabels = Lens.lens (moderationLabels :: GetContentModerationResponse -> Lude.Maybe [ContentModerationDetection]) (\s a -> s {moderationLabels = a} :: GetContentModerationResponse)
{-# DEPRECATED gcmrsModerationLabels "Use generic-lens or generic-optics with 'moderationLabels' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsResponseStatus :: Lens.Lens' GetContentModerationResponse Lude.Int
gcmrsResponseStatus = Lens.lens (responseStatus :: GetContentModerationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContentModerationResponse)
{-# DEPRECATED gcmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
