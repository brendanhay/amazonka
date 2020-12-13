{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.GetLabelDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the label detection results of a Amazon Rekognition Video analysis started by 'StartLabelDetection' .
--
-- The label detection operation is started by a call to 'StartLabelDetection' which returns a job identifier (@JobId@ ). When the label detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartlabelDetection@ . To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call 'GetLabelDetection' and pass the job identifier (@JobId@ ) from the initial call to @StartLabelDetection@ .
-- @GetLabelDetection@ returns an array of detected labels (@Labels@ ) sorted by the time the labels were detected. You can also sort by the label name by specifying @NAME@ for the @SortBy@ input parameter.
-- The labels returned include the label name, the percentage confidence in the accuracy of the detected label, and the time the label was detected in the video.
-- The returned labels also include bounding box information for common objects, a hierarchical taxonomy of detected labels, and the version of the label model used for detection.
-- Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetlabelDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetLabelDetection@ .
module Network.AWS.Rekognition.GetLabelDetection
  ( -- * Creating a request
    GetLabelDetection (..),
    mkGetLabelDetection,

    -- ** Request lenses
    gldJobId,
    gldNextToken,
    gldMaxResults,
    gldSortBy,

    -- * Destructuring the response
    GetLabelDetectionResponse (..),
    mkGetLabelDetectionResponse,

    -- ** Response lenses
    gldrsNextToken,
    gldrsVideoMetadata,
    gldrsStatusMessage,
    gldrsLabels,
    gldrsJobStatus,
    gldrsLabelModelVersion,
    gldrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLabelDetection' smart constructor.
data GetLabelDetection = GetLabelDetection'
  { -- | Job identifier for the label detection operation for which you want results returned. You get the job identifer from an initial call to @StartlabelDetection@ .
    jobId :: Lude.Text,
    -- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of labels.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
    sortBy :: Lude.Maybe LabelDetectionSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLabelDetection' with the minimum fields required to make a request.
--
-- * 'jobId' - Job identifier for the label detection operation for which you want results returned. You get the job identifer from an initial call to @StartlabelDetection@ .
-- * 'nextToken' - If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of labels.
-- * 'maxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
-- * 'sortBy' - Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
mkGetLabelDetection ::
  -- | 'jobId'
  Lude.Text ->
  GetLabelDetection
mkGetLabelDetection pJobId_ =
  GetLabelDetection'
    { jobId = pJobId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | Job identifier for the label detection operation for which you want results returned. You get the job identifer from an initial call to @StartlabelDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldJobId :: Lens.Lens' GetLabelDetection Lude.Text
gldJobId = Lens.lens (jobId :: GetLabelDetection -> Lude.Text) (\s a -> s {jobId = a} :: GetLabelDetection)
{-# DEPRECATED gldJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of labels.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldNextToken :: Lens.Lens' GetLabelDetection (Lude.Maybe Lude.Text)
gldNextToken = Lens.lens (nextToken :: GetLabelDetection -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetLabelDetection)
{-# DEPRECATED gldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldMaxResults :: Lens.Lens' GetLabelDetection (Lude.Maybe Lude.Natural)
gldMaxResults = Lens.lens (maxResults :: GetLabelDetection -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetLabelDetection)
{-# DEPRECATED gldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldSortBy :: Lens.Lens' GetLabelDetection (Lude.Maybe LabelDetectionSortBy)
gldSortBy = Lens.lens (sortBy :: GetLabelDetection -> Lude.Maybe LabelDetectionSortBy) (\s a -> s {sortBy = a} :: GetLabelDetection)
{-# DEPRECATED gldSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Lude.AWSRequest GetLabelDetection where
  type Rs GetLabelDetection = GetLabelDetectionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLabelDetectionResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "VideoMetadata")
            Lude.<*> (x Lude..?> "StatusMessage")
            Lude.<*> (x Lude..?> "Labels" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (x Lude..?> "LabelModelVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLabelDetection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.GetLabelDetection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLabelDetection where
  toJSON GetLabelDetection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobId" Lude..= jobId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath GetLabelDetection where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLabelDetection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLabelDetectionResponse' smart constructor.
data GetLabelDetectionResponse = GetLabelDetectionResponse'
  { -- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of labels.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
    videoMetadata :: Lude.Maybe VideoMetadata,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | An array of labels detected in the video. Each element contains the detected label and the time, in milliseconds from the start of the video, that the label was detected.
    labels :: Lude.Maybe [LabelDetection],
    -- | The current status of the label detection job.
    jobStatus :: Lude.Maybe VideoJobStatus,
    -- | Version number of the label detection model that was used to detect labels.
    labelModelVersion :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLabelDetectionResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of labels.
-- * 'videoMetadata' - Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
-- * 'statusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
-- * 'labels' - An array of labels detected in the video. Each element contains the detected label and the time, in milliseconds from the start of the video, that the label was detected.
-- * 'jobStatus' - The current status of the label detection job.
-- * 'labelModelVersion' - Version number of the label detection model that was used to detect labels.
-- * 'responseStatus' - The response status code.
mkGetLabelDetectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLabelDetectionResponse
mkGetLabelDetectionResponse pResponseStatus_ =
  GetLabelDetectionResponse'
    { nextToken = Lude.Nothing,
      videoMetadata = Lude.Nothing,
      statusMessage = Lude.Nothing,
      labels = Lude.Nothing,
      jobStatus = Lude.Nothing,
      labelModelVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of labels.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsNextToken :: Lens.Lens' GetLabelDetectionResponse (Lude.Maybe Lude.Text)
gldrsNextToken = Lens.lens (nextToken :: GetLabelDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetLabelDetectionResponse)
{-# DEPRECATED gldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsVideoMetadata :: Lens.Lens' GetLabelDetectionResponse (Lude.Maybe VideoMetadata)
gldrsVideoMetadata = Lens.lens (videoMetadata :: GetLabelDetectionResponse -> Lude.Maybe VideoMetadata) (\s a -> s {videoMetadata = a} :: GetLabelDetectionResponse)
{-# DEPRECATED gldrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsStatusMessage :: Lens.Lens' GetLabelDetectionResponse (Lude.Maybe Lude.Text)
gldrsStatusMessage = Lens.lens (statusMessage :: GetLabelDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: GetLabelDetectionResponse)
{-# DEPRECATED gldrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | An array of labels detected in the video. Each element contains the detected label and the time, in milliseconds from the start of the video, that the label was detected.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsLabels :: Lens.Lens' GetLabelDetectionResponse (Lude.Maybe [LabelDetection])
gldrsLabels = Lens.lens (labels :: GetLabelDetectionResponse -> Lude.Maybe [LabelDetection]) (\s a -> s {labels = a} :: GetLabelDetectionResponse)
{-# DEPRECATED gldrsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The current status of the label detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsJobStatus :: Lens.Lens' GetLabelDetectionResponse (Lude.Maybe VideoJobStatus)
gldrsJobStatus = Lens.lens (jobStatus :: GetLabelDetectionResponse -> Lude.Maybe VideoJobStatus) (\s a -> s {jobStatus = a} :: GetLabelDetectionResponse)
{-# DEPRECATED gldrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | Version number of the label detection model that was used to detect labels.
--
-- /Note:/ Consider using 'labelModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsLabelModelVersion :: Lens.Lens' GetLabelDetectionResponse (Lude.Maybe Lude.Text)
gldrsLabelModelVersion = Lens.lens (labelModelVersion :: GetLabelDetectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {labelModelVersion = a} :: GetLabelDetectionResponse)
{-# DEPRECATED gldrsLabelModelVersion "Use generic-lens or generic-optics with 'labelModelVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrsResponseStatus :: Lens.Lens' GetLabelDetectionResponse Lude.Int
gldrsResponseStatus = Lens.lens (responseStatus :: GetLabelDetectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLabelDetectionResponse)
{-# DEPRECATED gldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
