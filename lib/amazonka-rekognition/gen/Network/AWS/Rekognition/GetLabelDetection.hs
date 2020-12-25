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
    gldMaxResults,
    gldNextToken,
    gldSortBy,

    -- * Destructuring the response
    GetLabelDetectionResponse (..),
    mkGetLabelDetectionResponse,

    -- ** Response lenses
    gldrrsJobStatus,
    gldrrsLabelModelVersion,
    gldrrsLabels,
    gldrrsNextToken,
    gldrrsStatusMessage,
    gldrrsVideoMetadata,
    gldrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLabelDetection' smart constructor.
data GetLabelDetection = GetLabelDetection'
  { -- | Job identifier for the label detection operation for which you want results returned. You get the job identifer from an initial call to @StartlabelDetection@ .
    jobId :: Types.JobId,
    -- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of labels.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
    sortBy :: Core.Maybe Types.LabelDetectionSortBy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLabelDetection' value with any optional fields omitted.
mkGetLabelDetection ::
  -- | 'jobId'
  Types.JobId ->
  GetLabelDetection
mkGetLabelDetection jobId =
  GetLabelDetection'
    { jobId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | Job identifier for the label detection operation for which you want results returned. You get the job identifer from an initial call to @StartlabelDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldJobId :: Lens.Lens' GetLabelDetection Types.JobId
gldJobId = Lens.field @"jobId"
{-# DEPRECATED gldJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldMaxResults :: Lens.Lens' GetLabelDetection (Core.Maybe Core.Natural)
gldMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of labels.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldNextToken :: Lens.Lens' GetLabelDetection (Core.Maybe Types.PaginationToken)
gldNextToken = Lens.field @"nextToken"
{-# DEPRECATED gldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldSortBy :: Lens.Lens' GetLabelDetection (Core.Maybe Types.LabelDetectionSortBy)
gldSortBy = Lens.field @"sortBy"
{-# DEPRECATED gldSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Core.FromJSON GetLabelDetection where
  toJSON GetLabelDetection {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobId" Core..= jobId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.AWSRequest GetLabelDetection where
  type Rs GetLabelDetection = GetLabelDetectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.GetLabelDetection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLabelDetectionResponse'
            Core.<$> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "LabelModelVersion")
            Core.<*> (x Core..:? "Labels")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "VideoMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetLabelDetectionResponse' smart constructor.
data GetLabelDetectionResponse = GetLabelDetectionResponse'
  { -- | The current status of the label detection job.
    jobStatus :: Core.Maybe Types.VideoJobStatus,
    -- | Version number of the label detection model that was used to detect labels.
    labelModelVersion :: Core.Maybe Types.String,
    -- | An array of labels detected in the video. Each element contains the detected label and the time, in milliseconds from the start of the video, that the label was detected.
    labels :: Core.Maybe [Types.LabelDetection],
    -- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of labels.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Core.Maybe Types.StatusMessage,
    -- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
    videoMetadata :: Core.Maybe Types.VideoMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLabelDetectionResponse' value with any optional fields omitted.
mkGetLabelDetectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLabelDetectionResponse
mkGetLabelDetectionResponse responseStatus =
  GetLabelDetectionResponse'
    { jobStatus = Core.Nothing,
      labelModelVersion = Core.Nothing,
      labels = Core.Nothing,
      nextToken = Core.Nothing,
      statusMessage = Core.Nothing,
      videoMetadata = Core.Nothing,
      responseStatus
    }

-- | The current status of the label detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsJobStatus :: Lens.Lens' GetLabelDetectionResponse (Core.Maybe Types.VideoJobStatus)
gldrrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED gldrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | Version number of the label detection model that was used to detect labels.
--
-- /Note:/ Consider using 'labelModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsLabelModelVersion :: Lens.Lens' GetLabelDetectionResponse (Core.Maybe Types.String)
gldrrsLabelModelVersion = Lens.field @"labelModelVersion"
{-# DEPRECATED gldrrsLabelModelVersion "Use generic-lens or generic-optics with 'labelModelVersion' instead." #-}

-- | An array of labels detected in the video. Each element contains the detected label and the time, in milliseconds from the start of the video, that the label was detected.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsLabels :: Lens.Lens' GetLabelDetectionResponse (Core.Maybe [Types.LabelDetection])
gldrrsLabels = Lens.field @"labels"
{-# DEPRECATED gldrrsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of labels.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsNextToken :: Lens.Lens' GetLabelDetectionResponse (Core.Maybe Types.PaginationToken)
gldrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gldrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsStatusMessage :: Lens.Lens' GetLabelDetectionResponse (Core.Maybe Types.StatusMessage)
gldrrsStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED gldrrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsVideoMetadata :: Lens.Lens' GetLabelDetectionResponse (Core.Maybe Types.VideoMetadata)
gldrrsVideoMetadata = Lens.field @"videoMetadata"
{-# DEPRECATED gldrrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldrrsResponseStatus :: Lens.Lens' GetLabelDetectionResponse Core.Int
gldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
