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
    gtdMaxResults,
    gtdNextToken,

    -- * Destructuring the response
    GetTextDetectionResponse (..),
    mkGetTextDetectionResponse,

    -- ** Response lenses
    gtdrrsJobStatus,
    gtdrrsNextToken,
    gtdrrsStatusMessage,
    gtdrrsTextDetections,
    gtdrrsTextModelVersion,
    gtdrrsVideoMetadata,
    gtdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTextDetection' smart constructor.
data GetTextDetection = GetTextDetection'
  { -- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartTextDetection@ .
    jobId :: Types.JobId,
    -- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTextDetection' value with any optional fields omitted.
mkGetTextDetection ::
  -- | 'jobId'
  Types.JobId ->
  GetTextDetection
mkGetTextDetection jobId =
  GetTextDetection'
    { jobId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartTextDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdJobId :: Lens.Lens' GetTextDetection Types.JobId
gtdJobId = Lens.field @"jobId"
{-# DEPRECATED gtdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdMaxResults :: Lens.Lens' GetTextDetection (Core.Maybe Core.Natural)
gtdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gtdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdNextToken :: Lens.Lens' GetTextDetection (Core.Maybe Types.PaginationToken)
gtdNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetTextDetection where
  toJSON GetTextDetection {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobId" Core..= jobId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetTextDetection where
  type Rs GetTextDetection = GetTextDetectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.GetTextDetection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTextDetectionResponse'
            Core.<$> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "TextDetections")
            Core.<*> (x Core..:? "TextModelVersion")
            Core.<*> (x Core..:? "VideoMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTextDetectionResponse' smart constructor.
data GetTextDetectionResponse = GetTextDetectionResponse'
  { -- | Current status of the text detection job.
    jobStatus :: Core.Maybe Types.VideoJobStatus,
    -- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | If the job fails, @StatusMessage@ provides a descriptive error message.
    statusMessage :: Core.Maybe Types.StatusMessage,
    -- | An array of text detected in the video. Each element contains the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
    textDetections :: Core.Maybe [Types.TextDetectionResult],
    -- | Version number of the text detection model that was used to detect text.
    textModelVersion :: Core.Maybe Types.TextModelVersion,
    videoMetadata :: Core.Maybe Types.VideoMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTextDetectionResponse' value with any optional fields omitted.
mkGetTextDetectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTextDetectionResponse
mkGetTextDetectionResponse responseStatus =
  GetTextDetectionResponse'
    { jobStatus = Core.Nothing,
      nextToken = Core.Nothing,
      statusMessage = Core.Nothing,
      textDetections = Core.Nothing,
      textModelVersion = Core.Nothing,
      videoMetadata = Core.Nothing,
      responseStatus
    }

-- | Current status of the text detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsJobStatus :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Types.VideoJobStatus)
gtdrrsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED gtdrrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsNextToken :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Types.PaginationToken)
gtdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsStatusMessage :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Types.StatusMessage)
gtdrrsStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED gtdrrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | An array of text detected in the video. Each element contains the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
--
-- /Note:/ Consider using 'textDetections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsTextDetections :: Lens.Lens' GetTextDetectionResponse (Core.Maybe [Types.TextDetectionResult])
gtdrrsTextDetections = Lens.field @"textDetections"
{-# DEPRECATED gtdrrsTextDetections "Use generic-lens or generic-optics with 'textDetections' instead." #-}

-- | Version number of the text detection model that was used to detect text.
--
-- /Note:/ Consider using 'textModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsTextModelVersion :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Types.TextModelVersion)
gtdrrsTextModelVersion = Lens.field @"textModelVersion"
{-# DEPRECATED gtdrrsTextModelVersion "Use generic-lens or generic-optics with 'textModelVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsVideoMetadata :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Types.VideoMetadata)
gtdrrsVideoMetadata = Lens.field @"videoMetadata"
{-# DEPRECATED gtdrrsVideoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsResponseStatus :: Lens.Lens' GetTextDetectionResponse Core.Int
gtdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
