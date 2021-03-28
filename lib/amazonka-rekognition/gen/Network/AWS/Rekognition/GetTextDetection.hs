{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTextDetection (..)
    , mkGetTextDetection
    -- ** Request lenses
    , gtdJobId
    , gtdMaxResults
    , gtdNextToken

    -- * Destructuring the response
    , GetTextDetectionResponse (..)
    , mkGetTextDetectionResponse
    -- ** Response lenses
    , gtdrrsJobStatus
    , gtdrrsNextToken
    , gtdrrsStatusMessage
    , gtdrrsTextDetections
    , gtdrrsTextModelVersion
    , gtdrrsVideoMetadata
    , gtdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTextDetection' smart constructor.
data GetTextDetection = GetTextDetection'
  { jobId :: Types.JobId
    -- ^ Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartTextDetection@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of results to return per paginated call. The largest value you can specify is 1000.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTextDetection' value with any optional fields omitted.
mkGetTextDetection
    :: Types.JobId -- ^ 'jobId'
    -> GetTextDetection
mkGetTextDetection jobId
  = GetTextDetection'{jobId, maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | Job identifier for the text detection operation for which you want results returned. You get the job identifer from an initial call to @StartTextDetection@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdJobId :: Lens.Lens' GetTextDetection Types.JobId
gtdJobId = Lens.field @"jobId"
{-# INLINEABLE gtdJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdMaxResults :: Lens.Lens' GetTextDetection (Core.Maybe Core.Natural)
gtdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gtdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was incomplete (because there are more labels to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdNextToken :: Lens.Lens' GetTextDetection (Core.Maybe Types.PaginationToken)
gtdNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetTextDetection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTextDetection where
        toHeaders GetTextDetection{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.GetTextDetection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTextDetection where
        toJSON GetTextDetection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobId" Core..= jobId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetTextDetection where
        type Rs GetTextDetection = GetTextDetectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTextDetectionResponse' Core.<$>
                   (x Core..:? "JobStatus") Core.<*> x Core..:? "NextToken" Core.<*>
                     x Core..:? "StatusMessage"
                     Core.<*> x Core..:? "TextDetections"
                     Core.<*> x Core..:? "TextModelVersion"
                     Core.<*> x Core..:? "VideoMetadata"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTextDetectionResponse' smart constructor.
data GetTextDetectionResponse = GetTextDetectionResponse'
  { jobStatus :: Core.Maybe Types.VideoJobStatus
    -- ^ Current status of the text detection job.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
  , statusMessage :: Core.Maybe Types.StatusMessage
    -- ^ If the job fails, @StatusMessage@ provides a descriptive error message.
  , textDetections :: Core.Maybe [Types.TextDetectionResult]
    -- ^ An array of text detected in the video. Each element contains the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
  , textModelVersion :: Core.Maybe Core.Text
    -- ^ Version number of the text detection model that was used to detect text.
  , videoMetadata :: Core.Maybe Types.VideoMetadata
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTextDetectionResponse' value with any optional fields omitted.
mkGetTextDetectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTextDetectionResponse
mkGetTextDetectionResponse responseStatus
  = GetTextDetectionResponse'{jobStatus = Core.Nothing,
                              nextToken = Core.Nothing, statusMessage = Core.Nothing,
                              textDetections = Core.Nothing, textModelVersion = Core.Nothing,
                              videoMetadata = Core.Nothing, responseStatus}

-- | Current status of the text detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsJobStatus :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Types.VideoJobStatus)
gtdrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE gtdrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of text.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsNextToken :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Types.PaginationToken)
gtdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsStatusMessage :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Types.StatusMessage)
gtdrrsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE gtdrrsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | An array of text detected in the video. Each element contains the detected text, the time in milliseconds from the start of the video that the text was detected, and where it was detected on the screen.
--
-- /Note:/ Consider using 'textDetections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsTextDetections :: Lens.Lens' GetTextDetectionResponse (Core.Maybe [Types.TextDetectionResult])
gtdrrsTextDetections = Lens.field @"textDetections"
{-# INLINEABLE gtdrrsTextDetections #-}
{-# DEPRECATED textDetections "Use generic-lens or generic-optics with 'textDetections' instead"  #-}

-- | Version number of the text detection model that was used to detect text.
--
-- /Note:/ Consider using 'textModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsTextModelVersion :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Core.Text)
gtdrrsTextModelVersion = Lens.field @"textModelVersion"
{-# INLINEABLE gtdrrsTextModelVersion #-}
{-# DEPRECATED textModelVersion "Use generic-lens or generic-optics with 'textModelVersion' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsVideoMetadata :: Lens.Lens' GetTextDetectionResponse (Core.Maybe Types.VideoMetadata)
gtdrrsVideoMetadata = Lens.field @"videoMetadata"
{-# INLINEABLE gtdrrsVideoMetadata #-}
{-# DEPRECATED videoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdrrsResponseStatus :: Lens.Lens' GetTextDetectionResponse Core.Int
gtdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
