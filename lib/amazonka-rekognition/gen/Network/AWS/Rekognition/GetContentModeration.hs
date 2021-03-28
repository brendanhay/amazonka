{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetContentModeration (..)
    , mkGetContentModeration
    -- ** Request lenses
    , gcmJobId
    , gcmMaxResults
    , gcmNextToken
    , gcmSortBy

    -- * Destructuring the response
    , GetContentModerationResponse (..)
    , mkGetContentModerationResponse
    -- ** Response lenses
    , gcmrrsJobStatus
    , gcmrrsModerationLabels
    , gcmrrsModerationModelVersion
    , gcmrrsNextToken
    , gcmrrsStatusMessage
    , gcmrrsVideoMetadata
    , gcmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContentModeration' smart constructor.
data GetContentModeration = GetContentModeration'
  { jobId :: Types.JobId
    -- ^ The identifier for the unsafe content job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of unsafe content labels.
  , sortBy :: Core.Maybe Types.ContentModerationSortBy
    -- ^ Sort to use for elements in the @ModerationLabelDetections@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContentModeration' value with any optional fields omitted.
mkGetContentModeration
    :: Types.JobId -- ^ 'jobId'
    -> GetContentModeration
mkGetContentModeration jobId
  = GetContentModeration'{jobId, maxResults = Core.Nothing,
                          nextToken = Core.Nothing, sortBy = Core.Nothing}

-- | The identifier for the unsafe content job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmJobId :: Lens.Lens' GetContentModeration Types.JobId
gcmJobId = Lens.field @"jobId"
{-# INLINEABLE gcmJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmMaxResults :: Lens.Lens' GetContentModeration (Core.Maybe Core.Natural)
gcmMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gcmMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of unsafe content labels.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmNextToken :: Lens.Lens' GetContentModeration (Core.Maybe Types.PaginationToken)
gcmNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcmNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Sort to use for elements in the @ModerationLabelDetections@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmSortBy :: Lens.Lens' GetContentModeration (Core.Maybe Types.ContentModerationSortBy)
gcmSortBy = Lens.field @"sortBy"
{-# INLINEABLE gcmSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

instance Core.ToQuery GetContentModeration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetContentModeration where
        toHeaders GetContentModeration{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.GetContentModeration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetContentModeration where
        toJSON GetContentModeration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobId" Core..= jobId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy])

instance Core.AWSRequest GetContentModeration where
        type Rs GetContentModeration = GetContentModerationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetContentModerationResponse' Core.<$>
                   (x Core..:? "JobStatus") Core.<*> x Core..:? "ModerationLabels"
                     Core.<*> x Core..:? "ModerationModelVersion"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> x Core..:? "StatusMessage"
                     Core.<*> x Core..:? "VideoMetadata"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetContentModerationResponse' smart constructor.
data GetContentModerationResponse = GetContentModerationResponse'
  { jobStatus :: Core.Maybe Types.VideoJobStatus
    -- ^ The current status of the unsafe content analysis job.
  , moderationLabels :: Core.Maybe [Types.ContentModerationDetection]
    -- ^ The detected unsafe content labels and the time(s) they were detected.
  , moderationModelVersion :: Core.Maybe Core.Text
    -- ^ Version number of the moderation detection model that was used to detect unsafe content.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of unsafe content labels. 
  , statusMessage :: Core.Maybe Types.StatusMessage
    -- ^ If the job fails, @StatusMessage@ provides a descriptive error message.
  , videoMetadata :: Core.Maybe Types.VideoMetadata
    -- ^ Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from @GetContentModeration@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContentModerationResponse' value with any optional fields omitted.
mkGetContentModerationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetContentModerationResponse
mkGetContentModerationResponse responseStatus
  = GetContentModerationResponse'{jobStatus = Core.Nothing,
                                  moderationLabels = Core.Nothing,
                                  moderationModelVersion = Core.Nothing, nextToken = Core.Nothing,
                                  statusMessage = Core.Nothing, videoMetadata = Core.Nothing,
                                  responseStatus}

-- | The current status of the unsafe content analysis job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsJobStatus :: Lens.Lens' GetContentModerationResponse (Core.Maybe Types.VideoJobStatus)
gcmrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE gcmrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The detected unsafe content labels and the time(s) they were detected.
--
-- /Note:/ Consider using 'moderationLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsModerationLabels :: Lens.Lens' GetContentModerationResponse (Core.Maybe [Types.ContentModerationDetection])
gcmrrsModerationLabels = Lens.field @"moderationLabels"
{-# INLINEABLE gcmrrsModerationLabels #-}
{-# DEPRECATED moderationLabels "Use generic-lens or generic-optics with 'moderationLabels' instead"  #-}

-- | Version number of the moderation detection model that was used to detect unsafe content.
--
-- /Note:/ Consider using 'moderationModelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsModerationModelVersion :: Lens.Lens' GetContentModerationResponse (Core.Maybe Core.Text)
gcmrrsModerationModelVersion = Lens.field @"moderationModelVersion"
{-# INLINEABLE gcmrrsModerationModelVersion #-}
{-# DEPRECATED moderationModelVersion "Use generic-lens or generic-optics with 'moderationModelVersion' instead"  #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of unsafe content labels. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsNextToken :: Lens.Lens' GetContentModerationResponse (Core.Maybe Types.PaginationToken)
gcmrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcmrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsStatusMessage :: Lens.Lens' GetContentModerationResponse (Core.Maybe Types.StatusMessage)
gcmrrsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE gcmrrsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from @GetContentModeration@ . 
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsVideoMetadata :: Lens.Lens' GetContentModerationResponse (Core.Maybe Types.VideoMetadata)
gcmrrsVideoMetadata = Lens.field @"videoMetadata"
{-# INLINEABLE gcmrrsVideoMetadata #-}
{-# DEPRECATED videoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsResponseStatus :: Lens.Lens' GetContentModerationResponse Core.Int
gcmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
