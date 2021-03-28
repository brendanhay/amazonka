{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetPersonTracking (..)
    , mkGetPersonTracking
    -- ** Request lenses
    , gptJobId
    , gptMaxResults
    , gptNextToken
    , gptSortBy

    -- * Destructuring the response
    , GetPersonTrackingResponse (..)
    , mkGetPersonTrackingResponse
    -- ** Response lenses
    , gptrrsJobStatus
    , gptrrsNextToken
    , gptrrsPersons
    , gptrrsStatusMessage
    , gptrrsVideoMetadata
    , gptrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPersonTracking' smart constructor.
data GetPersonTracking = GetPersonTracking'
  { jobId :: Types.JobId
    -- ^ The identifier for a job that tracks persons in a video. You get the @JobId@ from a call to @StartPersonTracking@ . 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the previous response was incomplete (because there are more persons to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of persons. 
  , sortBy :: Core.Maybe Types.PersonTrackingSortBy
    -- ^ Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort array elements by the time persons are detected. Use @INDEX@ to sort by the tracked persons. If you sort by @INDEX@ , the array elements for each person are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPersonTracking' value with any optional fields omitted.
mkGetPersonTracking
    :: Types.JobId -- ^ 'jobId'
    -> GetPersonTracking
mkGetPersonTracking jobId
  = GetPersonTracking'{jobId, maxResults = Core.Nothing,
                       nextToken = Core.Nothing, sortBy = Core.Nothing}

-- | The identifier for a job that tracks persons in a video. You get the @JobId@ from a call to @StartPersonTracking@ . 
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptJobId :: Lens.Lens' GetPersonTracking Types.JobId
gptJobId = Lens.field @"jobId"
{-# INLINEABLE gptJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptMaxResults :: Lens.Lens' GetPersonTracking (Core.Maybe Core.Natural)
gptMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gptMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was incomplete (because there are more persons to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of persons. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptNextToken :: Lens.Lens' GetPersonTracking (Core.Maybe Types.PaginationToken)
gptNextToken = Lens.field @"nextToken"
{-# INLINEABLE gptNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort array elements by the time persons are detected. Use @INDEX@ to sort by the tracked persons. If you sort by @INDEX@ , the array elements for each person are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptSortBy :: Lens.Lens' GetPersonTracking (Core.Maybe Types.PersonTrackingSortBy)
gptSortBy = Lens.field @"sortBy"
{-# INLINEABLE gptSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

instance Core.ToQuery GetPersonTracking where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPersonTracking where
        toHeaders GetPersonTracking{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.GetPersonTracking")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPersonTracking where
        toJSON GetPersonTracking{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobId" Core..= jobId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy])

instance Core.AWSRequest GetPersonTracking where
        type Rs GetPersonTracking = GetPersonTrackingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPersonTrackingResponse' Core.<$>
                   (x Core..:? "JobStatus") Core.<*> x Core..:? "NextToken" Core.<*>
                     x Core..:? "Persons"
                     Core.<*> x Core..:? "StatusMessage"
                     Core.<*> x Core..:? "VideoMetadata"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPersonTrackingResponse' smart constructor.
data GetPersonTrackingResponse = GetPersonTrackingResponse'
  { jobStatus :: Core.Maybe Types.VideoJobStatus
    -- ^ The current status of the person tracking job.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of persons. 
  , persons :: Core.Maybe [Types.PersonDetection]
    -- ^ An array of the persons detected in the video and the time(s) their path was tracked throughout the video. An array element will exist for each time a person's path is tracked. 
  , statusMessage :: Core.Maybe Types.StatusMessage
    -- ^ If the job fails, @StatusMessage@ provides a descriptive error message.
  , videoMetadata :: Core.Maybe Types.VideoMetadata
    -- ^ Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPersonTrackingResponse' value with any optional fields omitted.
mkGetPersonTrackingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPersonTrackingResponse
mkGetPersonTrackingResponse responseStatus
  = GetPersonTrackingResponse'{jobStatus = Core.Nothing,
                               nextToken = Core.Nothing, persons = Core.Nothing,
                               statusMessage = Core.Nothing, videoMetadata = Core.Nothing,
                               responseStatus}

-- | The current status of the person tracking job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrrsJobStatus :: Lens.Lens' GetPersonTrackingResponse (Core.Maybe Types.VideoJobStatus)
gptrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE gptrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of persons. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrrsNextToken :: Lens.Lens' GetPersonTrackingResponse (Core.Maybe Types.PaginationToken)
gptrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gptrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of the persons detected in the video and the time(s) their path was tracked throughout the video. An array element will exist for each time a person's path is tracked. 
--
-- /Note:/ Consider using 'persons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrrsPersons :: Lens.Lens' GetPersonTrackingResponse (Core.Maybe [Types.PersonDetection])
gptrrsPersons = Lens.field @"persons"
{-# INLINEABLE gptrrsPersons #-}
{-# DEPRECATED persons "Use generic-lens or generic-optics with 'persons' instead"  #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrrsStatusMessage :: Lens.Lens' GetPersonTrackingResponse (Core.Maybe Types.StatusMessage)
gptrrsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE gptrrsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrrsVideoMetadata :: Lens.Lens' GetPersonTrackingResponse (Core.Maybe Types.VideoMetadata)
gptrrsVideoMetadata = Lens.field @"videoMetadata"
{-# INLINEABLE gptrrsVideoMetadata #-}
{-# DEPRECATED videoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gptrrsResponseStatus :: Lens.Lens' GetPersonTrackingResponse Core.Int
gptrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gptrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
