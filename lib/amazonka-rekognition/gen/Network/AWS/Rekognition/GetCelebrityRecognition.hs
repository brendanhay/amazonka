{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetCelebrityRecognition (..)
    , mkGetCelebrityRecognition
    -- ** Request lenses
    , gcrJobId
    , gcrMaxResults
    , gcrNextToken
    , gcrSortBy

    -- * Destructuring the response
    , GetCelebrityRecognitionResponse (..)
    , mkGetCelebrityRecognitionResponse
    -- ** Response lenses
    , gcrrrsCelebrities
    , gcrrrsJobStatus
    , gcrrrsNextToken
    , gcrrrsStatusMessage
    , gcrrrsVideoMetadata
    , gcrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCelebrityRecognition' smart constructor.
data GetCelebrityRecognition = GetCelebrityRecognition'
  { jobId :: Types.JobId
    -- ^ Job identifier for the required celebrity recognition analysis. You can get the job identifer from a call to @StartCelebrityRecognition@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the previous response was incomplete (because there is more recognized celebrities to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of celebrities. 
  , sortBy :: Core.Maybe Types.CelebrityRecognitionSortBy
    -- ^ Sort to use for celebrities returned in @Celebrities@ field. Specify @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by the time the celebrity was recognized.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCelebrityRecognition' value with any optional fields omitted.
mkGetCelebrityRecognition
    :: Types.JobId -- ^ 'jobId'
    -> GetCelebrityRecognition
mkGetCelebrityRecognition jobId
  = GetCelebrityRecognition'{jobId, maxResults = Core.Nothing,
                             nextToken = Core.Nothing, sortBy = Core.Nothing}

-- | Job identifier for the required celebrity recognition analysis. You can get the job identifer from a call to @StartCelebrityRecognition@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrJobId :: Lens.Lens' GetCelebrityRecognition Types.JobId
gcrJobId = Lens.field @"jobId"
{-# INLINEABLE gcrJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrMaxResults :: Lens.Lens' GetCelebrityRecognition (Core.Maybe Core.Natural)
gcrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gcrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was incomplete (because there is more recognized celebrities to retrieve), Amazon Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of celebrities. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrNextToken :: Lens.Lens' GetCelebrityRecognition (Core.Maybe Types.PaginationToken)
gcrNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Sort to use for celebrities returned in @Celebrities@ field. Specify @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by the time the celebrity was recognized.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrSortBy :: Lens.Lens' GetCelebrityRecognition (Core.Maybe Types.CelebrityRecognitionSortBy)
gcrSortBy = Lens.field @"sortBy"
{-# INLINEABLE gcrSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

instance Core.ToQuery GetCelebrityRecognition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCelebrityRecognition where
        toHeaders GetCelebrityRecognition{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.GetCelebrityRecognition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCelebrityRecognition where
        toJSON GetCelebrityRecognition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobId" Core..= jobId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy])

instance Core.AWSRequest GetCelebrityRecognition where
        type Rs GetCelebrityRecognition = GetCelebrityRecognitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCelebrityRecognitionResponse' Core.<$>
                   (x Core..:? "Celebrities") Core.<*> x Core..:? "JobStatus" Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> x Core..:? "StatusMessage"
                     Core.<*> x Core..:? "VideoMetadata"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCelebrityRecognitionResponse' smart constructor.
data GetCelebrityRecognitionResponse = GetCelebrityRecognitionResponse'
  { celebrities :: Core.Maybe [Types.CelebrityRecognition]
    -- ^ Array of celebrities recognized in the video.
  , jobStatus :: Core.Maybe Types.VideoJobStatus
    -- ^ The current status of the celebrity recognition job.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of celebrities.
  , statusMessage :: Core.Maybe Types.StatusMessage
    -- ^ If the job fails, @StatusMessage@ provides a descriptive error message.
  , videoMetadata :: Core.Maybe Types.VideoMetadata
    -- ^ Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCelebrityRecognitionResponse' value with any optional fields omitted.
mkGetCelebrityRecognitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCelebrityRecognitionResponse
mkGetCelebrityRecognitionResponse responseStatus
  = GetCelebrityRecognitionResponse'{celebrities = Core.Nothing,
                                     jobStatus = Core.Nothing, nextToken = Core.Nothing,
                                     statusMessage = Core.Nothing, videoMetadata = Core.Nothing,
                                     responseStatus}

-- | Array of celebrities recognized in the video.
--
-- /Note:/ Consider using 'celebrities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsCelebrities :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe [Types.CelebrityRecognition])
gcrrrsCelebrities = Lens.field @"celebrities"
{-# INLINEABLE gcrrrsCelebrities #-}
{-# DEPRECATED celebrities "Use generic-lens or generic-optics with 'celebrities' instead"  #-}

-- | The current status of the celebrity recognition job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsJobStatus :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe Types.VideoJobStatus)
gcrrrsJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE gcrrrsJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | If the response is truncated, Amazon Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of celebrities.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsNextToken :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe Types.PaginationToken)
gcrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsStatusMessage :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe Types.StatusMessage)
gcrrrsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE gcrrrsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | Information about a video that Amazon Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition Video operation.
--
-- /Note:/ Consider using 'videoMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsVideoMetadata :: Lens.Lens' GetCelebrityRecognitionResponse (Core.Maybe Types.VideoMetadata)
gcrrrsVideoMetadata = Lens.field @"videoMetadata"
{-# INLINEABLE gcrrrsVideoMetadata #-}
{-# DEPRECATED videoMetadata "Use generic-lens or generic-optics with 'videoMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsResponseStatus :: Lens.Lens' GetCelebrityRecognitionResponse Core.Int
gcrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
