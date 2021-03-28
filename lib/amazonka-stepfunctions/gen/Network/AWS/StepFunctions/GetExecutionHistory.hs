{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.GetExecutionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified execution as a list of events. By default, the results are returned in ascending order of the @timeStamp@ of the events. Use the @reverseOrder@ parameter to get the latest events first.
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- This API action is not supported by @EXPRESS@ state machines.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.GetExecutionHistory
    (
    -- * Creating a request
      GetExecutionHistory (..)
    , mkGetExecutionHistory
    -- ** Request lenses
    , gehExecutionArn
    , gehIncludeExecutionData
    , gehMaxResults
    , gehNextToken
    , gehReverseOrder

    -- * Destructuring the response
    , GetExecutionHistoryResponse (..)
    , mkGetExecutionHistoryResponse
    -- ** Response lenses
    , gehrrsEvents
    , gehrrsNextToken
    , gehrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkGetExecutionHistory' smart constructor.
data GetExecutionHistory = GetExecutionHistory'
  { executionArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the execution.
  , includeExecutionData :: Core.Maybe Core.Bool
    -- ^ You can select whether execution data (input or output of a history event) is returned. The default is @true@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
  , reverseOrder :: Core.Maybe Core.Bool
    -- ^ Lists events in descending order of their @timeStamp@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExecutionHistory' value with any optional fields omitted.
mkGetExecutionHistory
    :: Types.Arn -- ^ 'executionArn'
    -> GetExecutionHistory
mkGetExecutionHistory executionArn
  = GetExecutionHistory'{executionArn,
                         includeExecutionData = Core.Nothing, maxResults = Core.Nothing,
                         nextToken = Core.Nothing, reverseOrder = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the execution.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehExecutionArn :: Lens.Lens' GetExecutionHistory Types.Arn
gehExecutionArn = Lens.field @"executionArn"
{-# INLINEABLE gehExecutionArn #-}
{-# DEPRECATED executionArn "Use generic-lens or generic-optics with 'executionArn' instead"  #-}

-- | You can select whether execution data (input or output of a history event) is returned. The default is @true@ .
--
-- /Note:/ Consider using 'includeExecutionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehIncludeExecutionData :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Bool)
gehIncludeExecutionData = Lens.field @"includeExecutionData"
{-# INLINEABLE gehIncludeExecutionData #-}
{-# DEPRECATED includeExecutionData "Use generic-lens or generic-optics with 'includeExecutionData' instead"  #-}

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehMaxResults :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Natural)
gehMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gehMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehNextToken :: Lens.Lens' GetExecutionHistory (Core.Maybe Types.NextToken)
gehNextToken = Lens.field @"nextToken"
{-# INLINEABLE gehNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Lists events in descending order of their @timeStamp@ .
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehReverseOrder :: Lens.Lens' GetExecutionHistory (Core.Maybe Core.Bool)
gehReverseOrder = Lens.field @"reverseOrder"
{-# INLINEABLE gehReverseOrder #-}
{-# DEPRECATED reverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead"  #-}

instance Core.ToQuery GetExecutionHistory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetExecutionHistory where
        toHeaders GetExecutionHistory{..}
          = Core.pure
              ("X-Amz-Target", "AWSStepFunctions.GetExecutionHistory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON GetExecutionHistory where
        toJSON GetExecutionHistory{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("executionArn" Core..= executionArn),
                  ("includeExecutionData" Core..=) Core.<$> includeExecutionData,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("reverseOrder" Core..=) Core.<$> reverseOrder])

instance Core.AWSRequest GetExecutionHistory where
        type Rs GetExecutionHistory = GetExecutionHistoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetExecutionHistoryResponse' Core.<$>
                   (x Core..:? "events" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetExecutionHistory where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"events") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetExecutionHistoryResponse' smart constructor.
data GetExecutionHistoryResponse = GetExecutionHistoryResponse'
  { events :: [Types.HistoryEvent]
    -- ^ The list of events that occurred in the execution.
  , nextToken :: Core.Maybe Types.PageToken
    -- ^ If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetExecutionHistoryResponse' value with any optional fields omitted.
mkGetExecutionHistoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetExecutionHistoryResponse
mkGetExecutionHistoryResponse responseStatus
  = GetExecutionHistoryResponse'{events = Core.mempty,
                                 nextToken = Core.Nothing, responseStatus}

-- | The list of events that occurred in the execution.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehrrsEvents :: Lens.Lens' GetExecutionHistoryResponse [Types.HistoryEvent]
gehrrsEvents = Lens.field @"events"
{-# INLINEABLE gehrrsEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehrrsNextToken :: Lens.Lens' GetExecutionHistoryResponse (Core.Maybe Types.PageToken)
gehrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gehrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gehrrsResponseStatus :: Lens.Lens' GetExecutionHistoryResponse Core.Int
gehrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gehrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
