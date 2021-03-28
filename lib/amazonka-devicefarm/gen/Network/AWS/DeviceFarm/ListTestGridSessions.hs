{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of sessions for a 'TestGridProject' .
module Network.AWS.DeviceFarm.ListTestGridSessions
    (
    -- * Creating a request
      ListTestGridSessions (..)
    , mkListTestGridSessions
    -- ** Request lenses
    , ltgsProjectArn
    , ltgsCreationTimeAfter
    , ltgsCreationTimeBefore
    , ltgsEndTimeAfter
    , ltgsEndTimeBefore
    , ltgsMaxResult
    , ltgsNextToken
    , ltgsStatus

    -- * Destructuring the response
    , ListTestGridSessionsResponse (..)
    , mkListTestGridSessionsResponse
    -- ** Response lenses
    , ltgsrrsNextToken
    , ltgsrrsTestGridSessions
    , ltgsrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTestGridSessions' smart constructor.
data ListTestGridSessions = ListTestGridSessions'
  { projectArn :: Types.DeviceFarmArn
    -- ^ ARN of a 'TestGridProject' .
  , creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ Return only sessions created after this time.
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ Return only sessions created before this time.
  , endTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ Return only sessions that ended after this time.
  , endTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ Return only sessions that ended before this time.
  , maxResult :: Core.Maybe Core.Natural
    -- ^ Return only this many results at a time.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Pagination token.
  , status :: Core.Maybe Types.TestGridSessionStatus
    -- ^ Return only sessions in this state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTestGridSessions' value with any optional fields omitted.
mkListTestGridSessions
    :: Types.DeviceFarmArn -- ^ 'projectArn'
    -> ListTestGridSessions
mkListTestGridSessions projectArn
  = ListTestGridSessions'{projectArn,
                          creationTimeAfter = Core.Nothing,
                          creationTimeBefore = Core.Nothing, endTimeAfter = Core.Nothing,
                          endTimeBefore = Core.Nothing, maxResult = Core.Nothing,
                          nextToken = Core.Nothing, status = Core.Nothing}

-- | ARN of a 'TestGridProject' .
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsProjectArn :: Lens.Lens' ListTestGridSessions Types.DeviceFarmArn
ltgsProjectArn = Lens.field @"projectArn"
{-# INLINEABLE ltgsProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | Return only sessions created after this time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsCreationTimeAfter :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.NominalDiffTime)
ltgsCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE ltgsCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | Return only sessions created before this time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsCreationTimeBefore :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.NominalDiffTime)
ltgsCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE ltgsCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | Return only sessions that ended after this time.
--
-- /Note:/ Consider using 'endTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsEndTimeAfter :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.NominalDiffTime)
ltgsEndTimeAfter = Lens.field @"endTimeAfter"
{-# INLINEABLE ltgsEndTimeAfter #-}
{-# DEPRECATED endTimeAfter "Use generic-lens or generic-optics with 'endTimeAfter' instead"  #-}

-- | Return only sessions that ended before this time.
--
-- /Note:/ Consider using 'endTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsEndTimeBefore :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.NominalDiffTime)
ltgsEndTimeBefore = Lens.field @"endTimeBefore"
{-# INLINEABLE ltgsEndTimeBefore #-}
{-# DEPRECATED endTimeBefore "Use generic-lens or generic-optics with 'endTimeBefore' instead"  #-}

-- | Return only this many results at a time.
--
-- /Note:/ Consider using 'maxResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsMaxResult :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.Natural)
ltgsMaxResult = Lens.field @"maxResult"
{-# INLINEABLE ltgsMaxResult #-}
{-# DEPRECATED maxResult "Use generic-lens or generic-optics with 'maxResult' instead"  #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsNextToken :: Lens.Lens' ListTestGridSessions (Core.Maybe Types.PaginationToken)
ltgsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltgsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Return only sessions in this state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsStatus :: Lens.Lens' ListTestGridSessions (Core.Maybe Types.TestGridSessionStatus)
ltgsStatus = Lens.field @"status"
{-# INLINEABLE ltgsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListTestGridSessions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTestGridSessions where
        toHeaders ListTestGridSessions{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.ListTestGridSessions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTestGridSessions where
        toJSON ListTestGridSessions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectArn" Core..= projectArn),
                  ("creationTimeAfter" Core..=) Core.<$> creationTimeAfter,
                  ("creationTimeBefore" Core..=) Core.<$> creationTimeBefore,
                  ("endTimeAfter" Core..=) Core.<$> endTimeAfter,
                  ("endTimeBefore" Core..=) Core.<$> endTimeBefore,
                  ("maxResult" Core..=) Core.<$> maxResult,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("status" Core..=) Core.<$> status])

instance Core.AWSRequest ListTestGridSessions where
        type Rs ListTestGridSessions = ListTestGridSessionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTestGridSessionsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "testGridSessions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTestGridSessionsResponse' smart constructor.
data ListTestGridSessionsResponse = ListTestGridSessionsResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Pagination token.
  , testGridSessions :: Core.Maybe [Types.TestGridSession]
    -- ^ The sessions that match the criteria in a 'ListTestGridSessionsRequest' . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTestGridSessionsResponse' value with any optional fields omitted.
mkListTestGridSessionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTestGridSessionsResponse
mkListTestGridSessionsResponse responseStatus
  = ListTestGridSessionsResponse'{nextToken = Core.Nothing,
                                  testGridSessions = Core.Nothing, responseStatus}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsrrsNextToken :: Lens.Lens' ListTestGridSessionsResponse (Core.Maybe Types.PaginationToken)
ltgsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltgsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The sessions that match the criteria in a 'ListTestGridSessionsRequest' . 
--
-- /Note:/ Consider using 'testGridSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsrrsTestGridSessions :: Lens.Lens' ListTestGridSessionsResponse (Core.Maybe [Types.TestGridSession])
ltgsrrsTestGridSessions = Lens.field @"testGridSessions"
{-# INLINEABLE ltgsrrsTestGridSessions #-}
{-# DEPRECATED testGridSessions "Use generic-lens or generic-optics with 'testGridSessions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsrrsResponseStatus :: Lens.Lens' ListTestGridSessionsResponse Core.Int
ltgsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltgsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
