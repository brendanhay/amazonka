{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListBusinessReportSchedules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details of the schedules that a user configured. A download URL of the report associated with each schedule is returned every time this action is called. A new download URL is returned each time, and is valid for 24 hours.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListBusinessReportSchedules
    (
    -- * Creating a request
      ListBusinessReportSchedules (..)
    , mkListBusinessReportSchedules
    -- ** Request lenses
    , lbrsMaxResults
    , lbrsNextToken

    -- * Destructuring the response
    , ListBusinessReportSchedulesResponse (..)
    , mkListBusinessReportSchedulesResponse
    -- ** Response lenses
    , lbrsrrsBusinessReportSchedules
    , lbrsrrsNextToken
    , lbrsrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBusinessReportSchedules' smart constructor.
data ListBusinessReportSchedules = ListBusinessReportSchedules'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of schedules listed in the call.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token used to list the remaining schedules from the previous API call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBusinessReportSchedules' value with any optional fields omitted.
mkListBusinessReportSchedules
    :: ListBusinessReportSchedules
mkListBusinessReportSchedules
  = ListBusinessReportSchedules'{maxResults = Core.Nothing,
                                 nextToken = Core.Nothing}

-- | The maximum number of schedules listed in the call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsMaxResults :: Lens.Lens' ListBusinessReportSchedules (Core.Maybe Core.Natural)
lbrsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lbrsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token used to list the remaining schedules from the previous API call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsNextToken :: Lens.Lens' ListBusinessReportSchedules (Core.Maybe Types.NextToken)
lbrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListBusinessReportSchedules where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListBusinessReportSchedules where
        toHeaders ListBusinessReportSchedules{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.ListBusinessReportSchedules")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListBusinessReportSchedules where
        toJSON ListBusinessReportSchedules{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListBusinessReportSchedules where
        type Rs ListBusinessReportSchedules =
             ListBusinessReportSchedulesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBusinessReportSchedulesResponse' Core.<$>
                   (x Core..:? "BusinessReportSchedules") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListBusinessReportSchedules where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"businessReportSchedules" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListBusinessReportSchedulesResponse' smart constructor.
data ListBusinessReportSchedulesResponse = ListBusinessReportSchedulesResponse'
  { businessReportSchedules :: Core.Maybe [Types.BusinessReportSchedule]
    -- ^ The schedule of the reports.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token used to list the remaining schedules from the previous API call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListBusinessReportSchedulesResponse' value with any optional fields omitted.
mkListBusinessReportSchedulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBusinessReportSchedulesResponse
mkListBusinessReportSchedulesResponse responseStatus
  = ListBusinessReportSchedulesResponse'{businessReportSchedules =
                                           Core.Nothing,
                                         nextToken = Core.Nothing, responseStatus}

-- | The schedule of the reports.
--
-- /Note:/ Consider using 'businessReportSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsrrsBusinessReportSchedules :: Lens.Lens' ListBusinessReportSchedulesResponse (Core.Maybe [Types.BusinessReportSchedule])
lbrsrrsBusinessReportSchedules = Lens.field @"businessReportSchedules"
{-# INLINEABLE lbrsrrsBusinessReportSchedules #-}
{-# DEPRECATED businessReportSchedules "Use generic-lens or generic-optics with 'businessReportSchedules' instead"  #-}

-- | The token used to list the remaining schedules from the previous API call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsrrsNextToken :: Lens.Lens' ListBusinessReportSchedulesResponse (Core.Maybe Types.NextToken)
lbrsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbrsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsrrsResponseStatus :: Lens.Lens' ListBusinessReportSchedulesResponse Core.Int
lbrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
