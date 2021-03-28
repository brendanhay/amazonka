{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListEventsDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the events detection jobs that you have submitted.
module Network.AWS.Comprehend.ListEventsDetectionJobs
    (
    -- * Creating a request
      ListEventsDetectionJobs (..)
    , mkListEventsDetectionJobs
    -- ** Request lenses
    , lFilter
    , lMaxResults
    , lNextToken

    -- * Destructuring the response
    , ListEventsDetectionJobsResponse (..)
    , mkListEventsDetectionJobsResponse
    -- ** Response lenses
    , lrsEventsDetectionJobPropertiesList
    , lrsNextToken
    , lrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListEventsDetectionJobs' smart constructor.
data ListEventsDetectionJobs = ListEventsDetectionJobs'
  { filter :: Core.Maybe Types.EventsDetectionJobFilter
    -- ^ Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in each page.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListEventsDetectionJobs' value with any optional fields omitted.
mkListEventsDetectionJobs
    :: ListEventsDetectionJobs
mkListEventsDetectionJobs
  = ListEventsDetectionJobs'{filter = Core.Nothing,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilter :: Lens.Lens' ListEventsDetectionJobs (Core.Maybe Types.EventsDetectionJobFilter)
lFilter = Lens.field @"filter"
{-# INLINEABLE lFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of results to return in each page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListEventsDetectionJobs (Core.Maybe Core.Natural)
lMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListEventsDetectionJobs (Core.Maybe Core.Text)
lNextToken = Lens.field @"nextToken"
{-# INLINEABLE lNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListEventsDetectionJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListEventsDetectionJobs where
        toHeaders ListEventsDetectionJobs{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.ListEventsDetectionJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListEventsDetectionJobs where
        toJSON ListEventsDetectionJobs{..}
          = Core.object
              (Core.catMaybes
                 [("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListEventsDetectionJobs where
        type Rs ListEventsDetectionJobs = ListEventsDetectionJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListEventsDetectionJobsResponse' Core.<$>
                   (x Core..:? "EventsDetectionJobPropertiesList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListEventsDetectionJobsResponse' smart constructor.
data ListEventsDetectionJobsResponse = ListEventsDetectionJobsResponse'
  { eventsDetectionJobPropertiesList :: Core.Maybe [Types.EventsDetectionJobProperties]
    -- ^ A list containing the properties of each job that is returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListEventsDetectionJobsResponse' value with any optional fields omitted.
mkListEventsDetectionJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListEventsDetectionJobsResponse
mkListEventsDetectionJobsResponse responseStatus
  = ListEventsDetectionJobsResponse'{eventsDetectionJobPropertiesList
                                       = Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'eventsDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsEventsDetectionJobPropertiesList :: Lens.Lens' ListEventsDetectionJobsResponse (Core.Maybe [Types.EventsDetectionJobProperties])
lrsEventsDetectionJobPropertiesList = Lens.field @"eventsDetectionJobPropertiesList"
{-# INLINEABLE lrsEventsDetectionJobPropertiesList #-}
{-# DEPRECATED eventsDetectionJobPropertiesList "Use generic-lens or generic-optics with 'eventsDetectionJobPropertiesList' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListEventsDetectionJobsResponse (Core.Maybe Core.Text)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListEventsDetectionJobsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
