{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the PII entity detection jobs that you have submitted.
module Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
    (
    -- * Creating a request
      ListPiiEntitiesDetectionJobs (..)
    , mkListPiiEntitiesDetectionJobs
    -- ** Request lenses
    , lpedjFilter
    , lpedjMaxResults
    , lpedjNextToken

    -- * Destructuring the response
    , ListPiiEntitiesDetectionJobsResponse (..)
    , mkListPiiEntitiesDetectionJobsResponse
    -- ** Response lenses
    , lpedjrrsNextToken
    , lpedjrrsPiiEntitiesDetectionJobPropertiesList
    , lpedjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPiiEntitiesDetectionJobs' smart constructor.
data ListPiiEntitiesDetectionJobs = ListPiiEntitiesDetectionJobs'
  { filter :: Core.Maybe Types.PiiEntitiesDetectionJobFilter
    -- ^ Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in each page.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPiiEntitiesDetectionJobs' value with any optional fields omitted.
mkListPiiEntitiesDetectionJobs
    :: ListPiiEntitiesDetectionJobs
mkListPiiEntitiesDetectionJobs
  = ListPiiEntitiesDetectionJobs'{filter = Core.Nothing,
                                  maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjFilter :: Lens.Lens' ListPiiEntitiesDetectionJobs (Core.Maybe Types.PiiEntitiesDetectionJobFilter)
lpedjFilter = Lens.field @"filter"
{-# INLINEABLE lpedjFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of results to return in each page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjMaxResults :: Lens.Lens' ListPiiEntitiesDetectionJobs (Core.Maybe Core.Natural)
lpedjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpedjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjNextToken :: Lens.Lens' ListPiiEntitiesDetectionJobs (Core.Maybe Core.Text)
lpedjNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpedjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPiiEntitiesDetectionJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPiiEntitiesDetectionJobs where
        toHeaders ListPiiEntitiesDetectionJobs{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.ListPiiEntitiesDetectionJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPiiEntitiesDetectionJobs where
        toJSON ListPiiEntitiesDetectionJobs{..}
          = Core.object
              (Core.catMaybes
                 [("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListPiiEntitiesDetectionJobs where
        type Rs ListPiiEntitiesDetectionJobs =
             ListPiiEntitiesDetectionJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPiiEntitiesDetectionJobsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "PiiEntitiesDetectionJobPropertiesList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListPiiEntitiesDetectionJobsResponse' smart constructor.
data ListPiiEntitiesDetectionJobsResponse = ListPiiEntitiesDetectionJobsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  , piiEntitiesDetectionJobPropertiesList :: Core.Maybe [Types.PiiEntitiesDetectionJobProperties]
    -- ^ A list containing the properties of each job that is returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPiiEntitiesDetectionJobsResponse' value with any optional fields omitted.
mkListPiiEntitiesDetectionJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPiiEntitiesDetectionJobsResponse
mkListPiiEntitiesDetectionJobsResponse responseStatus
  = ListPiiEntitiesDetectionJobsResponse'{nextToken = Core.Nothing,
                                          piiEntitiesDetectionJobPropertiesList = Core.Nothing,
                                          responseStatus}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjrrsNextToken :: Lens.Lens' ListPiiEntitiesDetectionJobsResponse (Core.Maybe Core.Text)
lpedjrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpedjrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'piiEntitiesDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjrrsPiiEntitiesDetectionJobPropertiesList :: Lens.Lens' ListPiiEntitiesDetectionJobsResponse (Core.Maybe [Types.PiiEntitiesDetectionJobProperties])
lpedjrrsPiiEntitiesDetectionJobPropertiesList = Lens.field @"piiEntitiesDetectionJobPropertiesList"
{-# INLINEABLE lpedjrrsPiiEntitiesDetectionJobPropertiesList #-}
{-# DEPRECATED piiEntitiesDetectionJobPropertiesList "Use generic-lens or generic-optics with 'piiEntitiesDetectionJobPropertiesList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjrrsResponseStatus :: Lens.Lens' ListPiiEntitiesDetectionJobsResponse Core.Int
lpedjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpedjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
