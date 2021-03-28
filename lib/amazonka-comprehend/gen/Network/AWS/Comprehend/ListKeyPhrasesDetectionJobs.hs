{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of key phrase detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
    (
    -- * Creating a request
      ListKeyPhrasesDetectionJobs (..)
    , mkListKeyPhrasesDetectionJobs
    -- ** Request lenses
    , lkpdjFilter
    , lkpdjMaxResults
    , lkpdjNextToken

    -- * Destructuring the response
    , ListKeyPhrasesDetectionJobsResponse (..)
    , mkListKeyPhrasesDetectionJobsResponse
    -- ** Response lenses
    , lkpdjrrsKeyPhrasesDetectionJobPropertiesList
    , lkpdjrrsNextToken
    , lkpdjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListKeyPhrasesDetectionJobs' smart constructor.
data ListKeyPhrasesDetectionJobs = ListKeyPhrasesDetectionJobs'
  { filter :: Core.Maybe Types.KeyPhrasesDetectionJobFilter
    -- ^ Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in each page. The default is 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListKeyPhrasesDetectionJobs' value with any optional fields omitted.
mkListKeyPhrasesDetectionJobs
    :: ListKeyPhrasesDetectionJobs
mkListKeyPhrasesDetectionJobs
  = ListKeyPhrasesDetectionJobs'{filter = Core.Nothing,
                                 maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjFilter :: Lens.Lens' ListKeyPhrasesDetectionJobs (Core.Maybe Types.KeyPhrasesDetectionJobFilter)
lkpdjFilter = Lens.field @"filter"
{-# INLINEABLE lkpdjFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjMaxResults :: Lens.Lens' ListKeyPhrasesDetectionJobs (Core.Maybe Core.Natural)
lkpdjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lkpdjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjNextToken :: Lens.Lens' ListKeyPhrasesDetectionJobs (Core.Maybe Core.Text)
lkpdjNextToken = Lens.field @"nextToken"
{-# INLINEABLE lkpdjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListKeyPhrasesDetectionJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListKeyPhrasesDetectionJobs where
        toHeaders ListKeyPhrasesDetectionJobs{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.ListKeyPhrasesDetectionJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListKeyPhrasesDetectionJobs where
        toJSON ListKeyPhrasesDetectionJobs{..}
          = Core.object
              (Core.catMaybes
                 [("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListKeyPhrasesDetectionJobs where
        type Rs ListKeyPhrasesDetectionJobs =
             ListKeyPhrasesDetectionJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListKeyPhrasesDetectionJobsResponse' Core.<$>
                   (x Core..:? "KeyPhrasesDetectionJobPropertiesList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListKeyPhrasesDetectionJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"keyPhrasesDetectionJobPropertiesList" Core..
                   Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListKeyPhrasesDetectionJobsResponse' smart constructor.
data ListKeyPhrasesDetectionJobsResponse = ListKeyPhrasesDetectionJobsResponse'
  { keyPhrasesDetectionJobPropertiesList :: Core.Maybe [Types.KeyPhrasesDetectionJobProperties]
    -- ^ A list containing the properties of each job that is returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListKeyPhrasesDetectionJobsResponse' value with any optional fields omitted.
mkListKeyPhrasesDetectionJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListKeyPhrasesDetectionJobsResponse
mkListKeyPhrasesDetectionJobsResponse responseStatus
  = ListKeyPhrasesDetectionJobsResponse'{keyPhrasesDetectionJobPropertiesList
                                           = Core.Nothing,
                                         nextToken = Core.Nothing, responseStatus}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'keyPhrasesDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjrrsKeyPhrasesDetectionJobPropertiesList :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Core.Maybe [Types.KeyPhrasesDetectionJobProperties])
lkpdjrrsKeyPhrasesDetectionJobPropertiesList = Lens.field @"keyPhrasesDetectionJobPropertiesList"
{-# INLINEABLE lkpdjrrsKeyPhrasesDetectionJobPropertiesList #-}
{-# DEPRECATED keyPhrasesDetectionJobPropertiesList "Use generic-lens or generic-optics with 'keyPhrasesDetectionJobPropertiesList' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjrrsNextToken :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Core.Maybe Core.Text)
lkpdjrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lkpdjrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjrrsResponseStatus :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse Core.Int
lkpdjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lkpdjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
