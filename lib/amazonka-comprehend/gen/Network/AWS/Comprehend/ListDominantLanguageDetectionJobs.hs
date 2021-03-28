{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the dominant language detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
    (
    -- * Creating a request
      ListDominantLanguageDetectionJobs (..)
    , mkListDominantLanguageDetectionJobs
    -- ** Request lenses
    , ldldjFilter
    , ldldjMaxResults
    , ldldjNextToken

    -- * Destructuring the response
    , ListDominantLanguageDetectionJobsResponse (..)
    , mkListDominantLanguageDetectionJobsResponse
    -- ** Response lenses
    , ldldjrrsDominantLanguageDetectionJobPropertiesList
    , ldldjrrsNextToken
    , ldldjrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDominantLanguageDetectionJobs' smart constructor.
data ListDominantLanguageDetectionJobs = ListDominantLanguageDetectionJobs'
  { filter :: Core.Maybe Types.DominantLanguageDetectionJobFilter
    -- ^ Filters that jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in each page. The default is 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDominantLanguageDetectionJobs' value with any optional fields omitted.
mkListDominantLanguageDetectionJobs
    :: ListDominantLanguageDetectionJobs
mkListDominantLanguageDetectionJobs
  = ListDominantLanguageDetectionJobs'{filter = Core.Nothing,
                                       maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Filters that jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjFilter :: Lens.Lens' ListDominantLanguageDetectionJobs (Core.Maybe Types.DominantLanguageDetectionJobFilter)
ldldjFilter = Lens.field @"filter"
{-# INLINEABLE ldldjFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjMaxResults :: Lens.Lens' ListDominantLanguageDetectionJobs (Core.Maybe Core.Natural)
ldldjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldldjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjNextToken :: Lens.Lens' ListDominantLanguageDetectionJobs (Core.Maybe Core.Text)
ldldjNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldldjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDominantLanguageDetectionJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDominantLanguageDetectionJobs where
        toHeaders ListDominantLanguageDetectionJobs{..}
          = Core.pure
              ("X-Amz-Target",
               "Comprehend_20171127.ListDominantLanguageDetectionJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListDominantLanguageDetectionJobs where
        toJSON ListDominantLanguageDetectionJobs{..}
          = Core.object
              (Core.catMaybes
                 [("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListDominantLanguageDetectionJobs where
        type Rs ListDominantLanguageDetectionJobs =
             ListDominantLanguageDetectionJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDominantLanguageDetectionJobsResponse' Core.<$>
                   (x Core..:? "DominantLanguageDetectionJobPropertiesList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDominantLanguageDetectionJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"dominantLanguageDetectionJobPropertiesList" Core..
                   Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDominantLanguageDetectionJobsResponse' smart constructor.
data ListDominantLanguageDetectionJobsResponse = ListDominantLanguageDetectionJobsResponse'
  { dominantLanguageDetectionJobPropertiesList :: Core.Maybe [Types.DominantLanguageDetectionJobProperties]
    -- ^ A list containing the properties of each job that is returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDominantLanguageDetectionJobsResponse' value with any optional fields omitted.
mkListDominantLanguageDetectionJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDominantLanguageDetectionJobsResponse
mkListDominantLanguageDetectionJobsResponse responseStatus
  = ListDominantLanguageDetectionJobsResponse'{dominantLanguageDetectionJobPropertiesList
                                                 = Core.Nothing,
                                               nextToken = Core.Nothing, responseStatus}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'dominantLanguageDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjrrsDominantLanguageDetectionJobPropertiesList :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Core.Maybe [Types.DominantLanguageDetectionJobProperties])
ldldjrrsDominantLanguageDetectionJobPropertiesList = Lens.field @"dominantLanguageDetectionJobPropertiesList"
{-# INLINEABLE ldldjrrsDominantLanguageDetectionJobPropertiesList #-}
{-# DEPRECATED dominantLanguageDetectionJobPropertiesList "Use generic-lens or generic-optics with 'dominantLanguageDetectionJobPropertiesList' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjrrsNextToken :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Core.Maybe Core.Text)
ldldjrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldldjrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjrrsResponseStatus :: Lens.Lens' ListDominantLanguageDetectionJobsResponse Core.Int
ldldjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldldjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
