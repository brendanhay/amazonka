{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all projects in AWS CodeStar associated with your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListProjects
    (
    -- * Creating a request
      ListProjects (..)
    , mkListProjects
    -- ** Request lenses
    , lpMaxResults
    , lpNextToken

    -- * Destructuring the response
    , ListProjectsResponse (..)
    , mkListProjectsResponse
    -- ** Response lenses
    , lprrsProjects
    , lprrsNextToken
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListProjects' smart constructor.
data ListProjects = ListProjects'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum amount of data that can be contained in a single set of results.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The continuation token to be used to return the next set of results, if the results cannot be returned in one response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProjects' value with any optional fields omitted.
mkListProjects
    :: ListProjects
mkListProjects
  = ListProjects'{maxResults = Core.Nothing,
                  nextToken = Core.Nothing}

-- | The maximum amount of data that can be contained in a single set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListProjects (Core.Maybe Core.Natural)
lpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The continuation token to be used to return the next set of results, if the results cannot be returned in one response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListProjects (Core.Maybe Types.PaginationToken)
lpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListProjects where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListProjects where
        toHeaders ListProjects{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.ListProjects")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListProjects where
        toJSON ListProjects{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListProjects where
        type Rs ListProjects = ListProjectsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListProjectsResponse' Core.<$>
                   (x Core..:? "projects" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListProjects where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"projects") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { projects :: [Types.ProjectSummary]
    -- ^ A list of projects.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The continuation token to use when requesting the next set of results, if there are more results to be returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProjectsResponse' value with any optional fields omitted.
mkListProjectsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListProjectsResponse
mkListProjectsResponse responseStatus
  = ListProjectsResponse'{projects = Core.mempty,
                          nextToken = Core.Nothing, responseStatus}

-- | A list of projects.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsProjects :: Lens.Lens' ListProjectsResponse [Types.ProjectSummary]
lprrsProjects = Lens.field @"projects"
{-# INLINEABLE lprrsProjects #-}
{-# DEPRECATED projects "Use generic-lens or generic-optics with 'projects' instead"  #-}

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListProjectsResponse (Core.Maybe Types.PaginationToken)
lprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListProjectsResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
