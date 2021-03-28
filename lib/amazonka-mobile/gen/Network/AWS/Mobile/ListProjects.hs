{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.ListProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists projects in AWS Mobile Hub. 
--
-- This operation returns paginated results.
module Network.AWS.Mobile.ListProjects
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
    , lprrsNextToken
    , lprrsProjects
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request projects list in AWS Mobile Hub. 
--
-- /See:/ 'mkListProjects' smart constructor.
data ListProjects = ListProjects'
  { maxResults :: Core.Maybe Core.Int
    -- ^ Maximum number of records to list in a single response. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Pagination token. Set to null to start listing projects from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more projects. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProjects' value with any optional fields omitted.
mkListProjects
    :: ListProjects
mkListProjects
  = ListProjects'{maxResults = Core.Nothing,
                  nextToken = Core.Nothing}

-- | Maximum number of records to list in a single response. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListProjects (Core.Maybe Core.Int)
lpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Pagination token. Set to null to start listing projects from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more projects. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListProjects (Core.Maybe Types.NextToken)
lpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListProjects where
        toQuery ListProjects{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListProjects where
        toHeaders ListProjects{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListProjects where
        type Rs ListProjects = ListProjectsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/projects",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListProjectsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "projects" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListProjects where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"projects" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Result structure used for requests to list projects in AWS Mobile Hub. 
--
-- /See:/ 'mkListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { nextToken :: Core.Maybe Types.NextToken
  , projects :: Core.Maybe [Types.ProjectSummary]
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
  = ListProjectsResponse'{nextToken = Core.Nothing,
                          projects = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListProjectsResponse (Core.Maybe Types.NextToken)
lprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsProjects :: Lens.Lens' ListProjectsResponse (Core.Maybe [Types.ProjectSummary])
lprrsProjects = Lens.field @"projects"
{-# INLINEABLE lprrsProjects #-}
{-# DEPRECATED projects "Use generic-lens or generic-optics with 'projects' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListProjectsResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
