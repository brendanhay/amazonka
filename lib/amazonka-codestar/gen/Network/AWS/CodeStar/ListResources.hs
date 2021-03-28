{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists resources associated with a project in AWS CodeStar.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListResources
    (
    -- * Creating a request
      ListResources (..)
    , mkListResources
    -- ** Request lenses
    , lrProjectId
    , lrMaxResults
    , lrNextToken

    -- * Destructuring the response
    , ListResourcesResponse (..)
    , mkListResourcesResponse
    -- ** Response lenses
    , lrrrsNextToken
    , lrrrsResources
    , lrrrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResources' smart constructor.
data ListResources = ListResources'
  { projectId :: Types.ProjectId
    -- ^ The ID of the project.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum amount of data that can be contained in a single set of results.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The continuation token for the next set of results, if the results cannot be returned in one response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResources' value with any optional fields omitted.
mkListResources
    :: Types.ProjectId -- ^ 'projectId'
    -> ListResources
mkListResources projectId
  = ListResources'{projectId, maxResults = Core.Nothing,
                   nextToken = Core.Nothing}

-- | The ID of the project.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrProjectId :: Lens.Lens' ListResources Types.ProjectId
lrProjectId = Lens.field @"projectId"
{-# INLINEABLE lrProjectId #-}
{-# DEPRECATED projectId "Use generic-lens or generic-optics with 'projectId' instead"  #-}

-- | The maximum amount of data that can be contained in a single set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListResources (Core.Maybe Core.Natural)
lrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The continuation token for the next set of results, if the results cannot be returned in one response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListResources (Core.Maybe Types.PaginationToken)
lrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListResources where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResources where
        toHeaders ListResources{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.ListResources")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResources where
        toJSON ListResources{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectId" Core..= projectId),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListResources where
        type Rs ListResources = ListResourcesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResourcesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "resources" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListResources where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"resources" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListResourcesResponse' smart constructor.
data ListResourcesResponse = ListResourcesResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The continuation token to use when requesting the next set of results, if there are more results to be returned.
  , resources :: Core.Maybe [Types.Resource]
    -- ^ An array of resources associated with the project. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourcesResponse' value with any optional fields omitted.
mkListResourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourcesResponse
mkListResourcesResponse responseStatus
  = ListResourcesResponse'{nextToken = Core.Nothing,
                           resources = Core.Nothing, responseStatus}

-- | The continuation token to use when requesting the next set of results, if there are more results to be returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListResourcesResponse (Core.Maybe Types.PaginationToken)
lrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of resources associated with the project. 
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResources :: Lens.Lens' ListResourcesResponse (Core.Maybe [Types.Resource])
lrrrsResources = Lens.field @"resources"
{-# INLINEABLE lrrrsResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResponseStatus :: Lens.Lens' ListResourcesResponse Core.Int
lrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
