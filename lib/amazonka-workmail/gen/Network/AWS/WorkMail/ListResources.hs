{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization's resources.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListResources
    (
    -- * Creating a request
      ListResources (..)
    , mkListResources
    -- ** Request lenses
    , lrOrganizationId
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkListResources' smart constructor.
data ListResources = ListResources'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization under which the resources exist.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. The first call does not contain any tokens.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResources' value with any optional fields omitted.
mkListResources
    :: Types.OrganizationId -- ^ 'organizationId'
    -> ListResources
mkListResources organizationId
  = ListResources'{organizationId, maxResults = Core.Nothing,
                   nextToken = Core.Nothing}

-- | The identifier for the organization under which the resources exist.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrOrganizationId :: Lens.Lens' ListResources Types.OrganizationId
lrOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE lrOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListResources (Core.Maybe Core.Natural)
lrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListResources (Core.Maybe Types.NextToken)
lrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListResources where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResources where
        toHeaders ListResources{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.ListResources")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResources where
        toJSON ListResources{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

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
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Resources" Core.<*>
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
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token used to paginate through all the organization's resources. While results are still available, it has an associated value. When the last page is reached, the token is empty.
  , resources :: Core.Maybe [Types.Resource]
    -- ^ One page of the organization's resource representation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListResourcesResponse' value with any optional fields omitted.
mkListResourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourcesResponse
mkListResourcesResponse responseStatus
  = ListResourcesResponse'{nextToken = Core.Nothing,
                           resources = Core.Nothing, responseStatus}

-- | The token used to paginate through all the organization's resources. While results are still available, it has an associated value. When the last page is reached, the token is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListResourcesResponse (Core.Maybe Types.NextToken)
lrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One page of the organization's resource representation.
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
