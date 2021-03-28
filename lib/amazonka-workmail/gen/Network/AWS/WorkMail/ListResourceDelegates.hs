{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListResourceDelegates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the delegates associated with a resource. Users and groups can be resource delegates and answer requests on behalf of the resource.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListResourceDelegates
    (
    -- * Creating a request
      ListResourceDelegates (..)
    , mkListResourceDelegates
    -- ** Request lenses
    , lrdOrganizationId
    , lrdResourceId
    , lrdMaxResults
    , lrdNextToken

    -- * Destructuring the response
    , ListResourceDelegatesResponse (..)
    , mkListResourceDelegatesResponse
    -- ** Response lenses
    , lrdrrsDelegates
    , lrdrrsNextToken
    , lrdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkListResourceDelegates' smart constructor.
data ListResourceDelegates = ListResourceDelegates'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization that contains the resource for which delegates are listed.
  , resourceId :: Types.ResourceId
    -- ^ The identifier for the resource whose delegates are listed.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The number of maximum results in a page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token used to paginate through the delegates associated with a resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceDelegates' value with any optional fields omitted.
mkListResourceDelegates
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.ResourceId -- ^ 'resourceId'
    -> ListResourceDelegates
mkListResourceDelegates organizationId resourceId
  = ListResourceDelegates'{organizationId, resourceId,
                           maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The identifier for the organization that contains the resource for which delegates are listed.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdOrganizationId :: Lens.Lens' ListResourceDelegates Types.OrganizationId
lrdOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE lrdOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier for the resource whose delegates are listed.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdResourceId :: Lens.Lens' ListResourceDelegates Types.ResourceId
lrdResourceId = Lens.field @"resourceId"
{-# INLINEABLE lrdResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The number of maximum results in a page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdMaxResults :: Lens.Lens' ListResourceDelegates (Core.Maybe Core.Natural)
lrdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token used to paginate through the delegates associated with a resource.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdNextToken :: Lens.Lens' ListResourceDelegates (Core.Maybe Types.NextToken)
lrdNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListResourceDelegates where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResourceDelegates where
        toHeaders ListResourceDelegates{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.ListResourceDelegates")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResourceDelegates where
        toJSON ListResourceDelegates{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("ResourceId" Core..= resourceId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListResourceDelegates where
        type Rs ListResourceDelegates = ListResourceDelegatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResourceDelegatesResponse' Core.<$>
                   (x Core..:? "Delegates") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListResourceDelegates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"delegates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListResourceDelegatesResponse' smart constructor.
data ListResourceDelegatesResponse = ListResourceDelegatesResponse'
  { delegates :: Core.Maybe [Types.Delegate]
    -- ^ One page of the resource's delegates.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token used to paginate through the delegates associated with a resource. While results are still available, it has an associated value. When the last page is reached, the token is empty. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceDelegatesResponse' value with any optional fields omitted.
mkListResourceDelegatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourceDelegatesResponse
mkListResourceDelegatesResponse responseStatus
  = ListResourceDelegatesResponse'{delegates = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | One page of the resource's delegates.
--
-- /Note:/ Consider using 'delegates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrrsDelegates :: Lens.Lens' ListResourceDelegatesResponse (Core.Maybe [Types.Delegate])
lrdrrsDelegates = Lens.field @"delegates"
{-# INLINEABLE lrdrrsDelegates #-}
{-# DEPRECATED delegates "Use generic-lens or generic-optics with 'delegates' instead"  #-}

-- | The token used to paginate through the delegates associated with a resource. While results are still available, it has an associated value. When the last page is reached, the token is empty. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrrsNextToken :: Lens.Lens' ListResourceDelegatesResponse (Core.Maybe Types.NextToken)
lrdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrrsResponseStatus :: Lens.Lens' ListResourceDelegatesResponse Core.Int
lrdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
