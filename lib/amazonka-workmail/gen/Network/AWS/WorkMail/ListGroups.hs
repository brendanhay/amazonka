{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization's groups.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListGroups
    (
    -- * Creating a request
      ListGroups (..)
    , mkListGroups
    -- ** Request lenses
    , lgOrganizationId
    , lgMaxResults
    , lgNextToken

    -- * Destructuring the response
    , ListGroupsResponse (..)
    , mkListGroupsResponse
    -- ** Response lenses
    , lgrrsGroups
    , lgrrsNextToken
    , lgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkListGroups' smart constructor.
data ListGroups = ListGroups'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization under which the groups exist.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. The first call does not contain any tokens.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroups' value with any optional fields omitted.
mkListGroups
    :: Types.OrganizationId -- ^ 'organizationId'
    -> ListGroups
mkListGroups organizationId
  = ListGroups'{organizationId, maxResults = Core.Nothing,
                nextToken = Core.Nothing}

-- | The identifier for the organization under which the groups exist.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgOrganizationId :: Lens.Lens' ListGroups Types.OrganizationId
lgOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE lgOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMaxResults :: Lens.Lens' ListGroups (Core.Maybe Core.Natural)
lgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextToken :: Lens.Lens' ListGroups (Core.Maybe Types.NextToken)
lgNextToken = Lens.field @"nextToken"
{-# INLINEABLE lgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListGroups where
        toHeaders ListGroups{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.ListGroups") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListGroups where
        toJSON ListGroups{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListGroups where
        type Rs ListGroups = ListGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListGroupsResponse' Core.<$>
                   (x Core..:? "Groups") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"groups" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { groups :: Core.Maybe [Types.Group]
    -- ^ The overview of groups for an organization.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListGroupsResponse' value with any optional fields omitted.
mkListGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListGroupsResponse
mkListGroupsResponse responseStatus
  = ListGroupsResponse'{groups = Core.Nothing,
                        nextToken = Core.Nothing, responseStatus}

-- | The overview of groups for an organization.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsGroups :: Lens.Lens' ListGroupsResponse (Core.Maybe [Types.Group])
lgrrsGroups = Lens.field @"groups"
{-# INLINEABLE lgrrsGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsNextToken :: Lens.Lens' ListGroupsResponse (Core.Maybe Types.NextToken)
lgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsResponseStatus :: Lens.Lens' ListGroupsResponse Core.Int
lgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
