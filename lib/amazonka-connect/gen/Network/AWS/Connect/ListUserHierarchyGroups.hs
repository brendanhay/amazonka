{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListUserHierarchyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the hierarchy groups for the specified Amazon Connect instance.
--
-- For more information about agent hierarchies, see <https://docs.aws.amazon.com/connect/latest/adminguide/agent-hierarchy.html Set Up Agent Hierarchies> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUserHierarchyGroups
    (
    -- * Creating a request
      ListUserHierarchyGroups (..)
    , mkListUserHierarchyGroups
    -- ** Request lenses
    , luhgInstanceId
    , luhgMaxResults
    , luhgNextToken

    -- * Destructuring the response
    , ListUserHierarchyGroupsResponse (..)
    , mkListUserHierarchyGroupsResponse
    -- ** Response lenses
    , luhgrrsNextToken
    , luhgrrsUserHierarchyGroupSummaryList
    , luhgrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListUserHierarchyGroups' smart constructor.
data ListUserHierarchyGroups = ListUserHierarchyGroups'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximimum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserHierarchyGroups' value with any optional fields omitted.
mkListUserHierarchyGroups
    :: Types.InstanceId -- ^ 'instanceId'
    -> ListUserHierarchyGroups
mkListUserHierarchyGroups instanceId
  = ListUserHierarchyGroups'{instanceId, maxResults = Core.Nothing,
                             nextToken = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgInstanceId :: Lens.Lens' ListUserHierarchyGroups Types.InstanceId
luhgInstanceId = Lens.field @"instanceId"
{-# INLINEABLE luhgInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgMaxResults :: Lens.Lens' ListUserHierarchyGroups (Core.Maybe Core.Natural)
luhgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE luhgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgNextToken :: Lens.Lens' ListUserHierarchyGroups (Core.Maybe Types.NextToken)
luhgNextToken = Lens.field @"nextToken"
{-# INLINEABLE luhgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListUserHierarchyGroups where
        toQuery ListUserHierarchyGroups{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListUserHierarchyGroups where
        toHeaders ListUserHierarchyGroups{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListUserHierarchyGroups where
        type Rs ListUserHierarchyGroups = ListUserHierarchyGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/user-hierarchy-groups-summary/" Core.<> Core.toText instanceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListUserHierarchyGroupsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "UserHierarchyGroupSummaryList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListUserHierarchyGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"userHierarchyGroupSummaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListUserHierarchyGroupsResponse' smart constructor.
data ListUserHierarchyGroupsResponse = ListUserHierarchyGroupsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If there are additional results, this is the token for the next set of results.
  , userHierarchyGroupSummaryList :: Core.Maybe [Types.HierarchyGroupSummary]
    -- ^ Information about the hierarchy groups.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserHierarchyGroupsResponse' value with any optional fields omitted.
mkListUserHierarchyGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListUserHierarchyGroupsResponse
mkListUserHierarchyGroupsResponse responseStatus
  = ListUserHierarchyGroupsResponse'{nextToken = Core.Nothing,
                                     userHierarchyGroupSummaryList = Core.Nothing, responseStatus}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgrrsNextToken :: Lens.Lens' ListUserHierarchyGroupsResponse (Core.Maybe Types.NextToken)
luhgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE luhgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the hierarchy groups.
--
-- /Note:/ Consider using 'userHierarchyGroupSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgrrsUserHierarchyGroupSummaryList :: Lens.Lens' ListUserHierarchyGroupsResponse (Core.Maybe [Types.HierarchyGroupSummary])
luhgrrsUserHierarchyGroupSummaryList = Lens.field @"userHierarchyGroupSummaryList"
{-# INLINEABLE luhgrrsUserHierarchyGroupSummaryList #-}
{-# DEPRECATED userHierarchyGroupSummaryList "Use generic-lens or generic-optics with 'userHierarchyGroupSummaryList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luhgrrsResponseStatus :: Lens.Lens' ListUserHierarchyGroupsResponse Core.Int
luhgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE luhgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
