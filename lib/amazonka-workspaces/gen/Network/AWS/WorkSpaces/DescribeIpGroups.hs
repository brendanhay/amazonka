{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeIpGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your IP access control groups.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeIpGroups
    (
    -- * Creating a request
      DescribeIpGroups (..)
    , mkDescribeIpGroups
    -- ** Request lenses
    , dGroupIds
    , dMaxResults
    , dNextToken

    -- * Destructuring the response
    , DescribeIpGroupsResponse (..)
    , mkDescribeIpGroupsResponse
    -- ** Response lenses
    , digrgrsNextToken
    , digrgrsResult
    , digrgrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeIpGroups' smart constructor.
data DescribeIpGroups = DescribeIpGroups'
  { groupIds :: Core.Maybe [Types.IpGroupId]
    -- ^ The identifiers of one or more IP access control groups.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIpGroups' value with any optional fields omitted.
mkDescribeIpGroups
    :: DescribeIpGroups
mkDescribeIpGroups
  = DescribeIpGroups'{groupIds = Core.Nothing,
                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The identifiers of one or more IP access control groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGroupIds :: Lens.Lens' DescribeIpGroups (Core.Maybe [Types.IpGroupId])
dGroupIds = Lens.field @"groupIds"
{-# INLINEABLE dGroupIds #-}
{-# DEPRECATED groupIds "Use generic-lens or generic-optics with 'groupIds' instead"  #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeIpGroups (Core.Maybe Core.Natural)
dMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeIpGroups (Core.Maybe Types.PaginationToken)
dNextToken = Lens.field @"nextToken"
{-# INLINEABLE dNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeIpGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeIpGroups where
        toHeaders DescribeIpGroups{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.DescribeIpGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeIpGroups where
        toJSON DescribeIpGroups{..}
          = Core.object
              (Core.catMaybes
                 [("GroupIds" Core..=) Core.<$> groupIds,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeIpGroups where
        type Rs DescribeIpGroups = DescribeIpGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeIpGroupsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Result" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeIpGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"result" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeIpGroupsResponse' smart constructor.
data DescribeIpGroupsResponse = DescribeIpGroupsResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token to use to retrieve the next set of results, or null if no more results are available.
  , result :: Core.Maybe [Types.WorkspacesIpGroup]
    -- ^ Information about the IP access control groups.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIpGroupsResponse' value with any optional fields omitted.
mkDescribeIpGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeIpGroupsResponse
mkDescribeIpGroupsResponse responseStatus
  = DescribeIpGroupsResponse'{nextToken = Core.Nothing,
                              result = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrgrsNextToken :: Lens.Lens' DescribeIpGroupsResponse (Core.Maybe Types.PaginationToken)
digrgrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE digrgrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the IP access control groups.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrgrsResult :: Lens.Lens' DescribeIpGroupsResponse (Core.Maybe [Types.WorkspacesIpGroup])
digrgrsResult = Lens.field @"result"
{-# INLINEABLE digrgrsResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrgrsResponseStatus :: Lens.Lens' DescribeIpGroupsResponse Core.Int
digrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE digrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
