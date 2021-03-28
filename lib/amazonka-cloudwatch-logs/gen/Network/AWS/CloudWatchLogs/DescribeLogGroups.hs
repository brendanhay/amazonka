{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified log groups. You can list all your log groups or filter the results by prefix. The results are ASCII-sorted by log group name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeLogGroups
    (
    -- * Creating a request
      DescribeLogGroups (..)
    , mkDescribeLogGroups
    -- ** Request lenses
    , dlgLimit
    , dlgLogGroupNamePrefix
    , dlgNextToken

    -- * Destructuring the response
    , DescribeLogGroupsResponse (..)
    , mkDescribeLogGroupsResponse
    -- ** Response lenses
    , dlgrrsLogGroups
    , dlgrrsNextToken
    , dlgrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLogGroups' smart constructor.
data DescribeLogGroups = DescribeLogGroups'
  { limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
  , logGroupNamePrefix :: Core.Maybe Types.LogGroupNamePrefix
    -- ^ The prefix to match.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLogGroups' value with any optional fields omitted.
mkDescribeLogGroups
    :: DescribeLogGroups
mkDescribeLogGroups
  = DescribeLogGroups'{limit = Core.Nothing,
                       logGroupNamePrefix = Core.Nothing, nextToken = Core.Nothing}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgLimit :: Lens.Lens' DescribeLogGroups (Core.Maybe Core.Natural)
dlgLimit = Lens.field @"limit"
{-# INLINEABLE dlgLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The prefix to match.
--
-- /Note:/ Consider using 'logGroupNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgLogGroupNamePrefix :: Lens.Lens' DescribeLogGroups (Core.Maybe Types.LogGroupNamePrefix)
dlgLogGroupNamePrefix = Lens.field @"logGroupNamePrefix"
{-# INLINEABLE dlgLogGroupNamePrefix #-}
{-# DEPRECATED logGroupNamePrefix "Use generic-lens or generic-optics with 'logGroupNamePrefix' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgNextToken :: Lens.Lens' DescribeLogGroups (Core.Maybe Types.NextToken)
dlgNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeLogGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeLogGroups where
        toHeaders DescribeLogGroups{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.DescribeLogGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeLogGroups where
        toJSON DescribeLogGroups{..}
          = Core.object
              (Core.catMaybes
                 [("limit" Core..=) Core.<$> limit,
                  ("logGroupNamePrefix" Core..=) Core.<$> logGroupNamePrefix,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeLogGroups where
        type Rs DescribeLogGroups = DescribeLogGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeLogGroupsResponse' Core.<$>
                   (x Core..:? "logGroups") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeLogGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"logGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeLogGroupsResponse' smart constructor.
data DescribeLogGroupsResponse = DescribeLogGroupsResponse'
  { logGroups :: Core.Maybe [Types.LogGroup]
    -- ^ The log groups.
--
-- If the @retentionInDays@ value if not included for a log group, then that log group is set to have its events never expire.
  , nextToken :: Core.Maybe Types.NextToken
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLogGroupsResponse' value with any optional fields omitted.
mkDescribeLogGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLogGroupsResponse
mkDescribeLogGroupsResponse responseStatus
  = DescribeLogGroupsResponse'{logGroups = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | The log groups.
--
-- If the @retentionInDays@ value if not included for a log group, then that log group is set to have its events never expire.
--
-- /Note:/ Consider using 'logGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrsLogGroups :: Lens.Lens' DescribeLogGroupsResponse (Core.Maybe [Types.LogGroup])
dlgrrsLogGroups = Lens.field @"logGroups"
{-# INLINEABLE dlgrrsLogGroups #-}
{-# DEPRECATED logGroups "Use generic-lens or generic-optics with 'logGroups' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrsNextToken :: Lens.Lens' DescribeLogGroupsResponse (Core.Maybe Types.NextToken)
dlgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrsResponseStatus :: Lens.Lens' DescribeLogGroupsResponse Core.Int
dlgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
