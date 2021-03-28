{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeQueries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of CloudWatch Logs Insights queries that are scheduled, executing, or have been executed recently in this account. You can request all queries or limit it to queries of a specific log group or queries with a certain status.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeQueries
    (
    -- * Creating a request
      DescribeQueries (..)
    , mkDescribeQueries
    -- ** Request lenses
    , dqLogGroupName
    , dqMaxResults
    , dqNextToken
    , dqStatus

    -- * Destructuring the response
    , DescribeQueriesResponse (..)
    , mkDescribeQueriesResponse
    -- ** Response lenses
    , dqrrsNextToken
    , dqrrsQueries
    , dqrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeQueries' smart constructor.
data DescribeQueries = DescribeQueries'
  { logGroupName :: Core.Maybe Types.LogGroupName
    -- ^ Limits the returned queries to only those for the specified log group.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Limits the number of returned queries to the specified number.
  , nextToken :: Core.Maybe Types.NextToken
  , status :: Core.Maybe Types.QueryStatus
    -- ^ Limits the returned queries to only those that have the specified status. Valid values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , and @Scheduled@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeQueries' value with any optional fields omitted.
mkDescribeQueries
    :: DescribeQueries
mkDescribeQueries
  = DescribeQueries'{logGroupName = Core.Nothing,
                     maxResults = Core.Nothing, nextToken = Core.Nothing,
                     status = Core.Nothing}

-- | Limits the returned queries to only those for the specified log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqLogGroupName :: Lens.Lens' DescribeQueries (Core.Maybe Types.LogGroupName)
dqLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE dqLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | Limits the number of returned queries to the specified number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqMaxResults :: Lens.Lens' DescribeQueries (Core.Maybe Core.Natural)
dqMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dqMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqNextToken :: Lens.Lens' DescribeQueries (Core.Maybe Types.NextToken)
dqNextToken = Lens.field @"nextToken"
{-# INLINEABLE dqNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Limits the returned queries to only those that have the specified status. Valid values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , and @Scheduled@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqStatus :: Lens.Lens' DescribeQueries (Core.Maybe Types.QueryStatus)
dqStatus = Lens.field @"status"
{-# INLINEABLE dqStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery DescribeQueries where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeQueries where
        toHeaders DescribeQueries{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.DescribeQueries")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeQueries where
        toJSON DescribeQueries{..}
          = Core.object
              (Core.catMaybes
                 [("logGroupName" Core..=) Core.<$> logGroupName,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("status" Core..=) Core.<$> status])

instance Core.AWSRequest DescribeQueries where
        type Rs DescribeQueries = DescribeQueriesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeQueriesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "queries" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeQueries where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"queries" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeQueriesResponse' smart constructor.
data DescribeQueriesResponse = DescribeQueriesResponse'
  { nextToken :: Core.Maybe Types.NextToken
  , queries :: Core.Maybe [Types.QueryInfo]
    -- ^ The list of queries that match the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeQueriesResponse' value with any optional fields omitted.
mkDescribeQueriesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeQueriesResponse
mkDescribeQueriesResponse responseStatus
  = DescribeQueriesResponse'{nextToken = Core.Nothing,
                             queries = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrrsNextToken :: Lens.Lens' DescribeQueriesResponse (Core.Maybe Types.NextToken)
dqrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dqrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of queries that match the request.
--
-- /Note:/ Consider using 'queries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrrsQueries :: Lens.Lens' DescribeQueriesResponse (Core.Maybe [Types.QueryInfo])
dqrrsQueries = Lens.field @"queries"
{-# INLINEABLE dqrrsQueries #-}
{-# DEPRECATED queries "Use generic-lens or generic-optics with 'queries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrrsResponseStatus :: Lens.Lens' DescribeQueriesResponse Core.Int
dqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
