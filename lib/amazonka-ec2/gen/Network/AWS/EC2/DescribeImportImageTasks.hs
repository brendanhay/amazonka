{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImportImageTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an import virtual machine or import snapshot tasks that are already created.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeImportImageTasks
    (
    -- * Creating a request
      DescribeImportImageTasks (..)
    , mkDescribeImportImageTasks
    -- ** Request lenses
    , diitDryRun
    , diitFilters
    , diitImportTaskIds
    , diitMaxResults
    , diitNextToken

    -- * Destructuring the response
    , DescribeImportImageTasksResponse (..)
    , mkDescribeImportImageTasksResponse
    -- ** Response lenses
    , diitrrsImportImageTasks
    , diitrrsNextToken
    , diitrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImportImageTasks' smart constructor.
data DescribeImportImageTasks = DescribeImportImageTasks'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
  , importTaskIds :: Core.Maybe [Types.ImportImageTaskId]
    -- ^ The IDs of the import image tasks.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token that indicates the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImportImageTasks' value with any optional fields omitted.
mkDescribeImportImageTasks
    :: DescribeImportImageTasks
mkDescribeImportImageTasks
  = DescribeImportImageTasks'{dryRun = Core.Nothing,
                              filters = Core.Nothing, importTaskIds = Core.Nothing,
                              maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitDryRun :: Lens.Lens' DescribeImportImageTasks (Core.Maybe Core.Bool)
diitDryRun = Lens.field @"dryRun"
{-# INLINEABLE diitDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitFilters :: Lens.Lens' DescribeImportImageTasks (Core.Maybe [Types.Filter])
diitFilters = Lens.field @"filters"
{-# INLINEABLE diitFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The IDs of the import image tasks.
--
-- /Note:/ Consider using 'importTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitImportTaskIds :: Lens.Lens' DescribeImportImageTasks (Core.Maybe [Types.ImportImageTaskId])
diitImportTaskIds = Lens.field @"importTaskIds"
{-# INLINEABLE diitImportTaskIds #-}
{-# DEPRECATED importTaskIds "Use generic-lens or generic-optics with 'importTaskIds' instead"  #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitMaxResults :: Lens.Lens' DescribeImportImageTasks (Core.Maybe Core.Int)
diitMaxResults = Lens.field @"maxResults"
{-# INLINEABLE diitMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token that indicates the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitNextToken :: Lens.Lens' DescribeImportImageTasks (Core.Maybe Core.Text)
diitNextToken = Lens.field @"nextToken"
{-# INLINEABLE diitNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeImportImageTasks where
        toQuery DescribeImportImageTasks{..}
          = Core.toQueryPair "Action"
              ("DescribeImportImageTasks" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filters") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ImportTaskId")
                importTaskIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeImportImageTasks where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeImportImageTasks where
        type Rs DescribeImportImageTasks = DescribeImportImageTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeImportImageTasksResponse' Core.<$>
                   (x Core..@? "importImageTaskSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeImportImageTasks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"importImageTasks" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeImportImageTasksResponse' smart constructor.
data DescribeImportImageTasksResponse = DescribeImportImageTasksResponse'
  { importImageTasks :: Core.Maybe [Types.ImportImageTask]
    -- ^ A list of zero or more import image tasks that are currently active or were completed or canceled in the previous 7 days.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to get the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImportImageTasksResponse' value with any optional fields omitted.
mkDescribeImportImageTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeImportImageTasksResponse
mkDescribeImportImageTasksResponse responseStatus
  = DescribeImportImageTasksResponse'{importImageTasks =
                                        Core.Nothing,
                                      nextToken = Core.Nothing, responseStatus}

-- | A list of zero or more import image tasks that are currently active or were completed or canceled in the previous 7 days.
--
-- /Note:/ Consider using 'importImageTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitrrsImportImageTasks :: Lens.Lens' DescribeImportImageTasksResponse (Core.Maybe [Types.ImportImageTask])
diitrrsImportImageTasks = Lens.field @"importImageTasks"
{-# INLINEABLE diitrrsImportImageTasks #-}
{-# DEPRECATED importImageTasks "Use generic-lens or generic-optics with 'importImageTasks' instead"  #-}

-- | The token to use to get the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitrrsNextToken :: Lens.Lens' DescribeImportImageTasksResponse (Core.Maybe Core.Text)
diitrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE diitrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitrrsResponseStatus :: Lens.Lens' DescribeImportImageTasksResponse Core.Int
diitrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diitrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
