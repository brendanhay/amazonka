{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeImportImageTasks (..),
    mkDescribeImportImageTasks,

    -- ** Request lenses
    diitDryRun,
    diitFilters,
    diitImportTaskIds,
    diitMaxResults,
    diitNextToken,

    -- * Destructuring the response
    DescribeImportImageTasksResponse (..),
    mkDescribeImportImageTasksResponse,

    -- ** Response lenses
    diitrrsImportImageTasks,
    diitrrsNextToken,
    diitrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImportImageTasks' smart constructor.
data DescribeImportImageTasks = DescribeImportImageTasks'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
    filters :: Core.Maybe [Types.Filter],
    -- | The IDs of the import image tasks.
    importTaskIds :: Core.Maybe [Types.ImportImageTaskId],
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Int,
    -- | A token that indicates the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImportImageTasks' value with any optional fields omitted.
mkDescribeImportImageTasks ::
  DescribeImportImageTasks
mkDescribeImportImageTasks =
  DescribeImportImageTasks'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      importTaskIds = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitDryRun :: Lens.Lens' DescribeImportImageTasks (Core.Maybe Core.Bool)
diitDryRun = Lens.field @"dryRun"
{-# DEPRECATED diitDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitFilters :: Lens.Lens' DescribeImportImageTasks (Core.Maybe [Types.Filter])
diitFilters = Lens.field @"filters"
{-# DEPRECATED diitFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the import image tasks.
--
-- /Note:/ Consider using 'importTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitImportTaskIds :: Lens.Lens' DescribeImportImageTasks (Core.Maybe [Types.ImportImageTaskId])
diitImportTaskIds = Lens.field @"importTaskIds"
{-# DEPRECATED diitImportTaskIds "Use generic-lens or generic-optics with 'importTaskIds' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitMaxResults :: Lens.Lens' DescribeImportImageTasks (Core.Maybe Core.Int)
diitMaxResults = Lens.field @"maxResults"
{-# DEPRECATED diitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token that indicates the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitNextToken :: Lens.Lens' DescribeImportImageTasks (Core.Maybe Types.String)
diitNextToken = Lens.field @"nextToken"
{-# DEPRECATED diitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeImportImageTasks where
  type Rs DescribeImportImageTasks = DescribeImportImageTasksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeImportImageTasks")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filters" Core.<$> filters)
                Core.<> (Core.toQueryList "ImportTaskId" Core.<$> importTaskIds)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImportImageTasksResponse'
            Core.<$> ( x Core..@? "importImageTaskSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeImportImageTasks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"importImageTasks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeImportImageTasksResponse' smart constructor.
data DescribeImportImageTasksResponse = DescribeImportImageTasksResponse'
  { -- | A list of zero or more import image tasks that are currently active or were completed or canceled in the previous 7 days.
    importImageTasks :: Core.Maybe [Types.ImportImageTask],
    -- | The token to use to get the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImportImageTasksResponse' value with any optional fields omitted.
mkDescribeImportImageTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImportImageTasksResponse
mkDescribeImportImageTasksResponse responseStatus =
  DescribeImportImageTasksResponse'
    { importImageTasks =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of zero or more import image tasks that are currently active or were completed or canceled in the previous 7 days.
--
-- /Note:/ Consider using 'importImageTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitrrsImportImageTasks :: Lens.Lens' DescribeImportImageTasksResponse (Core.Maybe [Types.ImportImageTask])
diitrrsImportImageTasks = Lens.field @"importImageTasks"
{-# DEPRECATED diitrrsImportImageTasks "Use generic-lens or generic-optics with 'importImageTasks' instead." #-}

-- | The token to use to get the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitrrsNextToken :: Lens.Lens' DescribeImportImageTasksResponse (Core.Maybe Types.String)
diitrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED diitrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitrrsResponseStatus :: Lens.Lens' DescribeImportImageTasksResponse Core.Int
diitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
