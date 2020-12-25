{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeImportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of import tasks for your account, including status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
module Network.AWS.Discovery.DescribeImportTasks
  ( -- * Creating a request
    DescribeImportTasks (..),
    mkDescribeImportTasks,

    -- ** Request lenses
    ditFilters,
    ditMaxResults,
    ditNextToken,

    -- * Destructuring the response
    DescribeImportTasksResponse (..),
    mkDescribeImportTasksResponse,

    -- ** Response lenses
    ditrrsNextToken,
    ditrrsTasks,
    ditrrsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImportTasks' smart constructor.
data DescribeImportTasks = DescribeImportTasks'
  { -- | An array of name-value pairs that you provide to filter the results for the @DescribeImportTask@ request to a specific subset of results. Currently, wildcard values aren't supported for filters.
    filters :: Core.Maybe [Types.ImportTaskFilter],
    -- | The maximum number of results that you want this request to return, up to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to request a specific page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImportTasks' value with any optional fields omitted.
mkDescribeImportTasks ::
  DescribeImportTasks
mkDescribeImportTasks =
  DescribeImportTasks'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | An array of name-value pairs that you provide to filter the results for the @DescribeImportTask@ request to a specific subset of results. Currently, wildcard values aren't supported for filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditFilters :: Lens.Lens' DescribeImportTasks (Core.Maybe [Types.ImportTaskFilter])
ditFilters = Lens.field @"filters"
{-# DEPRECATED ditFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results that you want this request to return, up to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditMaxResults :: Lens.Lens' DescribeImportTasks (Core.Maybe Core.Natural)
ditMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ditMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to request a specific page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditNextToken :: Lens.Lens' DescribeImportTasks (Core.Maybe Types.NextToken)
ditNextToken = Lens.field @"nextToken"
{-# DEPRECATED ditNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeImportTasks where
  toJSON DescribeImportTasks {..} =
    Core.object
      ( Core.catMaybes
          [ ("filters" Core..=) Core.<$> filters,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeImportTasks where
  type Rs DescribeImportTasks = DescribeImportTasksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSPoseidonService_V2015_11_01.DescribeImportTasks"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImportTasksResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "tasks")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeImportTasksResponse' smart constructor.
data DescribeImportTasksResponse = DescribeImportTasksResponse'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A returned array of import tasks that match any applied filters, up to the specified number of maximum results.
    tasks :: Core.Maybe [Types.ImportTask],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeImportTasksResponse' value with any optional fields omitted.
mkDescribeImportTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImportTasksResponse
mkDescribeImportTasksResponse responseStatus =
  DescribeImportTasksResponse'
    { nextToken = Core.Nothing,
      tasks = Core.Nothing,
      responseStatus
    }

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrrsNextToken :: Lens.Lens' DescribeImportTasksResponse (Core.Maybe Types.NextToken)
ditrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ditrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A returned array of import tasks that match any applied filters, up to the specified number of maximum results.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrrsTasks :: Lens.Lens' DescribeImportTasksResponse (Core.Maybe [Types.ImportTask])
ditrrsTasks = Lens.field @"tasks"
{-# DEPRECATED ditrrsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrrsResponseStatus :: Lens.Lens' DescribeImportTasksResponse Core.Int
ditrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ditrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
