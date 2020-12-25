{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export instance tasks or all of your export instance tasks.
module Network.AWS.EC2.DescribeExportTasks
  ( -- * Creating a request
    DescribeExportTasks (..),
    mkDescribeExportTasks,

    -- ** Request lenses
    detExportTaskIds,
    detFilters,

    -- * Destructuring the response
    DescribeExportTasksResponse (..),
    mkDescribeExportTasksResponse,

    -- ** Response lenses
    detrrsExportTasks,
    detrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | The export task IDs.
    exportTaskIds :: Core.Maybe [Types.ExportTaskId],
    -- | the filters for the export tasks.
    filters :: Core.Maybe [Types.Filter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExportTasks' value with any optional fields omitted.
mkDescribeExportTasks ::
  DescribeExportTasks
mkDescribeExportTasks =
  DescribeExportTasks'
    { exportTaskIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The export task IDs.
--
-- /Note:/ Consider using 'exportTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detExportTaskIds :: Lens.Lens' DescribeExportTasks (Core.Maybe [Types.ExportTaskId])
detExportTaskIds = Lens.field @"exportTaskIds"
{-# DEPRECATED detExportTaskIds "Use generic-lens or generic-optics with 'exportTaskIds' instead." #-}

-- | the filters for the export tasks.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilters :: Lens.Lens' DescribeExportTasks (Core.Maybe [Types.Filter])
detFilters = Lens.field @"filters"
{-# DEPRECATED detFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Core.AWSRequest DescribeExportTasks where
  type Rs DescribeExportTasks = DescribeExportTasksResponse
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
            ( Core.pure ("Action", "DescribeExportTasks")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "ExportTaskId" Core.<$> exportTaskIds)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeExportTasksResponse'
            Core.<$> (x Core..@? "exportTaskSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | Information about the export tasks.
    exportTasks :: Core.Maybe [Types.ExportTask],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExportTasksResponse' value with any optional fields omitted.
mkDescribeExportTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeExportTasksResponse
mkDescribeExportTasksResponse responseStatus =
  DescribeExportTasksResponse'
    { exportTasks = Core.Nothing,
      responseStatus
    }

-- | Information about the export tasks.
--
-- /Note:/ Consider using 'exportTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsExportTasks :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe [Types.ExportTask])
detrrsExportTasks = Lens.field @"exportTasks"
{-# DEPRECATED detrrsExportTasks "Use generic-lens or generic-optics with 'exportTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsResponseStatus :: Lens.Lens' DescribeExportTasksResponse Core.Int
detrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED detrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
