{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a snapshot export to Amazon S3. This API operation supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeExportTasks
  ( -- * Creating a request
    DescribeExportTasks (..),
    mkDescribeExportTasks,

    -- ** Request lenses
    detExportTaskIdentifier,
    detFilters,
    detMarker,
    detMaxRecords,
    detSourceArn,

    -- * Destructuring the response
    DescribeExportTasksResponse (..),
    mkDescribeExportTasksResponse,

    -- ** Response lenses
    detrrsExportTasks,
    detrrsMarker,
    detrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | The identifier of the snapshot export task to be described.
    exportTaskIdentifier :: Core.Maybe Types.String,
    -- | Filters specify one or more snapshot exports to describe. The filters are specified as name-value pairs that define what to include in the output. Filter names and values are case-sensitive.
    --
    -- Supported filters include the following:
    --
    --     * @export-task-identifier@ - An identifier for the snapshot export task.
    --
    --
    --     * @s3-bucket@ - The Amazon S3 bucket the snapshot is exported to.
    --
    --
    --     * @source-arn@ - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3
    --
    --
    --     * @status@ - The status of the export task. Must be lowercase, for example, @complete@ .
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous @DescribeExportTasks@ request. If you specify this parameter, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified value, a pagination token called a marker is included in the response. You can use the marker in a later @DescribeExportTasks@ request to retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
    sourceArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExportTasks' value with any optional fields omitted.
mkDescribeExportTasks ::
  DescribeExportTasks
mkDescribeExportTasks =
  DescribeExportTasks'
    { exportTaskIdentifier = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      sourceArn = Core.Nothing
    }

-- | The identifier of the snapshot export task to be described.
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detExportTaskIdentifier :: Lens.Lens' DescribeExportTasks (Core.Maybe Types.String)
detExportTaskIdentifier = Lens.field @"exportTaskIdentifier"
{-# DEPRECATED detExportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead." #-}

-- | Filters specify one or more snapshot exports to describe. The filters are specified as name-value pairs that define what to include in the output. Filter names and values are case-sensitive.
--
-- Supported filters include the following:
--
--     * @export-task-identifier@ - An identifier for the snapshot export task.
--
--
--     * @s3-bucket@ - The Amazon S3 bucket the snapshot is exported to.
--
--
--     * @source-arn@ - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3
--
--
--     * @status@ - The status of the export task. Must be lowercase, for example, @complete@ .
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilters :: Lens.Lens' DescribeExportTasks (Core.Maybe [Types.Filter])
detFilters = Lens.field @"filters"
{-# DEPRECATED detFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeExportTasks@ request. If you specify this parameter, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMarker :: Lens.Lens' DescribeExportTasks (Core.Maybe Types.String)
detMarker = Lens.field @"marker"
{-# DEPRECATED detMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified value, a pagination token called a marker is included in the response. You can use the marker in a later @DescribeExportTasks@ request to retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMaxRecords :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Natural)
detMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED detMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detSourceArn :: Lens.Lens' DescribeExportTasks (Core.Maybe Types.String)
detSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED detSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

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
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue "ExportTaskIdentifier"
                            Core.<$> exportTaskIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "SourceArn" Core.<$> sourceArn)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeExportTasksResult"
      ( \s h x ->
          DescribeExportTasksResponse'
            Core.<$> (x Core..@? "ExportTasks" Core..<@> Core.parseXMLList "ExportTask")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeExportTasks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"exportTasks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | Information about an export of a snapshot to Amazon S3.
    exportTasks :: Core.Maybe [Types.ExportTask],
    -- | A pagination token that can be used in a later @DescribeExportTasks@ request. A marker is used for pagination to identify the location to begin output for the next response of @DescribeExportTasks@ .
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeExportTasksResponse' value with any optional fields omitted.
mkDescribeExportTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeExportTasksResponse
mkDescribeExportTasksResponse responseStatus =
  DescribeExportTasksResponse'
    { exportTasks = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | Information about an export of a snapshot to Amazon S3.
--
-- /Note:/ Consider using 'exportTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsExportTasks :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe [Types.ExportTask])
detrrsExportTasks = Lens.field @"exportTasks"
{-# DEPRECATED detrrsExportTasks "Use generic-lens or generic-optics with 'exportTasks' instead." #-}

-- | A pagination token that can be used in a later @DescribeExportTasks@ request. A marker is used for pagination to identify the location to begin output for the next response of @DescribeExportTasks@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsMarker :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe Types.String)
detrrsMarker = Lens.field @"marker"
{-# DEPRECATED detrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsResponseStatus :: Lens.Lens' DescribeExportTasksResponse Core.Int
detrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED detrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
