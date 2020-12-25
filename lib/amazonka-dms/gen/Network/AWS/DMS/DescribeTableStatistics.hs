{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeTableStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table statistics on the database migration task, including table name, rows inserted, rows updated, and rows deleted.
--
-- Note that the "last updated" column the DMS console only indicates the time that AWS DMS last updated the table statistics record for a table. It does not indicate the time of the last update to the table.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeTableStatistics
  ( -- * Creating a request
    DescribeTableStatistics (..),
    mkDescribeTableStatistics,

    -- ** Request lenses
    dtsReplicationTaskArn,
    dtsFilters,
    dtsMarker,
    dtsMaxRecords,

    -- * Destructuring the response
    DescribeTableStatisticsResponse (..),
    mkDescribeTableStatisticsResponse,

    -- ** Response lenses
    dtsrrsMarker,
    dtsrrsReplicationTaskArn,
    dtsrrsTableStatistics,
    dtsrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeTableStatistics' smart constructor.
data DescribeTableStatistics = DescribeTableStatistics'
  { -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Types.ReplicationTaskArn,
    -- | Filters applied to table statistics.
    --
    -- Valid filter names: schema-name | table-name | table-state
    -- A combination of filters creates an AND condition where each record matches all specified filters.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 500.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTableStatistics' value with any optional fields omitted.
mkDescribeTableStatistics ::
  -- | 'replicationTaskArn'
  Types.ReplicationTaskArn ->
  DescribeTableStatistics
mkDescribeTableStatistics replicationTaskArn =
  DescribeTableStatistics'
    { replicationTaskArn,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsReplicationTaskArn :: Lens.Lens' DescribeTableStatistics Types.ReplicationTaskArn
dtsReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# DEPRECATED dtsReplicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead." #-}

-- | Filters applied to table statistics.
--
-- Valid filter names: schema-name | table-name | table-state
-- A combination of filters creates an AND condition where each record matches all specified filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsFilters :: Lens.Lens' DescribeTableStatistics (Core.Maybe [Types.Filter])
dtsFilters = Lens.field @"filters"
{-# DEPRECATED dtsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsMarker :: Lens.Lens' DescribeTableStatistics (Core.Maybe Types.Marker)
dtsMarker = Lens.field @"marker"
{-# DEPRECATED dtsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 500.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsMaxRecords :: Lens.Lens' DescribeTableStatistics (Core.Maybe Core.Int)
dtsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dtsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.FromJSON DescribeTableStatistics where
  toJSON DescribeTableStatistics {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn),
            ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance Core.AWSRequest DescribeTableStatistics where
  type Rs DescribeTableStatistics = DescribeTableStatisticsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.DescribeTableStatistics")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTableStatisticsResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "ReplicationTaskArn")
            Core.<*> (x Core..:? "TableStatistics")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTableStatistics where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"tableStatistics" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- |
--
-- /See:/ 'mkDescribeTableStatisticsResponse' smart constructor.
data DescribeTableStatisticsResponse = DescribeTableStatisticsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Core.Maybe Types.String,
    -- | The table statistics.
    tableStatistics :: Core.Maybe [Types.TableStatistics],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTableStatisticsResponse' value with any optional fields omitted.
mkDescribeTableStatisticsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTableStatisticsResponse
mkDescribeTableStatisticsResponse responseStatus =
  DescribeTableStatisticsResponse'
    { marker = Core.Nothing,
      replicationTaskArn = Core.Nothing,
      tableStatistics = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsMarker :: Lens.Lens' DescribeTableStatisticsResponse (Core.Maybe Types.String)
dtsrrsMarker = Lens.field @"marker"
{-# DEPRECATED dtsrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsReplicationTaskArn :: Lens.Lens' DescribeTableStatisticsResponse (Core.Maybe Types.String)
dtsrrsReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# DEPRECATED dtsrrsReplicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead." #-}

-- | The table statistics.
--
-- /Note:/ Consider using 'tableStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsTableStatistics :: Lens.Lens' DescribeTableStatisticsResponse (Core.Maybe [Types.TableStatistics])
dtsrrsTableStatistics = Lens.field @"tableStatistics"
{-# DEPRECATED dtsrrsTableStatistics "Use generic-lens or generic-optics with 'tableStatistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsResponseStatus :: Lens.Lens' DescribeTableStatisticsResponse Core.Int
dtsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
