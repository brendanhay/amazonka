{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeTableStatistics (..)
    , mkDescribeTableStatistics
    -- ** Request lenses
    , dtsReplicationTaskArn
    , dtsFilters
    , dtsMarker
    , dtsMaxRecords

    -- * Destructuring the response
    , DescribeTableStatisticsResponse (..)
    , mkDescribeTableStatisticsResponse
    -- ** Response lenses
    , dtsrrsMarker
    , dtsrrsReplicationTaskArn
    , dtsrrsTableStatistics
    , dtsrrsResponseStatus
    ) where

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
  { replicationTaskArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication task.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ Filters applied to table statistics.
--
-- Valid filter names: schema-name | table-name | table-state
-- A combination of filters creates an AND condition where each record matches all specified filters.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 500.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTableStatistics' value with any optional fields omitted.
mkDescribeTableStatistics
    :: Core.Text -- ^ 'replicationTaskArn'
    -> DescribeTableStatistics
mkDescribeTableStatistics replicationTaskArn
  = DescribeTableStatistics'{replicationTaskArn,
                             filters = Core.Nothing, marker = Core.Nothing,
                             maxRecords = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsReplicationTaskArn :: Lens.Lens' DescribeTableStatistics Core.Text
dtsReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE dtsReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

-- | Filters applied to table statistics.
--
-- Valid filter names: schema-name | table-name | table-state
-- A combination of filters creates an AND condition where each record matches all specified filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsFilters :: Lens.Lens' DescribeTableStatistics (Core.Maybe [Types.Filter])
dtsFilters = Lens.field @"filters"
{-# INLINEABLE dtsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsMarker :: Lens.Lens' DescribeTableStatistics (Core.Maybe Core.Text)
dtsMarker = Lens.field @"marker"
{-# INLINEABLE dtsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 500.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsMaxRecords :: Lens.Lens' DescribeTableStatistics (Core.Maybe Core.Int)
dtsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dtsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeTableStatistics where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTableStatistics where
        toHeaders DescribeTableStatistics{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DescribeTableStatistics")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTableStatistics where
        toJSON DescribeTableStatistics{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn),
                  ("Filters" Core..=) Core.<$> filters,
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords])

instance Core.AWSRequest DescribeTableStatistics where
        type Rs DescribeTableStatistics = DescribeTableStatisticsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTableStatisticsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "ReplicationTaskArn"
                     Core.<*> x Core..:? "TableStatistics"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTableStatistics where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"tableStatistics" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeTableStatisticsResponse' smart constructor.
data DescribeTableStatisticsResponse = DescribeTableStatisticsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , replicationTaskArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication task.
  , tableStatistics :: Core.Maybe [Types.TableStatistics]
    -- ^ The table statistics.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTableStatisticsResponse' value with any optional fields omitted.
mkDescribeTableStatisticsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTableStatisticsResponse
mkDescribeTableStatisticsResponse responseStatus
  = DescribeTableStatisticsResponse'{marker = Core.Nothing,
                                     replicationTaskArn = Core.Nothing,
                                     tableStatistics = Core.Nothing, responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsMarker :: Lens.Lens' DescribeTableStatisticsResponse (Core.Maybe Core.Text)
dtsrrsMarker = Lens.field @"marker"
{-# INLINEABLE dtsrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsReplicationTaskArn :: Lens.Lens' DescribeTableStatisticsResponse (Core.Maybe Core.Text)
dtsrrsReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE dtsrrsReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

-- | The table statistics.
--
-- /Note:/ Consider using 'tableStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsTableStatistics :: Lens.Lens' DescribeTableStatisticsResponse (Core.Maybe [Types.TableStatistics])
dtsrrsTableStatistics = Lens.field @"tableStatistics"
{-# INLINEABLE dtsrrsTableStatistics #-}
{-# DEPRECATED tableStatistics "Use generic-lens or generic-optics with 'tableStatistics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsResponseStatus :: Lens.Lens' DescribeTableStatisticsResponse Core.Int
dtsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
