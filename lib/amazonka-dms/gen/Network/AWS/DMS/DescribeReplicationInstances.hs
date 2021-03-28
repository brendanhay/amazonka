{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about replication instances for your account in the current region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationInstances
    (
    -- * Creating a request
      DescribeReplicationInstances (..)
    , mkDescribeReplicationInstances
    -- ** Request lenses
    , driFilters
    , driMarker
    , driMaxRecords

    -- * Destructuring the response
    , DescribeReplicationInstancesResponse (..)
    , mkDescribeReplicationInstancesResponse
    -- ** Response lenses
    , drirfrsMarker
    , drirfrsReplicationInstances
    , drirfrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeReplicationInstances' smart constructor.
data DescribeReplicationInstances = DescribeReplicationInstances'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ Filters applied to replication instances.
--
-- Valid filter names: replication-instance-arn | replication-instance-id | replication-instance-class | engine-version
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplicationInstances' value with any optional fields omitted.
mkDescribeReplicationInstances
    :: DescribeReplicationInstances
mkDescribeReplicationInstances
  = DescribeReplicationInstances'{filters = Core.Nothing,
                                  marker = Core.Nothing, maxRecords = Core.Nothing}

-- | Filters applied to replication instances.
--
-- Valid filter names: replication-instance-arn | replication-instance-id | replication-instance-class | engine-version
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driFilters :: Lens.Lens' DescribeReplicationInstances (Core.Maybe [Types.Filter])
driFilters = Lens.field @"filters"
{-# INLINEABLE driFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driMarker :: Lens.Lens' DescribeReplicationInstances (Core.Maybe Core.Text)
driMarker = Lens.field @"marker"
{-# INLINEABLE driMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driMaxRecords :: Lens.Lens' DescribeReplicationInstances (Core.Maybe Core.Int)
driMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE driMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeReplicationInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeReplicationInstances where
        toHeaders DescribeReplicationInstances{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DescribeReplicationInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeReplicationInstances where
        toJSON DescribeReplicationInstances{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords])

instance Core.AWSRequest DescribeReplicationInstances where
        type Rs DescribeReplicationInstances =
             DescribeReplicationInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeReplicationInstancesResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "ReplicationInstances"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeReplicationInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"replicationInstances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeReplicationInstancesResponse' smart constructor.
data DescribeReplicationInstancesResponse = DescribeReplicationInstancesResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , replicationInstances :: Core.Maybe [Types.ReplicationInstance]
    -- ^ The replication instances described.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReplicationInstancesResponse' value with any optional fields omitted.
mkDescribeReplicationInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReplicationInstancesResponse
mkDescribeReplicationInstancesResponse responseStatus
  = DescribeReplicationInstancesResponse'{marker = Core.Nothing,
                                          replicationInstances = Core.Nothing, responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirfrsMarker :: Lens.Lens' DescribeReplicationInstancesResponse (Core.Maybe Core.Text)
drirfrsMarker = Lens.field @"marker"
{-# INLINEABLE drirfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The replication instances described.
--
-- /Note:/ Consider using 'replicationInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirfrsReplicationInstances :: Lens.Lens' DescribeReplicationInstancesResponse (Core.Maybe [Types.ReplicationInstance])
drirfrsReplicationInstances = Lens.field @"replicationInstances"
{-# INLINEABLE drirfrsReplicationInstances #-}
{-# DEPRECATED replicationInstances "Use generic-lens or generic-optics with 'replicationInstances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirfrsResponseStatus :: Lens.Lens' DescribeReplicationInstancesResponse Core.Int
drirfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drirfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
