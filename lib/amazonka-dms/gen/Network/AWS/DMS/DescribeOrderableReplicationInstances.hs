{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeOrderableReplicationInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the replication instance types that can be created in the specified region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeOrderableReplicationInstances
    (
    -- * Creating a request
      DescribeOrderableReplicationInstances (..)
    , mkDescribeOrderableReplicationInstances
    -- ** Request lenses
    , doriMarker
    , doriMaxRecords

    -- * Destructuring the response
    , DescribeOrderableReplicationInstancesResponse (..)
    , mkDescribeOrderableReplicationInstancesResponse
    -- ** Response lenses
    , dorirrsMarker
    , dorirrsOrderableReplicationInstances
    , dorirrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeOrderableReplicationInstances' smart constructor.
data DescribeOrderableReplicationInstances = DescribeOrderableReplicationInstances'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrderableReplicationInstances' value with any optional fields omitted.
mkDescribeOrderableReplicationInstances
    :: DescribeOrderableReplicationInstances
mkDescribeOrderableReplicationInstances
  = DescribeOrderableReplicationInstances'{marker = Core.Nothing,
                                           maxRecords = Core.Nothing}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doriMarker :: Lens.Lens' DescribeOrderableReplicationInstances (Core.Maybe Core.Text)
doriMarker = Lens.field @"marker"
{-# INLINEABLE doriMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doriMaxRecords :: Lens.Lens' DescribeOrderableReplicationInstances (Core.Maybe Core.Int)
doriMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE doriMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeOrderableReplicationInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOrderableReplicationInstances where
        toHeaders DescribeOrderableReplicationInstances{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.DescribeOrderableReplicationInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeOrderableReplicationInstances where
        toJSON DescribeOrderableReplicationInstances{..}
          = Core.object
              (Core.catMaybes
                 [("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords])

instance Core.AWSRequest DescribeOrderableReplicationInstances
         where
        type Rs DescribeOrderableReplicationInstances =
             DescribeOrderableReplicationInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOrderableReplicationInstancesResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*>
                     x Core..:? "OrderableReplicationInstances"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeOrderableReplicationInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"orderableReplicationInstances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeOrderableReplicationInstancesResponse' smart constructor.
data DescribeOrderableReplicationInstancesResponse = DescribeOrderableReplicationInstancesResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , orderableReplicationInstances :: Core.Maybe [Types.OrderableReplicationInstance]
    -- ^ The order-able replication instances available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrderableReplicationInstancesResponse' value with any optional fields omitted.
mkDescribeOrderableReplicationInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOrderableReplicationInstancesResponse
mkDescribeOrderableReplicationInstancesResponse responseStatus
  = DescribeOrderableReplicationInstancesResponse'{marker =
                                                     Core.Nothing,
                                                   orderableReplicationInstances = Core.Nothing,
                                                   responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorirrsMarker :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Core.Maybe Core.Text)
dorirrsMarker = Lens.field @"marker"
{-# INLINEABLE dorirrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The order-able replication instances available.
--
-- /Note:/ Consider using 'orderableReplicationInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorirrsOrderableReplicationInstances :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Core.Maybe [Types.OrderableReplicationInstance])
dorirrsOrderableReplicationInstances = Lens.field @"orderableReplicationInstances"
{-# INLINEABLE dorirrsOrderableReplicationInstances #-}
{-# DEPRECATED orderableReplicationInstances "Use generic-lens or generic-optics with 'orderableReplicationInstances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorirrsResponseStatus :: Lens.Lens' DescribeOrderableReplicationInstancesResponse Core.Int
dorirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dorirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
