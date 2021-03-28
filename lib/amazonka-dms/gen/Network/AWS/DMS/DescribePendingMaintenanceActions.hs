{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For internal use only
module Network.AWS.DMS.DescribePendingMaintenanceActions
    (
    -- * Creating a request
      DescribePendingMaintenanceActions (..)
    , mkDescribePendingMaintenanceActions
    -- ** Request lenses
    , dpmaFilters
    , dpmaMarker
    , dpmaMaxRecords
    , dpmaReplicationInstanceArn

    -- * Destructuring the response
    , DescribePendingMaintenanceActionsResponse (..)
    , mkDescribePendingMaintenanceActionsResponse
    -- ** Response lenses
    , dpmarrsMarker
    , dpmarrsPendingMaintenanceActions
    , dpmarrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , replicationInstanceArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePendingMaintenanceActions' value with any optional fields omitted.
mkDescribePendingMaintenanceActions
    :: DescribePendingMaintenanceActions
mkDescribePendingMaintenanceActions
  = DescribePendingMaintenanceActions'{filters = Core.Nothing,
                                       marker = Core.Nothing, maxRecords = Core.Nothing,
                                       replicationInstanceArn = Core.Nothing}

-- | 
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaFilters :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe [Types.Filter])
dpmaFilters = Lens.field @"filters"
{-# INLINEABLE dpmaFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaMarker :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Text)
dpmaMarker = Lens.field @"marker"
{-# INLINEABLE dpmaMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaMaxRecords :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Int)
dpmaMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dpmaMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaReplicationInstanceArn :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Text)
dpmaReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE dpmaReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

instance Core.ToQuery DescribePendingMaintenanceActions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePendingMaintenanceActions where
        toHeaders DescribePendingMaintenanceActions{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.DescribePendingMaintenanceActions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribePendingMaintenanceActions where
        toJSON DescribePendingMaintenanceActions{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords,
                  ("ReplicationInstanceArn" Core..=) Core.<$>
                    replicationInstanceArn])

instance Core.AWSRequest DescribePendingMaintenanceActions where
        type Rs DescribePendingMaintenanceActions =
             DescribePendingMaintenanceActionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePendingMaintenanceActionsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*>
                     x Core..:? "PendingMaintenanceActions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDescribePendingMaintenanceActionsResponse' smart constructor.
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , pendingMaintenanceActions :: Core.Maybe [Types.ResourcePendingMaintenanceActions]
    -- ^ The pending maintenance action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribePendingMaintenanceActionsResponse' value with any optional fields omitted.
mkDescribePendingMaintenanceActionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePendingMaintenanceActionsResponse
mkDescribePendingMaintenanceActionsResponse responseStatus
  = DescribePendingMaintenanceActionsResponse'{marker = Core.Nothing,
                                               pendingMaintenanceActions = Core.Nothing,
                                               responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarrsMarker :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Core.Maybe Core.Text)
dpmarrsMarker = Lens.field @"marker"
{-# INLINEABLE dpmarrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The pending maintenance action.
--
-- /Note:/ Consider using 'pendingMaintenanceActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarrsPendingMaintenanceActions :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Core.Maybe [Types.ResourcePendingMaintenanceActions])
dpmarrsPendingMaintenanceActions = Lens.field @"pendingMaintenanceActions"
{-# INLINEABLE dpmarrsPendingMaintenanceActions #-}
{-# DEPRECATED pendingMaintenanceActions "Use generic-lens or generic-optics with 'pendingMaintenanceActions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarrsResponseStatus :: Lens.Lens' DescribePendingMaintenanceActionsResponse Core.Int
dpmarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpmarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
