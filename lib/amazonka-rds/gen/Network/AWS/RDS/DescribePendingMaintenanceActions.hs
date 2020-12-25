{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resources (for example, DB instances) that have at least one pending maintenance action.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribePendingMaintenanceActions
  ( -- * Creating a request
    DescribePendingMaintenanceActions (..),
    mkDescribePendingMaintenanceActions,

    -- ** Request lenses
    dpmaFilters,
    dpmaMarker,
    dpmaMaxRecords,
    dpmaResourceIdentifier,

    -- * Destructuring the response
    DescribePendingMaintenanceActionsResponse (..),
    mkDescribePendingMaintenanceActionsResponse,

    -- ** Response lenses
    dpmarrsMarker,
    dpmarrsPendingMaintenanceActions,
    dpmarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { -- | A filter that specifies one or more resources to return pending maintenance actions for.
    --
    -- Supported filters:
    --
    --     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include pending maintenance actions for the DB clusters identified by these ARNs.
    --
    --
    --     * @db-instance-id@ - Accepts DB instance identifiers and DB instance ARNs. The results list will only include pending maintenance actions for the DB instances identified by these ARNs.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The ARN of a resource to return pending maintenance actions for.
    resourceIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePendingMaintenanceActions' value with any optional fields omitted.
mkDescribePendingMaintenanceActions ::
  DescribePendingMaintenanceActions
mkDescribePendingMaintenanceActions =
  DescribePendingMaintenanceActions'
    { filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      resourceIdentifier = Core.Nothing
    }

-- | A filter that specifies one or more resources to return pending maintenance actions for.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include pending maintenance actions for the DB clusters identified by these ARNs.
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and DB instance ARNs. The results list will only include pending maintenance actions for the DB instances identified by these ARNs.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaFilters :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe [Types.Filter])
dpmaFilters = Lens.field @"filters"
{-# DEPRECATED dpmaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaMarker :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Types.String)
dpmaMarker = Lens.field @"marker"
{-# DEPRECATED dpmaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaMaxRecords :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Core.Int)
dpmaMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dpmaMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The ARN of a resource to return pending maintenance actions for.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmaResourceIdentifier :: Lens.Lens' DescribePendingMaintenanceActions (Core.Maybe Types.String)
dpmaResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED dpmaResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Core.AWSRequest DescribePendingMaintenanceActions where
  type
    Rs DescribePendingMaintenanceActions =
      DescribePendingMaintenanceActionsResponse
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
            ( Core.pure ("Action", "DescribePendingMaintenanceActions")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> ( Core.toQueryValue "ResourceIdentifier"
                            Core.<$> resourceIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribePendingMaintenanceActionsResult"
      ( \s h x ->
          DescribePendingMaintenanceActionsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "PendingMaintenanceActions"
                         Core..<@> Core.parseXMLList "ResourcePendingMaintenanceActions"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribePendingMaintenanceActions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"pendingMaintenanceActions" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Data returned from the __DescribePendingMaintenanceActions__ action.
--
-- /See:/ 'mkDescribePendingMaintenanceActionsResponse' smart constructor.
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
  { -- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | A list of the pending maintenance actions for the resource.
    pendingMaintenanceActions :: Core.Maybe [Types.ResourcePendingMaintenanceActions],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribePendingMaintenanceActionsResponse' value with any optional fields omitted.
mkDescribePendingMaintenanceActionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePendingMaintenanceActionsResponse
mkDescribePendingMaintenanceActionsResponse responseStatus =
  DescribePendingMaintenanceActionsResponse'
    { marker = Core.Nothing,
      pendingMaintenanceActions = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarrsMarker :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Core.Maybe Types.String)
dpmarrsMarker = Lens.field @"marker"
{-# DEPRECATED dpmarrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of the pending maintenance actions for the resource.
--
-- /Note:/ Consider using 'pendingMaintenanceActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarrsPendingMaintenanceActions :: Lens.Lens' DescribePendingMaintenanceActionsResponse (Core.Maybe [Types.ResourcePendingMaintenanceActions])
dpmarrsPendingMaintenanceActions = Lens.field @"pendingMaintenanceActions"
{-# DEPRECATED dpmarrsPendingMaintenanceActions "Use generic-lens or generic-optics with 'pendingMaintenanceActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpmarrsResponseStatus :: Lens.Lens' DescribePendingMaintenanceActionsResponse Core.Int
dpmarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpmarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
