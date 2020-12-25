{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the connections that have been made between the replication instance and an endpoint. Connections are created when you test an endpoint.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeConnections
  ( -- * Creating a request
    DescribeConnections (..),
    mkDescribeConnections,

    -- ** Request lenses
    dcFilters,
    dcMarker,
    dcMaxRecords,

    -- * Destructuring the response
    DescribeConnectionsResponse (..),
    mkDescribeConnectionsResponse,

    -- ** Response lenses
    dcrfrsConnections,
    dcrfrsMarker,
    dcrfrsResponseStatus,
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
-- /See:/ 'mkDescribeConnections' smart constructor.
data DescribeConnections = DescribeConnections'
  { -- | The filters applied to the connection.
    --
    -- Valid filter names: endpoint-arn | replication-instance-arn
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConnections' value with any optional fields omitted.
mkDescribeConnections ::
  DescribeConnections
mkDescribeConnections =
  DescribeConnections'
    { filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The filters applied to the connection.
--
-- Valid filter names: endpoint-arn | replication-instance-arn
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcFilters :: Lens.Lens' DescribeConnections (Core.Maybe [Types.Filter])
dcFilters = Lens.field @"filters"
{-# DEPRECATED dcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMarker :: Lens.Lens' DescribeConnections (Core.Maybe Types.String)
dcMarker = Lens.field @"marker"
{-# DEPRECATED dcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxRecords :: Lens.Lens' DescribeConnections (Core.Maybe Core.Int)
dcMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dcMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.FromJSON DescribeConnections where
  toJSON DescribeConnections {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance Core.AWSRequest DescribeConnections where
  type Rs DescribeConnections = DescribeConnectionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.DescribeConnections")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectionsResponse'
            Core.<$> (x Core..:? "Connections")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeConnections where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"connections" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- |
--
-- /See:/ 'mkDescribeConnectionsResponse' smart constructor.
data DescribeConnectionsResponse = DescribeConnectionsResponse'
  { -- | A description of the connections.
    connections :: Core.Maybe [Types.Connection],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConnectionsResponse' value with any optional fields omitted.
mkDescribeConnectionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConnectionsResponse
mkDescribeConnectionsResponse responseStatus =
  DescribeConnectionsResponse'
    { connections = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A description of the connections.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsConnections :: Lens.Lens' DescribeConnectionsResponse (Core.Maybe [Types.Connection])
dcrfrsConnections = Lens.field @"connections"
{-# DEPRECATED dcrfrsConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsMarker :: Lens.Lens' DescribeConnectionsResponse (Core.Maybe Types.String)
dcrfrsMarker = Lens.field @"marker"
{-# DEPRECATED dcrfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsResponseStatus :: Lens.Lens' DescribeConnectionsResponse Core.Int
dcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
