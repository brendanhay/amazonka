{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationSubnetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the replication subnet groups.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationSubnetGroups
  ( -- * Creating a request
    DescribeReplicationSubnetGroups (..),
    mkDescribeReplicationSubnetGroups,

    -- ** Request lenses
    drsgFilters,
    drsgMarker,
    drsgMaxRecords,

    -- * Destructuring the response
    DescribeReplicationSubnetGroupsResponse (..),
    mkDescribeReplicationSubnetGroupsResponse,

    -- ** Response lenses
    drsgrfrsMarker,
    drsgrfrsReplicationSubnetGroups,
    drsgrfrsResponseStatus,
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
-- /See:/ 'mkDescribeReplicationSubnetGroups' smart constructor.
data DescribeReplicationSubnetGroups = DescribeReplicationSubnetGroups'
  { -- | Filters applied to replication subnet groups.
    --
    -- Valid filter names: replication-subnet-group-id
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplicationSubnetGroups' value with any optional fields omitted.
mkDescribeReplicationSubnetGroups ::
  DescribeReplicationSubnetGroups
mkDescribeReplicationSubnetGroups =
  DescribeReplicationSubnetGroups'
    { filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Filters applied to replication subnet groups.
--
-- Valid filter names: replication-subnet-group-id
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgFilters :: Lens.Lens' DescribeReplicationSubnetGroups (Core.Maybe [Types.Filter])
drsgFilters = Lens.field @"filters"
{-# DEPRECATED drsgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgMarker :: Lens.Lens' DescribeReplicationSubnetGroups (Core.Maybe Types.Marker)
drsgMarker = Lens.field @"marker"
{-# DEPRECATED drsgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgMaxRecords :: Lens.Lens' DescribeReplicationSubnetGroups (Core.Maybe Core.Int)
drsgMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED drsgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.FromJSON DescribeReplicationSubnetGroups where
  toJSON DescribeReplicationSubnetGroups {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance Core.AWSRequest DescribeReplicationSubnetGroups where
  type
    Rs DescribeReplicationSubnetGroups =
      DescribeReplicationSubnetGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonDMSv20160101.DescribeReplicationSubnetGroups"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationSubnetGroupsResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "ReplicationSubnetGroups")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeReplicationSubnetGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"replicationSubnetGroups" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- |
--
-- /See:/ 'mkDescribeReplicationSubnetGroupsResponse' smart constructor.
data DescribeReplicationSubnetGroupsResponse = DescribeReplicationSubnetGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | A description of the replication subnet groups.
    replicationSubnetGroups :: Core.Maybe [Types.ReplicationSubnetGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplicationSubnetGroupsResponse' value with any optional fields omitted.
mkDescribeReplicationSubnetGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeReplicationSubnetGroupsResponse
mkDescribeReplicationSubnetGroupsResponse responseStatus =
  DescribeReplicationSubnetGroupsResponse'
    { marker = Core.Nothing,
      replicationSubnetGroups = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgrfrsMarker :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Core.Maybe Types.String)
drsgrfrsMarker = Lens.field @"marker"
{-# DEPRECATED drsgrfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A description of the replication subnet groups.
--
-- /Note:/ Consider using 'replicationSubnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgrfrsReplicationSubnetGroups :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Core.Maybe [Types.ReplicationSubnetGroup])
drsgrfrsReplicationSubnetGroups = Lens.field @"replicationSubnetGroups"
{-# DEPRECATED drsgrfrsReplicationSubnetGroups "Use generic-lens or generic-optics with 'replicationSubnetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsgrfrsResponseStatus :: Lens.Lens' DescribeReplicationSubnetGroupsResponse Core.Int
drsgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
