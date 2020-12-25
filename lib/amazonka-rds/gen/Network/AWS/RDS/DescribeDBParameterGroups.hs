{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBParameterGroup@ descriptions. If a @DBParameterGroupName@ is specified, the list will contain only the description of the specified DB parameter group.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBParameterGroups
  ( -- * Creating a request
    DescribeDBParameterGroups (..),
    mkDescribeDBParameterGroups,

    -- ** Request lenses
    ddbpgDBParameterGroupName,
    ddbpgFilters,
    ddbpgMarker,
    ddbpgMaxRecords,

    -- * Destructuring the response
    DescribeDBParameterGroupsResponse (..),
    mkDescribeDBParameterGroupsResponse,

    -- ** Response lenses
    ddbpgrrsDBParameterGroups,
    ddbpgrrsMarker,
    ddbpgrrsResponseStatus,
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
-- /See:/ 'mkDescribeDBParameterGroups' smart constructor.
data DescribeDBParameterGroups = DescribeDBParameterGroups'
  { -- | The name of a specific DB parameter group to return details for.
    --
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing DBClusterParameterGroup.
    dBParameterGroupName :: Core.Maybe Types.String,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous @DescribeDBParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBParameterGroups' value with any optional fields omitted.
mkDescribeDBParameterGroups ::
  DescribeDBParameterGroups
mkDescribeDBParameterGroups =
  DescribeDBParameterGroups'
    { dBParameterGroupName = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgDBParameterGroupName :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe Types.String)
ddbpgDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# DEPRECATED ddbpgDBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgFilters :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe [Types.Filter])
ddbpgFilters = Lens.field @"filters"
{-# DEPRECATED ddbpgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgMarker :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe Types.String)
ddbpgMarker = Lens.field @"marker"
{-# DEPRECATED ddbpgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgMaxRecords :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe Core.Int)
ddbpgMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbpgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeDBParameterGroups where
  type
    Rs DescribeDBParameterGroups =
      DescribeDBParameterGroupsResponse
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
            ( Core.pure ("Action", "DescribeDBParameterGroups")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue "DBParameterGroupName"
                            Core.<$> dBParameterGroupName
                        )
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDBParameterGroupsResult"
      ( \s h x ->
          DescribeDBParameterGroupsResponse'
            Core.<$> ( x Core..@? "DBParameterGroups"
                         Core..<@> Core.parseXMLList "DBParameterGroup"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBParameterGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"dBParameterGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeDBParameterGroups@ action.
--
-- /See:/ 'mkDescribeDBParameterGroupsResponse' smart constructor.
data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse'
  { -- | A list of @DBParameterGroup@ instances.
    dBParameterGroups :: Core.Maybe [Types.DBParameterGroup],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBParameterGroupsResponse' value with any optional fields omitted.
mkDescribeDBParameterGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBParameterGroupsResponse
mkDescribeDBParameterGroupsResponse responseStatus =
  DescribeDBParameterGroupsResponse'
    { dBParameterGroups =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of @DBParameterGroup@ instances.
--
-- /Note:/ Consider using 'dBParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgrrsDBParameterGroups :: Lens.Lens' DescribeDBParameterGroupsResponse (Core.Maybe [Types.DBParameterGroup])
ddbpgrrsDBParameterGroups = Lens.field @"dBParameterGroups"
{-# DEPRECATED ddbpgrrsDBParameterGroups "Use generic-lens or generic-optics with 'dBParameterGroups' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgrrsMarker :: Lens.Lens' DescribeDBParameterGroupsResponse (Core.Maybe Types.String)
ddbpgrrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbpgrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgrrsResponseStatus :: Lens.Lens' DescribeDBParameterGroupsResponse Core.Int
ddbpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
