{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBProxyTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxy target groups, represented by @DBProxyTargetGroup@ data structures.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxyTargetGroups
  ( -- * Creating a request
    DescribeDBProxyTargetGroups (..),
    mkDescribeDBProxyTargetGroups,

    -- ** Request lenses
    ddbptgDBProxyName,
    ddbptgFilters,
    ddbptgMarker,
    ddbptgMaxRecords,
    ddbptgTargetGroupName,

    -- * Destructuring the response
    DescribeDBProxyTargetGroupsResponse (..),
    mkDescribeDBProxyTargetGroupsResponse,

    -- ** Response lenses
    ddbptgrrsMarker,
    ddbptgrrsTargetGroups,
    ddbptgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDBProxyTargetGroups' smart constructor.
data DescribeDBProxyTargetGroups = DescribeDBProxyTargetGroups'
  { -- | The identifier of the @DBProxy@ associated with the target group.
    dBProxyName :: Types.String,
    -- | This parameter is not currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Natural,
    -- | The identifier of the @DBProxyTargetGroup@ to describe.
    targetGroupName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBProxyTargetGroups' value with any optional fields omitted.
mkDescribeDBProxyTargetGroups ::
  -- | 'dBProxyName'
  Types.String ->
  DescribeDBProxyTargetGroups
mkDescribeDBProxyTargetGroups dBProxyName =
  DescribeDBProxyTargetGroups'
    { dBProxyName,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      targetGroupName = Core.Nothing
    }

-- | The identifier of the @DBProxy@ associated with the target group.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgDBProxyName :: Lens.Lens' DescribeDBProxyTargetGroups Types.String
ddbptgDBProxyName = Lens.field @"dBProxyName"
{-# DEPRECATED ddbptgDBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead." #-}

-- | This parameter is not currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgFilters :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe [Types.Filter])
ddbptgFilters = Lens.field @"filters"
{-# DEPRECATED ddbptgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgMarker :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe Types.String)
ddbptgMarker = Lens.field @"marker"
{-# DEPRECATED ddbptgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgMaxRecords :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe Core.Natural)
ddbptgMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbptgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The identifier of the @DBProxyTargetGroup@ to describe.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgTargetGroupName :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe Types.String)
ddbptgTargetGroupName = Lens.field @"targetGroupName"
{-# DEPRECATED ddbptgTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

instance Core.AWSRequest DescribeDBProxyTargetGroups where
  type
    Rs DescribeDBProxyTargetGroups =
      DescribeDBProxyTargetGroupsResponse
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
            ( Core.pure ("Action", "DescribeDBProxyTargetGroups")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBProxyName" dBProxyName)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "TargetGroupName" Core.<$> targetGroupName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDBProxyTargetGroupsResult"
      ( \s h x ->
          DescribeDBProxyTargetGroupsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> (x Core..@? "TargetGroups" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBProxyTargetGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"targetGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeDBProxyTargetGroupsResponse' smart constructor.
data DescribeDBProxyTargetGroupsResponse = DescribeDBProxyTargetGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details of the corresponding target groups.
    targetGroups :: Core.Maybe [Types.DBProxyTargetGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDBProxyTargetGroupsResponse' value with any optional fields omitted.
mkDescribeDBProxyTargetGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBProxyTargetGroupsResponse
mkDescribeDBProxyTargetGroupsResponse responseStatus =
  DescribeDBProxyTargetGroupsResponse'
    { marker = Core.Nothing,
      targetGroups = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgrrsMarker :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Core.Maybe Types.String)
ddbptgrrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbptgrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details of the corresponding target groups.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgrrsTargetGroups :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Core.Maybe [Types.DBProxyTargetGroup])
ddbptgrrsTargetGroups = Lens.field @"targetGroups"
{-# DEPRECATED ddbptgrrsTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgrrsResponseStatus :: Lens.Lens' DescribeDBProxyTargetGroupsResponse Core.Int
ddbptgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbptgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
