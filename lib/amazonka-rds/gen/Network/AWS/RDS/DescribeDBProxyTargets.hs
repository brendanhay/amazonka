{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBProxyTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about @DBProxyTarget@ objects. This API supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxyTargets
  ( -- * Creating a request
    DescribeDBProxyTargets (..),
    mkDescribeDBProxyTargets,

    -- ** Request lenses
    ddbptDBProxyName,
    ddbptFilters,
    ddbptMarker,
    ddbptMaxRecords,
    ddbptTargetGroupName,

    -- * Destructuring the response
    DescribeDBProxyTargetsResponse (..),
    mkDescribeDBProxyTargetsResponse,

    -- ** Response lenses
    ddbptrfrsMarker,
    ddbptrfrsTargets,
    ddbptrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDBProxyTargets' smart constructor.
data DescribeDBProxyTargets = DescribeDBProxyTargets'
  { -- | The identifier of the @DBProxyTarget@ to describe.
    dBProxyName :: Types.DBProxyName,
    -- | This parameter is not currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Natural,
    -- | The identifier of the @DBProxyTargetGroup@ to describe.
    targetGroupName :: Core.Maybe Types.TargetGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBProxyTargets' value with any optional fields omitted.
mkDescribeDBProxyTargets ::
  -- | 'dBProxyName'
  Types.DBProxyName ->
  DescribeDBProxyTargets
mkDescribeDBProxyTargets dBProxyName =
  DescribeDBProxyTargets'
    { dBProxyName,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      targetGroupName = Core.Nothing
    }

-- | The identifier of the @DBProxyTarget@ to describe.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptDBProxyName :: Lens.Lens' DescribeDBProxyTargets Types.DBProxyName
ddbptDBProxyName = Lens.field @"dBProxyName"
{-# DEPRECATED ddbptDBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead." #-}

-- | This parameter is not currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptFilters :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe [Types.Filter])
ddbptFilters = Lens.field @"filters"
{-# DEPRECATED ddbptFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptMarker :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe Types.Marker)
ddbptMarker = Lens.field @"marker"
{-# DEPRECATED ddbptMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptMaxRecords :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe Core.Natural)
ddbptMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbptMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The identifier of the @DBProxyTargetGroup@ to describe.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptTargetGroupName :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe Types.TargetGroupName)
ddbptTargetGroupName = Lens.field @"targetGroupName"
{-# DEPRECATED ddbptTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

instance Core.AWSRequest DescribeDBProxyTargets where
  type Rs DescribeDBProxyTargets = DescribeDBProxyTargetsResponse
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
            ( Core.pure ("Action", "DescribeDBProxyTargets")
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
      "DescribeDBProxyTargetsResult"
      ( \s h x ->
          DescribeDBProxyTargetsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> (x Core..@? "Targets" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBProxyTargets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"targets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeDBProxyTargetsResponse' smart constructor.
data DescribeDBProxyTargetsResponse = DescribeDBProxyTargetsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | An arbitrary number of @DBProxyTarget@ objects, containing details of the corresponding targets.
    targets :: Core.Maybe [Types.DBProxyTarget],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBProxyTargetsResponse' value with any optional fields omitted.
mkDescribeDBProxyTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBProxyTargetsResponse
mkDescribeDBProxyTargetsResponse responseStatus =
  DescribeDBProxyTargetsResponse'
    { marker = Core.Nothing,
      targets = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrfrsMarker :: Lens.Lens' DescribeDBProxyTargetsResponse (Core.Maybe Types.String)
ddbptrfrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbptrfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An arbitrary number of @DBProxyTarget@ objects, containing details of the corresponding targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrfrsTargets :: Lens.Lens' DescribeDBProxyTargetsResponse (Core.Maybe [Types.DBProxyTarget])
ddbptrfrsTargets = Lens.field @"targets"
{-# DEPRECATED ddbptrfrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrfrsResponseStatus :: Lens.Lens' DescribeDBProxyTargetsResponse Core.Int
ddbptrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbptrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
