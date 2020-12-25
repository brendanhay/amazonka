{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSubnetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DBSubnetGroup descriptions. If a DBSubnetGroupName is specified, the list will contain only the descriptions of the specified DBSubnetGroup.
--
-- For an overview of CIDR ranges, go to the <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Wikipedia Tutorial> .
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSubnetGroups
  ( -- * Creating a request
    DescribeDBSubnetGroups (..),
    mkDescribeDBSubnetGroups,

    -- ** Request lenses
    ddbsgDBSubnetGroupName,
    ddbsgFilters,
    ddbsgMarker,
    ddbsgMaxRecords,

    -- * Destructuring the response
    DescribeDBSubnetGroupsResponse (..),
    mkDescribeDBSubnetGroupsResponse,

    -- ** Response lenses
    ddbsgrrsDBSubnetGroups,
    ddbsgrrsMarker,
    ddbsgrrsResponseStatus,
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
-- /See:/ 'mkDescribeDBSubnetGroups' smart constructor.
data DescribeDBSubnetGroups = DescribeDBSubnetGroups'
  { -- | The name of the DB subnet group to return details for.
    dBSubnetGroupName :: Core.Maybe Types.DBSubnetGroupName,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous DescribeDBSubnetGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBSubnetGroups' value with any optional fields omitted.
mkDescribeDBSubnetGroups ::
  DescribeDBSubnetGroups
mkDescribeDBSubnetGroups =
  DescribeDBSubnetGroups'
    { dBSubnetGroupName = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the DB subnet group to return details for.
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgDBSubnetGroupName :: Lens.Lens' DescribeDBSubnetGroups (Core.Maybe Types.DBSubnetGroupName)
ddbsgDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED ddbsgDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgFilters :: Lens.Lens' DescribeDBSubnetGroups (Core.Maybe [Types.Filter])
ddbsgFilters = Lens.field @"filters"
{-# DEPRECATED ddbsgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous DescribeDBSubnetGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgMarker :: Lens.Lens' DescribeDBSubnetGroups (Core.Maybe Types.Marker)
ddbsgMarker = Lens.field @"marker"
{-# DEPRECATED ddbsgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgMaxRecords :: Lens.Lens' DescribeDBSubnetGroups (Core.Maybe Core.Int)
ddbsgMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbsgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeDBSubnetGroups where
  type Rs DescribeDBSubnetGroups = DescribeDBSubnetGroupsResponse
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
            ( Core.pure ("Action", "DescribeDBSubnetGroups")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSubnetGroupName" Core.<$> dBSubnetGroupName)
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
      "DescribeDBSubnetGroupsResult"
      ( \s h x ->
          DescribeDBSubnetGroupsResponse'
            Core.<$> ( x Core..@? "DBSubnetGroups"
                         Core..<@> Core.parseXMLList "DBSubnetGroup"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBSubnetGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"dBSubnetGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeDBSubnetGroups@ action.
--
-- /See:/ 'mkDescribeDBSubnetGroupsResponse' smart constructor.
data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse'
  { -- | A list of @DBSubnetGroup@ instances.
    dBSubnetGroups :: Core.Maybe [Types.DBSubnetGroup],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBSubnetGroupsResponse' value with any optional fields omitted.
mkDescribeDBSubnetGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBSubnetGroupsResponse
mkDescribeDBSubnetGroupsResponse responseStatus =
  DescribeDBSubnetGroupsResponse'
    { dBSubnetGroups = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of @DBSubnetGroup@ instances.
--
-- /Note:/ Consider using 'dBSubnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgrrsDBSubnetGroups :: Lens.Lens' DescribeDBSubnetGroupsResponse (Core.Maybe [Types.DBSubnetGroup])
ddbsgrrsDBSubnetGroups = Lens.field @"dBSubnetGroups"
{-# DEPRECATED ddbsgrrsDBSubnetGroups "Use generic-lens or generic-optics with 'dBSubnetGroups' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgrrsMarker :: Lens.Lens' DescribeDBSubnetGroupsResponse (Core.Maybe Types.String)
ddbsgrrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbsgrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgrrsResponseStatus :: Lens.Lens' DescribeDBSubnetGroupsResponse Core.Int
ddbsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
