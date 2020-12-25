{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned Aurora DB clusters. This API supports pagination.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusters
  ( -- * Creating a request
    DescribeDBClusters (..),
    mkDescribeDBClusters,

    -- ** Request lenses
    ddbcDBClusterIdentifier,
    ddbcFilters,
    ddbcIncludeShared,
    ddbcMarker,
    ddbcMaxRecords,

    -- * Destructuring the response
    DescribeDBClustersResponse (..),
    mkDescribeDBClustersResponse,

    -- ** Response lenses
    ddbcrrsDBClusters,
    ddbcrrsMarker,
    ddbcrrsResponseStatus,
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
-- /See:/ 'mkDescribeDBClusters' smart constructor.
data DescribeDBClusters = DescribeDBClusters'
  { -- | The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * If supplied, must match an existing DBClusterIdentifier.
    dBClusterIdentifier :: Core.Maybe Types.String,
    -- | A filter that specifies one or more DB clusters to describe.
    --
    -- Supported filters:
    --
    --     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
    filters :: Core.Maybe [Types.Filter],
    -- | Optional Boolean parameter that specifies whether the output includes information about clusters shared from other AWS accounts.
    includeShared :: Core.Maybe Core.Bool,
    -- | An optional pagination token provided by a previous @DescribeDBClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusters' value with any optional fields omitted.
mkDescribeDBClusters ::
  DescribeDBClusters
mkDescribeDBClusters =
  DescribeDBClusters'
    { dBClusterIdentifier = Core.Nothing,
      filters = Core.Nothing,
      includeShared = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match an existing DBClusterIdentifier.
--
--
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcDBClusterIdentifier :: Lens.Lens' DescribeDBClusters (Core.Maybe Types.String)
ddbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED ddbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | A filter that specifies one or more DB clusters to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcFilters :: Lens.Lens' DescribeDBClusters (Core.Maybe [Types.Filter])
ddbcFilters = Lens.field @"filters"
{-# DEPRECATED ddbcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Optional Boolean parameter that specifies whether the output includes information about clusters shared from other AWS accounts.
--
-- /Note:/ Consider using 'includeShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcIncludeShared :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Bool)
ddbcIncludeShared = Lens.field @"includeShared"
{-# DEPRECATED ddbcIncludeShared "Use generic-lens or generic-optics with 'includeShared' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcMarker :: Lens.Lens' DescribeDBClusters (Core.Maybe Types.String)
ddbcMarker = Lens.field @"marker"
{-# DEPRECATED ddbcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcMaxRecords :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Int)
ddbcMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbcMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeDBClusters where
  type Rs DescribeDBClusters = DescribeDBClustersResponse
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
            ( Core.pure ("Action", "DescribeDBClusters")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue "DBClusterIdentifier"
                            Core.<$> dBClusterIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "IncludeShared" Core.<$> includeShared)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDBClustersResult"
      ( \s h x ->
          DescribeDBClustersResponse'
            Core.<$> (x Core..@? "DBClusters" Core..<@> Core.parseXMLList "DBCluster")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBClusters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"dBClusters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeDBClusters@ action.
--
-- /See:/ 'mkDescribeDBClustersResponse' smart constructor.
data DescribeDBClustersResponse = DescribeDBClustersResponse'
  { -- | Contains a list of DB clusters for the user.
    dBClusters :: Core.Maybe [Types.DBCluster],
    -- | A pagination token that can be used in a later DescribeDBClusters request.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDBClustersResponse' value with any optional fields omitted.
mkDescribeDBClustersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBClustersResponse
mkDescribeDBClustersResponse responseStatus =
  DescribeDBClustersResponse'
    { dBClusters = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | Contains a list of DB clusters for the user.
--
-- /Note:/ Consider using 'dBClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrrsDBClusters :: Lens.Lens' DescribeDBClustersResponse (Core.Maybe [Types.DBCluster])
ddbcrrsDBClusters = Lens.field @"dBClusters"
{-# DEPRECATED ddbcrrsDBClusters "Use generic-lens or generic-optics with 'dBClusters' instead." #-}

-- | A pagination token that can be used in a later DescribeDBClusters request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrrsMarker :: Lens.Lens' DescribeDBClustersResponse (Core.Maybe Types.String)
ddbcrrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbcrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrrsResponseStatus :: Lens.Lens' DescribeDBClustersResponse Core.Int
ddbcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
