{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterBacktracks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about backtracks for a DB cluster.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterBacktracks
  ( -- * Creating a request
    DescribeDBClusterBacktracks (..),
    mkDescribeDBClusterBacktracks,

    -- ** Request lenses
    ddbcbDBClusterIdentifier,
    ddbcbBacktrackIdentifier,
    ddbcbFilters,
    ddbcbMarker,
    ddbcbMaxRecords,

    -- * Destructuring the response
    DescribeDBClusterBacktracksResponse (..),
    mkDescribeDBClusterBacktracksResponse,

    -- ** Response lenses
    ddbcbrrsDBClusterBacktracks,
    ddbcbrrsMarker,
    ddbcbrrsResponseStatus,
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
-- /See:/ 'mkDescribeDBClusterBacktracks' smart constructor.
data DescribeDBClusterBacktracks = DescribeDBClusterBacktracks'
  { -- | The DB cluster identifier of the DB cluster to be described. This parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 alphanumeric characters or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens.
    --
    --
    -- Example: @my-cluster1@
    dBClusterIdentifier :: Types.String,
    -- | If specified, this value is the backtrack identifier of the backtrack to be described.
    --
    -- Constraints:
    --
    --     * Must contain a valid universally unique identifier (UUID). For more information about UUIDs, see <http://www.ietf.org/rfc/rfc4122.txt A Universally Unique Identifier (UUID) URN Namespace> .
    --
    --
    -- Example: @123e4567-e89b-12d3-a456-426655440000@
    backtrackIdentifier :: Core.Maybe Types.String,
    -- | A filter that specifies one or more DB clusters to describe. Supported filters include the following:
    --
    --
    --     * @db-cluster-backtrack-id@ - Accepts backtrack identifiers. The results list includes information about only the backtracks identified by these identifiers.
    --
    --
    --     * @db-cluster-backtrack-status@ - Accepts any of the following backtrack status values:
    --
    --     * @applying@
    --
    --
    --     * @completed@
    --
    --
    --     * @failed@
    --
    --
    --     * @pending@
    --
    --
    -- The results list includes information about only the backtracks identified by these values.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous @DescribeDBClusterBacktracks@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterBacktracks' value with any optional fields omitted.
mkDescribeDBClusterBacktracks ::
  -- | 'dBClusterIdentifier'
  Types.String ->
  DescribeDBClusterBacktracks
mkDescribeDBClusterBacktracks dBClusterIdentifier =
  DescribeDBClusterBacktracks'
    { dBClusterIdentifier,
      backtrackIdentifier = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The DB cluster identifier of the DB cluster to be described. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1@
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcbDBClusterIdentifier :: Lens.Lens' DescribeDBClusterBacktracks Types.String
ddbcbDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED ddbcbDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | If specified, this value is the backtrack identifier of the backtrack to be described.
--
-- Constraints:
--
--     * Must contain a valid universally unique identifier (UUID). For more information about UUIDs, see <http://www.ietf.org/rfc/rfc4122.txt A Universally Unique Identifier (UUID) URN Namespace> .
--
--
-- Example: @123e4567-e89b-12d3-a456-426655440000@
--
-- /Note:/ Consider using 'backtrackIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcbBacktrackIdentifier :: Lens.Lens' DescribeDBClusterBacktracks (Core.Maybe Types.String)
ddbcbBacktrackIdentifier = Lens.field @"backtrackIdentifier"
{-# DEPRECATED ddbcbBacktrackIdentifier "Use generic-lens or generic-optics with 'backtrackIdentifier' instead." #-}

-- | A filter that specifies one or more DB clusters to describe. Supported filters include the following:
--
--
--     * @db-cluster-backtrack-id@ - Accepts backtrack identifiers. The results list includes information about only the backtracks identified by these identifiers.
--
--
--     * @db-cluster-backtrack-status@ - Accepts any of the following backtrack status values:
--
--     * @applying@
--
--
--     * @completed@
--
--
--     * @failed@
--
--
--     * @pending@
--
--
-- The results list includes information about only the backtracks identified by these values.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcbFilters :: Lens.Lens' DescribeDBClusterBacktracks (Core.Maybe [Types.Filter])
ddbcbFilters = Lens.field @"filters"
{-# DEPRECATED ddbcbFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterBacktracks@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcbMarker :: Lens.Lens' DescribeDBClusterBacktracks (Core.Maybe Types.String)
ddbcbMarker = Lens.field @"marker"
{-# DEPRECATED ddbcbMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcbMaxRecords :: Lens.Lens' DescribeDBClusterBacktracks (Core.Maybe Core.Int)
ddbcbMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbcbMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeDBClusterBacktracks where
  type
    Rs DescribeDBClusterBacktracks =
      DescribeDBClusterBacktracksResponse
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
            ( Core.pure ("Action", "DescribeDBClusterBacktracks")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> ( Core.toQueryValue "BacktrackIdentifier"
                            Core.<$> backtrackIdentifier
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
      "DescribeDBClusterBacktracksResult"
      ( \s h x ->
          DescribeDBClusterBacktracksResponse'
            Core.<$> ( x Core..@? "DBClusterBacktracks"
                         Core..<@> Core.parseXMLList "DBClusterBacktrack"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBClusterBacktracks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"dBClusterBacktracks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeDBClusterBacktracks@ action.
--
-- /See:/ 'mkDescribeDBClusterBacktracksResponse' smart constructor.
data DescribeDBClusterBacktracksResponse = DescribeDBClusterBacktracksResponse'
  { -- | Contains a list of backtracks for the user.
    dBClusterBacktracks :: Core.Maybe [Types.DBClusterBacktrack],
    -- | A pagination token that can be used in a later @DescribeDBClusterBacktracks@ request.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDBClusterBacktracksResponse' value with any optional fields omitted.
mkDescribeDBClusterBacktracksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBClusterBacktracksResponse
mkDescribeDBClusterBacktracksResponse responseStatus =
  DescribeDBClusterBacktracksResponse'
    { dBClusterBacktracks =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | Contains a list of backtracks for the user.
--
-- /Note:/ Consider using 'dBClusterBacktracks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcbrrsDBClusterBacktracks :: Lens.Lens' DescribeDBClusterBacktracksResponse (Core.Maybe [Types.DBClusterBacktrack])
ddbcbrrsDBClusterBacktracks = Lens.field @"dBClusterBacktracks"
{-# DEPRECATED ddbcbrrsDBClusterBacktracks "Use generic-lens or generic-optics with 'dBClusterBacktracks' instead." #-}

-- | A pagination token that can be used in a later @DescribeDBClusterBacktracks@ request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcbrrsMarker :: Lens.Lens' DescribeDBClusterBacktracksResponse (Core.Maybe Types.String)
ddbcbrrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbcbrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcbrrsResponseStatus :: Lens.Lens' DescribeDBClusterBacktracksResponse Core.Int
ddbcbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbcbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
