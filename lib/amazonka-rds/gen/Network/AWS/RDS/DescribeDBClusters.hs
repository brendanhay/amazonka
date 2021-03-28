{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeDBClusters (..)
    , mkDescribeDBClusters
    -- ** Request lenses
    , ddbcDBClusterIdentifier
    , ddbcFilters
    , ddbcIncludeShared
    , ddbcMarker
    , ddbcMaxRecords

    -- * Destructuring the response
    , DescribeDBClustersResponse (..)
    , mkDescribeDBClustersResponse
    -- ** Response lenses
    , ddbcrrsDBClusters
    , ddbcrrsMarker
    , ddbcrrsResponseStatus
    ) where

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
  { dBClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match an existing DBClusterIdentifier.
--
--
  , filters :: Core.Maybe [Types.Filter]
    -- ^ A filter that specifies one or more DB clusters to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
--
  , includeShared :: Core.Maybe Core.Bool
    -- ^ Optional Boolean parameter that specifies whether the output includes information about clusters shared from other AWS accounts.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusters' value with any optional fields omitted.
mkDescribeDBClusters
    :: DescribeDBClusters
mkDescribeDBClusters
  = DescribeDBClusters'{dBClusterIdentifier = Core.Nothing,
                        filters = Core.Nothing, includeShared = Core.Nothing,
                        marker = Core.Nothing, maxRecords = Core.Nothing}

-- | The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match an existing DBClusterIdentifier.
--
--
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcDBClusterIdentifier :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Text)
ddbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE ddbcDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

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
{-# INLINEABLE ddbcFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | Optional Boolean parameter that specifies whether the output includes information about clusters shared from other AWS accounts.
--
-- /Note:/ Consider using 'includeShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcIncludeShared :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Bool)
ddbcIncludeShared = Lens.field @"includeShared"
{-# INLINEABLE ddbcIncludeShared #-}
{-# DEPRECATED includeShared "Use generic-lens or generic-optics with 'includeShared' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcMarker :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Text)
ddbcMarker = Lens.field @"marker"
{-# INLINEABLE ddbcMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcMaxRecords :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Int)
ddbcMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbcMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeDBClusters where
        toQuery DescribeDBClusters{..}
          = Core.toQueryPair "Action" ("DescribeDBClusters" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBClusterIdentifier")
                dBClusterIdentifier
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IncludeShared")
                includeShared
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeDBClusters where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBClusters where
        type Rs DescribeDBClusters = DescribeDBClustersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeDBClustersResult"
              (\ s h x ->
                 DescribeDBClustersResponse' Core.<$>
                   (x Core..@? "DBClusters" Core..<@> Core.parseXMLList "DBCluster")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBClusters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dBClusters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeDBClusters@ action.
--
-- /See:/ 'mkDescribeDBClustersResponse' smart constructor.
data DescribeDBClustersResponse = DescribeDBClustersResponse'
  { dBClusters :: Core.Maybe [Types.DBCluster]
    -- ^ Contains a list of DB clusters for the user.
  , marker :: Core.Maybe Core.Text
    -- ^ A pagination token that can be used in a later DescribeDBClusters request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDBClustersResponse' value with any optional fields omitted.
mkDescribeDBClustersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBClustersResponse
mkDescribeDBClustersResponse responseStatus
  = DescribeDBClustersResponse'{dBClusters = Core.Nothing,
                                marker = Core.Nothing, responseStatus}

-- | Contains a list of DB clusters for the user.
--
-- /Note:/ Consider using 'dBClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrrsDBClusters :: Lens.Lens' DescribeDBClustersResponse (Core.Maybe [Types.DBCluster])
ddbcrrsDBClusters = Lens.field @"dBClusters"
{-# INLINEABLE ddbcrrsDBClusters #-}
{-# DEPRECATED dBClusters "Use generic-lens or generic-optics with 'dBClusters' instead"  #-}

-- | A pagination token that can be used in a later DescribeDBClusters request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrrsMarker :: Lens.Lens' DescribeDBClustersResponse (Core.Maybe Core.Text)
ddbcrrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbcrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrrsResponseStatus :: Lens.Lens' DescribeDBClustersResponse Core.Int
ddbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
