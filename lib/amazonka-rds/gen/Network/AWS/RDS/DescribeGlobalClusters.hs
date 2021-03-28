{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeGlobalClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Aurora global database clusters. This API supports pagination. 
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeGlobalClusters
    (
    -- * Creating a request
      DescribeGlobalClusters (..)
    , mkDescribeGlobalClusters
    -- ** Request lenses
    , dgcsFilters
    , dgcsGlobalClusterIdentifier
    , dgcsMarker
    , dgcsMaxRecords

    -- * Destructuring the response
    , DescribeGlobalClustersResponse (..)
    , mkDescribeGlobalClustersResponse
    -- ** Response lenses
    , dgcrfrsGlobalClusters
    , dgcrfrsMarker
    , dgcrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeGlobalClusters' smart constructor.
data DescribeGlobalClusters = DescribeGlobalClusters'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ A filter that specifies one or more global DB clusters to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
--
  , globalClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * If supplied, must match an existing DBClusterIdentifier.
--
--
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGlobalClusters' value with any optional fields omitted.
mkDescribeGlobalClusters
    :: DescribeGlobalClusters
mkDescribeGlobalClusters
  = DescribeGlobalClusters'{filters = Core.Nothing,
                            globalClusterIdentifier = Core.Nothing, marker = Core.Nothing,
                            maxRecords = Core.Nothing}

-- | A filter that specifies one or more global DB clusters to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcsFilters :: Lens.Lens' DescribeGlobalClusters (Core.Maybe [Types.Filter])
dgcsFilters = Lens.field @"filters"
{-# INLINEABLE dgcsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * If supplied, must match an existing DBClusterIdentifier.
--
--
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcsGlobalClusterIdentifier :: Lens.Lens' DescribeGlobalClusters (Core.Maybe Core.Text)
dgcsGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# INLINEABLE dgcsGlobalClusterIdentifier #-}
{-# DEPRECATED globalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcsMarker :: Lens.Lens' DescribeGlobalClusters (Core.Maybe Core.Text)
dgcsMarker = Lens.field @"marker"
{-# INLINEABLE dgcsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcsMaxRecords :: Lens.Lens' DescribeGlobalClusters (Core.Maybe Core.Int)
dgcsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dgcsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeGlobalClusters where
        toQuery DescribeGlobalClusters{..}
          = Core.toQueryPair "Action" ("DescribeGlobalClusters" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GlobalClusterIdentifier")
                globalClusterIdentifier
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeGlobalClusters where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeGlobalClusters where
        type Rs DescribeGlobalClusters = DescribeGlobalClustersResponse
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
          = Response.receiveXMLWrapper "DescribeGlobalClustersResult"
              (\ s h x ->
                 DescribeGlobalClustersResponse' Core.<$>
                   (x Core..@? "GlobalClusters" Core..<@>
                      Core.parseXMLList "GlobalClusterMember")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeGlobalClusters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"globalClusters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeGlobalClustersResponse' smart constructor.
data DescribeGlobalClustersResponse = DescribeGlobalClustersResponse'
  { globalClusters :: Core.Maybe [Types.GlobalCluster]
    -- ^ The list of global clusters returned by this request. 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGlobalClustersResponse' value with any optional fields omitted.
mkDescribeGlobalClustersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeGlobalClustersResponse
mkDescribeGlobalClustersResponse responseStatus
  = DescribeGlobalClustersResponse'{globalClusters = Core.Nothing,
                                    marker = Core.Nothing, responseStatus}

-- | The list of global clusters returned by this request. 
--
-- /Note:/ Consider using 'globalClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrfrsGlobalClusters :: Lens.Lens' DescribeGlobalClustersResponse (Core.Maybe [Types.GlobalCluster])
dgcrfrsGlobalClusters = Lens.field @"globalClusters"
{-# INLINEABLE dgcrfrsGlobalClusters #-}
{-# DEPRECATED globalClusters "Use generic-lens or generic-optics with 'globalClusters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrfrsMarker :: Lens.Lens' DescribeGlobalClustersResponse (Core.Maybe Core.Text)
dgcrfrsMarker = Lens.field @"marker"
{-# INLINEABLE dgcrfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrfrsResponseStatus :: Lens.Lens' DescribeGlobalClustersResponse Core.Int
dgcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
