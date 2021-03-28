{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBClusterParameterGroup@ descriptions. If a @DBClusterParameterGroupName@ parameter is specified, the list will contain only the description of the specified DB cluster parameter group. 
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterParameterGroups
    (
    -- * Creating a request
      DescribeDBClusterParameterGroups (..)
    , mkDescribeDBClusterParameterGroups
    -- ** Request lenses
    , ddbcpgDBClusterParameterGroupName
    , ddbcpgFilters
    , ddbcpgMarker
    , ddbcpgMaxRecords

    -- * Destructuring the response
    , DescribeDBClusterParameterGroupsResponse (..)
    , mkDescribeDBClusterParameterGroupsResponse
    -- ** Response lenses
    , ddbcpgrrsDBClusterParameterGroups
    , ddbcpgrrsMarker
    , ddbcpgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeDBClusterParameterGroups' smart constructor.
data DescribeDBClusterParameterGroups = DescribeDBClusterParameterGroups'
  { dBClusterParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of a specific DB cluster parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterParameterGroups' value with any optional fields omitted.
mkDescribeDBClusterParameterGroups
    :: DescribeDBClusterParameterGroups
mkDescribeDBClusterParameterGroups
  = DescribeDBClusterParameterGroups'{dBClusterParameterGroupName =
                                        Core.Nothing,
                                      filters = Core.Nothing, marker = Core.Nothing,
                                      maxRecords = Core.Nothing}

-- | The name of a specific DB cluster parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpgDBClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameterGroups (Core.Maybe Core.Text)
ddbcpgDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# INLINEABLE ddbcpgDBClusterParameterGroupName #-}
{-# DEPRECATED dBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpgFilters :: Lens.Lens' DescribeDBClusterParameterGroups (Core.Maybe [Types.Filter])
ddbcpgFilters = Lens.field @"filters"
{-# INLINEABLE ddbcpgFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpgMarker :: Lens.Lens' DescribeDBClusterParameterGroups (Core.Maybe Core.Text)
ddbcpgMarker = Lens.field @"marker"
{-# INLINEABLE ddbcpgMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpgMaxRecords :: Lens.Lens' DescribeDBClusterParameterGroups (Core.Maybe Core.Int)
ddbcpgMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbcpgMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeDBClusterParameterGroups where
        toQuery DescribeDBClusterParameterGroups{..}
          = Core.toQueryPair "Action"
              ("DescribeDBClusterParameterGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DBClusterParameterGroupName")
                dBClusterParameterGroupName
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeDBClusterParameterGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBClusterParameterGroups where
        type Rs DescribeDBClusterParameterGroups =
             DescribeDBClusterParameterGroupsResponse
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
          = Response.receiveXMLWrapper
              "DescribeDBClusterParameterGroupsResult"
              (\ s h x ->
                 DescribeDBClusterParameterGroupsResponse' Core.<$>
                   (x Core..@? "DBClusterParameterGroups" Core..<@>
                      Core.parseXMLList "DBClusterParameterGroup")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBClusterParameterGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"dBClusterParameterGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeDBClusterParameterGroupsResponse' smart constructor.
data DescribeDBClusterParameterGroupsResponse = DescribeDBClusterParameterGroupsResponse'
  { dBClusterParameterGroups :: Core.Maybe [Types.DBClusterParameterGroup]
    -- ^ A list of DB cluster parameter groups.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterParameterGroupsResponse' value with any optional fields omitted.
mkDescribeDBClusterParameterGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBClusterParameterGroupsResponse
mkDescribeDBClusterParameterGroupsResponse responseStatus
  = DescribeDBClusterParameterGroupsResponse'{dBClusterParameterGroups
                                                = Core.Nothing,
                                              marker = Core.Nothing, responseStatus}

-- | A list of DB cluster parameter groups.
--
-- /Note:/ Consider using 'dBClusterParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpgrrsDBClusterParameterGroups :: Lens.Lens' DescribeDBClusterParameterGroupsResponse (Core.Maybe [Types.DBClusterParameterGroup])
ddbcpgrrsDBClusterParameterGroups = Lens.field @"dBClusterParameterGroups"
{-# INLINEABLE ddbcpgrrsDBClusterParameterGroups #-}
{-# DEPRECATED dBClusterParameterGroups "Use generic-lens or generic-optics with 'dBClusterParameterGroups' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpgrrsMarker :: Lens.Lens' DescribeDBClusterParameterGroupsResponse (Core.Maybe Core.Text)
ddbcpgrrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbcpgrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcpgrrsResponseStatus :: Lens.Lens' DescribeDBClusterParameterGroupsResponse Core.Int
ddbcpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbcpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
