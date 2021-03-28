{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeDBParameterGroups (..)
    , mkDescribeDBParameterGroups
    -- ** Request lenses
    , ddbpgDBParameterGroupName
    , ddbpgFilters
    , ddbpgMarker
    , ddbpgMaxRecords

    -- * Destructuring the response
    , DescribeDBParameterGroupsResponse (..)
    , mkDescribeDBParameterGroupsResponse
    -- ** Response lenses
    , ddbpgrrsDBParameterGroups
    , ddbpgrrsMarker
    , ddbpgrrsResponseStatus
    ) where

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
  { dBParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBParameterGroups' value with any optional fields omitted.
mkDescribeDBParameterGroups
    :: DescribeDBParameterGroups
mkDescribeDBParameterGroups
  = DescribeDBParameterGroups'{dBParameterGroupName = Core.Nothing,
                               filters = Core.Nothing, marker = Core.Nothing,
                               maxRecords = Core.Nothing}

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgDBParameterGroupName :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe Core.Text)
ddbpgDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE ddbpgDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgFilters :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe [Types.Filter])
ddbpgFilters = Lens.field @"filters"
{-# INLINEABLE ddbpgFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgMarker :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe Core.Text)
ddbpgMarker = Lens.field @"marker"
{-# INLINEABLE ddbpgMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgMaxRecords :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe Core.Int)
ddbpgMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbpgMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeDBParameterGroups where
        toQuery DescribeDBParameterGroups{..}
          = Core.toQueryPair "Action"
              ("DescribeDBParameterGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBParameterGroupName")
                dBParameterGroupName
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeDBParameterGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBParameterGroups where
        type Rs DescribeDBParameterGroups =
             DescribeDBParameterGroupsResponse
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
          = Response.receiveXMLWrapper "DescribeDBParameterGroupsResult"
              (\ s h x ->
                 DescribeDBParameterGroupsResponse' Core.<$>
                   (x Core..@? "DBParameterGroups" Core..<@>
                      Core.parseXMLList "DBParameterGroup")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBParameterGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dBParameterGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeDBParameterGroups@ action. 
--
-- /See:/ 'mkDescribeDBParameterGroupsResponse' smart constructor.
data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse'
  { dBParameterGroups :: Core.Maybe [Types.DBParameterGroup]
    -- ^ A list of @DBParameterGroup@ instances. 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBParameterGroupsResponse' value with any optional fields omitted.
mkDescribeDBParameterGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBParameterGroupsResponse
mkDescribeDBParameterGroupsResponse responseStatus
  = DescribeDBParameterGroupsResponse'{dBParameterGroups =
                                         Core.Nothing,
                                       marker = Core.Nothing, responseStatus}

-- | A list of @DBParameterGroup@ instances. 
--
-- /Note:/ Consider using 'dBParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgrrsDBParameterGroups :: Lens.Lens' DescribeDBParameterGroupsResponse (Core.Maybe [Types.DBParameterGroup])
ddbpgrrsDBParameterGroups = Lens.field @"dBParameterGroups"
{-# INLINEABLE ddbpgrrsDBParameterGroups #-}
{-# DEPRECATED dBParameterGroups "Use generic-lens or generic-optics with 'dBParameterGroups' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgrrsMarker :: Lens.Lens' DescribeDBParameterGroupsResponse (Core.Maybe Core.Text)
ddbpgrrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbpgrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpgrrsResponseStatus :: Lens.Lens' DescribeDBParameterGroupsResponse Core.Int
ddbpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
