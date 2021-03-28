{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeDBProxyTargetGroups (..)
    , mkDescribeDBProxyTargetGroups
    -- ** Request lenses
    , ddbptgDBProxyName
    , ddbptgFilters
    , ddbptgMarker
    , ddbptgMaxRecords
    , ddbptgTargetGroupName

    -- * Destructuring the response
    , DescribeDBProxyTargetGroupsResponse (..)
    , mkDescribeDBProxyTargetGroupsResponse
    -- ** Response lenses
    , ddbptgrrsMarker
    , ddbptgrrsTargetGroups
    , ddbptgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDBProxyTargetGroups' smart constructor.
data DescribeDBProxyTargetGroups = DescribeDBProxyTargetGroups'
  { dBProxyName :: Core.Text
    -- ^ The identifier of the @DBProxy@ associated with the target group.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter is not currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Natural
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , targetGroupName :: Core.Maybe Core.Text
    -- ^ The identifier of the @DBProxyTargetGroup@ to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBProxyTargetGroups' value with any optional fields omitted.
mkDescribeDBProxyTargetGroups
    :: Core.Text -- ^ 'dBProxyName'
    -> DescribeDBProxyTargetGroups
mkDescribeDBProxyTargetGroups dBProxyName
  = DescribeDBProxyTargetGroups'{dBProxyName, filters = Core.Nothing,
                                 marker = Core.Nothing, maxRecords = Core.Nothing,
                                 targetGroupName = Core.Nothing}

-- | The identifier of the @DBProxy@ associated with the target group.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgDBProxyName :: Lens.Lens' DescribeDBProxyTargetGroups Core.Text
ddbptgDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE ddbptgDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

-- | This parameter is not currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgFilters :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe [Types.Filter])
ddbptgFilters = Lens.field @"filters"
{-# INLINEABLE ddbptgFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgMarker :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe Core.Text)
ddbptgMarker = Lens.field @"marker"
{-# INLINEABLE ddbptgMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgMaxRecords :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe Core.Natural)
ddbptgMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbptgMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The identifier of the @DBProxyTargetGroup@ to describe.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgTargetGroupName :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe Core.Text)
ddbptgTargetGroupName = Lens.field @"targetGroupName"
{-# INLINEABLE ddbptgTargetGroupName #-}
{-# DEPRECATED targetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead"  #-}

instance Core.ToQuery DescribeDBProxyTargetGroups where
        toQuery DescribeDBProxyTargetGroups{..}
          = Core.toQueryPair "Action"
              ("DescribeDBProxyTargetGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBProxyName" dBProxyName
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetGroupName")
                targetGroupName

instance Core.ToHeaders DescribeDBProxyTargetGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBProxyTargetGroups where
        type Rs DescribeDBProxyTargetGroups =
             DescribeDBProxyTargetGroupsResponse
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
          = Response.receiveXMLWrapper "DescribeDBProxyTargetGroupsResult"
              (\ s h x ->
                 DescribeDBProxyTargetGroupsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "TargetGroups" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBProxyTargetGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"targetGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeDBProxyTargetGroupsResponse' smart constructor.
data DescribeDBProxyTargetGroupsResponse = DescribeDBProxyTargetGroupsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , targetGroups :: Core.Maybe [Types.DBProxyTargetGroup]
    -- ^ An arbitrary number of @DBProxyTargetGroup@ objects, containing details of the corresponding target groups.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDBProxyTargetGroupsResponse' value with any optional fields omitted.
mkDescribeDBProxyTargetGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBProxyTargetGroupsResponse
mkDescribeDBProxyTargetGroupsResponse responseStatus
  = DescribeDBProxyTargetGroupsResponse'{marker = Core.Nothing,
                                         targetGroups = Core.Nothing, responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgrrsMarker :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Core.Maybe Core.Text)
ddbptgrrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbptgrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details of the corresponding target groups.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgrrsTargetGroups :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Core.Maybe [Types.DBProxyTargetGroup])
ddbptgrrsTargetGroups = Lens.field @"targetGroups"
{-# INLINEABLE ddbptgrrsTargetGroups #-}
{-# DEPRECATED targetGroups "Use generic-lens or generic-optics with 'targetGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptgrrsResponseStatus :: Lens.Lens' DescribeDBProxyTargetGroupsResponse Core.Int
ddbptgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbptgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
