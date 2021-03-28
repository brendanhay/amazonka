{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeDBProxyTargets (..)
    , mkDescribeDBProxyTargets
    -- ** Request lenses
    , ddbptDBProxyName
    , ddbptFilters
    , ddbptMarker
    , ddbptMaxRecords
    , ddbptTargetGroupName

    -- * Destructuring the response
    , DescribeDBProxyTargetsResponse (..)
    , mkDescribeDBProxyTargetsResponse
    -- ** Response lenses
    , ddbptrfrsMarker
    , ddbptrfrsTargets
    , ddbptrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDBProxyTargets' smart constructor.
data DescribeDBProxyTargets = DescribeDBProxyTargets'
  { dBProxyName :: Core.Text
    -- ^ The identifier of the @DBProxyTarget@ to describe.
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

-- | Creates a 'DescribeDBProxyTargets' value with any optional fields omitted.
mkDescribeDBProxyTargets
    :: Core.Text -- ^ 'dBProxyName'
    -> DescribeDBProxyTargets
mkDescribeDBProxyTargets dBProxyName
  = DescribeDBProxyTargets'{dBProxyName, filters = Core.Nothing,
                            marker = Core.Nothing, maxRecords = Core.Nothing,
                            targetGroupName = Core.Nothing}

-- | The identifier of the @DBProxyTarget@ to describe.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptDBProxyName :: Lens.Lens' DescribeDBProxyTargets Core.Text
ddbptDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE ddbptDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

-- | This parameter is not currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptFilters :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe [Types.Filter])
ddbptFilters = Lens.field @"filters"
{-# INLINEABLE ddbptFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptMarker :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe Core.Text)
ddbptMarker = Lens.field @"marker"
{-# INLINEABLE ddbptMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptMaxRecords :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe Core.Natural)
ddbptMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbptMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The identifier of the @DBProxyTargetGroup@ to describe.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptTargetGroupName :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe Core.Text)
ddbptTargetGroupName = Lens.field @"targetGroupName"
{-# INLINEABLE ddbptTargetGroupName #-}
{-# DEPRECATED targetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead"  #-}

instance Core.ToQuery DescribeDBProxyTargets where
        toQuery DescribeDBProxyTargets{..}
          = Core.toQueryPair "Action" ("DescribeDBProxyTargets" :: Core.Text)
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

instance Core.ToHeaders DescribeDBProxyTargets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBProxyTargets where
        type Rs DescribeDBProxyTargets = DescribeDBProxyTargetsResponse
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
          = Response.receiveXMLWrapper "DescribeDBProxyTargetsResult"
              (\ s h x ->
                 DescribeDBProxyTargetsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "Targets" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBProxyTargets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"targets" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeDBProxyTargetsResponse' smart constructor.
data DescribeDBProxyTargetsResponse = DescribeDBProxyTargetsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , targets :: Core.Maybe [Types.DBProxyTarget]
    -- ^ An arbitrary number of @DBProxyTarget@ objects, containing details of the corresponding targets.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBProxyTargetsResponse' value with any optional fields omitted.
mkDescribeDBProxyTargetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBProxyTargetsResponse
mkDescribeDBProxyTargetsResponse responseStatus
  = DescribeDBProxyTargetsResponse'{marker = Core.Nothing,
                                    targets = Core.Nothing, responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrfrsMarker :: Lens.Lens' DescribeDBProxyTargetsResponse (Core.Maybe Core.Text)
ddbptrfrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbptrfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | An arbitrary number of @DBProxyTarget@ objects, containing details of the corresponding targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrfrsTargets :: Lens.Lens' DescribeDBProxyTargetsResponse (Core.Maybe [Types.DBProxyTarget])
ddbptrfrsTargets = Lens.field @"targets"
{-# INLINEABLE ddbptrfrsTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptrfrsResponseStatus :: Lens.Lens' DescribeDBProxyTargetsResponse Core.Int
ddbptrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbptrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
