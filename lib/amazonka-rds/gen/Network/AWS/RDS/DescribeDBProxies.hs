{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBProxies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxies.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxies
    (
    -- * Creating a request
      DescribeDBProxies (..)
    , mkDescribeDBProxies
    -- ** Request lenses
    , ddbpsDBProxyName
    , ddbpsFilters
    , ddbpsMarker
    , ddbpsMaxRecords

    -- * Destructuring the response
    , DescribeDBProxiesResponse (..)
    , mkDescribeDBProxiesResponse
    -- ** Response lenses
    , drsDBProxies
    , drsMarker
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDBProxies' smart constructor.
data DescribeDBProxies = DescribeDBProxies'
  { dBProxyName :: Core.Maybe Core.Text
    -- ^ The name of the DB proxy.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter is not currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Natural
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBProxies' value with any optional fields omitted.
mkDescribeDBProxies
    :: DescribeDBProxies
mkDescribeDBProxies
  = DescribeDBProxies'{dBProxyName = Core.Nothing,
                       filters = Core.Nothing, marker = Core.Nothing,
                       maxRecords = Core.Nothing}

-- | The name of the DB proxy.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpsDBProxyName :: Lens.Lens' DescribeDBProxies (Core.Maybe Core.Text)
ddbpsDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE ddbpsDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

-- | This parameter is not currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpsFilters :: Lens.Lens' DescribeDBProxies (Core.Maybe [Types.Filter])
ddbpsFilters = Lens.field @"filters"
{-# INLINEABLE ddbpsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpsMarker :: Lens.Lens' DescribeDBProxies (Core.Maybe Core.Text)
ddbpsMarker = Lens.field @"marker"
{-# INLINEABLE ddbpsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpsMaxRecords :: Lens.Lens' DescribeDBProxies (Core.Maybe Core.Natural)
ddbpsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbpsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeDBProxies where
        toQuery DescribeDBProxies{..}
          = Core.toQueryPair "Action" ("DescribeDBProxies" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBProxyName") dBProxyName
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeDBProxies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBProxies where
        type Rs DescribeDBProxies = DescribeDBProxiesResponse
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
          = Response.receiveXMLWrapper "DescribeDBProxiesResult"
              (\ s h x ->
                 DescribeDBProxiesResponse' Core.<$>
                   (x Core..@? "DBProxies" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBProxies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"dBProxies" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeDBProxiesResponse' smart constructor.
data DescribeDBProxiesResponse = DescribeDBProxiesResponse'
  { dBProxies :: Core.Maybe [Types.DBProxy]
    -- ^ A return value representing an arbitrary number of @DBProxy@ data structures.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDBProxiesResponse' value with any optional fields omitted.
mkDescribeDBProxiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBProxiesResponse
mkDescribeDBProxiesResponse responseStatus
  = DescribeDBProxiesResponse'{dBProxies = Core.Nothing,
                               marker = Core.Nothing, responseStatus}

-- | A return value representing an arbitrary number of @DBProxy@ data structures.
--
-- /Note:/ Consider using 'dBProxies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDBProxies :: Lens.Lens' DescribeDBProxiesResponse (Core.Maybe [Types.DBProxy])
drsDBProxies = Lens.field @"dBProxies"
{-# INLINEABLE drsDBProxies #-}
{-# DEPRECATED dBProxies "Use generic-lens or generic-optics with 'dBProxies' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsMarker :: Lens.Lens' DescribeDBProxiesResponse (Core.Maybe Core.Text)
drsMarker = Lens.field @"marker"
{-# INLINEABLE drsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeDBProxiesResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
