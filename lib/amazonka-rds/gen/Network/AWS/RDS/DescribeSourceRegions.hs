{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeSourceRegions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the source AWS Regions where the current AWS Region can create a read replica or copy a DB snapshot from. This API action supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeSourceRegions
    (
    -- * Creating a request
      DescribeSourceRegions (..)
    , mkDescribeSourceRegions
    -- ** Request lenses
    , dsrFilters
    , dsrMarker
    , dsrMaxRecords
    , dsrRegionName

    -- * Destructuring the response
    , DescribeSourceRegionsResponse (..)
    , mkDescribeSourceRegionsResponse
    -- ** Response lenses
    , dsrrrsMarker
    , dsrrrsSourceRegions
    , dsrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeSourceRegions' smart constructor.
data DescribeSourceRegions = DescribeSourceRegions'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeSourceRegions@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , regionName :: Core.Maybe Core.Text
    -- ^ The source AWS Region name. For example, @us-east-1@ .
--
-- Constraints:
--
--     * Must specify a valid AWS Region name.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSourceRegions' value with any optional fields omitted.
mkDescribeSourceRegions
    :: DescribeSourceRegions
mkDescribeSourceRegions
  = DescribeSourceRegions'{filters = Core.Nothing,
                           marker = Core.Nothing, maxRecords = Core.Nothing,
                           regionName = Core.Nothing}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrFilters :: Lens.Lens' DescribeSourceRegions (Core.Maybe [Types.Filter])
dsrFilters = Lens.field @"filters"
{-# INLINEABLE dsrFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeSourceRegions@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMarker :: Lens.Lens' DescribeSourceRegions (Core.Maybe Core.Text)
dsrMarker = Lens.field @"marker"
{-# INLINEABLE dsrMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMaxRecords :: Lens.Lens' DescribeSourceRegions (Core.Maybe Core.Int)
dsrMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dsrMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The source AWS Region name. For example, @us-east-1@ .
--
-- Constraints:
--
--     * Must specify a valid AWS Region name.
--
--
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrRegionName :: Lens.Lens' DescribeSourceRegions (Core.Maybe Core.Text)
dsrRegionName = Lens.field @"regionName"
{-# INLINEABLE dsrRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

instance Core.ToQuery DescribeSourceRegions where
        toQuery DescribeSourceRegions{..}
          = Core.toQueryPair "Action" ("DescribeSourceRegions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RegionName") regionName

instance Core.ToHeaders DescribeSourceRegions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSourceRegions where
        type Rs DescribeSourceRegions = DescribeSourceRegionsResponse
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
          = Response.receiveXMLWrapper "DescribeSourceRegionsResult"
              (\ s h x ->
                 DescribeSourceRegionsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "SourceRegions" Core..<@>
                       Core.parseXMLList "SourceRegion"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSourceRegions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"sourceRegions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeSourceRegions@ action.
--
-- /See:/ 'mkDescribeSourceRegionsResponse' smart constructor.
data DescribeSourceRegionsResponse = DescribeSourceRegionsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , sourceRegions :: Core.Maybe [Types.SourceRegion]
    -- ^ A list of SourceRegion instances that contains each source AWS Region that the current AWS Region can get a read replica or a DB snapshot from.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSourceRegionsResponse' value with any optional fields omitted.
mkDescribeSourceRegionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSourceRegionsResponse
mkDescribeSourceRegionsResponse responseStatus
  = DescribeSourceRegionsResponse'{marker = Core.Nothing,
                                   sourceRegions = Core.Nothing, responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsMarker :: Lens.Lens' DescribeSourceRegionsResponse (Core.Maybe Core.Text)
dsrrrsMarker = Lens.field @"marker"
{-# INLINEABLE dsrrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of SourceRegion instances that contains each source AWS Region that the current AWS Region can get a read replica or a DB snapshot from.
--
-- /Note:/ Consider using 'sourceRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsSourceRegions :: Lens.Lens' DescribeSourceRegionsResponse (Core.Maybe [Types.SourceRegion])
dsrrrsSourceRegions = Lens.field @"sourceRegions"
{-# INLINEABLE dsrrrsSourceRegions #-}
{-# DEPRECATED sourceRegions "Use generic-lens or generic-optics with 'sourceRegions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrrsResponseStatus :: Lens.Lens' DescribeSourceRegionsResponse Core.Int
dsrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
