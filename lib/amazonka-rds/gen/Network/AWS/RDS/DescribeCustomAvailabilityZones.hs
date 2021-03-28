{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeCustomAvailabilityZones
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about custom Availability Zones (AZs).
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware vSphere cluster.
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ > 
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeCustomAvailabilityZones
    (
    -- * Creating a request
      DescribeCustomAvailabilityZones (..)
    , mkDescribeCustomAvailabilityZones
    -- ** Request lenses
    , dcazCustomAvailabilityZoneId
    , dcazFilters
    , dcazMarker
    , dcazMaxRecords

    -- * Destructuring the response
    , DescribeCustomAvailabilityZonesResponse (..)
    , mkDescribeCustomAvailabilityZonesResponse
    -- ** Response lenses
    , dcazrrsCustomAvailabilityZones
    , dcazrrsMarker
    , dcazrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCustomAvailabilityZones' smart constructor.
data DescribeCustomAvailabilityZones = DescribeCustomAvailabilityZones'
  { customAvailabilityZoneId :: Core.Maybe Core.Text
    -- ^ The custom AZ identifier. If this parameter is specified, information from only the specific custom AZ is returned.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ A filter that specifies one or more custom AZs to describe.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeCustomAvailabilityZones@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCustomAvailabilityZones' value with any optional fields omitted.
mkDescribeCustomAvailabilityZones
    :: DescribeCustomAvailabilityZones
mkDescribeCustomAvailabilityZones
  = DescribeCustomAvailabilityZones'{customAvailabilityZoneId =
                                       Core.Nothing,
                                     filters = Core.Nothing, marker = Core.Nothing,
                                     maxRecords = Core.Nothing}

-- | The custom AZ identifier. If this parameter is specified, information from only the specific custom AZ is returned.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazCustomAvailabilityZoneId :: Lens.Lens' DescribeCustomAvailabilityZones (Core.Maybe Core.Text)
dcazCustomAvailabilityZoneId = Lens.field @"customAvailabilityZoneId"
{-# INLINEABLE dcazCustomAvailabilityZoneId #-}
{-# DEPRECATED customAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead"  #-}

-- | A filter that specifies one or more custom AZs to describe.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazFilters :: Lens.Lens' DescribeCustomAvailabilityZones (Core.Maybe [Types.Filter])
dcazFilters = Lens.field @"filters"
{-# INLINEABLE dcazFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeCustomAvailabilityZones@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazMarker :: Lens.Lens' DescribeCustomAvailabilityZones (Core.Maybe Core.Text)
dcazMarker = Lens.field @"marker"
{-# INLINEABLE dcazMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazMaxRecords :: Lens.Lens' DescribeCustomAvailabilityZones (Core.Maybe Core.Int)
dcazMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dcazMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeCustomAvailabilityZones where
        toQuery DescribeCustomAvailabilityZones{..}
          = Core.toQueryPair "Action"
              ("DescribeCustomAvailabilityZones" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "CustomAvailabilityZoneId")
                customAvailabilityZoneId
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeCustomAvailabilityZones where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeCustomAvailabilityZones where
        type Rs DescribeCustomAvailabilityZones =
             DescribeCustomAvailabilityZonesResponse
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
              "DescribeCustomAvailabilityZonesResult"
              (\ s h x ->
                 DescribeCustomAvailabilityZonesResponse' Core.<$>
                   (x Core..@? "CustomAvailabilityZones" Core..<@>
                      Core.parseXMLList "CustomAvailabilityZone")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeCustomAvailabilityZones where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"customAvailabilityZones" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeCustomAvailabilityZonesResponse' smart constructor.
data DescribeCustomAvailabilityZonesResponse = DescribeCustomAvailabilityZonesResponse'
  { customAvailabilityZones :: Core.Maybe [Types.CustomAvailabilityZone]
    -- ^ The list of 'CustomAvailabilityZone' objects for the AWS account.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeCustomAvailabilityZones@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCustomAvailabilityZonesResponse' value with any optional fields omitted.
mkDescribeCustomAvailabilityZonesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCustomAvailabilityZonesResponse
mkDescribeCustomAvailabilityZonesResponse responseStatus
  = DescribeCustomAvailabilityZonesResponse'{customAvailabilityZones
                                               = Core.Nothing,
                                             marker = Core.Nothing, responseStatus}

-- | The list of 'CustomAvailabilityZone' objects for the AWS account.
--
-- /Note:/ Consider using 'customAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazrrsCustomAvailabilityZones :: Lens.Lens' DescribeCustomAvailabilityZonesResponse (Core.Maybe [Types.CustomAvailabilityZone])
dcazrrsCustomAvailabilityZones = Lens.field @"customAvailabilityZones"
{-# INLINEABLE dcazrrsCustomAvailabilityZones #-}
{-# DEPRECATED customAvailabilityZones "Use generic-lens or generic-optics with 'customAvailabilityZones' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeCustomAvailabilityZones@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazrrsMarker :: Lens.Lens' DescribeCustomAvailabilityZonesResponse (Core.Maybe Core.Text)
dcazrrsMarker = Lens.field @"marker"
{-# INLINEABLE dcazrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcazrrsResponseStatus :: Lens.Lens' DescribeCustomAvailabilityZonesResponse Core.Int
dcazrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcazrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
