{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeReservedDBInstancesOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved DB instance offerings.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeReservedDBInstancesOfferings
    (
    -- * Creating a request
      DescribeReservedDBInstancesOfferings (..)
    , mkDescribeReservedDBInstancesOfferings
    -- ** Request lenses
    , drdbioDBInstanceClass
    , drdbioDuration
    , drdbioFilters
    , drdbioMarker
    , drdbioMaxRecords
    , drdbioMultiAZ
    , drdbioOfferingType
    , drdbioProductDescription
    , drdbioReservedDBInstancesOfferingId

    -- * Destructuring the response
    , DescribeReservedDBInstancesOfferingsResponse (..)
    , mkDescribeReservedDBInstancesOfferingsResponse
    -- ** Response lenses
    , drdbiorrsMarker
    , drdbiorrsReservedDBInstancesOfferings
    , drdbiorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeReservedDBInstancesOfferings' smart constructor.
data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings'
  { dBInstanceClass :: Core.Maybe Core.Text
    -- ^ The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
  , duration :: Core.Maybe Core.Text
    -- ^ Duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@ 
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , multiAZ :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to show only those reservations that support Multi-AZ.
  , offeringType :: Core.Maybe Core.Text
    -- ^ The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @ 
  , productDescription :: Core.Maybe Core.Text
    -- ^ Product description filter value. Specify this parameter to show only the available offerings that contain the specified product description.
  , reservedDBInstancesOfferingId :: Core.Maybe Core.Text
    -- ^ The offering identifier filter value. Specify this parameter to show only the available offering that matches the specified reservation identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedDBInstancesOfferings' value with any optional fields omitted.
mkDescribeReservedDBInstancesOfferings
    :: DescribeReservedDBInstancesOfferings
mkDescribeReservedDBInstancesOfferings
  = DescribeReservedDBInstancesOfferings'{dBInstanceClass =
                                            Core.Nothing,
                                          duration = Core.Nothing, filters = Core.Nothing,
                                          marker = Core.Nothing, maxRecords = Core.Nothing,
                                          multiAZ = Core.Nothing, offeringType = Core.Nothing,
                                          productDescription = Core.Nothing,
                                          reservedDBInstancesOfferingId = Core.Nothing}

-- | The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbioDBInstanceClass :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
drdbioDBInstanceClass = Lens.field @"dBInstanceClass"
{-# INLINEABLE drdbioDBInstanceClass #-}
{-# DEPRECATED dBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead"  #-}

-- | Duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@ 
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbioDuration :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
drdbioDuration = Lens.field @"duration"
{-# INLINEABLE drdbioDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbioFilters :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe [Types.Filter])
drdbioFilters = Lens.field @"filters"
{-# INLINEABLE drdbioFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbioMarker :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
drdbioMarker = Lens.field @"marker"
{-# INLINEABLE drdbioMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbioMaxRecords :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Int)
drdbioMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE drdbioMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | A value that indicates whether to show only those reservations that support Multi-AZ.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbioMultiAZ :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Bool)
drdbioMultiAZ = Lens.field @"multiAZ"
{-# INLINEABLE drdbioMultiAZ #-}
{-# DEPRECATED multiAZ "Use generic-lens or generic-optics with 'multiAZ' instead"  #-}

-- | The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @ 
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbioOfferingType :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
drdbioOfferingType = Lens.field @"offeringType"
{-# INLINEABLE drdbioOfferingType #-}
{-# DEPRECATED offeringType "Use generic-lens or generic-optics with 'offeringType' instead"  #-}

-- | Product description filter value. Specify this parameter to show only the available offerings that contain the specified product description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbioProductDescription :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
drdbioProductDescription = Lens.field @"productDescription"
{-# INLINEABLE drdbioProductDescription #-}
{-# DEPRECATED productDescription "Use generic-lens or generic-optics with 'productDescription' instead"  #-}

-- | The offering identifier filter value. Specify this parameter to show only the available offering that matches the specified reservation identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@ 
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbioReservedDBInstancesOfferingId :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
drdbioReservedDBInstancesOfferingId = Lens.field @"reservedDBInstancesOfferingId"
{-# INLINEABLE drdbioReservedDBInstancesOfferingId #-}
{-# DEPRECATED reservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead"  #-}

instance Core.ToQuery DescribeReservedDBInstancesOfferings where
        toQuery DescribeReservedDBInstancesOfferings{..}
          = Core.toQueryPair "Action"
              ("DescribeReservedDBInstancesOfferings" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBInstanceClass")
                dBInstanceClass
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Duration") duration
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "MultiAZ") multiAZ
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OfferingType")
                offeringType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ProductDescription")
                productDescription
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ReservedDBInstancesOfferingId")
                reservedDBInstancesOfferingId

instance Core.ToHeaders DescribeReservedDBInstancesOfferings where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeReservedDBInstancesOfferings where
        type Rs DescribeReservedDBInstancesOfferings =
             DescribeReservedDBInstancesOfferingsResponse
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
              "DescribeReservedDBInstancesOfferingsResult"
              (\ s h x ->
                 DescribeReservedDBInstancesOfferingsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "ReservedDBInstancesOfferings" Core..<@>
                       Core.parseXMLList "ReservedDBInstancesOffering"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeReservedDBInstancesOfferings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"reservedDBInstancesOfferings" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeReservedDBInstancesOfferings@ action. 
--
-- /See:/ 'mkDescribeReservedDBInstancesOfferingsResponse' smart constructor.
data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , reservedDBInstancesOfferings :: Core.Maybe [Types.ReservedDBInstancesOffering]
    -- ^ A list of reserved DB instance offerings.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedDBInstancesOfferingsResponse' value with any optional fields omitted.
mkDescribeReservedDBInstancesOfferingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReservedDBInstancesOfferingsResponse
mkDescribeReservedDBInstancesOfferingsResponse responseStatus
  = DescribeReservedDBInstancesOfferingsResponse'{marker =
                                                    Core.Nothing,
                                                  reservedDBInstancesOfferings = Core.Nothing,
                                                  responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiorrsMarker :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse (Core.Maybe Core.Text)
drdbiorrsMarker = Lens.field @"marker"
{-# INLINEABLE drdbiorrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of reserved DB instance offerings.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiorrsReservedDBInstancesOfferings :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse (Core.Maybe [Types.ReservedDBInstancesOffering])
drdbiorrsReservedDBInstancesOfferings = Lens.field @"reservedDBInstancesOfferings"
{-# INLINEABLE drdbiorrsReservedDBInstancesOfferings #-}
{-# DEPRECATED reservedDBInstancesOfferings "Use generic-lens or generic-optics with 'reservedDBInstancesOfferings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiorrsResponseStatus :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse Core.Int
drdbiorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drdbiorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
