{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeReservedDBInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved DB instances for this account, or about a specified reserved DB instance.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeReservedDBInstances
  ( -- * Creating a request
    DescribeReservedDBInstances (..),
    mkDescribeReservedDBInstances,

    -- ** Request lenses
    drdbiDBInstanceClass,
    drdbiDuration,
    drdbiFilters,
    drdbiLeaseId,
    drdbiMarker,
    drdbiMaxRecords,
    drdbiMultiAZ,
    drdbiOfferingType,
    drdbiProductDescription,
    drdbiReservedDBInstanceId,
    drdbiReservedDBInstancesOfferingId,

    -- * Destructuring the response
    DescribeReservedDBInstancesResponse (..),
    mkDescribeReservedDBInstancesResponse,

    -- ** Response lenses
    drdbirrsMarker,
    drdbirrsReservedDBInstances,
    drdbirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeReservedDBInstances' smart constructor.
data DescribeReservedDBInstances = DescribeReservedDBInstances'
  { -- | The DB instance class filter value. Specify this parameter to show only those reservations matching the specified DB instances class.
    dBInstanceClass :: Core.Maybe Types.String,
    -- | The duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration.
    --
    -- Valid Values: @1 | 3 | 31536000 | 94608000@
    duration :: Core.Maybe Types.String,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | The lease identifier filter value. Specify this parameter to show only the reservation that matches the specified lease ID.
    leaseId :: Core.Maybe Types.String,
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | A value that indicates whether to show only those reservations that support Multi-AZ.
    multiAZ :: Core.Maybe Core.Bool,
    -- | The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type.
    --
    -- Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
    offeringType :: Core.Maybe Types.String,
    -- | The product description filter value. Specify this parameter to show only those reservations matching the specified product description.
    productDescription :: Core.Maybe Types.String,
    -- | The reserved DB instance identifier filter value. Specify this parameter to show only the reservation that matches the specified reservation ID.
    reservedDBInstanceId :: Core.Maybe Types.String,
    -- | The offering identifier filter value. Specify this parameter to show only purchased reservations matching the specified offering identifier.
    reservedDBInstancesOfferingId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedDBInstances' value with any optional fields omitted.
mkDescribeReservedDBInstances ::
  DescribeReservedDBInstances
mkDescribeReservedDBInstances =
  DescribeReservedDBInstances'
    { dBInstanceClass = Core.Nothing,
      duration = Core.Nothing,
      filters = Core.Nothing,
      leaseId = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      multiAZ = Core.Nothing,
      offeringType = Core.Nothing,
      productDescription = Core.Nothing,
      reservedDBInstanceId = Core.Nothing,
      reservedDBInstancesOfferingId = Core.Nothing
    }

-- | The DB instance class filter value. Specify this parameter to show only those reservations matching the specified DB instances class.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiDBInstanceClass :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Types.String)
drdbiDBInstanceClass = Lens.field @"dBInstanceClass"
{-# DEPRECATED drdbiDBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead." #-}

-- | The duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiDuration :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Types.String)
drdbiDuration = Lens.field @"duration"
{-# DEPRECATED drdbiDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiFilters :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe [Types.Filter])
drdbiFilters = Lens.field @"filters"
{-# DEPRECATED drdbiFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The lease identifier filter value. Specify this parameter to show only the reservation that matches the specified lease ID.
--
-- /Note:/ Consider using 'leaseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiLeaseId :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Types.String)
drdbiLeaseId = Lens.field @"leaseId"
{-# DEPRECATED drdbiLeaseId "Use generic-lens or generic-optics with 'leaseId' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiMarker :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Types.String)
drdbiMarker = Lens.field @"marker"
{-# DEPRECATED drdbiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiMaxRecords :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Core.Int)
drdbiMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED drdbiMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A value that indicates whether to show only those reservations that support Multi-AZ.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiMultiAZ :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Core.Bool)
drdbiMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED drdbiMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiOfferingType :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Types.String)
drdbiOfferingType = Lens.field @"offeringType"
{-# DEPRECATED drdbiOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The product description filter value. Specify this parameter to show only those reservations matching the specified product description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiProductDescription :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Types.String)
drdbiProductDescription = Lens.field @"productDescription"
{-# DEPRECATED drdbiProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The reserved DB instance identifier filter value. Specify this parameter to show only the reservation that matches the specified reservation ID.
--
-- /Note:/ Consider using 'reservedDBInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiReservedDBInstanceId :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Types.String)
drdbiReservedDBInstanceId = Lens.field @"reservedDBInstanceId"
{-# DEPRECATED drdbiReservedDBInstanceId "Use generic-lens or generic-optics with 'reservedDBInstanceId' instead." #-}

-- | The offering identifier filter value. Specify this parameter to show only purchased reservations matching the specified offering identifier.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbiReservedDBInstancesOfferingId :: Lens.Lens' DescribeReservedDBInstances (Core.Maybe Types.String)
drdbiReservedDBInstancesOfferingId = Lens.field @"reservedDBInstancesOfferingId"
{-# DEPRECATED drdbiReservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead." #-}

instance Core.AWSRequest DescribeReservedDBInstances where
  type
    Rs DescribeReservedDBInstances =
      DescribeReservedDBInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeReservedDBInstances")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBInstanceClass" Core.<$> dBInstanceClass)
                Core.<> (Core.toQueryValue "Duration" Core.<$> duration)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "LeaseId" Core.<$> leaseId)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "MultiAZ" Core.<$> multiAZ)
                Core.<> (Core.toQueryValue "OfferingType" Core.<$> offeringType)
                Core.<> ( Core.toQueryValue "ProductDescription"
                            Core.<$> productDescription
                        )
                Core.<> ( Core.toQueryValue "ReservedDBInstanceId"
                            Core.<$> reservedDBInstanceId
                        )
                Core.<> ( Core.toQueryValue "ReservedDBInstancesOfferingId"
                            Core.<$> reservedDBInstancesOfferingId
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeReservedDBInstancesResult"
      ( \s h x ->
          DescribeReservedDBInstancesResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "ReservedDBInstances"
                         Core..<@> Core.parseXMLList "ReservedDBInstance"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeReservedDBInstances where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"reservedDBInstances" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeReservedDBInstances@ action.
--
-- /See:/ 'mkDescribeReservedDBInstancesResponse' smart constructor.
data DescribeReservedDBInstancesResponse = DescribeReservedDBInstancesResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | A list of reserved DB instances.
    reservedDBInstances :: Core.Maybe [Types.ReservedDBInstance],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeReservedDBInstancesResponse' value with any optional fields omitted.
mkDescribeReservedDBInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeReservedDBInstancesResponse
mkDescribeReservedDBInstancesResponse responseStatus =
  DescribeReservedDBInstancesResponse'
    { marker = Core.Nothing,
      reservedDBInstances = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbirrsMarker :: Lens.Lens' DescribeReservedDBInstancesResponse (Core.Maybe Types.String)
drdbirrsMarker = Lens.field @"marker"
{-# DEPRECATED drdbirrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of reserved DB instances.
--
-- /Note:/ Consider using 'reservedDBInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbirrsReservedDBInstances :: Lens.Lens' DescribeReservedDBInstancesResponse (Core.Maybe [Types.ReservedDBInstance])
drdbirrsReservedDBInstances = Lens.field @"reservedDBInstances"
{-# DEPRECATED drdbirrsReservedDBInstances "Use generic-lens or generic-optics with 'reservedDBInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdbirrsResponseStatus :: Lens.Lens' DescribeReservedDBInstancesResponse Core.Int
drdbirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drdbirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
