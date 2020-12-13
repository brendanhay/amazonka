{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeReservedDBInstancesOfferings (..),
    mkDescribeReservedDBInstancesOfferings,

    -- ** Request lenses
    drdioProductDescription,
    drdioFilters,
    drdioDBInstanceClass,
    drdioMarker,
    drdioMaxRecords,
    drdioMultiAZ,
    drdioReservedDBInstancesOfferingId,
    drdioOfferingType,
    drdioDuration,

    -- * Destructuring the response
    DescribeReservedDBInstancesOfferingsResponse (..),
    mkDescribeReservedDBInstancesOfferingsResponse,

    -- ** Response lenses
    drdiorsMarker,
    drdiorsReservedDBInstancesOfferings,
    drdiorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeReservedDBInstancesOfferings' smart constructor.
data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings'
  { -- | Product description filter value. Specify this parameter to show only the available offerings that contain the specified product description.
    productDescription :: Lude.Maybe Lude.Text,
    -- | This parameter isn't currently supported.
    filters :: Lude.Maybe [Filter],
    -- | The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
    dbInstanceClass :: Lude.Maybe Lude.Text,
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to show only those reservations that support Multi-AZ.
    multiAZ :: Lude.Maybe Lude.Bool,
    -- | The offering identifier filter value. Specify this parameter to show only the available offering that matches the specified reservation identifier.
    --
    -- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
    reservedDBInstancesOfferingId :: Lude.Maybe Lude.Text,
    -- | The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type.
    --
    -- Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
    offeringType :: Lude.Maybe Lude.Text,
    -- | Duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration.
    --
    -- Valid Values: @1 | 3 | 31536000 | 94608000@
    duration :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedDBInstancesOfferings' with the minimum fields required to make a request.
--
-- * 'productDescription' - Product description filter value. Specify this parameter to show only the available offerings that contain the specified product description.
-- * 'filters' - This parameter isn't currently supported.
-- * 'dbInstanceClass' - The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'multiAZ' - A value that indicates whether to show only those reservations that support Multi-AZ.
-- * 'reservedDBInstancesOfferingId' - The offering identifier filter value. Specify this parameter to show only the available offering that matches the specified reservation identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
-- * 'offeringType' - The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
-- * 'duration' - Duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
mkDescribeReservedDBInstancesOfferings ::
  DescribeReservedDBInstancesOfferings
mkDescribeReservedDBInstancesOfferings =
  DescribeReservedDBInstancesOfferings'
    { productDescription =
        Lude.Nothing,
      filters = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      multiAZ = Lude.Nothing,
      reservedDBInstancesOfferingId = Lude.Nothing,
      offeringType = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | Product description filter value. Specify this parameter to show only the available offerings that contain the specified product description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdioProductDescription :: Lens.Lens' DescribeReservedDBInstancesOfferings (Lude.Maybe Lude.Text)
drdioProductDescription = Lens.lens (productDescription :: DescribeReservedDBInstancesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: DescribeReservedDBInstancesOfferings)
{-# DEPRECATED drdioProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdioFilters :: Lens.Lens' DescribeReservedDBInstancesOfferings (Lude.Maybe [Filter])
drdioFilters = Lens.lens (filters :: DescribeReservedDBInstancesOfferings -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReservedDBInstancesOfferings)
{-# DEPRECATED drdioFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdioDBInstanceClass :: Lens.Lens' DescribeReservedDBInstancesOfferings (Lude.Maybe Lude.Text)
drdioDBInstanceClass = Lens.lens (dbInstanceClass :: DescribeReservedDBInstancesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: DescribeReservedDBInstancesOfferings)
{-# DEPRECATED drdioDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdioMarker :: Lens.Lens' DescribeReservedDBInstancesOfferings (Lude.Maybe Lude.Text)
drdioMarker = Lens.lens (marker :: DescribeReservedDBInstancesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedDBInstancesOfferings)
{-# DEPRECATED drdioMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdioMaxRecords :: Lens.Lens' DescribeReservedDBInstancesOfferings (Lude.Maybe Lude.Int)
drdioMaxRecords = Lens.lens (maxRecords :: DescribeReservedDBInstancesOfferings -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReservedDBInstancesOfferings)
{-# DEPRECATED drdioMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A value that indicates whether to show only those reservations that support Multi-AZ.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdioMultiAZ :: Lens.Lens' DescribeReservedDBInstancesOfferings (Lude.Maybe Lude.Bool)
drdioMultiAZ = Lens.lens (multiAZ :: DescribeReservedDBInstancesOfferings -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: DescribeReservedDBInstancesOfferings)
{-# DEPRECATED drdioMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The offering identifier filter value. Specify this parameter to show only the available offering that matches the specified reservation identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdioReservedDBInstancesOfferingId :: Lens.Lens' DescribeReservedDBInstancesOfferings (Lude.Maybe Lude.Text)
drdioReservedDBInstancesOfferingId = Lens.lens (reservedDBInstancesOfferingId :: DescribeReservedDBInstancesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {reservedDBInstancesOfferingId = a} :: DescribeReservedDBInstancesOfferings)
{-# DEPRECATED drdioReservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead." #-}

-- | The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdioOfferingType :: Lens.Lens' DescribeReservedDBInstancesOfferings (Lude.Maybe Lude.Text)
drdioOfferingType = Lens.lens (offeringType :: DescribeReservedDBInstancesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: DescribeReservedDBInstancesOfferings)
{-# DEPRECATED drdioOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | Duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdioDuration :: Lens.Lens' DescribeReservedDBInstancesOfferings (Lude.Maybe Lude.Text)
drdioDuration = Lens.lens (duration :: DescribeReservedDBInstancesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {duration = a} :: DescribeReservedDBInstancesOfferings)
{-# DEPRECATED drdioDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Page.AWSPager DescribeReservedDBInstancesOfferings where
  page rq rs
    | Page.stop (rs Lens.^. drdiorsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drdiorsReservedDBInstancesOfferings) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drdioMarker Lens..~ rs Lens.^. drdiorsMarker

instance Lude.AWSRequest DescribeReservedDBInstancesOfferings where
  type
    Rs DescribeReservedDBInstancesOfferings =
      DescribeReservedDBInstancesOfferingsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeReservedDBInstancesOfferingsResult"
      ( \s h x ->
          DescribeReservedDBInstancesOfferingsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "ReservedDBInstancesOfferings" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ReservedDBInstancesOffering")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedDBInstancesOfferings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedDBInstancesOfferings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedDBInstancesOfferings where
  toQuery DescribeReservedDBInstancesOfferings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeReservedDBInstancesOfferings" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ProductDescription" Lude.=: productDescription,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DBInstanceClass" Lude.=: dbInstanceClass,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "MultiAZ" Lude.=: multiAZ,
        "ReservedDBInstancesOfferingId"
          Lude.=: reservedDBInstancesOfferingId,
        "OfferingType" Lude.=: offeringType,
        "Duration" Lude.=: duration
      ]

-- | Contains the result of a successful invocation of the @DescribeReservedDBInstancesOfferings@ action.
--
-- /See:/ 'mkDescribeReservedDBInstancesOfferingsResponse' smart constructor.
data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | A list of reserved DB instance offerings.
    reservedDBInstancesOfferings :: Lude.Maybe [ReservedDBInstancesOffering],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedDBInstancesOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'reservedDBInstancesOfferings' - A list of reserved DB instance offerings.
-- * 'responseStatus' - The response status code.
mkDescribeReservedDBInstancesOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedDBInstancesOfferingsResponse
mkDescribeReservedDBInstancesOfferingsResponse pResponseStatus_ =
  DescribeReservedDBInstancesOfferingsResponse'
    { marker =
        Lude.Nothing,
      reservedDBInstancesOfferings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiorsMarker :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse (Lude.Maybe Lude.Text)
drdiorsMarker = Lens.lens (marker :: DescribeReservedDBInstancesOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedDBInstancesOfferingsResponse)
{-# DEPRECATED drdiorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of reserved DB instance offerings.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiorsReservedDBInstancesOfferings :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse (Lude.Maybe [ReservedDBInstancesOffering])
drdiorsReservedDBInstancesOfferings = Lens.lens (reservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferingsResponse -> Lude.Maybe [ReservedDBInstancesOffering]) (\s a -> s {reservedDBInstancesOfferings = a} :: DescribeReservedDBInstancesOfferingsResponse)
{-# DEPRECATED drdiorsReservedDBInstancesOfferings "Use generic-lens or generic-optics with 'reservedDBInstancesOfferings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiorsResponseStatus :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse Lude.Int
drdiorsResponseStatus = Lens.lens (responseStatus :: DescribeReservedDBInstancesOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedDBInstancesOfferingsResponse)
{-# DEPRECATED drdiorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
