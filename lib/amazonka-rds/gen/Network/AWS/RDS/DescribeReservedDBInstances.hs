{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    drdiProductDescription,
    drdiFilters,
    drdiLeaseId,
    drdiReservedDBInstanceId,
    drdiDBInstanceClass,
    drdiMarker,
    drdiMaxRecords,
    drdiMultiAZ,
    drdiReservedDBInstancesOfferingId,
    drdiOfferingType,
    drdiDuration,

    -- * Destructuring the response
    DescribeReservedDBInstancesResponse (..),
    mkDescribeReservedDBInstancesResponse,

    -- ** Response lenses
    drdirsReservedDBInstances,
    drdirsMarker,
    drdirsResponseStatus,
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
-- /See:/ 'mkDescribeReservedDBInstances' smart constructor.
data DescribeReservedDBInstances = DescribeReservedDBInstances'
  { productDescription ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [Filter],
    leaseId :: Lude.Maybe Lude.Text,
    reservedDBInstanceId ::
      Lude.Maybe Lude.Text,
    dbInstanceClass ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    multiAZ :: Lude.Maybe Lude.Bool,
    reservedDBInstancesOfferingId ::
      Lude.Maybe Lude.Text,
    offeringType ::
      Lude.Maybe Lude.Text,
    duration :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedDBInstances' with the minimum fields required to make a request.
--
-- * 'dbInstanceClass' - The DB instance class filter value. Specify this parameter to show only those reservations matching the specified DB instances class.
-- * 'duration' - The duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
-- * 'filters' - This parameter isn't currently supported.
-- * 'leaseId' - The lease identifier filter value. Specify this parameter to show only the reservation that matches the specified lease ID.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'multiAZ' - A value that indicates whether to show only those reservations that support Multi-AZ.
-- * 'offeringType' - The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
-- * 'productDescription' - The product description filter value. Specify this parameter to show only those reservations matching the specified product description.
-- * 'reservedDBInstanceId' - The reserved DB instance identifier filter value. Specify this parameter to show only the reservation that matches the specified reservation ID.
-- * 'reservedDBInstancesOfferingId' - The offering identifier filter value. Specify this parameter to show only purchased reservations matching the specified offering identifier.
mkDescribeReservedDBInstances ::
  DescribeReservedDBInstances
mkDescribeReservedDBInstances =
  DescribeReservedDBInstances'
    { productDescription = Lude.Nothing,
      filters = Lude.Nothing,
      leaseId = Lude.Nothing,
      reservedDBInstanceId = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      multiAZ = Lude.Nothing,
      reservedDBInstancesOfferingId = Lude.Nothing,
      offeringType = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | The product description filter value. Specify this parameter to show only those reservations matching the specified product description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiProductDescription :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Text)
drdiProductDescription = Lens.lens (productDescription :: DescribeReservedDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiFilters :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe [Filter])
drdiFilters = Lens.lens (filters :: DescribeReservedDBInstances -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The lease identifier filter value. Specify this parameter to show only the reservation that matches the specified lease ID.
--
-- /Note:/ Consider using 'leaseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiLeaseId :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Text)
drdiLeaseId = Lens.lens (leaseId :: DescribeReservedDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {leaseId = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiLeaseId "Use generic-lens or generic-optics with 'leaseId' instead." #-}

-- | The reserved DB instance identifier filter value. Specify this parameter to show only the reservation that matches the specified reservation ID.
--
-- /Note:/ Consider using 'reservedDBInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiReservedDBInstanceId :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Text)
drdiReservedDBInstanceId = Lens.lens (reservedDBInstanceId :: DescribeReservedDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {reservedDBInstanceId = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiReservedDBInstanceId "Use generic-lens or generic-optics with 'reservedDBInstanceId' instead." #-}

-- | The DB instance class filter value. Specify this parameter to show only those reservations matching the specified DB instances class.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiDBInstanceClass :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Text)
drdiDBInstanceClass = Lens.lens (dbInstanceClass :: DescribeReservedDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiMarker :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Text)
drdiMarker = Lens.lens (marker :: DescribeReservedDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiMaxRecords :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Int)
drdiMaxRecords = Lens.lens (maxRecords :: DescribeReservedDBInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A value that indicates whether to show only those reservations that support Multi-AZ.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiMultiAZ :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Bool)
drdiMultiAZ = Lens.lens (multiAZ :: DescribeReservedDBInstances -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The offering identifier filter value. Specify this parameter to show only purchased reservations matching the specified offering identifier.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiReservedDBInstancesOfferingId :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Text)
drdiReservedDBInstancesOfferingId = Lens.lens (reservedDBInstancesOfferingId :: DescribeReservedDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {reservedDBInstancesOfferingId = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiReservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead." #-}

-- | The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiOfferingType :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Text)
drdiOfferingType = Lens.lens (offeringType :: DescribeReservedDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiDuration :: Lens.Lens' DescribeReservedDBInstances (Lude.Maybe Lude.Text)
drdiDuration = Lens.lens (duration :: DescribeReservedDBInstances -> Lude.Maybe Lude.Text) (\s a -> s {duration = a} :: DescribeReservedDBInstances)
{-# DEPRECATED drdiDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Page.AWSPager DescribeReservedDBInstances where
  page rq rs
    | Page.stop (rs Lens.^. drdirsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drdirsReservedDBInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drdiMarker Lens..~ rs Lens.^. drdirsMarker

instance Lude.AWSRequest DescribeReservedDBInstances where
  type
    Rs DescribeReservedDBInstances =
      DescribeReservedDBInstancesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeReservedDBInstancesResult"
      ( \s h x ->
          DescribeReservedDBInstancesResponse'
            Lude.<$> ( x Lude..@? "ReservedDBInstances" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ReservedDBInstance")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedDBInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedDBInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedDBInstances where
  toQuery DescribeReservedDBInstances' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeReservedDBInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ProductDescription" Lude.=: productDescription,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "LeaseId" Lude.=: leaseId,
        "ReservedDBInstanceId" Lude.=: reservedDBInstanceId,
        "DBInstanceClass" Lude.=: dbInstanceClass,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "MultiAZ" Lude.=: multiAZ,
        "ReservedDBInstancesOfferingId"
          Lude.=: reservedDBInstancesOfferingId,
        "OfferingType" Lude.=: offeringType,
        "Duration" Lude.=: duration
      ]

-- | Contains the result of a successful invocation of the @DescribeReservedDBInstances@ action.
--
-- /See:/ 'mkDescribeReservedDBInstancesResponse' smart constructor.
data DescribeReservedDBInstancesResponse = DescribeReservedDBInstancesResponse'
  { reservedDBInstances ::
      Lude.Maybe
        [ReservedDBInstance],
    marker ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedDBInstancesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'reservedDBInstances' - A list of reserved DB instances.
-- * 'responseStatus' - The response status code.
mkDescribeReservedDBInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedDBInstancesResponse
mkDescribeReservedDBInstancesResponse pResponseStatus_ =
  DescribeReservedDBInstancesResponse'
    { reservedDBInstances =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of reserved DB instances.
--
-- /Note:/ Consider using 'reservedDBInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdirsReservedDBInstances :: Lens.Lens' DescribeReservedDBInstancesResponse (Lude.Maybe [ReservedDBInstance])
drdirsReservedDBInstances = Lens.lens (reservedDBInstances :: DescribeReservedDBInstancesResponse -> Lude.Maybe [ReservedDBInstance]) (\s a -> s {reservedDBInstances = a} :: DescribeReservedDBInstancesResponse)
{-# DEPRECATED drdirsReservedDBInstances "Use generic-lens or generic-optics with 'reservedDBInstances' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdirsMarker :: Lens.Lens' DescribeReservedDBInstancesResponse (Lude.Maybe Lude.Text)
drdirsMarker = Lens.lens (marker :: DescribeReservedDBInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedDBInstancesResponse)
{-# DEPRECATED drdirsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdirsResponseStatus :: Lens.Lens' DescribeReservedDBInstancesResponse Lude.Int
drdirsResponseStatus = Lens.lens (responseStatus :: DescribeReservedDBInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedDBInstancesResponse)
{-# DEPRECATED drdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
