{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeReservedDBInstancesOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved DB instance offerings.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeReservedDBInstancesOfferings
  ( -- * Creating a Request
    DescribeReservedDBInstancesOfferings (..),
    newDescribeReservedDBInstancesOfferings,

    -- * Request Lenses
    describeReservedDBInstancesOfferings_duration,
    describeReservedDBInstancesOfferings_multiAZ,
    describeReservedDBInstancesOfferings_dbInstanceClass,
    describeReservedDBInstancesOfferings_filters,
    describeReservedDBInstancesOfferings_offeringType,
    describeReservedDBInstancesOfferings_productDescription,
    describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId,
    describeReservedDBInstancesOfferings_marker,
    describeReservedDBInstancesOfferings_maxRecords,

    -- * Destructuring the Response
    DescribeReservedDBInstancesOfferingsResponse (..),
    newDescribeReservedDBInstancesOfferingsResponse,

    -- * Response Lenses
    describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings,
    describeReservedDBInstancesOfferingsResponse_marker,
    describeReservedDBInstancesOfferingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeReservedDBInstancesOfferings' smart constructor.
data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings'
  { -- | Duration filter value, specified in years or seconds. Specify this
    -- parameter to show only reservations for this duration.
    --
    -- Valid Values: @1 | 3 | 31536000 | 94608000@
    duration :: Core.Maybe Core.Text,
    -- | A value that indicates whether to show only those reservations that
    -- support Multi-AZ.
    multiAZ :: Core.Maybe Core.Bool,
    -- | The DB instance class filter value. Specify this parameter to show only
    -- the available offerings matching the specified DB instance class.
    dbInstanceClass :: Core.Maybe Core.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | The offering type filter value. Specify this parameter to show only the
    -- available offerings matching the specified offering type.
    --
    -- Valid Values: @\"Partial Upfront\" | \"All Upfront\" | \"No Upfront\" @
    offeringType :: Core.Maybe Core.Text,
    -- | Product description filter value. Specify this parameter to show only
    -- the available offerings that contain the specified product description.
    --
    -- The results show offerings that partially match the filter value.
    productDescription :: Core.Maybe Core.Text,
    -- | The offering identifier filter value. Specify this parameter to show
    -- only the available offering that matches the specified reservation
    -- identifier.
    --
    -- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
    reservedDBInstancesOfferingId :: Core.Maybe Core.Text,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more than
    -- the @MaxRecords@ value is available, a pagination token called a marker
    -- is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedDBInstancesOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'describeReservedDBInstancesOfferings_duration' - Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- 'multiAZ', 'describeReservedDBInstancesOfferings_multiAZ' - A value that indicates whether to show only those reservations that
-- support Multi-AZ.
--
-- 'dbInstanceClass', 'describeReservedDBInstancesOfferings_dbInstanceClass' - The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
--
-- 'filters', 'describeReservedDBInstancesOfferings_filters' - This parameter isn\'t currently supported.
--
-- 'offeringType', 'describeReservedDBInstancesOfferings_offeringType' - The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values: @\"Partial Upfront\" | \"All Upfront\" | \"No Upfront\" @
--
-- 'productDescription', 'describeReservedDBInstancesOfferings_productDescription' - Product description filter value. Specify this parameter to show only
-- the available offerings that contain the specified product description.
--
-- The results show offerings that partially match the filter value.
--
-- 'reservedDBInstancesOfferingId', 'describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId' - The offering identifier filter value. Specify this parameter to show
-- only the available offering that matches the specified reservation
-- identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
--
-- 'marker', 'describeReservedDBInstancesOfferings_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReservedDBInstancesOfferings_maxRecords' - The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so you can retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeReservedDBInstancesOfferings ::
  DescribeReservedDBInstancesOfferings
newDescribeReservedDBInstancesOfferings =
  DescribeReservedDBInstancesOfferings'
    { duration =
        Core.Nothing,
      multiAZ = Core.Nothing,
      dbInstanceClass = Core.Nothing,
      filters = Core.Nothing,
      offeringType = Core.Nothing,
      productDescription = Core.Nothing,
      reservedDBInstancesOfferingId =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
describeReservedDBInstancesOfferings_duration :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
describeReservedDBInstancesOfferings_duration = Lens.lens (\DescribeReservedDBInstancesOfferings' {duration} -> duration) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {duration = a} :: DescribeReservedDBInstancesOfferings)

-- | A value that indicates whether to show only those reservations that
-- support Multi-AZ.
describeReservedDBInstancesOfferings_multiAZ :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Bool)
describeReservedDBInstancesOfferings_multiAZ = Lens.lens (\DescribeReservedDBInstancesOfferings' {multiAZ} -> multiAZ) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {multiAZ = a} :: DescribeReservedDBInstancesOfferings)

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
describeReservedDBInstancesOfferings_dbInstanceClass :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
describeReservedDBInstancesOfferings_dbInstanceClass = Lens.lens (\DescribeReservedDBInstancesOfferings' {dbInstanceClass} -> dbInstanceClass) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {dbInstanceClass = a} :: DescribeReservedDBInstancesOfferings)

-- | This parameter isn\'t currently supported.
describeReservedDBInstancesOfferings_filters :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe [Filter])
describeReservedDBInstancesOfferings_filters = Lens.lens (\DescribeReservedDBInstancesOfferings' {filters} -> filters) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {filters = a} :: DescribeReservedDBInstancesOfferings) Core.. Lens.mapping Lens._Coerce

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values: @\"Partial Upfront\" | \"All Upfront\" | \"No Upfront\" @
describeReservedDBInstancesOfferings_offeringType :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
describeReservedDBInstancesOfferings_offeringType = Lens.lens (\DescribeReservedDBInstancesOfferings' {offeringType} -> offeringType) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {offeringType = a} :: DescribeReservedDBInstancesOfferings)

-- | Product description filter value. Specify this parameter to show only
-- the available offerings that contain the specified product description.
--
-- The results show offerings that partially match the filter value.
describeReservedDBInstancesOfferings_productDescription :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
describeReservedDBInstancesOfferings_productDescription = Lens.lens (\DescribeReservedDBInstancesOfferings' {productDescription} -> productDescription) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {productDescription = a} :: DescribeReservedDBInstancesOfferings)

-- | The offering identifier filter value. Specify this parameter to show
-- only the available offering that matches the specified reservation
-- identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId = Lens.lens (\DescribeReservedDBInstancesOfferings' {reservedDBInstancesOfferingId} -> reservedDBInstancesOfferingId) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {reservedDBInstancesOfferingId = a} :: DescribeReservedDBInstancesOfferings)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReservedDBInstancesOfferings_marker :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Text)
describeReservedDBInstancesOfferings_marker = Lens.lens (\DescribeReservedDBInstancesOfferings' {marker} -> marker) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {marker = a} :: DescribeReservedDBInstancesOfferings)

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so you can retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReservedDBInstancesOfferings_maxRecords :: Lens.Lens' DescribeReservedDBInstancesOfferings (Core.Maybe Core.Int)
describeReservedDBInstancesOfferings_maxRecords = Lens.lens (\DescribeReservedDBInstancesOfferings' {maxRecords} -> maxRecords) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {maxRecords = a} :: DescribeReservedDBInstancesOfferings)

instance
  Core.AWSPager
    DescribeReservedDBInstancesOfferings
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedDBInstancesOfferingsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeReservedDBInstancesOfferings_marker
          Lens..~ rs
          Lens.^? describeReservedDBInstancesOfferingsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeReservedDBInstancesOfferings
  where
  type
    AWSResponse DescribeReservedDBInstancesOfferings =
      DescribeReservedDBInstancesOfferingsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeReservedDBInstancesOfferingsResult"
      ( \s h x ->
          DescribeReservedDBInstancesOfferingsResponse'
            Core.<$> ( x Core..@? "ReservedDBInstancesOfferings"
                         Core..!@ Core.mempty
                         Core.>>= Core.may
                           (Core.parseXMLList "ReservedDBInstancesOffering")
                     )
              Core.<*> (x Core..@? "Marker")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeReservedDBInstancesOfferings

instance
  Core.NFData
    DescribeReservedDBInstancesOfferings

instance
  Core.ToHeaders
    DescribeReservedDBInstancesOfferings
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeReservedDBInstancesOfferings
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeReservedDBInstancesOfferings
  where
  toQuery DescribeReservedDBInstancesOfferings' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeReservedDBInstancesOfferings" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Duration" Core.=: duration,
        "MultiAZ" Core.=: multiAZ,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "OfferingType" Core.=: offeringType,
        "ProductDescription" Core.=: productDescription,
        "ReservedDBInstancesOfferingId"
          Core.=: reservedDBInstancesOfferingId,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeReservedDBInstancesOfferings@ action.
--
-- /See:/ 'newDescribeReservedDBInstancesOfferingsResponse' smart constructor.
data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse'
  { -- | A list of reserved DB instance offerings.
    reservedDBInstancesOfferings :: Core.Maybe [ReservedDBInstancesOffering],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedDBInstancesOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedDBInstancesOfferings', 'describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings' - A list of reserved DB instance offerings.
--
-- 'marker', 'describeReservedDBInstancesOfferingsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeReservedDBInstancesOfferingsResponse_httpStatus' - The response's http status code.
newDescribeReservedDBInstancesOfferingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReservedDBInstancesOfferingsResponse
newDescribeReservedDBInstancesOfferingsResponse
  pHttpStatus_ =
    DescribeReservedDBInstancesOfferingsResponse'
      { reservedDBInstancesOfferings =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of reserved DB instance offerings.
describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse (Core.Maybe [ReservedDBInstancesOffering])
describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings = Lens.lens (\DescribeReservedDBInstancesOfferingsResponse' {reservedDBInstancesOfferings} -> reservedDBInstancesOfferings) (\s@DescribeReservedDBInstancesOfferingsResponse' {} a -> s {reservedDBInstancesOfferings = a} :: DescribeReservedDBInstancesOfferingsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReservedDBInstancesOfferingsResponse_marker :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse (Core.Maybe Core.Text)
describeReservedDBInstancesOfferingsResponse_marker = Lens.lens (\DescribeReservedDBInstancesOfferingsResponse' {marker} -> marker) (\s@DescribeReservedDBInstancesOfferingsResponse' {} a -> s {marker = a} :: DescribeReservedDBInstancesOfferingsResponse)

-- | The response's http status code.
describeReservedDBInstancesOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse Core.Int
describeReservedDBInstancesOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedDBInstancesOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedDBInstancesOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedDBInstancesOfferingsResponse)

instance
  Core.NFData
    DescribeReservedDBInstancesOfferingsResponse
