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
    describeReservedDBInstancesOfferings_maxRecords,
    describeReservedDBInstancesOfferings_marker,

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
import qualified Network.AWS.Prelude as Prelude
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
    duration :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to show only those reservations that
    -- support Multi-AZ.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance class filter value. Specify this parameter to show only
    -- the available offerings matching the specified DB instance class.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The offering type filter value. Specify this parameter to show only the
    -- available offerings matching the specified offering type.
    --
    -- Valid Values: @\"Partial Upfront\" | \"All Upfront\" | \"No Upfront\" @
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | Product description filter value. Specify this parameter to show only
    -- the available offerings that contain the specified product description.
    --
    -- The results show offerings that partially match the filter value.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | The offering identifier filter value. Specify this parameter to show
    -- only the available offering that matches the specified reservation
    -- identifier.
    --
    -- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
    reservedDBInstancesOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more than
    -- the @MaxRecords@ value is available, a pagination token called a marker
    -- is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxRecords', 'describeReservedDBInstancesOfferings_maxRecords' - The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so you can retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'marker', 'describeReservedDBInstancesOfferings_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
newDescribeReservedDBInstancesOfferings ::
  DescribeReservedDBInstancesOfferings
newDescribeReservedDBInstancesOfferings =
  DescribeReservedDBInstancesOfferings'
    { duration =
        Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      filters = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      reservedDBInstancesOfferingId =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
describeReservedDBInstancesOfferings_duration :: Lens.Lens' DescribeReservedDBInstancesOfferings (Prelude.Maybe Prelude.Text)
describeReservedDBInstancesOfferings_duration = Lens.lens (\DescribeReservedDBInstancesOfferings' {duration} -> duration) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {duration = a} :: DescribeReservedDBInstancesOfferings)

-- | A value that indicates whether to show only those reservations that
-- support Multi-AZ.
describeReservedDBInstancesOfferings_multiAZ :: Lens.Lens' DescribeReservedDBInstancesOfferings (Prelude.Maybe Prelude.Bool)
describeReservedDBInstancesOfferings_multiAZ = Lens.lens (\DescribeReservedDBInstancesOfferings' {multiAZ} -> multiAZ) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {multiAZ = a} :: DescribeReservedDBInstancesOfferings)

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
describeReservedDBInstancesOfferings_dbInstanceClass :: Lens.Lens' DescribeReservedDBInstancesOfferings (Prelude.Maybe Prelude.Text)
describeReservedDBInstancesOfferings_dbInstanceClass = Lens.lens (\DescribeReservedDBInstancesOfferings' {dbInstanceClass} -> dbInstanceClass) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {dbInstanceClass = a} :: DescribeReservedDBInstancesOfferings)

-- | This parameter isn\'t currently supported.
describeReservedDBInstancesOfferings_filters :: Lens.Lens' DescribeReservedDBInstancesOfferings (Prelude.Maybe [Filter])
describeReservedDBInstancesOfferings_filters = Lens.lens (\DescribeReservedDBInstancesOfferings' {filters} -> filters) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {filters = a} :: DescribeReservedDBInstancesOfferings) Prelude.. Lens.mapping Lens._Coerce

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values: @\"Partial Upfront\" | \"All Upfront\" | \"No Upfront\" @
describeReservedDBInstancesOfferings_offeringType :: Lens.Lens' DescribeReservedDBInstancesOfferings (Prelude.Maybe Prelude.Text)
describeReservedDBInstancesOfferings_offeringType = Lens.lens (\DescribeReservedDBInstancesOfferings' {offeringType} -> offeringType) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {offeringType = a} :: DescribeReservedDBInstancesOfferings)

-- | Product description filter value. Specify this parameter to show only
-- the available offerings that contain the specified product description.
--
-- The results show offerings that partially match the filter value.
describeReservedDBInstancesOfferings_productDescription :: Lens.Lens' DescribeReservedDBInstancesOfferings (Prelude.Maybe Prelude.Text)
describeReservedDBInstancesOfferings_productDescription = Lens.lens (\DescribeReservedDBInstancesOfferings' {productDescription} -> productDescription) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {productDescription = a} :: DescribeReservedDBInstancesOfferings)

-- | The offering identifier filter value. Specify this parameter to show
-- only the available offering that matches the specified reservation
-- identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId :: Lens.Lens' DescribeReservedDBInstancesOfferings (Prelude.Maybe Prelude.Text)
describeReservedDBInstancesOfferings_reservedDBInstancesOfferingId = Lens.lens (\DescribeReservedDBInstancesOfferings' {reservedDBInstancesOfferingId} -> reservedDBInstancesOfferingId) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {reservedDBInstancesOfferingId = a} :: DescribeReservedDBInstancesOfferings)

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so you can retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReservedDBInstancesOfferings_maxRecords :: Lens.Lens' DescribeReservedDBInstancesOfferings (Prelude.Maybe Prelude.Int)
describeReservedDBInstancesOfferings_maxRecords = Lens.lens (\DescribeReservedDBInstancesOfferings' {maxRecords} -> maxRecords) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {maxRecords = a} :: DescribeReservedDBInstancesOfferings)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReservedDBInstancesOfferings_marker :: Lens.Lens' DescribeReservedDBInstancesOfferings (Prelude.Maybe Prelude.Text)
describeReservedDBInstancesOfferings_marker = Lens.lens (\DescribeReservedDBInstancesOfferings' {marker} -> marker) (\s@DescribeReservedDBInstancesOfferings' {} a -> s {marker = a} :: DescribeReservedDBInstancesOfferings)

instance
  Core.AWSPager
    DescribeReservedDBInstancesOfferings
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedDBInstancesOfferingsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReservedDBInstancesOfferings_marker
          Lens..~ rs
          Lens.^? describeReservedDBInstancesOfferingsResponse_marker
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..@? "ReservedDBInstancesOfferings"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Core.parseXMLList "ReservedDBInstancesOffering")
                        )
              Prelude.<*> (x Core..@? "Marker")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedDBInstancesOfferings

instance
  Prelude.NFData
    DescribeReservedDBInstancesOfferings

instance
  Core.ToHeaders
    DescribeReservedDBInstancesOfferings
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeReservedDBInstancesOfferings
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeReservedDBInstancesOfferings
  where
  toQuery DescribeReservedDBInstancesOfferings' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeReservedDBInstancesOfferings" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Duration" Core.=: duration,
        "MultiAZ" Core.=: multiAZ,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "OfferingType" Core.=: offeringType,
        "ProductDescription" Core.=: productDescription,
        "ReservedDBInstancesOfferingId"
          Core.=: reservedDBInstancesOfferingId,
        "MaxRecords" Core.=: maxRecords,
        "Marker" Core.=: marker
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeReservedDBInstancesOfferings@ action.
--
-- /See:/ 'newDescribeReservedDBInstancesOfferingsResponse' smart constructor.
data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse'
  { -- | A list of reserved DB instance offerings.
    reservedDBInstancesOfferings :: Prelude.Maybe [ReservedDBInstancesOffering],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeReservedDBInstancesOfferingsResponse
newDescribeReservedDBInstancesOfferingsResponse
  pHttpStatus_ =
    DescribeReservedDBInstancesOfferingsResponse'
      { reservedDBInstancesOfferings =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of reserved DB instance offerings.
describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse (Prelude.Maybe [ReservedDBInstancesOffering])
describeReservedDBInstancesOfferingsResponse_reservedDBInstancesOfferings = Lens.lens (\DescribeReservedDBInstancesOfferingsResponse' {reservedDBInstancesOfferings} -> reservedDBInstancesOfferings) (\s@DescribeReservedDBInstancesOfferingsResponse' {} a -> s {reservedDBInstancesOfferings = a} :: DescribeReservedDBInstancesOfferingsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReservedDBInstancesOfferingsResponse_marker :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse (Prelude.Maybe Prelude.Text)
describeReservedDBInstancesOfferingsResponse_marker = Lens.lens (\DescribeReservedDBInstancesOfferingsResponse' {marker} -> marker) (\s@DescribeReservedDBInstancesOfferingsResponse' {} a -> s {marker = a} :: DescribeReservedDBInstancesOfferingsResponse)

-- | The response's http status code.
describeReservedDBInstancesOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedDBInstancesOfferingsResponse Prelude.Int
describeReservedDBInstancesOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedDBInstancesOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedDBInstancesOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedDBInstancesOfferingsResponse)

instance
  Prelude.NFData
    DescribeReservedDBInstancesOfferingsResponse
