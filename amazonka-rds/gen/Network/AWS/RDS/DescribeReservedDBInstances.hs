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
-- Module      : Network.AWS.RDS.DescribeReservedDBInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved DB instances for this account, or
-- about a specified reserved DB instance.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeReservedDBInstances
  ( -- * Creating a Request
    DescribeReservedDBInstances (..),
    newDescribeReservedDBInstances,

    -- * Request Lenses
    describeReservedDBInstances_duration,
    describeReservedDBInstances_multiAZ,
    describeReservedDBInstances_dbInstanceClass,
    describeReservedDBInstances_reservedDBInstanceId,
    describeReservedDBInstances_filters,
    describeReservedDBInstances_leaseId,
    describeReservedDBInstances_offeringType,
    describeReservedDBInstances_productDescription,
    describeReservedDBInstances_reservedDBInstancesOfferingId,
    describeReservedDBInstances_marker,
    describeReservedDBInstances_maxRecords,

    -- * Destructuring the Response
    DescribeReservedDBInstancesResponse (..),
    newDescribeReservedDBInstancesResponse,

    -- * Response Lenses
    describeReservedDBInstancesResponse_reservedDBInstances,
    describeReservedDBInstancesResponse_marker,
    describeReservedDBInstancesResponse_httpStatus,
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
-- /See:/ 'newDescribeReservedDBInstances' smart constructor.
data DescribeReservedDBInstances = DescribeReservedDBInstances'
  { -- | The duration filter value, specified in years or seconds. Specify this
    -- parameter to show only reservations for this duration.
    --
    -- Valid Values: @1 | 3 | 31536000 | 94608000@
    duration :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to show only those reservations that
    -- support Multi-AZ.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance class filter value. Specify this parameter to show only
    -- those reservations matching the specified DB instances class.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The reserved DB instance identifier filter value. Specify this parameter
    -- to show only the reservation that matches the specified reservation ID.
    reservedDBInstanceId :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The lease identifier filter value. Specify this parameter to show only
    -- the reservation that matches the specified lease ID.
    --
    -- AWS Support might request the lease ID for an issue related to a
    -- reserved DB instance.
    leaseId :: Prelude.Maybe Prelude.Text,
    -- | The offering type filter value. Specify this parameter to show only the
    -- available offerings matching the specified offering type.
    --
    -- Valid Values: @\"Partial Upfront\" | \"All Upfront\" | \"No Upfront\" @
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The product description filter value. Specify this parameter to show
    -- only those reservations matching the specified product description.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | The offering identifier filter value. Specify this parameter to show
    -- only purchased reservations matching the specified offering identifier.
    reservedDBInstancesOfferingId :: Prelude.Maybe Prelude.Text,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more than
    -- the @MaxRecords@ value is available, a pagination token called a marker
    -- is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedDBInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'describeReservedDBInstances_duration' - The duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- 'multiAZ', 'describeReservedDBInstances_multiAZ' - A value that indicates whether to show only those reservations that
-- support Multi-AZ.
--
-- 'dbInstanceClass', 'describeReservedDBInstances_dbInstanceClass' - The DB instance class filter value. Specify this parameter to show only
-- those reservations matching the specified DB instances class.
--
-- 'reservedDBInstanceId', 'describeReservedDBInstances_reservedDBInstanceId' - The reserved DB instance identifier filter value. Specify this parameter
-- to show only the reservation that matches the specified reservation ID.
--
-- 'filters', 'describeReservedDBInstances_filters' - This parameter isn\'t currently supported.
--
-- 'leaseId', 'describeReservedDBInstances_leaseId' - The lease identifier filter value. Specify this parameter to show only
-- the reservation that matches the specified lease ID.
--
-- AWS Support might request the lease ID for an issue related to a
-- reserved DB instance.
--
-- 'offeringType', 'describeReservedDBInstances_offeringType' - The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values: @\"Partial Upfront\" | \"All Upfront\" | \"No Upfront\" @
--
-- 'productDescription', 'describeReservedDBInstances_productDescription' - The product description filter value. Specify this parameter to show
-- only those reservations matching the specified product description.
--
-- 'reservedDBInstancesOfferingId', 'describeReservedDBInstances_reservedDBInstancesOfferingId' - The offering identifier filter value. Specify this parameter to show
-- only purchased reservations matching the specified offering identifier.
--
-- 'marker', 'describeReservedDBInstances_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReservedDBInstances_maxRecords' - The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so you can retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeReservedDBInstances ::
  DescribeReservedDBInstances
newDescribeReservedDBInstances =
  DescribeReservedDBInstances'
    { duration =
        Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      reservedDBInstanceId = Prelude.Nothing,
      filters = Prelude.Nothing,
      leaseId = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      reservedDBInstancesOfferingId =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The duration filter value, specified in years or seconds. Specify this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
describeReservedDBInstances_duration :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Text)
describeReservedDBInstances_duration = Lens.lens (\DescribeReservedDBInstances' {duration} -> duration) (\s@DescribeReservedDBInstances' {} a -> s {duration = a} :: DescribeReservedDBInstances)

-- | A value that indicates whether to show only those reservations that
-- support Multi-AZ.
describeReservedDBInstances_multiAZ :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Bool)
describeReservedDBInstances_multiAZ = Lens.lens (\DescribeReservedDBInstances' {multiAZ} -> multiAZ) (\s@DescribeReservedDBInstances' {} a -> s {multiAZ = a} :: DescribeReservedDBInstances)

-- | The DB instance class filter value. Specify this parameter to show only
-- those reservations matching the specified DB instances class.
describeReservedDBInstances_dbInstanceClass :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Text)
describeReservedDBInstances_dbInstanceClass = Lens.lens (\DescribeReservedDBInstances' {dbInstanceClass} -> dbInstanceClass) (\s@DescribeReservedDBInstances' {} a -> s {dbInstanceClass = a} :: DescribeReservedDBInstances)

-- | The reserved DB instance identifier filter value. Specify this parameter
-- to show only the reservation that matches the specified reservation ID.
describeReservedDBInstances_reservedDBInstanceId :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Text)
describeReservedDBInstances_reservedDBInstanceId = Lens.lens (\DescribeReservedDBInstances' {reservedDBInstanceId} -> reservedDBInstanceId) (\s@DescribeReservedDBInstances' {} a -> s {reservedDBInstanceId = a} :: DescribeReservedDBInstances)

-- | This parameter isn\'t currently supported.
describeReservedDBInstances_filters :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe [Filter])
describeReservedDBInstances_filters = Lens.lens (\DescribeReservedDBInstances' {filters} -> filters) (\s@DescribeReservedDBInstances' {} a -> s {filters = a} :: DescribeReservedDBInstances) Prelude.. Lens.mapping Lens._Coerce

-- | The lease identifier filter value. Specify this parameter to show only
-- the reservation that matches the specified lease ID.
--
-- AWS Support might request the lease ID for an issue related to a
-- reserved DB instance.
describeReservedDBInstances_leaseId :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Text)
describeReservedDBInstances_leaseId = Lens.lens (\DescribeReservedDBInstances' {leaseId} -> leaseId) (\s@DescribeReservedDBInstances' {} a -> s {leaseId = a} :: DescribeReservedDBInstances)

-- | The offering type filter value. Specify this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid Values: @\"Partial Upfront\" | \"All Upfront\" | \"No Upfront\" @
describeReservedDBInstances_offeringType :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Text)
describeReservedDBInstances_offeringType = Lens.lens (\DescribeReservedDBInstances' {offeringType} -> offeringType) (\s@DescribeReservedDBInstances' {} a -> s {offeringType = a} :: DescribeReservedDBInstances)

-- | The product description filter value. Specify this parameter to show
-- only those reservations matching the specified product description.
describeReservedDBInstances_productDescription :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Text)
describeReservedDBInstances_productDescription = Lens.lens (\DescribeReservedDBInstances' {productDescription} -> productDescription) (\s@DescribeReservedDBInstances' {} a -> s {productDescription = a} :: DescribeReservedDBInstances)

-- | The offering identifier filter value. Specify this parameter to show
-- only purchased reservations matching the specified offering identifier.
describeReservedDBInstances_reservedDBInstancesOfferingId :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Text)
describeReservedDBInstances_reservedDBInstancesOfferingId = Lens.lens (\DescribeReservedDBInstances' {reservedDBInstancesOfferingId} -> reservedDBInstancesOfferingId) (\s@DescribeReservedDBInstances' {} a -> s {reservedDBInstancesOfferingId = a} :: DescribeReservedDBInstances)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReservedDBInstances_marker :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Text)
describeReservedDBInstances_marker = Lens.lens (\DescribeReservedDBInstances' {marker} -> marker) (\s@DescribeReservedDBInstances' {} a -> s {marker = a} :: DescribeReservedDBInstances)

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so you can retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReservedDBInstances_maxRecords :: Lens.Lens' DescribeReservedDBInstances (Prelude.Maybe Prelude.Int)
describeReservedDBInstances_maxRecords = Lens.lens (\DescribeReservedDBInstances' {maxRecords} -> maxRecords) (\s@DescribeReservedDBInstances' {} a -> s {maxRecords = a} :: DescribeReservedDBInstances)

instance Core.AWSPager DescribeReservedDBInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedDBInstancesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedDBInstancesResponse_reservedDBInstances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReservedDBInstances_marker
          Lens..~ rs
          Lens.^? describeReservedDBInstancesResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeReservedDBInstances where
  type
    AWSResponse DescribeReservedDBInstances =
      DescribeReservedDBInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeReservedDBInstancesResult"
      ( \s h x ->
          DescribeReservedDBInstancesResponse'
            Prelude.<$> ( x Core..@? "ReservedDBInstances"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "ReservedDBInstance")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReservedDBInstances

instance Prelude.NFData DescribeReservedDBInstances

instance Core.ToHeaders DescribeReservedDBInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeReservedDBInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeReservedDBInstances where
  toQuery DescribeReservedDBInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeReservedDBInstances" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Duration" Core.=: duration,
        "MultiAZ" Core.=: multiAZ,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "ReservedDBInstanceId" Core.=: reservedDBInstanceId,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "LeaseId" Core.=: leaseId,
        "OfferingType" Core.=: offeringType,
        "ProductDescription" Core.=: productDescription,
        "ReservedDBInstancesOfferingId"
          Core.=: reservedDBInstancesOfferingId,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeReservedDBInstances@ action.
--
-- /See:/ 'newDescribeReservedDBInstancesResponse' smart constructor.
data DescribeReservedDBInstancesResponse = DescribeReservedDBInstancesResponse'
  { -- | A list of reserved DB instances.
    reservedDBInstances :: Prelude.Maybe [ReservedDBInstance],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedDBInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedDBInstances', 'describeReservedDBInstancesResponse_reservedDBInstances' - A list of reserved DB instances.
--
-- 'marker', 'describeReservedDBInstancesResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeReservedDBInstancesResponse_httpStatus' - The response's http status code.
newDescribeReservedDBInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedDBInstancesResponse
newDescribeReservedDBInstancesResponse pHttpStatus_ =
  DescribeReservedDBInstancesResponse'
    { reservedDBInstances =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of reserved DB instances.
describeReservedDBInstancesResponse_reservedDBInstances :: Lens.Lens' DescribeReservedDBInstancesResponse (Prelude.Maybe [ReservedDBInstance])
describeReservedDBInstancesResponse_reservedDBInstances = Lens.lens (\DescribeReservedDBInstancesResponse' {reservedDBInstances} -> reservedDBInstances) (\s@DescribeReservedDBInstancesResponse' {} a -> s {reservedDBInstances = a} :: DescribeReservedDBInstancesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReservedDBInstancesResponse_marker :: Lens.Lens' DescribeReservedDBInstancesResponse (Prelude.Maybe Prelude.Text)
describeReservedDBInstancesResponse_marker = Lens.lens (\DescribeReservedDBInstancesResponse' {marker} -> marker) (\s@DescribeReservedDBInstancesResponse' {} a -> s {marker = a} :: DescribeReservedDBInstancesResponse)

-- | The response's http status code.
describeReservedDBInstancesResponse_httpStatus :: Lens.Lens' DescribeReservedDBInstancesResponse Prelude.Int
describeReservedDBInstancesResponse_httpStatus = Lens.lens (\DescribeReservedDBInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedDBInstancesResponse' {} a -> s {httpStatus = a} :: DescribeReservedDBInstancesResponse)

instance
  Prelude.NFData
    DescribeReservedDBInstancesResponse
