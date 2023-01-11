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
-- Module      : Amazonka.Redshift.DescribeReservedNodeOfferings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available reserved node offerings by Amazon
-- Redshift with their descriptions including the node type, the fixed and
-- recurring costs of reserving the node and duration the node will be
-- reserved for you. These descriptions help you determine which reserve
-- node offering you want to purchase. You then use the unique offering ID
-- in you call to PurchaseReservedNodeOffering to reserve one or more nodes
-- for your Amazon Redshift cluster.
--
-- For more information about reserved node offerings, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html Purchasing Reserved Nodes>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeReservedNodeOfferings
  ( -- * Creating a Request
    DescribeReservedNodeOfferings (..),
    newDescribeReservedNodeOfferings,

    -- * Request Lenses
    describeReservedNodeOfferings_marker,
    describeReservedNodeOfferings_maxRecords,
    describeReservedNodeOfferings_reservedNodeOfferingId,

    -- * Destructuring the Response
    DescribeReservedNodeOfferingsResponse (..),
    newDescribeReservedNodeOfferingsResponse,

    -- * Response Lenses
    describeReservedNodeOfferingsResponse_marker,
    describeReservedNodeOfferingsResponse_reservedNodeOfferings,
    describeReservedNodeOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeReservedNodeOfferings' smart constructor.
data DescribeReservedNodeOfferings = DescribeReservedNodeOfferings'
  { -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeReservedNodeOfferings
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the offering.
    reservedNodeOfferingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedNodeOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeReservedNodeOfferings_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodeOfferings
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'maxRecords', 'describeReservedNodeOfferings_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
--
-- 'reservedNodeOfferingId', 'describeReservedNodeOfferings_reservedNodeOfferingId' - The unique identifier for the offering.
newDescribeReservedNodeOfferings ::
  DescribeReservedNodeOfferings
newDescribeReservedNodeOfferings =
  DescribeReservedNodeOfferings'
    { marker =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      reservedNodeOfferingId = Prelude.Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodeOfferings
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeReservedNodeOfferings_marker :: Lens.Lens' DescribeReservedNodeOfferings (Prelude.Maybe Prelude.Text)
describeReservedNodeOfferings_marker = Lens.lens (\DescribeReservedNodeOfferings' {marker} -> marker) (\s@DescribeReservedNodeOfferings' {} a -> s {marker = a} :: DescribeReservedNodeOfferings)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeReservedNodeOfferings_maxRecords :: Lens.Lens' DescribeReservedNodeOfferings (Prelude.Maybe Prelude.Int)
describeReservedNodeOfferings_maxRecords = Lens.lens (\DescribeReservedNodeOfferings' {maxRecords} -> maxRecords) (\s@DescribeReservedNodeOfferings' {} a -> s {maxRecords = a} :: DescribeReservedNodeOfferings)

-- | The unique identifier for the offering.
describeReservedNodeOfferings_reservedNodeOfferingId :: Lens.Lens' DescribeReservedNodeOfferings (Prelude.Maybe Prelude.Text)
describeReservedNodeOfferings_reservedNodeOfferingId = Lens.lens (\DescribeReservedNodeOfferings' {reservedNodeOfferingId} -> reservedNodeOfferingId) (\s@DescribeReservedNodeOfferings' {} a -> s {reservedNodeOfferingId = a} :: DescribeReservedNodeOfferings)

instance Core.AWSPager DescribeReservedNodeOfferings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedNodeOfferingsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedNodeOfferingsResponse_reservedNodeOfferings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReservedNodeOfferings_marker
          Lens..~ rs
          Lens.^? describeReservedNodeOfferingsResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeReservedNodeOfferings
  where
  type
    AWSResponse DescribeReservedNodeOfferings =
      DescribeReservedNodeOfferingsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeReservedNodeOfferingsResult"
      ( \s h x ->
          DescribeReservedNodeOfferingsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "ReservedNodeOfferings"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "ReservedNodeOffering")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedNodeOfferings
  where
  hashWithSalt _salt DescribeReservedNodeOfferings' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` reservedNodeOfferingId

instance Prelude.NFData DescribeReservedNodeOfferings where
  rnf DescribeReservedNodeOfferings' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf reservedNodeOfferingId

instance Data.ToHeaders DescribeReservedNodeOfferings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeReservedNodeOfferings where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReservedNodeOfferings where
  toQuery DescribeReservedNodeOfferings' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeReservedNodeOfferings" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "ReservedNodeOfferingId"
          Data.=: reservedNodeOfferingId
      ]

-- |
--
-- /See:/ 'newDescribeReservedNodeOfferingsResponse' smart constructor.
data DescribeReservedNodeOfferingsResponse = DescribeReservedNodeOfferingsResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of @ReservedNodeOffering@ objects.
    reservedNodeOfferings :: Prelude.Maybe [ReservedNodeOffering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedNodeOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeReservedNodeOfferingsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'reservedNodeOfferings', 'describeReservedNodeOfferingsResponse_reservedNodeOfferings' - A list of @ReservedNodeOffering@ objects.
--
-- 'httpStatus', 'describeReservedNodeOfferingsResponse_httpStatus' - The response's http status code.
newDescribeReservedNodeOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedNodeOfferingsResponse
newDescribeReservedNodeOfferingsResponse pHttpStatus_ =
  DescribeReservedNodeOfferingsResponse'
    { marker =
        Prelude.Nothing,
      reservedNodeOfferings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeReservedNodeOfferingsResponse_marker :: Lens.Lens' DescribeReservedNodeOfferingsResponse (Prelude.Maybe Prelude.Text)
describeReservedNodeOfferingsResponse_marker = Lens.lens (\DescribeReservedNodeOfferingsResponse' {marker} -> marker) (\s@DescribeReservedNodeOfferingsResponse' {} a -> s {marker = a} :: DescribeReservedNodeOfferingsResponse)

-- | A list of @ReservedNodeOffering@ objects.
describeReservedNodeOfferingsResponse_reservedNodeOfferings :: Lens.Lens' DescribeReservedNodeOfferingsResponse (Prelude.Maybe [ReservedNodeOffering])
describeReservedNodeOfferingsResponse_reservedNodeOfferings = Lens.lens (\DescribeReservedNodeOfferingsResponse' {reservedNodeOfferings} -> reservedNodeOfferings) (\s@DescribeReservedNodeOfferingsResponse' {} a -> s {reservedNodeOfferings = a} :: DescribeReservedNodeOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservedNodeOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedNodeOfferingsResponse Prelude.Int
describeReservedNodeOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedNodeOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedNodeOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedNodeOfferingsResponse)

instance
  Prelude.NFData
    DescribeReservedNodeOfferingsResponse
  where
  rnf DescribeReservedNodeOfferingsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf reservedNodeOfferings
      `Prelude.seq` Prelude.rnf httpStatus
