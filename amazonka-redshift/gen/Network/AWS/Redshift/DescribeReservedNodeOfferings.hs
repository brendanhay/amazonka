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
-- Module      : Network.AWS.Redshift.DescribeReservedNodeOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Redshift.DescribeReservedNodeOfferings
  ( -- * Creating a Request
    DescribeReservedNodeOfferings (..),
    newDescribeReservedNodeOfferings,

    -- * Request Lenses
    describeReservedNodeOfferings_reservedNodeOfferingId,
    describeReservedNodeOfferings_marker,
    describeReservedNodeOfferings_maxRecords,

    -- * Destructuring the Response
    DescribeReservedNodeOfferingsResponse (..),
    newDescribeReservedNodeOfferingsResponse,

    -- * Response Lenses
    describeReservedNodeOfferingsResponse_reservedNodeOfferings,
    describeReservedNodeOfferingsResponse_marker,
    describeReservedNodeOfferingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeReservedNodeOfferings' smart constructor.
data DescribeReservedNodeOfferings = DescribeReservedNodeOfferings'
  { -- | The unique identifier for the offering.
    reservedNodeOfferingId :: Core.Maybe Core.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeReservedNodeOfferings
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedNodeOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNodeOfferingId', 'describeReservedNodeOfferings_reservedNodeOfferingId' - The unique identifier for the offering.
--
-- 'marker', 'describeReservedNodeOfferings_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodeOfferings
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
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
newDescribeReservedNodeOfferings ::
  DescribeReservedNodeOfferings
newDescribeReservedNodeOfferings =
  DescribeReservedNodeOfferings'
    { reservedNodeOfferingId =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The unique identifier for the offering.
describeReservedNodeOfferings_reservedNodeOfferingId :: Lens.Lens' DescribeReservedNodeOfferings (Core.Maybe Core.Text)
describeReservedNodeOfferings_reservedNodeOfferingId = Lens.lens (\DescribeReservedNodeOfferings' {reservedNodeOfferingId} -> reservedNodeOfferingId) (\s@DescribeReservedNodeOfferings' {} a -> s {reservedNodeOfferingId = a} :: DescribeReservedNodeOfferings)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodeOfferings
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeReservedNodeOfferings_marker :: Lens.Lens' DescribeReservedNodeOfferings (Core.Maybe Core.Text)
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
describeReservedNodeOfferings_maxRecords :: Lens.Lens' DescribeReservedNodeOfferings (Core.Maybe Core.Int)
describeReservedNodeOfferings_maxRecords = Lens.lens (\DescribeReservedNodeOfferings' {maxRecords} -> maxRecords) (\s@DescribeReservedNodeOfferings' {} a -> s {maxRecords = a} :: DescribeReservedNodeOfferings)

instance Core.AWSPager DescribeReservedNodeOfferings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedNodeOfferingsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedNodeOfferingsResponse_reservedNodeOfferings
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeReservedNodeOfferings_marker
          Lens..~ rs
          Lens.^? describeReservedNodeOfferingsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeReservedNodeOfferings
  where
  type
    AWSResponse DescribeReservedNodeOfferings =
      DescribeReservedNodeOfferingsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeReservedNodeOfferingsResult"
      ( \s h x ->
          DescribeReservedNodeOfferingsResponse'
            Core.<$> ( x Core..@? "ReservedNodeOfferings"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "ReservedNodeOffering")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeReservedNodeOfferings

instance Core.NFData DescribeReservedNodeOfferings

instance Core.ToHeaders DescribeReservedNodeOfferings where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeReservedNodeOfferings where
  toPath = Core.const "/"

instance Core.ToQuery DescribeReservedNodeOfferings where
  toQuery DescribeReservedNodeOfferings' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeReservedNodeOfferings" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ReservedNodeOfferingId"
          Core.=: reservedNodeOfferingId,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeReservedNodeOfferingsResponse' smart constructor.
data DescribeReservedNodeOfferingsResponse = DescribeReservedNodeOfferingsResponse'
  { -- | A list of @ReservedNodeOffering@ objects.
    reservedNodeOfferings :: Core.Maybe [ReservedNodeOffering],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedNodeOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNodeOfferings', 'describeReservedNodeOfferingsResponse_reservedNodeOfferings' - A list of @ReservedNodeOffering@ objects.
--
-- 'marker', 'describeReservedNodeOfferingsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeReservedNodeOfferingsResponse_httpStatus' - The response's http status code.
newDescribeReservedNodeOfferingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReservedNodeOfferingsResponse
newDescribeReservedNodeOfferingsResponse pHttpStatus_ =
  DescribeReservedNodeOfferingsResponse'
    { reservedNodeOfferings =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @ReservedNodeOffering@ objects.
describeReservedNodeOfferingsResponse_reservedNodeOfferings :: Lens.Lens' DescribeReservedNodeOfferingsResponse (Core.Maybe [ReservedNodeOffering])
describeReservedNodeOfferingsResponse_reservedNodeOfferings = Lens.lens (\DescribeReservedNodeOfferingsResponse' {reservedNodeOfferings} -> reservedNodeOfferings) (\s@DescribeReservedNodeOfferingsResponse' {} a -> s {reservedNodeOfferings = a} :: DescribeReservedNodeOfferingsResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeReservedNodeOfferingsResponse_marker :: Lens.Lens' DescribeReservedNodeOfferingsResponse (Core.Maybe Core.Text)
describeReservedNodeOfferingsResponse_marker = Lens.lens (\DescribeReservedNodeOfferingsResponse' {marker} -> marker) (\s@DescribeReservedNodeOfferingsResponse' {} a -> s {marker = a} :: DescribeReservedNodeOfferingsResponse)

-- | The response's http status code.
describeReservedNodeOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedNodeOfferingsResponse Core.Int
describeReservedNodeOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedNodeOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedNodeOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedNodeOfferingsResponse)

instance
  Core.NFData
    DescribeReservedNodeOfferingsResponse
