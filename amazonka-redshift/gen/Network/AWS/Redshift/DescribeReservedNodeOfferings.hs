{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeReservedNodeOfferings' smart constructor.
data DescribeReservedNodeOfferings = DescribeReservedNodeOfferings'
  { -- | The unique identifier for the offering.
    reservedNodeOfferingId :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeReservedNodeOfferings
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
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
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The unique identifier for the offering.
describeReservedNodeOfferings_reservedNodeOfferingId :: Lens.Lens' DescribeReservedNodeOfferings (Prelude.Maybe Prelude.Text)
describeReservedNodeOfferings_reservedNodeOfferingId = Lens.lens (\DescribeReservedNodeOfferings' {reservedNodeOfferingId} -> reservedNodeOfferingId) (\s@DescribeReservedNodeOfferings' {} a -> s {reservedNodeOfferingId = a} :: DescribeReservedNodeOfferings)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeReservedNodeOfferings
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
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

instance Pager.AWSPager DescribeReservedNodeOfferings where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeReservedNodeOfferingsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeReservedNodeOfferingsResponse_reservedNodeOfferings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeReservedNodeOfferings_marker
          Lens..~ rs
          Lens.^? describeReservedNodeOfferingsResponse_marker
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeReservedNodeOfferings
  where
  type
    Rs DescribeReservedNodeOfferings =
      DescribeReservedNodeOfferingsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeReservedNodeOfferingsResult"
      ( \s h x ->
          DescribeReservedNodeOfferingsResponse'
            Prelude.<$> ( x Prelude..@? "ReservedNodeOfferings"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may
                              (Prelude.parseXMLList "ReservedNodeOffering")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedNodeOfferings

instance Prelude.NFData DescribeReservedNodeOfferings

instance
  Prelude.ToHeaders
    DescribeReservedNodeOfferings
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeReservedNodeOfferings where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeReservedNodeOfferings
  where
  toQuery DescribeReservedNodeOfferings' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeReservedNodeOfferings" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "ReservedNodeOfferingId"
          Prelude.=: reservedNodeOfferingId,
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeReservedNodeOfferingsResponse' smart constructor.
data DescribeReservedNodeOfferingsResponse = DescribeReservedNodeOfferingsResponse'
  { -- | A list of @ReservedNodeOffering@ objects.
    reservedNodeOfferings :: Prelude.Maybe [ReservedNodeOffering],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeReservedNodeOfferingsResponse
newDescribeReservedNodeOfferingsResponse pHttpStatus_ =
  DescribeReservedNodeOfferingsResponse'
    { reservedNodeOfferings =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @ReservedNodeOffering@ objects.
describeReservedNodeOfferingsResponse_reservedNodeOfferings :: Lens.Lens' DescribeReservedNodeOfferingsResponse (Prelude.Maybe [ReservedNodeOffering])
describeReservedNodeOfferingsResponse_reservedNodeOfferings = Lens.lens (\DescribeReservedNodeOfferingsResponse' {reservedNodeOfferings} -> reservedNodeOfferings) (\s@DescribeReservedNodeOfferingsResponse' {} a -> s {reservedNodeOfferings = a} :: DescribeReservedNodeOfferingsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeReservedNodeOfferingsResponse_marker :: Lens.Lens' DescribeReservedNodeOfferingsResponse (Prelude.Maybe Prelude.Text)
describeReservedNodeOfferingsResponse_marker = Lens.lens (\DescribeReservedNodeOfferingsResponse' {marker} -> marker) (\s@DescribeReservedNodeOfferingsResponse' {} a -> s {marker = a} :: DescribeReservedNodeOfferingsResponse)

-- | The response's http status code.
describeReservedNodeOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedNodeOfferingsResponse Prelude.Int
describeReservedNodeOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedNodeOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedNodeOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedNodeOfferingsResponse)

instance
  Prelude.NFData
    DescribeReservedNodeOfferingsResponse
