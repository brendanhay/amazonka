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
-- Module      : Amazonka.Redshift.DescribeReservedNodeExchangeStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns exchange status details and associated metadata for a
-- reserved-node exchange. Statuses include such values as in progress and
-- requested.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeReservedNodeExchangeStatus
  ( -- * Creating a Request
    DescribeReservedNodeExchangeStatus (..),
    newDescribeReservedNodeExchangeStatus,

    -- * Request Lenses
    describeReservedNodeExchangeStatus_marker,
    describeReservedNodeExchangeStatus_reservedNodeExchangeRequestId,
    describeReservedNodeExchangeStatus_reservedNodeId,
    describeReservedNodeExchangeStatus_maxRecords,

    -- * Destructuring the Response
    DescribeReservedNodeExchangeStatusResponse (..),
    newDescribeReservedNodeExchangeStatusResponse,

    -- * Response Lenses
    describeReservedNodeExchangeStatusResponse_marker,
    describeReservedNodeExchangeStatusResponse_reservedNodeExchangeStatusDetails,
    describeReservedNodeExchangeStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReservedNodeExchangeStatus' smart constructor.
data DescribeReservedNodeExchangeStatus = DescribeReservedNodeExchangeStatus'
  { -- | An optional pagination token provided by a previous
    -- @DescribeReservedNodeExchangeStatus@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by the @MaxRecords@ parameter. You can retrieve the
    -- next set of response records by providing the returned marker value in
    -- the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the reserved-node exchange request.
    reservedNodeExchangeRequestId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the source reserved node in a reserved-node exchange
    -- request.
    reservedNodeId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @Marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedNodeExchangeStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeReservedNodeExchangeStatus_marker' - An optional pagination token provided by a previous
-- @DescribeReservedNodeExchangeStatus@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by the @MaxRecords@ parameter. You can retrieve the
-- next set of response records by providing the returned marker value in
-- the @Marker@ parameter and retrying the request.
--
-- 'reservedNodeExchangeRequestId', 'describeReservedNodeExchangeStatus_reservedNodeExchangeRequestId' - The identifier of the reserved-node exchange request.
--
-- 'reservedNodeId', 'describeReservedNodeExchangeStatus_reservedNodeId' - The identifier of the source reserved node in a reserved-node exchange
-- request.
--
-- 'maxRecords', 'describeReservedNodeExchangeStatus_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @Marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
newDescribeReservedNodeExchangeStatus ::
  DescribeReservedNodeExchangeStatus
newDescribeReservedNodeExchangeStatus =
  DescribeReservedNodeExchangeStatus'
    { marker =
        Prelude.Nothing,
      reservedNodeExchangeRequestId =
        Prelude.Nothing,
      reservedNodeId = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous
-- @DescribeReservedNodeExchangeStatus@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by the @MaxRecords@ parameter. You can retrieve the
-- next set of response records by providing the returned marker value in
-- the @Marker@ parameter and retrying the request.
describeReservedNodeExchangeStatus_marker :: Lens.Lens' DescribeReservedNodeExchangeStatus (Prelude.Maybe Prelude.Text)
describeReservedNodeExchangeStatus_marker = Lens.lens (\DescribeReservedNodeExchangeStatus' {marker} -> marker) (\s@DescribeReservedNodeExchangeStatus' {} a -> s {marker = a} :: DescribeReservedNodeExchangeStatus)

-- | The identifier of the reserved-node exchange request.
describeReservedNodeExchangeStatus_reservedNodeExchangeRequestId :: Lens.Lens' DescribeReservedNodeExchangeStatus (Prelude.Maybe Prelude.Text)
describeReservedNodeExchangeStatus_reservedNodeExchangeRequestId = Lens.lens (\DescribeReservedNodeExchangeStatus' {reservedNodeExchangeRequestId} -> reservedNodeExchangeRequestId) (\s@DescribeReservedNodeExchangeStatus' {} a -> s {reservedNodeExchangeRequestId = a} :: DescribeReservedNodeExchangeStatus)

-- | The identifier of the source reserved node in a reserved-node exchange
-- request.
describeReservedNodeExchangeStatus_reservedNodeId :: Lens.Lens' DescribeReservedNodeExchangeStatus (Prelude.Maybe Prelude.Text)
describeReservedNodeExchangeStatus_reservedNodeId = Lens.lens (\DescribeReservedNodeExchangeStatus' {reservedNodeId} -> reservedNodeId) (\s@DescribeReservedNodeExchangeStatus' {} a -> s {reservedNodeId = a} :: DescribeReservedNodeExchangeStatus)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @Marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
describeReservedNodeExchangeStatus_maxRecords :: Lens.Lens' DescribeReservedNodeExchangeStatus (Prelude.Maybe Prelude.Int)
describeReservedNodeExchangeStatus_maxRecords = Lens.lens (\DescribeReservedNodeExchangeStatus' {maxRecords} -> maxRecords) (\s@DescribeReservedNodeExchangeStatus' {} a -> s {maxRecords = a} :: DescribeReservedNodeExchangeStatus)

instance
  Core.AWSPager
    DescribeReservedNodeExchangeStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedNodeExchangeStatusResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedNodeExchangeStatusResponse_reservedNodeExchangeStatusDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReservedNodeExchangeStatus_marker
          Lens..~ rs
          Lens.^? describeReservedNodeExchangeStatusResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeReservedNodeExchangeStatus
  where
  type
    AWSResponse DescribeReservedNodeExchangeStatus =
      DescribeReservedNodeExchangeStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeReservedNodeExchangeStatusResult"
      ( \s h x ->
          DescribeReservedNodeExchangeStatusResponse'
            Prelude.<$> (x Data..@? "Marker")
              Prelude.<*> ( x Data..@? "ReservedNodeExchangeStatusDetails"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may
                                (Data.parseXMLList "ReservedNodeExchangeStatus")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedNodeExchangeStatus
  where
  hashWithSalt
    _salt
    DescribeReservedNodeExchangeStatus' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` reservedNodeExchangeRequestId
        `Prelude.hashWithSalt` reservedNodeId
        `Prelude.hashWithSalt` maxRecords

instance
  Prelude.NFData
    DescribeReservedNodeExchangeStatus
  where
  rnf DescribeReservedNodeExchangeStatus' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf reservedNodeExchangeRequestId
      `Prelude.seq` Prelude.rnf reservedNodeId
      `Prelude.seq` Prelude.rnf maxRecords

instance
  Data.ToHeaders
    DescribeReservedNodeExchangeStatus
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeReservedNodeExchangeStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeReservedNodeExchangeStatus
  where
  toQuery DescribeReservedNodeExchangeStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeReservedNodeExchangeStatus" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "ReservedNodeExchangeRequestId"
          Data.=: reservedNodeExchangeRequestId,
        "ReservedNodeId" Data.=: reservedNodeId,
        "MaxRecords" Data.=: maxRecords
      ]

-- | /See:/ 'newDescribeReservedNodeExchangeStatusResponse' smart constructor.
data DescribeReservedNodeExchangeStatusResponse = DescribeReservedNodeExchangeStatusResponse'
  { -- | A pagination token provided by a previous
    -- @DescribeReservedNodeExchangeStatus@ request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The details of the reserved-node exchange request, including the status,
    -- request time, source reserved-node identifier, and additional details.
    reservedNodeExchangeStatusDetails :: Prelude.Maybe [ReservedNodeExchangeStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedNodeExchangeStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeReservedNodeExchangeStatusResponse_marker' - A pagination token provided by a previous
-- @DescribeReservedNodeExchangeStatus@ request.
--
-- 'reservedNodeExchangeStatusDetails', 'describeReservedNodeExchangeStatusResponse_reservedNodeExchangeStatusDetails' - The details of the reserved-node exchange request, including the status,
-- request time, source reserved-node identifier, and additional details.
--
-- 'httpStatus', 'describeReservedNodeExchangeStatusResponse_httpStatus' - The response's http status code.
newDescribeReservedNodeExchangeStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedNodeExchangeStatusResponse
newDescribeReservedNodeExchangeStatusResponse
  pHttpStatus_ =
    DescribeReservedNodeExchangeStatusResponse'
      { marker =
          Prelude.Nothing,
        reservedNodeExchangeStatusDetails =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A pagination token provided by a previous
-- @DescribeReservedNodeExchangeStatus@ request.
describeReservedNodeExchangeStatusResponse_marker :: Lens.Lens' DescribeReservedNodeExchangeStatusResponse (Prelude.Maybe Prelude.Text)
describeReservedNodeExchangeStatusResponse_marker = Lens.lens (\DescribeReservedNodeExchangeStatusResponse' {marker} -> marker) (\s@DescribeReservedNodeExchangeStatusResponse' {} a -> s {marker = a} :: DescribeReservedNodeExchangeStatusResponse)

-- | The details of the reserved-node exchange request, including the status,
-- request time, source reserved-node identifier, and additional details.
describeReservedNodeExchangeStatusResponse_reservedNodeExchangeStatusDetails :: Lens.Lens' DescribeReservedNodeExchangeStatusResponse (Prelude.Maybe [ReservedNodeExchangeStatus])
describeReservedNodeExchangeStatusResponse_reservedNodeExchangeStatusDetails = Lens.lens (\DescribeReservedNodeExchangeStatusResponse' {reservedNodeExchangeStatusDetails} -> reservedNodeExchangeStatusDetails) (\s@DescribeReservedNodeExchangeStatusResponse' {} a -> s {reservedNodeExchangeStatusDetails = a} :: DescribeReservedNodeExchangeStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservedNodeExchangeStatusResponse_httpStatus :: Lens.Lens' DescribeReservedNodeExchangeStatusResponse Prelude.Int
describeReservedNodeExchangeStatusResponse_httpStatus = Lens.lens (\DescribeReservedNodeExchangeStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedNodeExchangeStatusResponse' {} a -> s {httpStatus = a} :: DescribeReservedNodeExchangeStatusResponse)

instance
  Prelude.NFData
    DescribeReservedNodeExchangeStatusResponse
  where
  rnf DescribeReservedNodeExchangeStatusResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf reservedNodeExchangeStatusDetails
      `Prelude.seq` Prelude.rnf httpStatus
