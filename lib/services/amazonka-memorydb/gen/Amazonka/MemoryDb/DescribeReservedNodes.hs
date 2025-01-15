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
-- Module      : Amazonka.MemoryDb.DescribeReservedNodes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved nodes for this account, or about a
-- specified reserved node.
--
-- This operation returns paginated results.
module Amazonka.MemoryDb.DescribeReservedNodes
  ( -- * Creating a Request
    DescribeReservedNodes (..),
    newDescribeReservedNodes,

    -- * Request Lenses
    describeReservedNodes_duration,
    describeReservedNodes_maxResults,
    describeReservedNodes_nextToken,
    describeReservedNodes_nodeType,
    describeReservedNodes_offeringType,
    describeReservedNodes_reservationId,
    describeReservedNodes_reservedNodesOfferingId,

    -- * Destructuring the Response
    DescribeReservedNodesResponse (..),
    newDescribeReservedNodesResponse,

    -- * Response Lenses
    describeReservedNodesResponse_nextToken,
    describeReservedNodesResponse_reservedNodes,
    describeReservedNodesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReservedNodes' smart constructor.
data DescribeReservedNodes = DescribeReservedNodes'
  { -- | The duration filter value, specified in years or seconds. Use this
    -- parameter to show only reservations for this duration.
    duration :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxRecords value, a marker is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The node type filter value. Use this parameter to show only those
    -- reservations matching the specified node type. For more information, see
    -- <https://docs.aws.amazon.com/memorydb/latest/devguide/nodes.reserved.html#reserved-nodes-supported Supported node types>.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The offering type filter value. Use this parameter to show only the
    -- available offerings matching the specified offering type. Valid values:
    -- \"All Upfront\"|\"Partial Upfront\"| \"No Upfront\"
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The reserved node identifier filter value. Use this parameter to show
    -- only the reservation that matches the specified reservation ID.
    reservationId :: Prelude.Maybe Prelude.Text,
    -- | The offering identifier filter value. Use this parameter to show only
    -- purchased reservations matching the specified offering identifier.
    reservedNodesOfferingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'describeReservedNodes_duration' - The duration filter value, specified in years or seconds. Use this
-- parameter to show only reservations for this duration.
--
-- 'maxResults', 'describeReservedNodes_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'describeReservedNodes_nextToken' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
--
-- 'nodeType', 'describeReservedNodes_nodeType' - The node type filter value. Use this parameter to show only those
-- reservations matching the specified node type. For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/nodes.reserved.html#reserved-nodes-supported Supported node types>.
--
-- 'offeringType', 'describeReservedNodes_offeringType' - The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid values:
-- \"All Upfront\"|\"Partial Upfront\"| \"No Upfront\"
--
-- 'reservationId', 'describeReservedNodes_reservationId' - The reserved node identifier filter value. Use this parameter to show
-- only the reservation that matches the specified reservation ID.
--
-- 'reservedNodesOfferingId', 'describeReservedNodes_reservedNodesOfferingId' - The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
newDescribeReservedNodes ::
  DescribeReservedNodes
newDescribeReservedNodes =
  DescribeReservedNodes'
    { duration = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      reservationId = Prelude.Nothing,
      reservedNodesOfferingId = Prelude.Nothing
    }

-- | The duration filter value, specified in years or seconds. Use this
-- parameter to show only reservations for this duration.
describeReservedNodes_duration :: Lens.Lens' DescribeReservedNodes (Prelude.Maybe Prelude.Text)
describeReservedNodes_duration = Lens.lens (\DescribeReservedNodes' {duration} -> duration) (\s@DescribeReservedNodes' {} a -> s {duration = a} :: DescribeReservedNodes)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
describeReservedNodes_maxResults :: Lens.Lens' DescribeReservedNodes (Prelude.Maybe Prelude.Int)
describeReservedNodes_maxResults = Lens.lens (\DescribeReservedNodes' {maxResults} -> maxResults) (\s@DescribeReservedNodes' {} a -> s {maxResults = a} :: DescribeReservedNodes)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
describeReservedNodes_nextToken :: Lens.Lens' DescribeReservedNodes (Prelude.Maybe Prelude.Text)
describeReservedNodes_nextToken = Lens.lens (\DescribeReservedNodes' {nextToken} -> nextToken) (\s@DescribeReservedNodes' {} a -> s {nextToken = a} :: DescribeReservedNodes)

-- | The node type filter value. Use this parameter to show only those
-- reservations matching the specified node type. For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/nodes.reserved.html#reserved-nodes-supported Supported node types>.
describeReservedNodes_nodeType :: Lens.Lens' DescribeReservedNodes (Prelude.Maybe Prelude.Text)
describeReservedNodes_nodeType = Lens.lens (\DescribeReservedNodes' {nodeType} -> nodeType) (\s@DescribeReservedNodes' {} a -> s {nodeType = a} :: DescribeReservedNodes)

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid values:
-- \"All Upfront\"|\"Partial Upfront\"| \"No Upfront\"
describeReservedNodes_offeringType :: Lens.Lens' DescribeReservedNodes (Prelude.Maybe Prelude.Text)
describeReservedNodes_offeringType = Lens.lens (\DescribeReservedNodes' {offeringType} -> offeringType) (\s@DescribeReservedNodes' {} a -> s {offeringType = a} :: DescribeReservedNodes)

-- | The reserved node identifier filter value. Use this parameter to show
-- only the reservation that matches the specified reservation ID.
describeReservedNodes_reservationId :: Lens.Lens' DescribeReservedNodes (Prelude.Maybe Prelude.Text)
describeReservedNodes_reservationId = Lens.lens (\DescribeReservedNodes' {reservationId} -> reservationId) (\s@DescribeReservedNodes' {} a -> s {reservationId = a} :: DescribeReservedNodes)

-- | The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
describeReservedNodes_reservedNodesOfferingId :: Lens.Lens' DescribeReservedNodes (Prelude.Maybe Prelude.Text)
describeReservedNodes_reservedNodesOfferingId = Lens.lens (\DescribeReservedNodes' {reservedNodesOfferingId} -> reservedNodesOfferingId) (\s@DescribeReservedNodes' {} a -> s {reservedNodesOfferingId = a} :: DescribeReservedNodes)

instance Core.AWSPager DescribeReservedNodes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedNodesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedNodesResponse_reservedNodes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeReservedNodes_nextToken
              Lens..~ rs
              Lens.^? describeReservedNodesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeReservedNodes where
  type
    AWSResponse DescribeReservedNodes =
      DescribeReservedNodesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservedNodesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ReservedNodes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReservedNodes where
  hashWithSalt _salt DescribeReservedNodes' {..} =
    _salt
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` reservationId
      `Prelude.hashWithSalt` reservedNodesOfferingId

instance Prelude.NFData DescribeReservedNodes where
  rnf DescribeReservedNodes' {..} =
    Prelude.rnf duration `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf nodeType `Prelude.seq`
            Prelude.rnf offeringType `Prelude.seq`
              Prelude.rnf reservationId `Prelude.seq`
                Prelude.rnf reservedNodesOfferingId

instance Data.ToHeaders DescribeReservedNodes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.DescribeReservedNodes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeReservedNodes where
  toJSON DescribeReservedNodes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Duration" Data..=) Prelude.<$> duration,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("NodeType" Data..=) Prelude.<$> nodeType,
            ("OfferingType" Data..=) Prelude.<$> offeringType,
            ("ReservationId" Data..=) Prelude.<$> reservationId,
            ("ReservedNodesOfferingId" Data..=)
              Prelude.<$> reservedNodesOfferingId
          ]
      )

instance Data.ToPath DescribeReservedNodes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReservedNodes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReservedNodesResponse' smart constructor.
data DescribeReservedNodesResponse = DescribeReservedNodesResponse'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns information about reserved nodes for this account, or about a
    -- specified reserved node.
    reservedNodes :: Prelude.Maybe [ReservedNode],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedNodesResponse_nextToken' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
--
-- 'reservedNodes', 'describeReservedNodesResponse_reservedNodes' - Returns information about reserved nodes for this account, or about a
-- specified reserved node.
--
-- 'httpStatus', 'describeReservedNodesResponse_httpStatus' - The response's http status code.
newDescribeReservedNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedNodesResponse
newDescribeReservedNodesResponse pHttpStatus_ =
  DescribeReservedNodesResponse'
    { nextToken =
        Prelude.Nothing,
      reservedNodes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
describeReservedNodesResponse_nextToken :: Lens.Lens' DescribeReservedNodesResponse (Prelude.Maybe Prelude.Text)
describeReservedNodesResponse_nextToken = Lens.lens (\DescribeReservedNodesResponse' {nextToken} -> nextToken) (\s@DescribeReservedNodesResponse' {} a -> s {nextToken = a} :: DescribeReservedNodesResponse)

-- | Returns information about reserved nodes for this account, or about a
-- specified reserved node.
describeReservedNodesResponse_reservedNodes :: Lens.Lens' DescribeReservedNodesResponse (Prelude.Maybe [ReservedNode])
describeReservedNodesResponse_reservedNodes = Lens.lens (\DescribeReservedNodesResponse' {reservedNodes} -> reservedNodes) (\s@DescribeReservedNodesResponse' {} a -> s {reservedNodes = a} :: DescribeReservedNodesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservedNodesResponse_httpStatus :: Lens.Lens' DescribeReservedNodesResponse Prelude.Int
describeReservedNodesResponse_httpStatus = Lens.lens (\DescribeReservedNodesResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedNodesResponse' {} a -> s {httpStatus = a} :: DescribeReservedNodesResponse)

instance Prelude.NFData DescribeReservedNodesResponse where
  rnf DescribeReservedNodesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf reservedNodes `Prelude.seq`
        Prelude.rnf httpStatus
