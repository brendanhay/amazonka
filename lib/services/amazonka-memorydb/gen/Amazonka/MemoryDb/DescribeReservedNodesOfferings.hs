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
-- Module      : Amazonka.MemoryDb.DescribeReservedNodesOfferings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved node offerings.
--
-- This operation returns paginated results.
module Amazonka.MemoryDb.DescribeReservedNodesOfferings
  ( -- * Creating a Request
    DescribeReservedNodesOfferings (..),
    newDescribeReservedNodesOfferings,

    -- * Request Lenses
    describeReservedNodesOfferings_duration,
    describeReservedNodesOfferings_maxResults,
    describeReservedNodesOfferings_nextToken,
    describeReservedNodesOfferings_nodeType,
    describeReservedNodesOfferings_offeringType,
    describeReservedNodesOfferings_reservedNodesOfferingId,

    -- * Destructuring the Response
    DescribeReservedNodesOfferingsResponse (..),
    newDescribeReservedNodesOfferingsResponse,

    -- * Response Lenses
    describeReservedNodesOfferingsResponse_nextToken,
    describeReservedNodesOfferingsResponse_reservedNodesOfferings,
    describeReservedNodesOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReservedNodesOfferings' smart constructor.
data DescribeReservedNodesOfferings = DescribeReservedNodesOfferings'
  { -- | Duration filter value, specified in years or seconds. Use this parameter
    -- to show only reservations for a given duration.
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
    -- | The node type for the reserved nodes. For more information, see
    -- <https://docs.aws.amazon.com/memorydb/latest/devguide/nodes.reserved.html#reserved-nodes-supported Supported node types>.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The offering type filter value. Use this parameter to show only the
    -- available offerings matching the specified offering type. Valid values:
    -- \"All Upfront\"|\"Partial Upfront\"| \"No Upfront\"
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The offering identifier filter value. Use this parameter to show only
    -- the available offering that matches the specified reservation
    -- identifier.
    reservedNodesOfferingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedNodesOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'describeReservedNodesOfferings_duration' - Duration filter value, specified in years or seconds. Use this parameter
-- to show only reservations for a given duration.
--
-- 'maxResults', 'describeReservedNodesOfferings_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'describeReservedNodesOfferings_nextToken' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
--
-- 'nodeType', 'describeReservedNodesOfferings_nodeType' - The node type for the reserved nodes. For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/nodes.reserved.html#reserved-nodes-supported Supported node types>.
--
-- 'offeringType', 'describeReservedNodesOfferings_offeringType' - The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid values:
-- \"All Upfront\"|\"Partial Upfront\"| \"No Upfront\"
--
-- 'reservedNodesOfferingId', 'describeReservedNodesOfferings_reservedNodesOfferingId' - The offering identifier filter value. Use this parameter to show only
-- the available offering that matches the specified reservation
-- identifier.
newDescribeReservedNodesOfferings ::
  DescribeReservedNodesOfferings
newDescribeReservedNodesOfferings =
  DescribeReservedNodesOfferings'
    { duration =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      reservedNodesOfferingId = Prelude.Nothing
    }

-- | Duration filter value, specified in years or seconds. Use this parameter
-- to show only reservations for a given duration.
describeReservedNodesOfferings_duration :: Lens.Lens' DescribeReservedNodesOfferings (Prelude.Maybe Prelude.Text)
describeReservedNodesOfferings_duration = Lens.lens (\DescribeReservedNodesOfferings' {duration} -> duration) (\s@DescribeReservedNodesOfferings' {} a -> s {duration = a} :: DescribeReservedNodesOfferings)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
describeReservedNodesOfferings_maxResults :: Lens.Lens' DescribeReservedNodesOfferings (Prelude.Maybe Prelude.Int)
describeReservedNodesOfferings_maxResults = Lens.lens (\DescribeReservedNodesOfferings' {maxResults} -> maxResults) (\s@DescribeReservedNodesOfferings' {} a -> s {maxResults = a} :: DescribeReservedNodesOfferings)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
describeReservedNodesOfferings_nextToken :: Lens.Lens' DescribeReservedNodesOfferings (Prelude.Maybe Prelude.Text)
describeReservedNodesOfferings_nextToken = Lens.lens (\DescribeReservedNodesOfferings' {nextToken} -> nextToken) (\s@DescribeReservedNodesOfferings' {} a -> s {nextToken = a} :: DescribeReservedNodesOfferings)

-- | The node type for the reserved nodes. For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/nodes.reserved.html#reserved-nodes-supported Supported node types>.
describeReservedNodesOfferings_nodeType :: Lens.Lens' DescribeReservedNodesOfferings (Prelude.Maybe Prelude.Text)
describeReservedNodesOfferings_nodeType = Lens.lens (\DescribeReservedNodesOfferings' {nodeType} -> nodeType) (\s@DescribeReservedNodesOfferings' {} a -> s {nodeType = a} :: DescribeReservedNodesOfferings)

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid values:
-- \"All Upfront\"|\"Partial Upfront\"| \"No Upfront\"
describeReservedNodesOfferings_offeringType :: Lens.Lens' DescribeReservedNodesOfferings (Prelude.Maybe Prelude.Text)
describeReservedNodesOfferings_offeringType = Lens.lens (\DescribeReservedNodesOfferings' {offeringType} -> offeringType) (\s@DescribeReservedNodesOfferings' {} a -> s {offeringType = a} :: DescribeReservedNodesOfferings)

-- | The offering identifier filter value. Use this parameter to show only
-- the available offering that matches the specified reservation
-- identifier.
describeReservedNodesOfferings_reservedNodesOfferingId :: Lens.Lens' DescribeReservedNodesOfferings (Prelude.Maybe Prelude.Text)
describeReservedNodesOfferings_reservedNodesOfferingId = Lens.lens (\DescribeReservedNodesOfferings' {reservedNodesOfferingId} -> reservedNodesOfferingId) (\s@DescribeReservedNodesOfferings' {} a -> s {reservedNodesOfferingId = a} :: DescribeReservedNodesOfferings)

instance Core.AWSPager DescribeReservedNodesOfferings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedNodesOfferingsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedNodesOfferingsResponse_reservedNodesOfferings
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeReservedNodesOfferings_nextToken
          Lens..~ rs
          Lens.^? describeReservedNodesOfferingsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeReservedNodesOfferings
  where
  type
    AWSResponse DescribeReservedNodesOfferings =
      DescribeReservedNodesOfferingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservedNodesOfferingsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ReservedNodesOfferings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedNodesOfferings
  where
  hashWithSalt
    _salt
    DescribeReservedNodesOfferings' {..} =
      _salt
        `Prelude.hashWithSalt` duration
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` nodeType
        `Prelude.hashWithSalt` offeringType
        `Prelude.hashWithSalt` reservedNodesOfferingId

instance
  Prelude.NFData
    DescribeReservedNodesOfferings
  where
  rnf DescribeReservedNodesOfferings' {..} =
    Prelude.rnf duration
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf reservedNodesOfferingId

instance
  Data.ToHeaders
    DescribeReservedNodesOfferings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.DescribeReservedNodesOfferings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeReservedNodesOfferings where
  toJSON DescribeReservedNodesOfferings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Duration" Data..=) Prelude.<$> duration,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("NodeType" Data..=) Prelude.<$> nodeType,
            ("OfferingType" Data..=) Prelude.<$> offeringType,
            ("ReservedNodesOfferingId" Data..=)
              Prelude.<$> reservedNodesOfferingId
          ]
      )

instance Data.ToPath DescribeReservedNodesOfferings where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReservedNodesOfferings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReservedNodesOfferingsResponse' smart constructor.
data DescribeReservedNodesOfferingsResponse = DescribeReservedNodesOfferingsResponse'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists available reserved node offerings.
    reservedNodesOfferings :: Prelude.Maybe [ReservedNodesOffering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedNodesOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedNodesOfferingsResponse_nextToken' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
--
-- 'reservedNodesOfferings', 'describeReservedNodesOfferingsResponse_reservedNodesOfferings' - Lists available reserved node offerings.
--
-- 'httpStatus', 'describeReservedNodesOfferingsResponse_httpStatus' - The response's http status code.
newDescribeReservedNodesOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedNodesOfferingsResponse
newDescribeReservedNodesOfferingsResponse
  pHttpStatus_ =
    DescribeReservedNodesOfferingsResponse'
      { nextToken =
          Prelude.Nothing,
        reservedNodesOfferings =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
describeReservedNodesOfferingsResponse_nextToken :: Lens.Lens' DescribeReservedNodesOfferingsResponse (Prelude.Maybe Prelude.Text)
describeReservedNodesOfferingsResponse_nextToken = Lens.lens (\DescribeReservedNodesOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeReservedNodesOfferingsResponse' {} a -> s {nextToken = a} :: DescribeReservedNodesOfferingsResponse)

-- | Lists available reserved node offerings.
describeReservedNodesOfferingsResponse_reservedNodesOfferings :: Lens.Lens' DescribeReservedNodesOfferingsResponse (Prelude.Maybe [ReservedNodesOffering])
describeReservedNodesOfferingsResponse_reservedNodesOfferings = Lens.lens (\DescribeReservedNodesOfferingsResponse' {reservedNodesOfferings} -> reservedNodesOfferings) (\s@DescribeReservedNodesOfferingsResponse' {} a -> s {reservedNodesOfferings = a} :: DescribeReservedNodesOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservedNodesOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedNodesOfferingsResponse Prelude.Int
describeReservedNodesOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedNodesOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedNodesOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedNodesOfferingsResponse)

instance
  Prelude.NFData
    DescribeReservedNodesOfferingsResponse
  where
  rnf DescribeReservedNodesOfferingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reservedNodesOfferings
      `Prelude.seq` Prelude.rnf httpStatus
