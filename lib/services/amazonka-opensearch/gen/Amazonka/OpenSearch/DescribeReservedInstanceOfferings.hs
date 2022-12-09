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
-- Module      : Amazonka.OpenSearch.DescribeReservedInstanceOfferings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available Amazon OpenSearch Service Reserved Instance
-- offerings for a given Region. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/ri.html Reserved Instances in Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DescribeReservedInstanceOfferings
  ( -- * Creating a Request
    DescribeReservedInstanceOfferings (..),
    newDescribeReservedInstanceOfferings,

    -- * Request Lenses
    describeReservedInstanceOfferings_maxResults,
    describeReservedInstanceOfferings_nextToken,
    describeReservedInstanceOfferings_reservedInstanceOfferingId,

    -- * Destructuring the Response
    DescribeReservedInstanceOfferingsResponse (..),
    newDescribeReservedInstanceOfferingsResponse,

    -- * Response Lenses
    describeReservedInstanceOfferingsResponse_nextToken,
    describeReservedInstanceOfferingsResponse_reservedInstanceOfferings,
    describeReservedInstanceOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to a
-- @DescribeReservedInstanceOfferings@ operation.
--
-- /See:/ 'newDescribeReservedInstanceOfferings' smart constructor.
data DescribeReservedInstanceOfferings = DescribeReservedInstanceOfferings'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If your initial @DescribeReservedInstanceOfferings@ operation returns a
    -- @nextToken@, you can include the returned @nextToken@ in subsequent
    -- @DescribeReservedInstanceOfferings@ operations, which returns results in
    -- the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Reserved Instance identifier filter value. Use this parameter to
    -- show only the available instance types that match the specified
    -- reservation identifier.
    reservedInstanceOfferingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstanceOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeReservedInstanceOfferings_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'nextToken', 'describeReservedInstanceOfferings_nextToken' - If your initial @DescribeReservedInstanceOfferings@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeReservedInstanceOfferings@ operations, which returns results in
-- the next page.
--
-- 'reservedInstanceOfferingId', 'describeReservedInstanceOfferings_reservedInstanceOfferingId' - The Reserved Instance identifier filter value. Use this parameter to
-- show only the available instance types that match the specified
-- reservation identifier.
newDescribeReservedInstanceOfferings ::
  DescribeReservedInstanceOfferings
newDescribeReservedInstanceOfferings =
  DescribeReservedInstanceOfferings'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      reservedInstanceOfferingId =
        Prelude.Nothing
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
describeReservedInstanceOfferings_maxResults :: Lens.Lens' DescribeReservedInstanceOfferings (Prelude.Maybe Prelude.Int)
describeReservedInstanceOfferings_maxResults = Lens.lens (\DescribeReservedInstanceOfferings' {maxResults} -> maxResults) (\s@DescribeReservedInstanceOfferings' {} a -> s {maxResults = a} :: DescribeReservedInstanceOfferings)

-- | If your initial @DescribeReservedInstanceOfferings@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeReservedInstanceOfferings@ operations, which returns results in
-- the next page.
describeReservedInstanceOfferings_nextToken :: Lens.Lens' DescribeReservedInstanceOfferings (Prelude.Maybe Prelude.Text)
describeReservedInstanceOfferings_nextToken = Lens.lens (\DescribeReservedInstanceOfferings' {nextToken} -> nextToken) (\s@DescribeReservedInstanceOfferings' {} a -> s {nextToken = a} :: DescribeReservedInstanceOfferings)

-- | The Reserved Instance identifier filter value. Use this parameter to
-- show only the available instance types that match the specified
-- reservation identifier.
describeReservedInstanceOfferings_reservedInstanceOfferingId :: Lens.Lens' DescribeReservedInstanceOfferings (Prelude.Maybe Prelude.Text)
describeReservedInstanceOfferings_reservedInstanceOfferingId = Lens.lens (\DescribeReservedInstanceOfferings' {reservedInstanceOfferingId} -> reservedInstanceOfferingId) (\s@DescribeReservedInstanceOfferings' {} a -> s {reservedInstanceOfferingId = a} :: DescribeReservedInstanceOfferings)

instance
  Core.AWSRequest
    DescribeReservedInstanceOfferings
  where
  type
    AWSResponse DescribeReservedInstanceOfferings =
      DescribeReservedInstanceOfferingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservedInstanceOfferingsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x Data..?> "ReservedInstanceOfferings"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedInstanceOfferings
  where
  hashWithSalt
    _salt
    DescribeReservedInstanceOfferings' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` reservedInstanceOfferingId

instance
  Prelude.NFData
    DescribeReservedInstanceOfferings
  where
  rnf DescribeReservedInstanceOfferings' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reservedInstanceOfferingId

instance
  Data.ToHeaders
    DescribeReservedInstanceOfferings
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeReservedInstanceOfferings
  where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/reservedInstanceOfferings"

instance
  Data.ToQuery
    DescribeReservedInstanceOfferings
  where
  toQuery DescribeReservedInstanceOfferings' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "offeringId" Data.=: reservedInstanceOfferingId
      ]

-- | Container for results of a @DescribeReservedInstanceOfferings@ request.
--
-- /See:/ 'newDescribeReservedInstanceOfferingsResponse' smart constructor.
data DescribeReservedInstanceOfferingsResponse = DescribeReservedInstanceOfferingsResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of Reserved Instance offerings.
    reservedInstanceOfferings :: Prelude.Maybe [ReservedInstanceOffering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstanceOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedInstanceOfferingsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'reservedInstanceOfferings', 'describeReservedInstanceOfferingsResponse_reservedInstanceOfferings' - List of Reserved Instance offerings.
--
-- 'httpStatus', 'describeReservedInstanceOfferingsResponse_httpStatus' - The response's http status code.
newDescribeReservedInstanceOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedInstanceOfferingsResponse
newDescribeReservedInstanceOfferingsResponse
  pHttpStatus_ =
    DescribeReservedInstanceOfferingsResponse'
      { nextToken =
          Prelude.Nothing,
        reservedInstanceOfferings =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
describeReservedInstanceOfferingsResponse_nextToken :: Lens.Lens' DescribeReservedInstanceOfferingsResponse (Prelude.Maybe Prelude.Text)
describeReservedInstanceOfferingsResponse_nextToken = Lens.lens (\DescribeReservedInstanceOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeReservedInstanceOfferingsResponse' {} a -> s {nextToken = a} :: DescribeReservedInstanceOfferingsResponse)

-- | List of Reserved Instance offerings.
describeReservedInstanceOfferingsResponse_reservedInstanceOfferings :: Lens.Lens' DescribeReservedInstanceOfferingsResponse (Prelude.Maybe [ReservedInstanceOffering])
describeReservedInstanceOfferingsResponse_reservedInstanceOfferings = Lens.lens (\DescribeReservedInstanceOfferingsResponse' {reservedInstanceOfferings} -> reservedInstanceOfferings) (\s@DescribeReservedInstanceOfferingsResponse' {} a -> s {reservedInstanceOfferings = a} :: DescribeReservedInstanceOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservedInstanceOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedInstanceOfferingsResponse Prelude.Int
describeReservedInstanceOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedInstanceOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedInstanceOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedInstanceOfferingsResponse)

instance
  Prelude.NFData
    DescribeReservedInstanceOfferingsResponse
  where
  rnf DescribeReservedInstanceOfferingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reservedInstanceOfferings
      `Prelude.seq` Prelude.rnf httpStatus
