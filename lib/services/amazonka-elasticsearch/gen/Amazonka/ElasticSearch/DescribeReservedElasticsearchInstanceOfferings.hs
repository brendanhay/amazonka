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
-- Module      : Amazonka.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved Elasticsearch instance offerings.
--
-- This operation returns paginated results.
module Amazonka.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
  ( -- * Creating a Request
    DescribeReservedElasticsearchInstanceOfferings (..),
    newDescribeReservedElasticsearchInstanceOfferings,

    -- * Request Lenses
    describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId,
    describeReservedElasticsearchInstanceOfferings_nextToken,
    describeReservedElasticsearchInstanceOfferings_maxResults,

    -- * Destructuring the Response
    DescribeReservedElasticsearchInstanceOfferingsResponse (..),
    newDescribeReservedElasticsearchInstanceOfferingsResponse,

    -- * Response Lenses
    describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings,
    describeReservedElasticsearchInstanceOfferingsResponse_nextToken,
    describeReservedElasticsearchInstanceOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for parameters to
-- @DescribeReservedElasticsearchInstanceOfferings@
--
-- /See:/ 'newDescribeReservedElasticsearchInstanceOfferings' smart constructor.
data DescribeReservedElasticsearchInstanceOfferings = DescribeReservedElasticsearchInstanceOfferings'
  { -- | The offering identifier filter value. Use this parameter to show only
    -- the available offering that matches the specified reservation
    -- identifier.
    reservedElasticsearchInstanceOfferingId :: Prelude.Maybe Prelude.Text,
    -- | NextToken should be sent in case if earlier API call produced result
    -- containing NextToken. It is used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Set this value to limit the number of results returned. If not
    -- specified, defaults to 100.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedElasticsearchInstanceOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedElasticsearchInstanceOfferingId', 'describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId' - The offering identifier filter value. Use this parameter to show only
-- the available offering that matches the specified reservation
-- identifier.
--
-- 'nextToken', 'describeReservedElasticsearchInstanceOfferings_nextToken' - NextToken should be sent in case if earlier API call produced result
-- containing NextToken. It is used for pagination.
--
-- 'maxResults', 'describeReservedElasticsearchInstanceOfferings_maxResults' - Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
newDescribeReservedElasticsearchInstanceOfferings ::
  DescribeReservedElasticsearchInstanceOfferings
newDescribeReservedElasticsearchInstanceOfferings =
  DescribeReservedElasticsearchInstanceOfferings'
    { reservedElasticsearchInstanceOfferingId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults =
        Prelude.Nothing
    }

-- | The offering identifier filter value. Use this parameter to show only
-- the available offering that matches the specified reservation
-- identifier.
describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Prelude.Maybe Prelude.Text)
describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId = Lens.lens (\DescribeReservedElasticsearchInstanceOfferings' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@DescribeReservedElasticsearchInstanceOfferings' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: DescribeReservedElasticsearchInstanceOfferings)

-- | NextToken should be sent in case if earlier API call produced result
-- containing NextToken. It is used for pagination.
describeReservedElasticsearchInstanceOfferings_nextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Prelude.Maybe Prelude.Text)
describeReservedElasticsearchInstanceOfferings_nextToken = Lens.lens (\DescribeReservedElasticsearchInstanceOfferings' {nextToken} -> nextToken) (\s@DescribeReservedElasticsearchInstanceOfferings' {} a -> s {nextToken = a} :: DescribeReservedElasticsearchInstanceOfferings)

-- | Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
describeReservedElasticsearchInstanceOfferings_maxResults :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Prelude.Maybe Prelude.Int)
describeReservedElasticsearchInstanceOfferings_maxResults = Lens.lens (\DescribeReservedElasticsearchInstanceOfferings' {maxResults} -> maxResults) (\s@DescribeReservedElasticsearchInstanceOfferings' {} a -> s {maxResults = a} :: DescribeReservedElasticsearchInstanceOfferings)

instance
  Core.AWSPager
    DescribeReservedElasticsearchInstanceOfferings
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedElasticsearchInstanceOfferingsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReservedElasticsearchInstanceOfferings_nextToken
          Lens..~ rs
            Lens.^? describeReservedElasticsearchInstanceOfferingsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeReservedElasticsearchInstanceOfferings
  where
  type
    AWSResponse
      DescribeReservedElasticsearchInstanceOfferings =
      DescribeReservedElasticsearchInstanceOfferingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservedElasticsearchInstanceOfferingsResponse'
            Prelude.<$> ( x Core..?> "ReservedElasticsearchInstanceOfferings"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReservedElasticsearchInstanceOfferings

instance
  Prelude.NFData
    DescribeReservedElasticsearchInstanceOfferings

instance
  Core.ToHeaders
    DescribeReservedElasticsearchInstanceOfferings
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeReservedElasticsearchInstanceOfferings
  where
  toPath =
    Prelude.const
      "/2015-01-01/es/reservedInstanceOfferings"

instance
  Core.ToQuery
    DescribeReservedElasticsearchInstanceOfferings
  where
  toQuery
    DescribeReservedElasticsearchInstanceOfferings' {..} =
      Prelude.mconcat
        [ "offeringId"
            Core.=: reservedElasticsearchInstanceOfferingId,
          "nextToken" Core.=: nextToken,
          "maxResults" Core.=: maxResults
        ]

-- | Container for results from
-- @DescribeReservedElasticsearchInstanceOfferings@
--
-- /See:/ 'newDescribeReservedElasticsearchInstanceOfferingsResponse' smart constructor.
data DescribeReservedElasticsearchInstanceOfferingsResponse = DescribeReservedElasticsearchInstanceOfferingsResponse'
  { -- | List of reserved Elasticsearch instance offerings
    reservedElasticsearchInstanceOfferings :: Prelude.Maybe [ReservedElasticsearchInstanceOffering],
    -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedElasticsearchInstanceOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedElasticsearchInstanceOfferings', 'describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings' - List of reserved Elasticsearch instance offerings
--
-- 'nextToken', 'describeReservedElasticsearchInstanceOfferingsResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeReservedElasticsearchInstanceOfferingsResponse_httpStatus' - The response's http status code.
newDescribeReservedElasticsearchInstanceOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedElasticsearchInstanceOfferingsResponse
newDescribeReservedElasticsearchInstanceOfferingsResponse
  pHttpStatus_ =
    DescribeReservedElasticsearchInstanceOfferingsResponse'
      { reservedElasticsearchInstanceOfferings =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | List of reserved Elasticsearch instance offerings
describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Prelude.Maybe [ReservedElasticsearchInstanceOffering])
describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings = Lens.lens (\DescribeReservedElasticsearchInstanceOfferingsResponse' {reservedElasticsearchInstanceOfferings} -> reservedElasticsearchInstanceOfferings) (\s@DescribeReservedElasticsearchInstanceOfferingsResponse' {} a -> s {reservedElasticsearchInstanceOfferings = a} :: DescribeReservedElasticsearchInstanceOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Provides an identifier to allow retrieval of paginated results.
describeReservedElasticsearchInstanceOfferingsResponse_nextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Prelude.Maybe Prelude.Text)
describeReservedElasticsearchInstanceOfferingsResponse_nextToken = Lens.lens (\DescribeReservedElasticsearchInstanceOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeReservedElasticsearchInstanceOfferingsResponse' {} a -> s {nextToken = a} :: DescribeReservedElasticsearchInstanceOfferingsResponse)

-- | The response's http status code.
describeReservedElasticsearchInstanceOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse Prelude.Int
describeReservedElasticsearchInstanceOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedElasticsearchInstanceOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedElasticsearchInstanceOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedElasticsearchInstanceOfferingsResponse)

instance
  Prelude.NFData
    DescribeReservedElasticsearchInstanceOfferingsResponse
