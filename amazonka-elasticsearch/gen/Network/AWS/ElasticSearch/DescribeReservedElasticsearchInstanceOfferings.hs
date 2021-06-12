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
-- Module      : Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved Elasticsearch instance offerings.
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
  ( -- * Creating a Request
    DescribeReservedElasticsearchInstanceOfferings (..),
    newDescribeReservedElasticsearchInstanceOfferings,

    -- * Request Lenses
    describeReservedElasticsearchInstanceOfferings_nextToken,
    describeReservedElasticsearchInstanceOfferings_maxResults,
    describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId,

    -- * Destructuring the Response
    DescribeReservedElasticsearchInstanceOfferingsResponse (..),
    newDescribeReservedElasticsearchInstanceOfferingsResponse,

    -- * Response Lenses
    describeReservedElasticsearchInstanceOfferingsResponse_nextToken,
    describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings,
    describeReservedElasticsearchInstanceOfferingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for parameters to
-- @DescribeReservedElasticsearchInstanceOfferings@
--
-- /See:/ 'newDescribeReservedElasticsearchInstanceOfferings' smart constructor.
data DescribeReservedElasticsearchInstanceOfferings = DescribeReservedElasticsearchInstanceOfferings'
  { -- | NextToken should be sent in case if earlier API call produced result
    -- containing NextToken. It is used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | Set this value to limit the number of results returned. If not
    -- specified, defaults to 100.
    maxResults :: Core.Maybe Core.Int,
    -- | The offering identifier filter value. Use this parameter to show only
    -- the available offering that matches the specified reservation
    -- identifier.
    reservedElasticsearchInstanceOfferingId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedElasticsearchInstanceOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedElasticsearchInstanceOfferings_nextToken' - NextToken should be sent in case if earlier API call produced result
-- containing NextToken. It is used for pagination.
--
-- 'maxResults', 'describeReservedElasticsearchInstanceOfferings_maxResults' - Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
--
-- 'reservedElasticsearchInstanceOfferingId', 'describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId' - The offering identifier filter value. Use this parameter to show only
-- the available offering that matches the specified reservation
-- identifier.
newDescribeReservedElasticsearchInstanceOfferings ::
  DescribeReservedElasticsearchInstanceOfferings
newDescribeReservedElasticsearchInstanceOfferings =
  DescribeReservedElasticsearchInstanceOfferings'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      reservedElasticsearchInstanceOfferingId =
        Core.Nothing
    }

-- | NextToken should be sent in case if earlier API call produced result
-- containing NextToken. It is used for pagination.
describeReservedElasticsearchInstanceOfferings_nextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Core.Maybe Core.Text)
describeReservedElasticsearchInstanceOfferings_nextToken = Lens.lens (\DescribeReservedElasticsearchInstanceOfferings' {nextToken} -> nextToken) (\s@DescribeReservedElasticsearchInstanceOfferings' {} a -> s {nextToken = a} :: DescribeReservedElasticsearchInstanceOfferings)

-- | Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
describeReservedElasticsearchInstanceOfferings_maxResults :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Core.Maybe Core.Int)
describeReservedElasticsearchInstanceOfferings_maxResults = Lens.lens (\DescribeReservedElasticsearchInstanceOfferings' {maxResults} -> maxResults) (\s@DescribeReservedElasticsearchInstanceOfferings' {} a -> s {maxResults = a} :: DescribeReservedElasticsearchInstanceOfferings)

-- | The offering identifier filter value. Use this parameter to show only
-- the available offering that matches the specified reservation
-- identifier.
describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Core.Maybe Core.Text)
describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId = Lens.lens (\DescribeReservedElasticsearchInstanceOfferings' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@DescribeReservedElasticsearchInstanceOfferings' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: DescribeReservedElasticsearchInstanceOfferings)

instance
  Core.AWSPager
    DescribeReservedElasticsearchInstanceOfferings
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedElasticsearchInstanceOfferingsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeReservedElasticsearchInstanceOfferings_nextToken
          Lens..~ rs
            Lens.^? describeReservedElasticsearchInstanceOfferingsResponse_nextToken
              Core.. Lens._Just

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
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "ReservedElasticsearchInstanceOfferings"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeReservedElasticsearchInstanceOfferings

instance
  Core.NFData
    DescribeReservedElasticsearchInstanceOfferings

instance
  Core.ToHeaders
    DescribeReservedElasticsearchInstanceOfferings
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeReservedElasticsearchInstanceOfferings
  where
  toPath =
    Core.const
      "/2015-01-01/es/reservedInstanceOfferings"

instance
  Core.ToQuery
    DescribeReservedElasticsearchInstanceOfferings
  where
  toQuery
    DescribeReservedElasticsearchInstanceOfferings' {..} =
      Core.mconcat
        [ "nextToken" Core.=: nextToken,
          "maxResults" Core.=: maxResults,
          "offeringId"
            Core.=: reservedElasticsearchInstanceOfferingId
        ]

-- | Container for results from
-- @DescribeReservedElasticsearchInstanceOfferings@
--
-- /See:/ 'newDescribeReservedElasticsearchInstanceOfferingsResponse' smart constructor.
data DescribeReservedElasticsearchInstanceOfferingsResponse = DescribeReservedElasticsearchInstanceOfferingsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Core.Text,
    -- | List of reserved Elasticsearch instance offerings
    reservedElasticsearchInstanceOfferings :: Core.Maybe [ReservedElasticsearchInstanceOffering],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedElasticsearchInstanceOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedElasticsearchInstanceOfferingsResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'reservedElasticsearchInstanceOfferings', 'describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings' - List of reserved Elasticsearch instance offerings
--
-- 'httpStatus', 'describeReservedElasticsearchInstanceOfferingsResponse_httpStatus' - The response's http status code.
newDescribeReservedElasticsearchInstanceOfferingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReservedElasticsearchInstanceOfferingsResponse
newDescribeReservedElasticsearchInstanceOfferingsResponse
  pHttpStatus_ =
    DescribeReservedElasticsearchInstanceOfferingsResponse'
      { nextToken =
          Core.Nothing,
        reservedElasticsearchInstanceOfferings =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Provides an identifier to allow retrieval of paginated results.
describeReservedElasticsearchInstanceOfferingsResponse_nextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Core.Maybe Core.Text)
describeReservedElasticsearchInstanceOfferingsResponse_nextToken = Lens.lens (\DescribeReservedElasticsearchInstanceOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeReservedElasticsearchInstanceOfferingsResponse' {} a -> s {nextToken = a} :: DescribeReservedElasticsearchInstanceOfferingsResponse)

-- | List of reserved Elasticsearch instance offerings
describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Core.Maybe [ReservedElasticsearchInstanceOffering])
describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings = Lens.lens (\DescribeReservedElasticsearchInstanceOfferingsResponse' {reservedElasticsearchInstanceOfferings} -> reservedElasticsearchInstanceOfferings) (\s@DescribeReservedElasticsearchInstanceOfferingsResponse' {} a -> s {reservedElasticsearchInstanceOfferings = a} :: DescribeReservedElasticsearchInstanceOfferingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeReservedElasticsearchInstanceOfferingsResponse_httpStatus :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse Core.Int
describeReservedElasticsearchInstanceOfferingsResponse_httpStatus = Lens.lens (\DescribeReservedElasticsearchInstanceOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedElasticsearchInstanceOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeReservedElasticsearchInstanceOfferingsResponse)

instance
  Core.NFData
    DescribeReservedElasticsearchInstanceOfferingsResponse
