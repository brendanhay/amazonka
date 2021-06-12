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
-- Module      : Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved Elasticsearch instances for this
-- account.
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
  ( -- * Creating a Request
    DescribeReservedElasticsearchInstances (..),
    newDescribeReservedElasticsearchInstances,

    -- * Request Lenses
    describeReservedElasticsearchInstances_nextToken,
    describeReservedElasticsearchInstances_maxResults,
    describeReservedElasticsearchInstances_reservedElasticsearchInstanceId,

    -- * Destructuring the Response
    DescribeReservedElasticsearchInstancesResponse (..),
    newDescribeReservedElasticsearchInstancesResponse,

    -- * Response Lenses
    describeReservedElasticsearchInstancesResponse_nextToken,
    describeReservedElasticsearchInstancesResponse_reservedElasticsearchInstances,
    describeReservedElasticsearchInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for parameters to @DescribeReservedElasticsearchInstances@
--
-- /See:/ 'newDescribeReservedElasticsearchInstances' smart constructor.
data DescribeReservedElasticsearchInstances = DescribeReservedElasticsearchInstances'
  { -- | NextToken should be sent in case if earlier API call produced result
    -- containing NextToken. It is used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | Set this value to limit the number of results returned. If not
    -- specified, defaults to 100.
    maxResults :: Core.Maybe Core.Int,
    -- | The reserved instance identifier filter value. Use this parameter to
    -- show only the reservation that matches the specified reserved
    -- Elasticsearch instance ID.
    reservedElasticsearchInstanceId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedElasticsearchInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedElasticsearchInstances_nextToken' - NextToken should be sent in case if earlier API call produced result
-- containing NextToken. It is used for pagination.
--
-- 'maxResults', 'describeReservedElasticsearchInstances_maxResults' - Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
--
-- 'reservedElasticsearchInstanceId', 'describeReservedElasticsearchInstances_reservedElasticsearchInstanceId' - The reserved instance identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reserved
-- Elasticsearch instance ID.
newDescribeReservedElasticsearchInstances ::
  DescribeReservedElasticsearchInstances
newDescribeReservedElasticsearchInstances =
  DescribeReservedElasticsearchInstances'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      reservedElasticsearchInstanceId =
        Core.Nothing
    }

-- | NextToken should be sent in case if earlier API call produced result
-- containing NextToken. It is used for pagination.
describeReservedElasticsearchInstances_nextToken :: Lens.Lens' DescribeReservedElasticsearchInstances (Core.Maybe Core.Text)
describeReservedElasticsearchInstances_nextToken = Lens.lens (\DescribeReservedElasticsearchInstances' {nextToken} -> nextToken) (\s@DescribeReservedElasticsearchInstances' {} a -> s {nextToken = a} :: DescribeReservedElasticsearchInstances)

-- | Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
describeReservedElasticsearchInstances_maxResults :: Lens.Lens' DescribeReservedElasticsearchInstances (Core.Maybe Core.Int)
describeReservedElasticsearchInstances_maxResults = Lens.lens (\DescribeReservedElasticsearchInstances' {maxResults} -> maxResults) (\s@DescribeReservedElasticsearchInstances' {} a -> s {maxResults = a} :: DescribeReservedElasticsearchInstances)

-- | The reserved instance identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reserved
-- Elasticsearch instance ID.
describeReservedElasticsearchInstances_reservedElasticsearchInstanceId :: Lens.Lens' DescribeReservedElasticsearchInstances (Core.Maybe Core.Text)
describeReservedElasticsearchInstances_reservedElasticsearchInstanceId = Lens.lens (\DescribeReservedElasticsearchInstances' {reservedElasticsearchInstanceId} -> reservedElasticsearchInstanceId) (\s@DescribeReservedElasticsearchInstances' {} a -> s {reservedElasticsearchInstanceId = a} :: DescribeReservedElasticsearchInstances)

instance
  Core.AWSPager
    DescribeReservedElasticsearchInstances
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedElasticsearchInstancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedElasticsearchInstancesResponse_reservedElasticsearchInstances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeReservedElasticsearchInstances_nextToken
          Lens..~ rs
            Lens.^? describeReservedElasticsearchInstancesResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeReservedElasticsearchInstances
  where
  type
    AWSResponse
      DescribeReservedElasticsearchInstances =
      DescribeReservedElasticsearchInstancesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservedElasticsearchInstancesResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "ReservedElasticsearchInstances"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeReservedElasticsearchInstances

instance
  Core.NFData
    DescribeReservedElasticsearchInstances

instance
  Core.ToHeaders
    DescribeReservedElasticsearchInstances
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeReservedElasticsearchInstances
  where
  toPath =
    Core.const "/2015-01-01/es/reservedInstances"

instance
  Core.ToQuery
    DescribeReservedElasticsearchInstances
  where
  toQuery DescribeReservedElasticsearchInstances' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "reservationId"
          Core.=: reservedElasticsearchInstanceId
      ]

-- | Container for results from @DescribeReservedElasticsearchInstances@
--
-- /See:/ 'newDescribeReservedElasticsearchInstancesResponse' smart constructor.
data DescribeReservedElasticsearchInstancesResponse = DescribeReservedElasticsearchInstancesResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Core.Text,
    -- | List of reserved Elasticsearch instances.
    reservedElasticsearchInstances :: Core.Maybe [ReservedElasticsearchInstance],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReservedElasticsearchInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedElasticsearchInstancesResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'reservedElasticsearchInstances', 'describeReservedElasticsearchInstancesResponse_reservedElasticsearchInstances' - List of reserved Elasticsearch instances.
--
-- 'httpStatus', 'describeReservedElasticsearchInstancesResponse_httpStatus' - The response's http status code.
newDescribeReservedElasticsearchInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReservedElasticsearchInstancesResponse
newDescribeReservedElasticsearchInstancesResponse
  pHttpStatus_ =
    DescribeReservedElasticsearchInstancesResponse'
      { nextToken =
          Core.Nothing,
        reservedElasticsearchInstances =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Provides an identifier to allow retrieval of paginated results.
describeReservedElasticsearchInstancesResponse_nextToken :: Lens.Lens' DescribeReservedElasticsearchInstancesResponse (Core.Maybe Core.Text)
describeReservedElasticsearchInstancesResponse_nextToken = Lens.lens (\DescribeReservedElasticsearchInstancesResponse' {nextToken} -> nextToken) (\s@DescribeReservedElasticsearchInstancesResponse' {} a -> s {nextToken = a} :: DescribeReservedElasticsearchInstancesResponse)

-- | List of reserved Elasticsearch instances.
describeReservedElasticsearchInstancesResponse_reservedElasticsearchInstances :: Lens.Lens' DescribeReservedElasticsearchInstancesResponse (Core.Maybe [ReservedElasticsearchInstance])
describeReservedElasticsearchInstancesResponse_reservedElasticsearchInstances = Lens.lens (\DescribeReservedElasticsearchInstancesResponse' {reservedElasticsearchInstances} -> reservedElasticsearchInstances) (\s@DescribeReservedElasticsearchInstancesResponse' {} a -> s {reservedElasticsearchInstances = a} :: DescribeReservedElasticsearchInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeReservedElasticsearchInstancesResponse_httpStatus :: Lens.Lens' DescribeReservedElasticsearchInstancesResponse Core.Int
describeReservedElasticsearchInstancesResponse_httpStatus = Lens.lens (\DescribeReservedElasticsearchInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedElasticsearchInstancesResponse' {} a -> s {httpStatus = a} :: DescribeReservedElasticsearchInstancesResponse)

instance
  Core.NFData
    DescribeReservedElasticsearchInstancesResponse
