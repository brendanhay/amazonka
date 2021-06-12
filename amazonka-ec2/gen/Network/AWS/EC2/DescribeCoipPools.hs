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
-- Module      : Network.AWS.EC2.DescribeCoipPools
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified customer-owned address pools or all of your
-- customer-owned address pools.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCoipPools
  ( -- * Creating a Request
    DescribeCoipPools (..),
    newDescribeCoipPools,

    -- * Request Lenses
    describeCoipPools_nextToken,
    describeCoipPools_dryRun,
    describeCoipPools_maxResults,
    describeCoipPools_filters,
    describeCoipPools_poolIds,

    -- * Destructuring the Response
    DescribeCoipPoolsResponse (..),
    newDescribeCoipPoolsResponse,

    -- * Response Lenses
    describeCoipPoolsResponse_nextToken,
    describeCoipPoolsResponse_coipPools,
    describeCoipPoolsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCoipPools' smart constructor.
data DescribeCoipPools = DescribeCoipPools'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The filters. The following are the possible values:
    --
    -- -   @coip-pool.pool-id@
    --
    -- -   @coip-pool.local-gateway-route-table-id@
    filters :: Core.Maybe [Filter],
    -- | The IDs of the address pools.
    poolIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCoipPools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCoipPools_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeCoipPools_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeCoipPools_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeCoipPools_filters' - The filters. The following are the possible values:
--
-- -   @coip-pool.pool-id@
--
-- -   @coip-pool.local-gateway-route-table-id@
--
-- 'poolIds', 'describeCoipPools_poolIds' - The IDs of the address pools.
newDescribeCoipPools ::
  DescribeCoipPools
newDescribeCoipPools =
  DescribeCoipPools'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      poolIds = Core.Nothing
    }

-- | The token for the next page of results.
describeCoipPools_nextToken :: Lens.Lens' DescribeCoipPools (Core.Maybe Core.Text)
describeCoipPools_nextToken = Lens.lens (\DescribeCoipPools' {nextToken} -> nextToken) (\s@DescribeCoipPools' {} a -> s {nextToken = a} :: DescribeCoipPools)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeCoipPools_dryRun :: Lens.Lens' DescribeCoipPools (Core.Maybe Core.Bool)
describeCoipPools_dryRun = Lens.lens (\DescribeCoipPools' {dryRun} -> dryRun) (\s@DescribeCoipPools' {} a -> s {dryRun = a} :: DescribeCoipPools)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeCoipPools_maxResults :: Lens.Lens' DescribeCoipPools (Core.Maybe Core.Natural)
describeCoipPools_maxResults = Lens.lens (\DescribeCoipPools' {maxResults} -> maxResults) (\s@DescribeCoipPools' {} a -> s {maxResults = a} :: DescribeCoipPools)

-- | The filters. The following are the possible values:
--
-- -   @coip-pool.pool-id@
--
-- -   @coip-pool.local-gateway-route-table-id@
describeCoipPools_filters :: Lens.Lens' DescribeCoipPools (Core.Maybe [Filter])
describeCoipPools_filters = Lens.lens (\DescribeCoipPools' {filters} -> filters) (\s@DescribeCoipPools' {} a -> s {filters = a} :: DescribeCoipPools) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the address pools.
describeCoipPools_poolIds :: Lens.Lens' DescribeCoipPools (Core.Maybe [Core.Text])
describeCoipPools_poolIds = Lens.lens (\DescribeCoipPools' {poolIds} -> poolIds) (\s@DescribeCoipPools' {} a -> s {poolIds = a} :: DescribeCoipPools) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeCoipPools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCoipPoolsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCoipPoolsResponse_coipPools
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeCoipPools_nextToken
          Lens..~ rs
          Lens.^? describeCoipPoolsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeCoipPools where
  type
    AWSResponse DescribeCoipPools =
      DescribeCoipPoolsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCoipPoolsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "coipPoolSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCoipPools

instance Core.NFData DescribeCoipPools

instance Core.ToHeaders DescribeCoipPools where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeCoipPools where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCoipPools where
  toQuery DescribeCoipPools' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeCoipPools" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        Core.toQuery
          (Core.toQueryList "PoolId" Core.<$> poolIds)
      ]

-- | /See:/ 'newDescribeCoipPoolsResponse' smart constructor.
data DescribeCoipPoolsResponse = DescribeCoipPoolsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the address pools.
    coipPools :: Core.Maybe [CoipPool],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCoipPoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCoipPoolsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'coipPools', 'describeCoipPoolsResponse_coipPools' - Information about the address pools.
--
-- 'httpStatus', 'describeCoipPoolsResponse_httpStatus' - The response's http status code.
newDescribeCoipPoolsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCoipPoolsResponse
newDescribeCoipPoolsResponse pHttpStatus_ =
  DescribeCoipPoolsResponse'
    { nextToken =
        Core.Nothing,
      coipPools = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeCoipPoolsResponse_nextToken :: Lens.Lens' DescribeCoipPoolsResponse (Core.Maybe Core.Text)
describeCoipPoolsResponse_nextToken = Lens.lens (\DescribeCoipPoolsResponse' {nextToken} -> nextToken) (\s@DescribeCoipPoolsResponse' {} a -> s {nextToken = a} :: DescribeCoipPoolsResponse)

-- | Information about the address pools.
describeCoipPoolsResponse_coipPools :: Lens.Lens' DescribeCoipPoolsResponse (Core.Maybe [CoipPool])
describeCoipPoolsResponse_coipPools = Lens.lens (\DescribeCoipPoolsResponse' {coipPools} -> coipPools) (\s@DescribeCoipPoolsResponse' {} a -> s {coipPools = a} :: DescribeCoipPoolsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeCoipPoolsResponse_httpStatus :: Lens.Lens' DescribeCoipPoolsResponse Core.Int
describeCoipPoolsResponse_httpStatus = Lens.lens (\DescribeCoipPoolsResponse' {httpStatus} -> httpStatus) (\s@DescribeCoipPoolsResponse' {} a -> s {httpStatus = a} :: DescribeCoipPoolsResponse)

instance Core.NFData DescribeCoipPoolsResponse
