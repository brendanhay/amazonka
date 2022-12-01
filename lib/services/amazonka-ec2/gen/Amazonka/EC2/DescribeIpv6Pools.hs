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
-- Module      : Amazonka.EC2.DescribeIpv6Pools
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your IPv6 address pools.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeIpv6Pools
  ( -- * Creating a Request
    DescribeIpv6Pools (..),
    newDescribeIpv6Pools,

    -- * Request Lenses
    describeIpv6Pools_nextToken,
    describeIpv6Pools_poolIds,
    describeIpv6Pools_filters,
    describeIpv6Pools_dryRun,
    describeIpv6Pools_maxResults,

    -- * Destructuring the Response
    DescribeIpv6PoolsResponse (..),
    newDescribeIpv6PoolsResponse,

    -- * Response Lenses
    describeIpv6PoolsResponse_nextToken,
    describeIpv6PoolsResponse_ipv6Pools,
    describeIpv6PoolsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIpv6Pools' smart constructor.
data DescribeIpv6Pools = DescribeIpv6Pools'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the IPv6 address pools.
    poolIds :: Prelude.Maybe [Prelude.Text],
    -- | One or more filters.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpv6Pools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeIpv6Pools_nextToken' - The token for the next page of results.
--
-- 'poolIds', 'describeIpv6Pools_poolIds' - The IDs of the IPv6 address pools.
--
-- 'filters', 'describeIpv6Pools_filters' - One or more filters.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- 'dryRun', 'describeIpv6Pools_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeIpv6Pools_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeIpv6Pools ::
  DescribeIpv6Pools
newDescribeIpv6Pools =
  DescribeIpv6Pools'
    { nextToken = Prelude.Nothing,
      poolIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
describeIpv6Pools_nextToken :: Lens.Lens' DescribeIpv6Pools (Prelude.Maybe Prelude.Text)
describeIpv6Pools_nextToken = Lens.lens (\DescribeIpv6Pools' {nextToken} -> nextToken) (\s@DescribeIpv6Pools' {} a -> s {nextToken = a} :: DescribeIpv6Pools)

-- | The IDs of the IPv6 address pools.
describeIpv6Pools_poolIds :: Lens.Lens' DescribeIpv6Pools (Prelude.Maybe [Prelude.Text])
describeIpv6Pools_poolIds = Lens.lens (\DescribeIpv6Pools' {poolIds} -> poolIds) (\s@DescribeIpv6Pools' {} a -> s {poolIds = a} :: DescribeIpv6Pools) Prelude.. Lens.mapping Lens.coerced

-- | One or more filters.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
describeIpv6Pools_filters :: Lens.Lens' DescribeIpv6Pools (Prelude.Maybe [Filter])
describeIpv6Pools_filters = Lens.lens (\DescribeIpv6Pools' {filters} -> filters) (\s@DescribeIpv6Pools' {} a -> s {filters = a} :: DescribeIpv6Pools) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeIpv6Pools_dryRun :: Lens.Lens' DescribeIpv6Pools (Prelude.Maybe Prelude.Bool)
describeIpv6Pools_dryRun = Lens.lens (\DescribeIpv6Pools' {dryRun} -> dryRun) (\s@DescribeIpv6Pools' {} a -> s {dryRun = a} :: DescribeIpv6Pools)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeIpv6Pools_maxResults :: Lens.Lens' DescribeIpv6Pools (Prelude.Maybe Prelude.Natural)
describeIpv6Pools_maxResults = Lens.lens (\DescribeIpv6Pools' {maxResults} -> maxResults) (\s@DescribeIpv6Pools' {} a -> s {maxResults = a} :: DescribeIpv6Pools)

instance Core.AWSPager DescribeIpv6Pools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIpv6PoolsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIpv6PoolsResponse_ipv6Pools
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeIpv6Pools_nextToken
          Lens..~ rs
          Lens.^? describeIpv6PoolsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeIpv6Pools where
  type
    AWSResponse DescribeIpv6Pools =
      DescribeIpv6PoolsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIpv6PoolsResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "ipv6PoolSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIpv6Pools where
  hashWithSalt _salt DescribeIpv6Pools' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` poolIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeIpv6Pools where
  rnf DescribeIpv6Pools' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf poolIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeIpv6Pools where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeIpv6Pools where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeIpv6Pools where
  toQuery DescribeIpv6Pools' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeIpv6Pools" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "PoolId" Prelude.<$> poolIds),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeIpv6PoolsResponse' smart constructor.
data DescribeIpv6PoolsResponse = DescribeIpv6PoolsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv6 address pools.
    ipv6Pools :: Prelude.Maybe [Ipv6Pool],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpv6PoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeIpv6PoolsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'ipv6Pools', 'describeIpv6PoolsResponse_ipv6Pools' - Information about the IPv6 address pools.
--
-- 'httpStatus', 'describeIpv6PoolsResponse_httpStatus' - The response's http status code.
newDescribeIpv6PoolsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIpv6PoolsResponse
newDescribeIpv6PoolsResponse pHttpStatus_ =
  DescribeIpv6PoolsResponse'
    { nextToken =
        Prelude.Nothing,
      ipv6Pools = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeIpv6PoolsResponse_nextToken :: Lens.Lens' DescribeIpv6PoolsResponse (Prelude.Maybe Prelude.Text)
describeIpv6PoolsResponse_nextToken = Lens.lens (\DescribeIpv6PoolsResponse' {nextToken} -> nextToken) (\s@DescribeIpv6PoolsResponse' {} a -> s {nextToken = a} :: DescribeIpv6PoolsResponse)

-- | Information about the IPv6 address pools.
describeIpv6PoolsResponse_ipv6Pools :: Lens.Lens' DescribeIpv6PoolsResponse (Prelude.Maybe [Ipv6Pool])
describeIpv6PoolsResponse_ipv6Pools = Lens.lens (\DescribeIpv6PoolsResponse' {ipv6Pools} -> ipv6Pools) (\s@DescribeIpv6PoolsResponse' {} a -> s {ipv6Pools = a} :: DescribeIpv6PoolsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeIpv6PoolsResponse_httpStatus :: Lens.Lens' DescribeIpv6PoolsResponse Prelude.Int
describeIpv6PoolsResponse_httpStatus = Lens.lens (\DescribeIpv6PoolsResponse' {httpStatus} -> httpStatus) (\s@DescribeIpv6PoolsResponse' {} a -> s {httpStatus = a} :: DescribeIpv6PoolsResponse)

instance Prelude.NFData DescribeIpv6PoolsResponse where
  rnf DescribeIpv6PoolsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ipv6Pools
      `Prelude.seq` Prelude.rnf httpStatus
