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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeIpv6Pools_dryRun,
    describeIpv6Pools_filters,
    describeIpv6Pools_maxResults,
    describeIpv6Pools_nextToken,
    describeIpv6Pools_poolIds,

    -- * Destructuring the Response
    DescribeIpv6PoolsResponse (..),
    newDescribeIpv6PoolsResponse,

    -- * Response Lenses
    describeIpv6PoolsResponse_ipv6Pools,
    describeIpv6PoolsResponse_nextToken,
    describeIpv6PoolsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIpv6Pools' smart constructor.
data DescribeIpv6Pools = DescribeIpv6Pools'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the IPv6 address pools.
    poolIds :: Prelude.Maybe [Prelude.Text]
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
-- 'dryRun', 'describeIpv6Pools_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- 'maxResults', 'describeIpv6Pools_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeIpv6Pools_nextToken' - The token for the next page of results.
--
-- 'poolIds', 'describeIpv6Pools_poolIds' - The IDs of the IPv6 address pools.
newDescribeIpv6Pools ::
  DescribeIpv6Pools
newDescribeIpv6Pools =
  DescribeIpv6Pools'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      poolIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeIpv6Pools_dryRun :: Lens.Lens' DescribeIpv6Pools (Prelude.Maybe Prelude.Bool)
describeIpv6Pools_dryRun = Lens.lens (\DescribeIpv6Pools' {dryRun} -> dryRun) (\s@DescribeIpv6Pools' {} a -> s {dryRun = a} :: DescribeIpv6Pools)

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

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeIpv6Pools_maxResults :: Lens.Lens' DescribeIpv6Pools (Prelude.Maybe Prelude.Natural)
describeIpv6Pools_maxResults = Lens.lens (\DescribeIpv6Pools' {maxResults} -> maxResults) (\s@DescribeIpv6Pools' {} a -> s {maxResults = a} :: DescribeIpv6Pools)

-- | The token for the next page of results.
describeIpv6Pools_nextToken :: Lens.Lens' DescribeIpv6Pools (Prelude.Maybe Prelude.Text)
describeIpv6Pools_nextToken = Lens.lens (\DescribeIpv6Pools' {nextToken} -> nextToken) (\s@DescribeIpv6Pools' {} a -> s {nextToken = a} :: DescribeIpv6Pools)

-- | The IDs of the IPv6 address pools.
describeIpv6Pools_poolIds :: Lens.Lens' DescribeIpv6Pools (Prelude.Maybe [Prelude.Text])
describeIpv6Pools_poolIds = Lens.lens (\DescribeIpv6Pools' {poolIds} -> poolIds) (\s@DescribeIpv6Pools' {} a -> s {poolIds = a} :: DescribeIpv6Pools) Prelude.. Lens.mapping Lens.coerced

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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> ( x
                            Data..@? "ipv6PoolSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIpv6Pools where
  hashWithSalt _salt DescribeIpv6Pools' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` poolIds

instance Prelude.NFData DescribeIpv6Pools where
  rnf DescribeIpv6Pools' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf poolIds

instance Data.ToHeaders DescribeIpv6Pools where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeIpv6Pools where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIpv6Pools where
  toQuery DescribeIpv6Pools' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeIpv6Pools" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "PoolId" Prelude.<$> poolIds)
      ]

-- | /See:/ 'newDescribeIpv6PoolsResponse' smart constructor.
data DescribeIpv6PoolsResponse = DescribeIpv6PoolsResponse'
  { -- | Information about the IPv6 address pools.
    ipv6Pools :: Prelude.Maybe [Ipv6Pool],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'ipv6Pools', 'describeIpv6PoolsResponse_ipv6Pools' - Information about the IPv6 address pools.
--
-- 'nextToken', 'describeIpv6PoolsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeIpv6PoolsResponse_httpStatus' - The response's http status code.
newDescribeIpv6PoolsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIpv6PoolsResponse
newDescribeIpv6PoolsResponse pHttpStatus_ =
  DescribeIpv6PoolsResponse'
    { ipv6Pools =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPv6 address pools.
describeIpv6PoolsResponse_ipv6Pools :: Lens.Lens' DescribeIpv6PoolsResponse (Prelude.Maybe [Ipv6Pool])
describeIpv6PoolsResponse_ipv6Pools = Lens.lens (\DescribeIpv6PoolsResponse' {ipv6Pools} -> ipv6Pools) (\s@DescribeIpv6PoolsResponse' {} a -> s {ipv6Pools = a} :: DescribeIpv6PoolsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeIpv6PoolsResponse_nextToken :: Lens.Lens' DescribeIpv6PoolsResponse (Prelude.Maybe Prelude.Text)
describeIpv6PoolsResponse_nextToken = Lens.lens (\DescribeIpv6PoolsResponse' {nextToken} -> nextToken) (\s@DescribeIpv6PoolsResponse' {} a -> s {nextToken = a} :: DescribeIpv6PoolsResponse)

-- | The response's http status code.
describeIpv6PoolsResponse_httpStatus :: Lens.Lens' DescribeIpv6PoolsResponse Prelude.Int
describeIpv6PoolsResponse_httpStatus = Lens.lens (\DescribeIpv6PoolsResponse' {httpStatus} -> httpStatus) (\s@DescribeIpv6PoolsResponse' {} a -> s {httpStatus = a} :: DescribeIpv6PoolsResponse)

instance Prelude.NFData DescribeIpv6PoolsResponse where
  rnf DescribeIpv6PoolsResponse' {..} =
    Prelude.rnf ipv6Pools
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
