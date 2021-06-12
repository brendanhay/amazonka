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
-- Module      : Network.AWS.EC2.DescribePublicIpv4Pools
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified IPv4 address pools.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePublicIpv4Pools
  ( -- * Creating a Request
    DescribePublicIpv4Pools (..),
    newDescribePublicIpv4Pools,

    -- * Request Lenses
    describePublicIpv4Pools_nextToken,
    describePublicIpv4Pools_maxResults,
    describePublicIpv4Pools_filters,
    describePublicIpv4Pools_poolIds,

    -- * Destructuring the Response
    DescribePublicIpv4PoolsResponse (..),
    newDescribePublicIpv4PoolsResponse,

    -- * Response Lenses
    describePublicIpv4PoolsResponse_nextToken,
    describePublicIpv4PoolsResponse_publicIpv4Pools,
    describePublicIpv4PoolsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePublicIpv4Pools' smart constructor.
data DescribePublicIpv4Pools = DescribePublicIpv4Pools'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
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
    filters :: Core.Maybe [Filter],
    -- | The IDs of the address pools.
    poolIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePublicIpv4Pools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePublicIpv4Pools_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'describePublicIpv4Pools_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describePublicIpv4Pools_filters' - One or more filters.
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
-- 'poolIds', 'describePublicIpv4Pools_poolIds' - The IDs of the address pools.
newDescribePublicIpv4Pools ::
  DescribePublicIpv4Pools
newDescribePublicIpv4Pools =
  DescribePublicIpv4Pools'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      poolIds = Core.Nothing
    }

-- | The token for the next page of results.
describePublicIpv4Pools_nextToken :: Lens.Lens' DescribePublicIpv4Pools (Core.Maybe Core.Text)
describePublicIpv4Pools_nextToken = Lens.lens (\DescribePublicIpv4Pools' {nextToken} -> nextToken) (\s@DescribePublicIpv4Pools' {} a -> s {nextToken = a} :: DescribePublicIpv4Pools)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describePublicIpv4Pools_maxResults :: Lens.Lens' DescribePublicIpv4Pools (Core.Maybe Core.Natural)
describePublicIpv4Pools_maxResults = Lens.lens (\DescribePublicIpv4Pools' {maxResults} -> maxResults) (\s@DescribePublicIpv4Pools' {} a -> s {maxResults = a} :: DescribePublicIpv4Pools)

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
describePublicIpv4Pools_filters :: Lens.Lens' DescribePublicIpv4Pools (Core.Maybe [Filter])
describePublicIpv4Pools_filters = Lens.lens (\DescribePublicIpv4Pools' {filters} -> filters) (\s@DescribePublicIpv4Pools' {} a -> s {filters = a} :: DescribePublicIpv4Pools) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the address pools.
describePublicIpv4Pools_poolIds :: Lens.Lens' DescribePublicIpv4Pools (Core.Maybe [Core.Text])
describePublicIpv4Pools_poolIds = Lens.lens (\DescribePublicIpv4Pools' {poolIds} -> poolIds) (\s@DescribePublicIpv4Pools' {} a -> s {poolIds = a} :: DescribePublicIpv4Pools) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribePublicIpv4Pools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePublicIpv4PoolsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describePublicIpv4PoolsResponse_publicIpv4Pools
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describePublicIpv4Pools_nextToken
          Lens..~ rs
          Lens.^? describePublicIpv4PoolsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribePublicIpv4Pools where
  type
    AWSResponse DescribePublicIpv4Pools =
      DescribePublicIpv4PoolsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePublicIpv4PoolsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "publicIpv4PoolSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePublicIpv4Pools

instance Core.NFData DescribePublicIpv4Pools

instance Core.ToHeaders DescribePublicIpv4Pools where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribePublicIpv4Pools where
  toPath = Core.const "/"

instance Core.ToQuery DescribePublicIpv4Pools where
  toQuery DescribePublicIpv4Pools' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribePublicIpv4Pools" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        Core.toQuery
          (Core.toQueryList "PoolId" Core.<$> poolIds)
      ]

-- | /See:/ 'newDescribePublicIpv4PoolsResponse' smart constructor.
data DescribePublicIpv4PoolsResponse = DescribePublicIpv4PoolsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the address pools.
    publicIpv4Pools :: Core.Maybe [PublicIpv4Pool],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePublicIpv4PoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePublicIpv4PoolsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'publicIpv4Pools', 'describePublicIpv4PoolsResponse_publicIpv4Pools' - Information about the address pools.
--
-- 'httpStatus', 'describePublicIpv4PoolsResponse_httpStatus' - The response's http status code.
newDescribePublicIpv4PoolsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePublicIpv4PoolsResponse
newDescribePublicIpv4PoolsResponse pHttpStatus_ =
  DescribePublicIpv4PoolsResponse'
    { nextToken =
        Core.Nothing,
      publicIpv4Pools = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describePublicIpv4PoolsResponse_nextToken :: Lens.Lens' DescribePublicIpv4PoolsResponse (Core.Maybe Core.Text)
describePublicIpv4PoolsResponse_nextToken = Lens.lens (\DescribePublicIpv4PoolsResponse' {nextToken} -> nextToken) (\s@DescribePublicIpv4PoolsResponse' {} a -> s {nextToken = a} :: DescribePublicIpv4PoolsResponse)

-- | Information about the address pools.
describePublicIpv4PoolsResponse_publicIpv4Pools :: Lens.Lens' DescribePublicIpv4PoolsResponse (Core.Maybe [PublicIpv4Pool])
describePublicIpv4PoolsResponse_publicIpv4Pools = Lens.lens (\DescribePublicIpv4PoolsResponse' {publicIpv4Pools} -> publicIpv4Pools) (\s@DescribePublicIpv4PoolsResponse' {} a -> s {publicIpv4Pools = a} :: DescribePublicIpv4PoolsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePublicIpv4PoolsResponse_httpStatus :: Lens.Lens' DescribePublicIpv4PoolsResponse Core.Int
describePublicIpv4PoolsResponse_httpStatus = Lens.lens (\DescribePublicIpv4PoolsResponse' {httpStatus} -> httpStatus) (\s@DescribePublicIpv4PoolsResponse' {} a -> s {httpStatus = a} :: DescribePublicIpv4PoolsResponse)

instance Core.NFData DescribePublicIpv4PoolsResponse
