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
-- Module      : Amazonka.EC2.DescribePublicIpv4Pools
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified IPv4 address pools.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribePublicIpv4Pools
  ( -- * Creating a Request
    DescribePublicIpv4Pools (..),
    newDescribePublicIpv4Pools,

    -- * Request Lenses
    describePublicIpv4Pools_nextToken,
    describePublicIpv4Pools_poolIds,
    describePublicIpv4Pools_filters,
    describePublicIpv4Pools_maxResults,

    -- * Destructuring the Response
    DescribePublicIpv4PoolsResponse (..),
    newDescribePublicIpv4PoolsResponse,

    -- * Response Lenses
    describePublicIpv4PoolsResponse_nextToken,
    describePublicIpv4PoolsResponse_publicIpv4Pools,
    describePublicIpv4PoolsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePublicIpv4Pools' smart constructor.
data DescribePublicIpv4Pools = DescribePublicIpv4Pools'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the address pools.
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
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'poolIds', 'describePublicIpv4Pools_poolIds' - The IDs of the address pools.
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
-- 'maxResults', 'describePublicIpv4Pools_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribePublicIpv4Pools ::
  DescribePublicIpv4Pools
newDescribePublicIpv4Pools =
  DescribePublicIpv4Pools'
    { nextToken =
        Prelude.Nothing,
      poolIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
describePublicIpv4Pools_nextToken :: Lens.Lens' DescribePublicIpv4Pools (Prelude.Maybe Prelude.Text)
describePublicIpv4Pools_nextToken = Lens.lens (\DescribePublicIpv4Pools' {nextToken} -> nextToken) (\s@DescribePublicIpv4Pools' {} a -> s {nextToken = a} :: DescribePublicIpv4Pools)

-- | The IDs of the address pools.
describePublicIpv4Pools_poolIds :: Lens.Lens' DescribePublicIpv4Pools (Prelude.Maybe [Prelude.Text])
describePublicIpv4Pools_poolIds = Lens.lens (\DescribePublicIpv4Pools' {poolIds} -> poolIds) (\s@DescribePublicIpv4Pools' {} a -> s {poolIds = a} :: DescribePublicIpv4Pools) Prelude.. Lens.mapping Lens.coerced

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
describePublicIpv4Pools_filters :: Lens.Lens' DescribePublicIpv4Pools (Prelude.Maybe [Filter])
describePublicIpv4Pools_filters = Lens.lens (\DescribePublicIpv4Pools' {filters} -> filters) (\s@DescribePublicIpv4Pools' {} a -> s {filters = a} :: DescribePublicIpv4Pools) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describePublicIpv4Pools_maxResults :: Lens.Lens' DescribePublicIpv4Pools (Prelude.Maybe Prelude.Natural)
describePublicIpv4Pools_maxResults = Lens.lens (\DescribePublicIpv4Pools' {maxResults} -> maxResults) (\s@DescribePublicIpv4Pools' {} a -> s {maxResults = a} :: DescribePublicIpv4Pools)

instance Core.AWSPager DescribePublicIpv4Pools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePublicIpv4PoolsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePublicIpv4PoolsResponse_publicIpv4Pools
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePublicIpv4Pools_nextToken
          Lens..~ rs
          Lens.^? describePublicIpv4PoolsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribePublicIpv4Pools where
  type
    AWSResponse DescribePublicIpv4Pools =
      DescribePublicIpv4PoolsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePublicIpv4PoolsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "publicIpv4PoolSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePublicIpv4Pools where
  hashWithSalt _salt DescribePublicIpv4Pools' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` poolIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribePublicIpv4Pools where
  rnf DescribePublicIpv4Pools' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf poolIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribePublicIpv4Pools where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribePublicIpv4Pools where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePublicIpv4Pools where
  toQuery DescribePublicIpv4Pools' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribePublicIpv4Pools" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "PoolId" Prelude.<$> poolIds),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newDescribePublicIpv4PoolsResponse' smart constructor.
data DescribePublicIpv4PoolsResponse = DescribePublicIpv4PoolsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the address pools.
    publicIpv4Pools :: Prelude.Maybe [PublicIpv4Pool],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribePublicIpv4PoolsResponse
newDescribePublicIpv4PoolsResponse pHttpStatus_ =
  DescribePublicIpv4PoolsResponse'
    { nextToken =
        Prelude.Nothing,
      publicIpv4Pools = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describePublicIpv4PoolsResponse_nextToken :: Lens.Lens' DescribePublicIpv4PoolsResponse (Prelude.Maybe Prelude.Text)
describePublicIpv4PoolsResponse_nextToken = Lens.lens (\DescribePublicIpv4PoolsResponse' {nextToken} -> nextToken) (\s@DescribePublicIpv4PoolsResponse' {} a -> s {nextToken = a} :: DescribePublicIpv4PoolsResponse)

-- | Information about the address pools.
describePublicIpv4PoolsResponse_publicIpv4Pools :: Lens.Lens' DescribePublicIpv4PoolsResponse (Prelude.Maybe [PublicIpv4Pool])
describePublicIpv4PoolsResponse_publicIpv4Pools = Lens.lens (\DescribePublicIpv4PoolsResponse' {publicIpv4Pools} -> publicIpv4Pools) (\s@DescribePublicIpv4PoolsResponse' {} a -> s {publicIpv4Pools = a} :: DescribePublicIpv4PoolsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePublicIpv4PoolsResponse_httpStatus :: Lens.Lens' DescribePublicIpv4PoolsResponse Prelude.Int
describePublicIpv4PoolsResponse_httpStatus = Lens.lens (\DescribePublicIpv4PoolsResponse' {httpStatus} -> httpStatus) (\s@DescribePublicIpv4PoolsResponse' {} a -> s {httpStatus = a} :: DescribePublicIpv4PoolsResponse)

instance
  Prelude.NFData
    DescribePublicIpv4PoolsResponse
  where
  rnf DescribePublicIpv4PoolsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf publicIpv4Pools
      `Prelude.seq` Prelude.rnf httpStatus
