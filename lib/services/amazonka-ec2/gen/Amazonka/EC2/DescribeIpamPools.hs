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
-- Module      : Amazonka.EC2.DescribeIpamPools
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about your IPAM pools.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeIpamPools
  ( -- * Creating a Request
    DescribeIpamPools (..),
    newDescribeIpamPools,

    -- * Request Lenses
    describeIpamPools_dryRun,
    describeIpamPools_filters,
    describeIpamPools_ipamPoolIds,
    describeIpamPools_maxResults,
    describeIpamPools_nextToken,

    -- * Destructuring the Response
    DescribeIpamPoolsResponse (..),
    newDescribeIpamPoolsResponse,

    -- * Response Lenses
    describeIpamPoolsResponse_ipamPools,
    describeIpamPoolsResponse_nextToken,
    describeIpamPoolsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIpamPools' smart constructor.
data DescribeIpamPools = DescribeIpamPools'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters for the request. For more information about
    -- filtering, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the IPAM pools you would like information on.
    ipamPoolIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpamPools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeIpamPools_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeIpamPools_filters' - One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
--
-- 'ipamPoolIds', 'describeIpamPools_ipamPoolIds' - The IDs of the IPAM pools you would like information on.
--
-- 'maxResults', 'describeIpamPools_maxResults' - The maximum number of results to return in the request.
--
-- 'nextToken', 'describeIpamPools_nextToken' - The token for the next page of results.
newDescribeIpamPools ::
  DescribeIpamPools
newDescribeIpamPools =
  DescribeIpamPools'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      ipamPoolIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
describeIpamPools_dryRun :: Lens.Lens' DescribeIpamPools (Prelude.Maybe Prelude.Bool)
describeIpamPools_dryRun = Lens.lens (\DescribeIpamPools' {dryRun} -> dryRun) (\s@DescribeIpamPools' {} a -> s {dryRun = a} :: DescribeIpamPools)

-- | One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
describeIpamPools_filters :: Lens.Lens' DescribeIpamPools (Prelude.Maybe [Filter])
describeIpamPools_filters = Lens.lens (\DescribeIpamPools' {filters} -> filters) (\s@DescribeIpamPools' {} a -> s {filters = a} :: DescribeIpamPools) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the IPAM pools you would like information on.
describeIpamPools_ipamPoolIds :: Lens.Lens' DescribeIpamPools (Prelude.Maybe [Prelude.Text])
describeIpamPools_ipamPoolIds = Lens.lens (\DescribeIpamPools' {ipamPoolIds} -> ipamPoolIds) (\s@DescribeIpamPools' {} a -> s {ipamPoolIds = a} :: DescribeIpamPools) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in the request.
describeIpamPools_maxResults :: Lens.Lens' DescribeIpamPools (Prelude.Maybe Prelude.Natural)
describeIpamPools_maxResults = Lens.lens (\DescribeIpamPools' {maxResults} -> maxResults) (\s@DescribeIpamPools' {} a -> s {maxResults = a} :: DescribeIpamPools)

-- | The token for the next page of results.
describeIpamPools_nextToken :: Lens.Lens' DescribeIpamPools (Prelude.Maybe Prelude.Text)
describeIpamPools_nextToken = Lens.lens (\DescribeIpamPools' {nextToken} -> nextToken) (\s@DescribeIpamPools' {} a -> s {nextToken = a} :: DescribeIpamPools)

instance Core.AWSPager DescribeIpamPools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIpamPoolsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIpamPoolsResponse_ipamPools
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeIpamPools_nextToken
          Lens..~ rs
          Lens.^? describeIpamPoolsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeIpamPools where
  type
    AWSResponse DescribeIpamPools =
      DescribeIpamPoolsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIpamPoolsResponse'
            Prelude.<$> ( x
                            Data..@? "ipamPoolSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIpamPools where
  hashWithSalt _salt DescribeIpamPools' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` ipamPoolIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeIpamPools where
  rnf DescribeIpamPools' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf ipamPoolIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeIpamPools where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeIpamPools where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIpamPools where
  toQuery DescribeIpamPools' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeIpamPools" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "IpamPoolId"
              Prelude.<$> ipamPoolIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeIpamPoolsResponse' smart constructor.
data DescribeIpamPoolsResponse = DescribeIpamPoolsResponse'
  { -- | Information about the IPAM pools.
    ipamPools :: Prelude.Maybe [IpamPool],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpamPoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamPools', 'describeIpamPoolsResponse_ipamPools' - Information about the IPAM pools.
--
-- 'nextToken', 'describeIpamPoolsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeIpamPoolsResponse_httpStatus' - The response's http status code.
newDescribeIpamPoolsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIpamPoolsResponse
newDescribeIpamPoolsResponse pHttpStatus_ =
  DescribeIpamPoolsResponse'
    { ipamPools =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPAM pools.
describeIpamPoolsResponse_ipamPools :: Lens.Lens' DescribeIpamPoolsResponse (Prelude.Maybe [IpamPool])
describeIpamPoolsResponse_ipamPools = Lens.lens (\DescribeIpamPoolsResponse' {ipamPools} -> ipamPools) (\s@DescribeIpamPoolsResponse' {} a -> s {ipamPools = a} :: DescribeIpamPoolsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeIpamPoolsResponse_nextToken :: Lens.Lens' DescribeIpamPoolsResponse (Prelude.Maybe Prelude.Text)
describeIpamPoolsResponse_nextToken = Lens.lens (\DescribeIpamPoolsResponse' {nextToken} -> nextToken) (\s@DescribeIpamPoolsResponse' {} a -> s {nextToken = a} :: DescribeIpamPoolsResponse)

-- | The response's http status code.
describeIpamPoolsResponse_httpStatus :: Lens.Lens' DescribeIpamPoolsResponse Prelude.Int
describeIpamPoolsResponse_httpStatus = Lens.lens (\DescribeIpamPoolsResponse' {httpStatus} -> httpStatus) (\s@DescribeIpamPoolsResponse' {} a -> s {httpStatus = a} :: DescribeIpamPoolsResponse)

instance Prelude.NFData DescribeIpamPoolsResponse where
  rnf DescribeIpamPoolsResponse' {..} =
    Prelude.rnf ipamPools
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
