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
-- Module      : Amazonka.EC2.DescribeIpams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about your IPAM pools.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/what-is-it-ipam.html What is IPAM?>
-- in the /Amazon VPC IPAM User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeIpams
  ( -- * Creating a Request
    DescribeIpams (..),
    newDescribeIpams,

    -- * Request Lenses
    describeIpams_dryRun,
    describeIpams_filters,
    describeIpams_ipamIds,
    describeIpams_maxResults,
    describeIpams_nextToken,

    -- * Destructuring the Response
    DescribeIpamsResponse (..),
    newDescribeIpamsResponse,

    -- * Response Lenses
    describeIpamsResponse_ipams,
    describeIpamsResponse_nextToken,
    describeIpamsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIpams' smart constructor.
data DescribeIpams = DescribeIpams'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters for the request. For more information about
    -- filtering, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the IPAMs you want information on.
    ipamIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeIpams_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeIpams_filters' - One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
--
-- 'ipamIds', 'describeIpams_ipamIds' - The IDs of the IPAMs you want information on.
--
-- 'maxResults', 'describeIpams_maxResults' - The maximum number of results to return in the request.
--
-- 'nextToken', 'describeIpams_nextToken' - The token for the next page of results.
newDescribeIpams ::
  DescribeIpams
newDescribeIpams =
  DescribeIpams'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      ipamIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
describeIpams_dryRun :: Lens.Lens' DescribeIpams (Prelude.Maybe Prelude.Bool)
describeIpams_dryRun = Lens.lens (\DescribeIpams' {dryRun} -> dryRun) (\s@DescribeIpams' {} a -> s {dryRun = a} :: DescribeIpams)

-- | One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
describeIpams_filters :: Lens.Lens' DescribeIpams (Prelude.Maybe [Filter])
describeIpams_filters = Lens.lens (\DescribeIpams' {filters} -> filters) (\s@DescribeIpams' {} a -> s {filters = a} :: DescribeIpams) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the IPAMs you want information on.
describeIpams_ipamIds :: Lens.Lens' DescribeIpams (Prelude.Maybe [Prelude.Text])
describeIpams_ipamIds = Lens.lens (\DescribeIpams' {ipamIds} -> ipamIds) (\s@DescribeIpams' {} a -> s {ipamIds = a} :: DescribeIpams) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in the request.
describeIpams_maxResults :: Lens.Lens' DescribeIpams (Prelude.Maybe Prelude.Natural)
describeIpams_maxResults = Lens.lens (\DescribeIpams' {maxResults} -> maxResults) (\s@DescribeIpams' {} a -> s {maxResults = a} :: DescribeIpams)

-- | The token for the next page of results.
describeIpams_nextToken :: Lens.Lens' DescribeIpams (Prelude.Maybe Prelude.Text)
describeIpams_nextToken = Lens.lens (\DescribeIpams' {nextToken} -> nextToken) (\s@DescribeIpams' {} a -> s {nextToken = a} :: DescribeIpams)

instance Core.AWSPager DescribeIpams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIpamsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIpamsResponse_ipams
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeIpams_nextToken
          Lens..~ rs
          Lens.^? describeIpamsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeIpams where
  type
    AWSResponse DescribeIpams =
      DescribeIpamsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIpamsResponse'
            Prelude.<$> ( x
                            Data..@? "ipamSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIpams where
  hashWithSalt _salt DescribeIpams' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` ipamIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeIpams where
  rnf DescribeIpams' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf ipamIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeIpams where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeIpams where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIpams where
  toQuery DescribeIpams' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeIpams" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          (Data.toQueryList "IpamId" Prelude.<$> ipamIds),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeIpamsResponse' smart constructor.
data DescribeIpamsResponse = DescribeIpamsResponse'
  { -- | Information about the IPAMs.
    ipams :: Prelude.Maybe [Ipam],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipams', 'describeIpamsResponse_ipams' - Information about the IPAMs.
--
-- 'nextToken', 'describeIpamsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeIpamsResponse_httpStatus' - The response's http status code.
newDescribeIpamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIpamsResponse
newDescribeIpamsResponse pHttpStatus_ =
  DescribeIpamsResponse'
    { ipams = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPAMs.
describeIpamsResponse_ipams :: Lens.Lens' DescribeIpamsResponse (Prelude.Maybe [Ipam])
describeIpamsResponse_ipams = Lens.lens (\DescribeIpamsResponse' {ipams} -> ipams) (\s@DescribeIpamsResponse' {} a -> s {ipams = a} :: DescribeIpamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeIpamsResponse_nextToken :: Lens.Lens' DescribeIpamsResponse (Prelude.Maybe Prelude.Text)
describeIpamsResponse_nextToken = Lens.lens (\DescribeIpamsResponse' {nextToken} -> nextToken) (\s@DescribeIpamsResponse' {} a -> s {nextToken = a} :: DescribeIpamsResponse)

-- | The response's http status code.
describeIpamsResponse_httpStatus :: Lens.Lens' DescribeIpamsResponse Prelude.Int
describeIpamsResponse_httpStatus = Lens.lens (\DescribeIpamsResponse' {httpStatus} -> httpStatus) (\s@DescribeIpamsResponse' {} a -> s {httpStatus = a} :: DescribeIpamsResponse)

instance Prelude.NFData DescribeIpamsResponse where
  rnf DescribeIpamsResponse' {..} =
    Prelude.rnf ipams
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
