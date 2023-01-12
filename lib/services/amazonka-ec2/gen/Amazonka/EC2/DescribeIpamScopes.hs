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
-- Module      : Amazonka.EC2.DescribeIpamScopes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about your IPAM scopes.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeIpamScopes
  ( -- * Creating a Request
    DescribeIpamScopes (..),
    newDescribeIpamScopes,

    -- * Request Lenses
    describeIpamScopes_dryRun,
    describeIpamScopes_filters,
    describeIpamScopes_ipamScopeIds,
    describeIpamScopes_maxResults,
    describeIpamScopes_nextToken,

    -- * Destructuring the Response
    DescribeIpamScopesResponse (..),
    newDescribeIpamScopesResponse,

    -- * Response Lenses
    describeIpamScopesResponse_ipamScopes,
    describeIpamScopesResponse_nextToken,
    describeIpamScopesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIpamScopes' smart constructor.
data DescribeIpamScopes = DescribeIpamScopes'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters for the request. For more information about
    -- filtering, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the scopes you want information on.
    ipamScopeIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpamScopes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeIpamScopes_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeIpamScopes_filters' - One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
--
-- 'ipamScopeIds', 'describeIpamScopes_ipamScopeIds' - The IDs of the scopes you want information on.
--
-- 'maxResults', 'describeIpamScopes_maxResults' - The maximum number of results to return in the request.
--
-- 'nextToken', 'describeIpamScopes_nextToken' - The token for the next page of results.
newDescribeIpamScopes ::
  DescribeIpamScopes
newDescribeIpamScopes =
  DescribeIpamScopes'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      ipamScopeIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
describeIpamScopes_dryRun :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe Prelude.Bool)
describeIpamScopes_dryRun = Lens.lens (\DescribeIpamScopes' {dryRun} -> dryRun) (\s@DescribeIpamScopes' {} a -> s {dryRun = a} :: DescribeIpamScopes)

-- | One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
describeIpamScopes_filters :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe [Filter])
describeIpamScopes_filters = Lens.lens (\DescribeIpamScopes' {filters} -> filters) (\s@DescribeIpamScopes' {} a -> s {filters = a} :: DescribeIpamScopes) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the scopes you want information on.
describeIpamScopes_ipamScopeIds :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe [Prelude.Text])
describeIpamScopes_ipamScopeIds = Lens.lens (\DescribeIpamScopes' {ipamScopeIds} -> ipamScopeIds) (\s@DescribeIpamScopes' {} a -> s {ipamScopeIds = a} :: DescribeIpamScopes) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in the request.
describeIpamScopes_maxResults :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe Prelude.Natural)
describeIpamScopes_maxResults = Lens.lens (\DescribeIpamScopes' {maxResults} -> maxResults) (\s@DescribeIpamScopes' {} a -> s {maxResults = a} :: DescribeIpamScopes)

-- | The token for the next page of results.
describeIpamScopes_nextToken :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe Prelude.Text)
describeIpamScopes_nextToken = Lens.lens (\DescribeIpamScopes' {nextToken} -> nextToken) (\s@DescribeIpamScopes' {} a -> s {nextToken = a} :: DescribeIpamScopes)

instance Core.AWSPager DescribeIpamScopes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIpamScopesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIpamScopesResponse_ipamScopes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeIpamScopes_nextToken
          Lens..~ rs
          Lens.^? describeIpamScopesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeIpamScopes where
  type
    AWSResponse DescribeIpamScopes =
      DescribeIpamScopesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIpamScopesResponse'
            Prelude.<$> ( x Data..@? "ipamScopeSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIpamScopes where
  hashWithSalt _salt DescribeIpamScopes' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` ipamScopeIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeIpamScopes where
  rnf DescribeIpamScopes' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf ipamScopeIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeIpamScopes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeIpamScopes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIpamScopes where
  toQuery DescribeIpamScopes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeIpamScopes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "IpamScopeId"
              Prelude.<$> ipamScopeIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeIpamScopesResponse' smart constructor.
data DescribeIpamScopesResponse = DescribeIpamScopesResponse'
  { -- | The scopes you want information on.
    ipamScopes :: Prelude.Maybe [IpamScope],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpamScopesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamScopes', 'describeIpamScopesResponse_ipamScopes' - The scopes you want information on.
--
-- 'nextToken', 'describeIpamScopesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeIpamScopesResponse_httpStatus' - The response's http status code.
newDescribeIpamScopesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIpamScopesResponse
newDescribeIpamScopesResponse pHttpStatus_ =
  DescribeIpamScopesResponse'
    { ipamScopes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The scopes you want information on.
describeIpamScopesResponse_ipamScopes :: Lens.Lens' DescribeIpamScopesResponse (Prelude.Maybe [IpamScope])
describeIpamScopesResponse_ipamScopes = Lens.lens (\DescribeIpamScopesResponse' {ipamScopes} -> ipamScopes) (\s@DescribeIpamScopesResponse' {} a -> s {ipamScopes = a} :: DescribeIpamScopesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeIpamScopesResponse_nextToken :: Lens.Lens' DescribeIpamScopesResponse (Prelude.Maybe Prelude.Text)
describeIpamScopesResponse_nextToken = Lens.lens (\DescribeIpamScopesResponse' {nextToken} -> nextToken) (\s@DescribeIpamScopesResponse' {} a -> s {nextToken = a} :: DescribeIpamScopesResponse)

-- | The response's http status code.
describeIpamScopesResponse_httpStatus :: Lens.Lens' DescribeIpamScopesResponse Prelude.Int
describeIpamScopesResponse_httpStatus = Lens.lens (\DescribeIpamScopesResponse' {httpStatus} -> httpStatus) (\s@DescribeIpamScopesResponse' {} a -> s {httpStatus = a} :: DescribeIpamScopesResponse)

instance Prelude.NFData DescribeIpamScopesResponse where
  rnf DescribeIpamScopesResponse' {..} =
    Prelude.rnf ipamScopes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
