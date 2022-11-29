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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeIpamScopes_nextToken,
    describeIpamScopes_filters,
    describeIpamScopes_dryRun,
    describeIpamScopes_maxResults,
    describeIpamScopes_ipamScopeIds,

    -- * Destructuring the Response
    DescribeIpamScopesResponse (..),
    newDescribeIpamScopesResponse,

    -- * Response Lenses
    describeIpamScopesResponse_nextToken,
    describeIpamScopesResponse_ipamScopes,
    describeIpamScopesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIpamScopes' smart constructor.
data DescribeIpamScopes = DescribeIpamScopes'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters for the request. For more information about
    -- filtering, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
    filters :: Prelude.Maybe [Filter],
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IDs of the scopes you want information on.
    ipamScopeIds :: Prelude.Maybe [Prelude.Text]
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
-- 'nextToken', 'describeIpamScopes_nextToken' - The token for the next page of results.
--
-- 'filters', 'describeIpamScopes_filters' - One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
--
-- 'dryRun', 'describeIpamScopes_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeIpamScopes_maxResults' - The maximum number of results to return in the request.
--
-- 'ipamScopeIds', 'describeIpamScopes_ipamScopeIds' - The IDs of the scopes you want information on.
newDescribeIpamScopes ::
  DescribeIpamScopes
newDescribeIpamScopes =
  DescribeIpamScopes'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      ipamScopeIds = Prelude.Nothing
    }

-- | The token for the next page of results.
describeIpamScopes_nextToken :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe Prelude.Text)
describeIpamScopes_nextToken = Lens.lens (\DescribeIpamScopes' {nextToken} -> nextToken) (\s@DescribeIpamScopes' {} a -> s {nextToken = a} :: DescribeIpamScopes)

-- | One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
describeIpamScopes_filters :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe [Filter])
describeIpamScopes_filters = Lens.lens (\DescribeIpamScopes' {filters} -> filters) (\s@DescribeIpamScopes' {} a -> s {filters = a} :: DescribeIpamScopes) Prelude.. Lens.mapping Lens.coerced

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
describeIpamScopes_dryRun :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe Prelude.Bool)
describeIpamScopes_dryRun = Lens.lens (\DescribeIpamScopes' {dryRun} -> dryRun) (\s@DescribeIpamScopes' {} a -> s {dryRun = a} :: DescribeIpamScopes)

-- | The maximum number of results to return in the request.
describeIpamScopes_maxResults :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe Prelude.Natural)
describeIpamScopes_maxResults = Lens.lens (\DescribeIpamScopes' {maxResults} -> maxResults) (\s@DescribeIpamScopes' {} a -> s {maxResults = a} :: DescribeIpamScopes)

-- | The IDs of the scopes you want information on.
describeIpamScopes_ipamScopeIds :: Lens.Lens' DescribeIpamScopes (Prelude.Maybe [Prelude.Text])
describeIpamScopes_ipamScopeIds = Lens.lens (\DescribeIpamScopes' {ipamScopeIds} -> ipamScopeIds) (\s@DescribeIpamScopes' {} a -> s {ipamScopeIds = a} :: DescribeIpamScopes) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "ipamScopeSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIpamScopes where
  hashWithSalt _salt DescribeIpamScopes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` ipamScopeIds

instance Prelude.NFData DescribeIpamScopes where
  rnf DescribeIpamScopes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf ipamScopeIds

instance Core.ToHeaders DescribeIpamScopes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeIpamScopes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeIpamScopes where
  toQuery DescribeIpamScopes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeIpamScopes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "IpamScopeId"
              Prelude.<$> ipamScopeIds
          )
      ]

-- | /See:/ 'newDescribeIpamScopesResponse' smart constructor.
data DescribeIpamScopesResponse = DescribeIpamScopesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The scopes you want information on.
    ipamScopes :: Prelude.Maybe [IpamScope],
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
-- 'nextToken', 'describeIpamScopesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'ipamScopes', 'describeIpamScopesResponse_ipamScopes' - The scopes you want information on.
--
-- 'httpStatus', 'describeIpamScopesResponse_httpStatus' - The response's http status code.
newDescribeIpamScopesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIpamScopesResponse
newDescribeIpamScopesResponse pHttpStatus_ =
  DescribeIpamScopesResponse'
    { nextToken =
        Prelude.Nothing,
      ipamScopes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeIpamScopesResponse_nextToken :: Lens.Lens' DescribeIpamScopesResponse (Prelude.Maybe Prelude.Text)
describeIpamScopesResponse_nextToken = Lens.lens (\DescribeIpamScopesResponse' {nextToken} -> nextToken) (\s@DescribeIpamScopesResponse' {} a -> s {nextToken = a} :: DescribeIpamScopesResponse)

-- | The scopes you want information on.
describeIpamScopesResponse_ipamScopes :: Lens.Lens' DescribeIpamScopesResponse (Prelude.Maybe [IpamScope])
describeIpamScopesResponse_ipamScopes = Lens.lens (\DescribeIpamScopesResponse' {ipamScopes} -> ipamScopes) (\s@DescribeIpamScopesResponse' {} a -> s {ipamScopes = a} :: DescribeIpamScopesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeIpamScopesResponse_httpStatus :: Lens.Lens' DescribeIpamScopesResponse Prelude.Int
describeIpamScopesResponse_httpStatus = Lens.lens (\DescribeIpamScopesResponse' {httpStatus} -> httpStatus) (\s@DescribeIpamScopesResponse' {} a -> s {httpStatus = a} :: DescribeIpamScopesResponse)

instance Prelude.NFData DescribeIpamScopesResponse where
  rnf DescribeIpamScopesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ipamScopes
      `Prelude.seq` Prelude.rnf httpStatus
