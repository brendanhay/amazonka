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
-- Module      : Amazonka.EC2.DescribeNetworkInsightsAccessScopes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Network Access Scopes.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeNetworkInsightsAccessScopes
  ( -- * Creating a Request
    DescribeNetworkInsightsAccessScopes (..),
    newDescribeNetworkInsightsAccessScopes,

    -- * Request Lenses
    describeNetworkInsightsAccessScopes_dryRun,
    describeNetworkInsightsAccessScopes_filters,
    describeNetworkInsightsAccessScopes_maxResults,
    describeNetworkInsightsAccessScopes_networkInsightsAccessScopeIds,
    describeNetworkInsightsAccessScopes_nextToken,

    -- * Destructuring the Response
    DescribeNetworkInsightsAccessScopesResponse (..),
    newDescribeNetworkInsightsAccessScopesResponse,

    -- * Response Lenses
    describeNetworkInsightsAccessScopesResponse_networkInsightsAccessScopes,
    describeNetworkInsightsAccessScopesResponse_nextToken,
    describeNetworkInsightsAccessScopesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNetworkInsightsAccessScopes' smart constructor.
data DescribeNetworkInsightsAccessScopes = DescribeNetworkInsightsAccessScopes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | There are no supported filters.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IDs of the Network Access Scopes.
    networkInsightsAccessScopeIds :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsAccessScopes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeNetworkInsightsAccessScopes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeNetworkInsightsAccessScopes_filters' - There are no supported filters.
--
-- 'maxResults', 'describeNetworkInsightsAccessScopes_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'networkInsightsAccessScopeIds', 'describeNetworkInsightsAccessScopes_networkInsightsAccessScopeIds' - The IDs of the Network Access Scopes.
--
-- 'nextToken', 'describeNetworkInsightsAccessScopes_nextToken' - The token for the next page of results.
newDescribeNetworkInsightsAccessScopes ::
  DescribeNetworkInsightsAccessScopes
newDescribeNetworkInsightsAccessScopes =
  DescribeNetworkInsightsAccessScopes'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      networkInsightsAccessScopeIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkInsightsAccessScopes_dryRun :: Lens.Lens' DescribeNetworkInsightsAccessScopes (Prelude.Maybe Prelude.Bool)
describeNetworkInsightsAccessScopes_dryRun = Lens.lens (\DescribeNetworkInsightsAccessScopes' {dryRun} -> dryRun) (\s@DescribeNetworkInsightsAccessScopes' {} a -> s {dryRun = a} :: DescribeNetworkInsightsAccessScopes)

-- | There are no supported filters.
describeNetworkInsightsAccessScopes_filters :: Lens.Lens' DescribeNetworkInsightsAccessScopes (Prelude.Maybe [Filter])
describeNetworkInsightsAccessScopes_filters = Lens.lens (\DescribeNetworkInsightsAccessScopes' {filters} -> filters) (\s@DescribeNetworkInsightsAccessScopes' {} a -> s {filters = a} :: DescribeNetworkInsightsAccessScopes) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeNetworkInsightsAccessScopes_maxResults :: Lens.Lens' DescribeNetworkInsightsAccessScopes (Prelude.Maybe Prelude.Natural)
describeNetworkInsightsAccessScopes_maxResults = Lens.lens (\DescribeNetworkInsightsAccessScopes' {maxResults} -> maxResults) (\s@DescribeNetworkInsightsAccessScopes' {} a -> s {maxResults = a} :: DescribeNetworkInsightsAccessScopes)

-- | The IDs of the Network Access Scopes.
describeNetworkInsightsAccessScopes_networkInsightsAccessScopeIds :: Lens.Lens' DescribeNetworkInsightsAccessScopes (Prelude.Maybe [Prelude.Text])
describeNetworkInsightsAccessScopes_networkInsightsAccessScopeIds = Lens.lens (\DescribeNetworkInsightsAccessScopes' {networkInsightsAccessScopeIds} -> networkInsightsAccessScopeIds) (\s@DescribeNetworkInsightsAccessScopes' {} a -> s {networkInsightsAccessScopeIds = a} :: DescribeNetworkInsightsAccessScopes) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeNetworkInsightsAccessScopes_nextToken :: Lens.Lens' DescribeNetworkInsightsAccessScopes (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAccessScopes_nextToken = Lens.lens (\DescribeNetworkInsightsAccessScopes' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsAccessScopes' {} a -> s {nextToken = a} :: DescribeNetworkInsightsAccessScopes)

instance
  Core.AWSPager
    DescribeNetworkInsightsAccessScopes
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNetworkInsightsAccessScopesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNetworkInsightsAccessScopesResponse_networkInsightsAccessScopes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeNetworkInsightsAccessScopes_nextToken
          Lens..~ rs
          Lens.^? describeNetworkInsightsAccessScopesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeNetworkInsightsAccessScopes
  where
  type
    AWSResponse DescribeNetworkInsightsAccessScopes =
      DescribeNetworkInsightsAccessScopesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInsightsAccessScopesResponse'
            Prelude.<$> ( x Data..@? "networkInsightsAccessScopeSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
              Prelude.<*> (x Data..@? "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNetworkInsightsAccessScopes
  where
  hashWithSalt
    _salt
    DescribeNetworkInsightsAccessScopes' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` networkInsightsAccessScopeIds
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeNetworkInsightsAccessScopes
  where
  rnf DescribeNetworkInsightsAccessScopes' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeIds
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeNetworkInsightsAccessScopes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeNetworkInsightsAccessScopes
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeNetworkInsightsAccessScopes
  where
  toQuery DescribeNetworkInsightsAccessScopes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeNetworkInsightsAccessScopes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        Data.toQuery
          ( Data.toQueryList "NetworkInsightsAccessScopeId"
              Prelude.<$> networkInsightsAccessScopeIds
          ),
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeNetworkInsightsAccessScopesResponse' smart constructor.
data DescribeNetworkInsightsAccessScopesResponse = DescribeNetworkInsightsAccessScopesResponse'
  { -- | The Network Access Scopes.
    networkInsightsAccessScopes :: Prelude.Maybe [NetworkInsightsAccessScope],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInsightsAccessScopesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAccessScopes', 'describeNetworkInsightsAccessScopesResponse_networkInsightsAccessScopes' - The Network Access Scopes.
--
-- 'nextToken', 'describeNetworkInsightsAccessScopesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeNetworkInsightsAccessScopesResponse_httpStatus' - The response's http status code.
newDescribeNetworkInsightsAccessScopesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNetworkInsightsAccessScopesResponse
newDescribeNetworkInsightsAccessScopesResponse
  pHttpStatus_ =
    DescribeNetworkInsightsAccessScopesResponse'
      { networkInsightsAccessScopes =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Network Access Scopes.
describeNetworkInsightsAccessScopesResponse_networkInsightsAccessScopes :: Lens.Lens' DescribeNetworkInsightsAccessScopesResponse (Prelude.Maybe [NetworkInsightsAccessScope])
describeNetworkInsightsAccessScopesResponse_networkInsightsAccessScopes = Lens.lens (\DescribeNetworkInsightsAccessScopesResponse' {networkInsightsAccessScopes} -> networkInsightsAccessScopes) (\s@DescribeNetworkInsightsAccessScopesResponse' {} a -> s {networkInsightsAccessScopes = a} :: DescribeNetworkInsightsAccessScopesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNetworkInsightsAccessScopesResponse_nextToken :: Lens.Lens' DescribeNetworkInsightsAccessScopesResponse (Prelude.Maybe Prelude.Text)
describeNetworkInsightsAccessScopesResponse_nextToken = Lens.lens (\DescribeNetworkInsightsAccessScopesResponse' {nextToken} -> nextToken) (\s@DescribeNetworkInsightsAccessScopesResponse' {} a -> s {nextToken = a} :: DescribeNetworkInsightsAccessScopesResponse)

-- | The response's http status code.
describeNetworkInsightsAccessScopesResponse_httpStatus :: Lens.Lens' DescribeNetworkInsightsAccessScopesResponse Prelude.Int
describeNetworkInsightsAccessScopesResponse_httpStatus = Lens.lens (\DescribeNetworkInsightsAccessScopesResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInsightsAccessScopesResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInsightsAccessScopesResponse)

instance
  Prelude.NFData
    DescribeNetworkInsightsAccessScopesResponse
  where
  rnf DescribeNetworkInsightsAccessScopesResponse' {..} =
    Prelude.rnf networkInsightsAccessScopes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
