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
-- Module      : Amazonka.QuickSight.SearchDashboards
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for dashboards that belong to a user.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.SearchDashboards
  ( -- * Creating a Request
    SearchDashboards (..),
    newSearchDashboards,

    -- * Request Lenses
    searchDashboards_maxResults,
    searchDashboards_nextToken,
    searchDashboards_awsAccountId,
    searchDashboards_filters,

    -- * Destructuring the Response
    SearchDashboardsResponse (..),
    newSearchDashboardsResponse,

    -- * Response Lenses
    searchDashboardsResponse_dashboardSummaryList,
    searchDashboardsResponse_nextToken,
    searchDashboardsResponse_requestId,
    searchDashboardsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchDashboards' smart constructor.
data SearchDashboards = SearchDashboards'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the user whose
    -- dashboards you\'re searching for.
    awsAccountId :: Prelude.Text,
    -- | The filters to apply to the search. Currently, you can search only by
    -- user name, for example,
    -- @\"Filters\": [ { \"Name\": \"QUICKSIGHT_USER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\" } ]@
    filters :: Prelude.NonEmpty DashboardSearchFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDashboards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchDashboards_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'searchDashboards_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'searchDashboards_awsAccountId' - The ID of the Amazon Web Services account that contains the user whose
-- dashboards you\'re searching for.
--
-- 'filters', 'searchDashboards_filters' - The filters to apply to the search. Currently, you can search only by
-- user name, for example,
-- @\"Filters\": [ { \"Name\": \"QUICKSIGHT_USER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\" } ]@
newSearchDashboards ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'filters'
  Prelude.NonEmpty DashboardSearchFilter ->
  SearchDashboards
newSearchDashboards pAwsAccountId_ pFilters_ =
  SearchDashboards'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      filters = Lens.coerced Lens.# pFilters_
    }

-- | The maximum number of results to be returned per request.
searchDashboards_maxResults :: Lens.Lens' SearchDashboards (Prelude.Maybe Prelude.Natural)
searchDashboards_maxResults = Lens.lens (\SearchDashboards' {maxResults} -> maxResults) (\s@SearchDashboards' {} a -> s {maxResults = a} :: SearchDashboards)

-- | The token for the next set of results, or null if there are no more
-- results.
searchDashboards_nextToken :: Lens.Lens' SearchDashboards (Prelude.Maybe Prelude.Text)
searchDashboards_nextToken = Lens.lens (\SearchDashboards' {nextToken} -> nextToken) (\s@SearchDashboards' {} a -> s {nextToken = a} :: SearchDashboards)

-- | The ID of the Amazon Web Services account that contains the user whose
-- dashboards you\'re searching for.
searchDashboards_awsAccountId :: Lens.Lens' SearchDashboards Prelude.Text
searchDashboards_awsAccountId = Lens.lens (\SearchDashboards' {awsAccountId} -> awsAccountId) (\s@SearchDashboards' {} a -> s {awsAccountId = a} :: SearchDashboards)

-- | The filters to apply to the search. Currently, you can search only by
-- user name, for example,
-- @\"Filters\": [ { \"Name\": \"QUICKSIGHT_USER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\" } ]@
searchDashboards_filters :: Lens.Lens' SearchDashboards (Prelude.NonEmpty DashboardSearchFilter)
searchDashboards_filters = Lens.lens (\SearchDashboards' {filters} -> filters) (\s@SearchDashboards' {} a -> s {filters = a} :: SearchDashboards) Prelude.. Lens.coerced

instance Core.AWSPager SearchDashboards where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchDashboardsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchDashboardsResponse_dashboardSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchDashboards_nextToken
          Lens..~ rs
          Lens.^? searchDashboardsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchDashboards where
  type
    AWSResponse SearchDashboards =
      SearchDashboardsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchDashboardsResponse'
            Prelude.<$> ( x
                            Data..?> "DashboardSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchDashboards where
  hashWithSalt _salt SearchDashboards' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchDashboards where
  rnf SearchDashboards' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders SearchDashboards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchDashboards where
  toJSON SearchDashboards' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Filters" Data..= filters)
          ]
      )

instance Data.ToPath SearchDashboards where
  toPath SearchDashboards' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/search/dashboards"
      ]

instance Data.ToQuery SearchDashboards where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchDashboardsResponse' smart constructor.
data SearchDashboardsResponse = SearchDashboardsResponse'
  { -- | The list of dashboards owned by the user specified in @Filters@ in your
    -- request.
    dashboardSummaryList :: Prelude.Maybe [DashboardSummary],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDashboardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardSummaryList', 'searchDashboardsResponse_dashboardSummaryList' - The list of dashboards owned by the user specified in @Filters@ in your
-- request.
--
-- 'nextToken', 'searchDashboardsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'searchDashboardsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'searchDashboardsResponse_status' - The HTTP status of the request.
newSearchDashboardsResponse ::
  -- | 'status'
  Prelude.Int ->
  SearchDashboardsResponse
newSearchDashboardsResponse pStatus_ =
  SearchDashboardsResponse'
    { dashboardSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The list of dashboards owned by the user specified in @Filters@ in your
-- request.
searchDashboardsResponse_dashboardSummaryList :: Lens.Lens' SearchDashboardsResponse (Prelude.Maybe [DashboardSummary])
searchDashboardsResponse_dashboardSummaryList = Lens.lens (\SearchDashboardsResponse' {dashboardSummaryList} -> dashboardSummaryList) (\s@SearchDashboardsResponse' {} a -> s {dashboardSummaryList = a} :: SearchDashboardsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
searchDashboardsResponse_nextToken :: Lens.Lens' SearchDashboardsResponse (Prelude.Maybe Prelude.Text)
searchDashboardsResponse_nextToken = Lens.lens (\SearchDashboardsResponse' {nextToken} -> nextToken) (\s@SearchDashboardsResponse' {} a -> s {nextToken = a} :: SearchDashboardsResponse)

-- | The Amazon Web Services request ID for this operation.
searchDashboardsResponse_requestId :: Lens.Lens' SearchDashboardsResponse (Prelude.Maybe Prelude.Text)
searchDashboardsResponse_requestId = Lens.lens (\SearchDashboardsResponse' {requestId} -> requestId) (\s@SearchDashboardsResponse' {} a -> s {requestId = a} :: SearchDashboardsResponse)

-- | The HTTP status of the request.
searchDashboardsResponse_status :: Lens.Lens' SearchDashboardsResponse Prelude.Int
searchDashboardsResponse_status = Lens.lens (\SearchDashboardsResponse' {status} -> status) (\s@SearchDashboardsResponse' {} a -> s {status = a} :: SearchDashboardsResponse)

instance Prelude.NFData SearchDashboardsResponse where
  rnf SearchDashboardsResponse' {..} =
    Prelude.rnf dashboardSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
