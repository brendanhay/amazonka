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
-- Module      : Network.AWS.QuickSight.SearchDashboards
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for dashboards that belong to a user.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- This operation returns paginated results.
module Network.AWS.QuickSight.SearchDashboards
  ( -- * Creating a Request
    SearchDashboards (..),
    newSearchDashboards,

    -- * Request Lenses
    searchDashboards_nextToken,
    searchDashboards_maxResults,
    searchDashboards_awsAccountId,
    searchDashboards_filters,

    -- * Destructuring the Response
    SearchDashboardsResponse (..),
    newSearchDashboardsResponse,

    -- * Response Lenses
    searchDashboardsResponse_requestId,
    searchDashboardsResponse_nextToken,
    searchDashboardsResponse_dashboardSummaryList,
    searchDashboardsResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchDashboards' smart constructor.
data SearchDashboards = SearchDashboards'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'searchDashboards_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'maxResults', 'searchDashboards_maxResults' - The maximum number of results to be returned per request.
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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      filters = Lens.coerced Lens.# pFilters_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
searchDashboards_nextToken :: Lens.Lens' SearchDashboards (Prelude.Maybe Prelude.Text)
searchDashboards_nextToken = Lens.lens (\SearchDashboards' {nextToken} -> nextToken) (\s@SearchDashboards' {} a -> s {nextToken = a} :: SearchDashboards)

-- | The maximum number of results to be returned per request.
searchDashboards_maxResults :: Lens.Lens' SearchDashboards (Prelude.Maybe Prelude.Natural)
searchDashboards_maxResults = Lens.lens (\SearchDashboards' {maxResults} -> maxResults) (\s@SearchDashboards' {} a -> s {maxResults = a} :: SearchDashboards)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& searchDashboards_nextToken
          Lens..~ rs
          Lens.^? searchDashboardsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchDashboards where
  type
    AWSResponse SearchDashboards =
      SearchDashboardsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchDashboardsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DashboardSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchDashboards

instance Prelude.NFData SearchDashboards

instance Core.ToHeaders SearchDashboards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchDashboards where
  toJSON SearchDashboards' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("Filters" Core..= filters)
          ]
      )

instance Core.ToPath SearchDashboards where
  toPath SearchDashboards' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/search/dashboards"
      ]

instance Core.ToQuery SearchDashboards where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchDashboardsResponse' smart constructor.
data SearchDashboardsResponse = SearchDashboardsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of dashboards owned by the user specified in @Filters@ in your
    -- request.
    dashboardSummaryList :: Prelude.Maybe [DashboardSummary],
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
-- 'requestId', 'searchDashboardsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'nextToken', 'searchDashboardsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'dashboardSummaryList', 'searchDashboardsResponse_dashboardSummaryList' - The list of dashboards owned by the user specified in @Filters@ in your
-- request.
--
-- 'status', 'searchDashboardsResponse_status' - The HTTP status of the request.
newSearchDashboardsResponse ::
  -- | 'status'
  Prelude.Int ->
  SearchDashboardsResponse
newSearchDashboardsResponse pStatus_ =
  SearchDashboardsResponse'
    { requestId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dashboardSummaryList = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
searchDashboardsResponse_requestId :: Lens.Lens' SearchDashboardsResponse (Prelude.Maybe Prelude.Text)
searchDashboardsResponse_requestId = Lens.lens (\SearchDashboardsResponse' {requestId} -> requestId) (\s@SearchDashboardsResponse' {} a -> s {requestId = a} :: SearchDashboardsResponse)

-- | The token for the next set of results, or null if there are no more
-- results.
searchDashboardsResponse_nextToken :: Lens.Lens' SearchDashboardsResponse (Prelude.Maybe Prelude.Text)
searchDashboardsResponse_nextToken = Lens.lens (\SearchDashboardsResponse' {nextToken} -> nextToken) (\s@SearchDashboardsResponse' {} a -> s {nextToken = a} :: SearchDashboardsResponse)

-- | The list of dashboards owned by the user specified in @Filters@ in your
-- request.
searchDashboardsResponse_dashboardSummaryList :: Lens.Lens' SearchDashboardsResponse (Prelude.Maybe [DashboardSummary])
searchDashboardsResponse_dashboardSummaryList = Lens.lens (\SearchDashboardsResponse' {dashboardSummaryList} -> dashboardSummaryList) (\s@SearchDashboardsResponse' {} a -> s {dashboardSummaryList = a} :: SearchDashboardsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
searchDashboardsResponse_status :: Lens.Lens' SearchDashboardsResponse Prelude.Int
searchDashboardsResponse_status = Lens.lens (\SearchDashboardsResponse' {status} -> status) (\s@SearchDashboardsResponse' {} a -> s {status = a} :: SearchDashboardsResponse)

instance Prelude.NFData SearchDashboardsResponse
