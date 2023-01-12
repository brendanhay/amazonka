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
-- Module      : Amazonka.QuickSight.ListDashboards
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists dashboards in an Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListDashboards
  ( -- * Creating a Request
    ListDashboards (..),
    newListDashboards,

    -- * Request Lenses
    listDashboards_maxResults,
    listDashboards_nextToken,
    listDashboards_awsAccountId,

    -- * Destructuring the Response
    ListDashboardsResponse (..),
    newListDashboardsResponse,

    -- * Response Lenses
    listDashboardsResponse_dashboardSummaryList,
    listDashboardsResponse_nextToken,
    listDashboardsResponse_requestId,
    listDashboardsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDashboards' smart constructor.
data ListDashboards = ListDashboards'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the dashboards
    -- that you\'re listing.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDashboards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDashboards_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listDashboards_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listDashboards_awsAccountId' - The ID of the Amazon Web Services account that contains the dashboards
-- that you\'re listing.
newListDashboards ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListDashboards
newListDashboards pAwsAccountId_ =
  ListDashboards'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listDashboards_maxResults :: Lens.Lens' ListDashboards (Prelude.Maybe Prelude.Natural)
listDashboards_maxResults = Lens.lens (\ListDashboards' {maxResults} -> maxResults) (\s@ListDashboards' {} a -> s {maxResults = a} :: ListDashboards)

-- | The token for the next set of results, or null if there are no more
-- results.
listDashboards_nextToken :: Lens.Lens' ListDashboards (Prelude.Maybe Prelude.Text)
listDashboards_nextToken = Lens.lens (\ListDashboards' {nextToken} -> nextToken) (\s@ListDashboards' {} a -> s {nextToken = a} :: ListDashboards)

-- | The ID of the Amazon Web Services account that contains the dashboards
-- that you\'re listing.
listDashboards_awsAccountId :: Lens.Lens' ListDashboards Prelude.Text
listDashboards_awsAccountId = Lens.lens (\ListDashboards' {awsAccountId} -> awsAccountId) (\s@ListDashboards' {} a -> s {awsAccountId = a} :: ListDashboards)

instance Core.AWSPager ListDashboards where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDashboardsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDashboardsResponse_dashboardSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDashboards_nextToken
          Lens..~ rs
          Lens.^? listDashboardsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDashboards where
  type
    AWSResponse ListDashboards =
      ListDashboardsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDashboardsResponse'
            Prelude.<$> ( x Data..?> "DashboardSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDashboards where
  hashWithSalt _salt ListDashboards' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListDashboards where
  rnf ListDashboards' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListDashboards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDashboards where
  toPath ListDashboards' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS awsAccountId, "/dashboards"]

instance Data.ToQuery ListDashboards where
  toQuery ListDashboards' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListDashboardsResponse' smart constructor.
data ListDashboardsResponse = ListDashboardsResponse'
  { -- | A structure that contains all of the dashboards in your Amazon Web
    -- Services account. This structure provides basic information about the
    -- dashboards.
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
-- Create a value of 'ListDashboardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardSummaryList', 'listDashboardsResponse_dashboardSummaryList' - A structure that contains all of the dashboards in your Amazon Web
-- Services account. This structure provides basic information about the
-- dashboards.
--
-- 'nextToken', 'listDashboardsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listDashboardsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listDashboardsResponse_status' - The HTTP status of the request.
newListDashboardsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListDashboardsResponse
newListDashboardsResponse pStatus_ =
  ListDashboardsResponse'
    { dashboardSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A structure that contains all of the dashboards in your Amazon Web
-- Services account. This structure provides basic information about the
-- dashboards.
listDashboardsResponse_dashboardSummaryList :: Lens.Lens' ListDashboardsResponse (Prelude.Maybe [DashboardSummary])
listDashboardsResponse_dashboardSummaryList = Lens.lens (\ListDashboardsResponse' {dashboardSummaryList} -> dashboardSummaryList) (\s@ListDashboardsResponse' {} a -> s {dashboardSummaryList = a} :: ListDashboardsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listDashboardsResponse_nextToken :: Lens.Lens' ListDashboardsResponse (Prelude.Maybe Prelude.Text)
listDashboardsResponse_nextToken = Lens.lens (\ListDashboardsResponse' {nextToken} -> nextToken) (\s@ListDashboardsResponse' {} a -> s {nextToken = a} :: ListDashboardsResponse)

-- | The Amazon Web Services request ID for this operation.
listDashboardsResponse_requestId :: Lens.Lens' ListDashboardsResponse (Prelude.Maybe Prelude.Text)
listDashboardsResponse_requestId = Lens.lens (\ListDashboardsResponse' {requestId} -> requestId) (\s@ListDashboardsResponse' {} a -> s {requestId = a} :: ListDashboardsResponse)

-- | The HTTP status of the request.
listDashboardsResponse_status :: Lens.Lens' ListDashboardsResponse Prelude.Int
listDashboardsResponse_status = Lens.lens (\ListDashboardsResponse' {status} -> status) (\s@ListDashboardsResponse' {} a -> s {status = a} :: ListDashboardsResponse)

instance Prelude.NFData ListDashboardsResponse where
  rnf ListDashboardsResponse' {..} =
    Prelude.rnf dashboardSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
