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
-- Module      : Network.AWS.QuickSight.ListDashboards
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists dashboards in an Amazon Web Services account.
--
-- This operation returns paginated results.
module Network.AWS.QuickSight.ListDashboards
  ( -- * Creating a Request
    ListDashboards (..),
    newListDashboards,

    -- * Request Lenses
    listDashboards_nextToken,
    listDashboards_maxResults,
    listDashboards_awsAccountId,

    -- * Destructuring the Response
    ListDashboardsResponse (..),
    newListDashboardsResponse,

    -- * Response Lenses
    listDashboardsResponse_requestId,
    listDashboardsResponse_nextToken,
    listDashboardsResponse_dashboardSummaryList,
    listDashboardsResponse_status,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDashboards' smart constructor.
data ListDashboards = ListDashboards'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listDashboards_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'maxResults', 'listDashboards_maxResults' - The maximum number of results to be returned per request.
--
-- 'awsAccountId', 'listDashboards_awsAccountId' - The ID of the Amazon Web Services account that contains the dashboards
-- that you\'re listing.
newListDashboards ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListDashboards
newListDashboards pAwsAccountId_ =
  ListDashboards'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listDashboards_nextToken :: Lens.Lens' ListDashboards (Prelude.Maybe Prelude.Text)
listDashboards_nextToken = Lens.lens (\ListDashboards' {nextToken} -> nextToken) (\s@ListDashboards' {} a -> s {nextToken = a} :: ListDashboards)

-- | The maximum number of results to be returned per request.
listDashboards_maxResults :: Lens.Lens' ListDashboards (Prelude.Maybe Prelude.Natural)
listDashboards_maxResults = Lens.lens (\ListDashboards' {maxResults} -> maxResults) (\s@ListDashboards' {} a -> s {maxResults = a} :: ListDashboards)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDashboardsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DashboardSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDashboards

instance Prelude.NFData ListDashboards

instance Core.ToHeaders ListDashboards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListDashboards where
  toPath ListDashboards' {..} =
    Prelude.mconcat
      ["/accounts/", Core.toBS awsAccountId, "/dashboards"]

instance Core.ToQuery ListDashboards where
  toQuery ListDashboards' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListDashboardsResponse' smart constructor.
data ListDashboardsResponse = ListDashboardsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains all of the dashboards in your Amazon Web
    -- Services account. This structure provides basic information about the
    -- dashboards.
    dashboardSummaryList :: Prelude.Maybe [DashboardSummary],
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
-- 'requestId', 'listDashboardsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'nextToken', 'listDashboardsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'dashboardSummaryList', 'listDashboardsResponse_dashboardSummaryList' - A structure that contains all of the dashboards in your Amazon Web
-- Services account. This structure provides basic information about the
-- dashboards.
--
-- 'status', 'listDashboardsResponse_status' - The HTTP status of the request.
newListDashboardsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListDashboardsResponse
newListDashboardsResponse pStatus_ =
  ListDashboardsResponse'
    { requestId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dashboardSummaryList = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
listDashboardsResponse_requestId :: Lens.Lens' ListDashboardsResponse (Prelude.Maybe Prelude.Text)
listDashboardsResponse_requestId = Lens.lens (\ListDashboardsResponse' {requestId} -> requestId) (\s@ListDashboardsResponse' {} a -> s {requestId = a} :: ListDashboardsResponse)

-- | The token for the next set of results, or null if there are no more
-- results.
listDashboardsResponse_nextToken :: Lens.Lens' ListDashboardsResponse (Prelude.Maybe Prelude.Text)
listDashboardsResponse_nextToken = Lens.lens (\ListDashboardsResponse' {nextToken} -> nextToken) (\s@ListDashboardsResponse' {} a -> s {nextToken = a} :: ListDashboardsResponse)

-- | A structure that contains all of the dashboards in your Amazon Web
-- Services account. This structure provides basic information about the
-- dashboards.
listDashboardsResponse_dashboardSummaryList :: Lens.Lens' ListDashboardsResponse (Prelude.Maybe [DashboardSummary])
listDashboardsResponse_dashboardSummaryList = Lens.lens (\ListDashboardsResponse' {dashboardSummaryList} -> dashboardSummaryList) (\s@ListDashboardsResponse' {} a -> s {dashboardSummaryList = a} :: ListDashboardsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listDashboardsResponse_status :: Lens.Lens' ListDashboardsResponse Prelude.Int
listDashboardsResponse_status = Lens.lens (\ListDashboardsResponse' {status} -> status) (\s@ListDashboardsResponse' {} a -> s {status = a} :: ListDashboardsResponse)

instance Prelude.NFData ListDashboardsResponse
