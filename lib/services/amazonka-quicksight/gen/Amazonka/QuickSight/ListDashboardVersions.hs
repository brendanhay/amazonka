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
-- Module      : Amazonka.QuickSight.ListDashboardVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the versions of the dashboards in the Amazon QuickSight
-- subscription.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListDashboardVersions
  ( -- * Creating a Request
    ListDashboardVersions (..),
    newListDashboardVersions,

    -- * Request Lenses
    listDashboardVersions_nextToken,
    listDashboardVersions_maxResults,
    listDashboardVersions_awsAccountId,
    listDashboardVersions_dashboardId,

    -- * Destructuring the Response
    ListDashboardVersionsResponse (..),
    newListDashboardVersionsResponse,

    -- * Response Lenses
    listDashboardVersionsResponse_requestId,
    listDashboardVersionsResponse_dashboardVersionSummaryList,
    listDashboardVersionsResponse_nextToken,
    listDashboardVersionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDashboardVersions' smart constructor.
data ListDashboardVersions = ListDashboardVersions'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services account that contains the dashboard
    -- that you\'re listing versions for.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dashboard.
    dashboardId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDashboardVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDashboardVersions_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'maxResults', 'listDashboardVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'awsAccountId', 'listDashboardVersions_awsAccountId' - The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re listing versions for.
--
-- 'dashboardId', 'listDashboardVersions_dashboardId' - The ID for the dashboard.
newListDashboardVersions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dashboardId'
  Prelude.Text ->
  ListDashboardVersions
newListDashboardVersions pAwsAccountId_ pDashboardId_ =
  ListDashboardVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      dashboardId = pDashboardId_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listDashboardVersions_nextToken :: Lens.Lens' ListDashboardVersions (Prelude.Maybe Prelude.Text)
listDashboardVersions_nextToken = Lens.lens (\ListDashboardVersions' {nextToken} -> nextToken) (\s@ListDashboardVersions' {} a -> s {nextToken = a} :: ListDashboardVersions)

-- | The maximum number of results to be returned per request.
listDashboardVersions_maxResults :: Lens.Lens' ListDashboardVersions (Prelude.Maybe Prelude.Natural)
listDashboardVersions_maxResults = Lens.lens (\ListDashboardVersions' {maxResults} -> maxResults) (\s@ListDashboardVersions' {} a -> s {maxResults = a} :: ListDashboardVersions)

-- | The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re listing versions for.
listDashboardVersions_awsAccountId :: Lens.Lens' ListDashboardVersions Prelude.Text
listDashboardVersions_awsAccountId = Lens.lens (\ListDashboardVersions' {awsAccountId} -> awsAccountId) (\s@ListDashboardVersions' {} a -> s {awsAccountId = a} :: ListDashboardVersions)

-- | The ID for the dashboard.
listDashboardVersions_dashboardId :: Lens.Lens' ListDashboardVersions Prelude.Text
listDashboardVersions_dashboardId = Lens.lens (\ListDashboardVersions' {dashboardId} -> dashboardId) (\s@ListDashboardVersions' {} a -> s {dashboardId = a} :: ListDashboardVersions)

instance Core.AWSPager ListDashboardVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDashboardVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDashboardVersionsResponse_dashboardVersionSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDashboardVersions_nextToken
          Lens..~ rs
          Lens.^? listDashboardVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDashboardVersions where
  type
    AWSResponse ListDashboardVersions =
      ListDashboardVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDashboardVersionsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> ( x Core..?> "DashboardVersionSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDashboardVersions where
  hashWithSalt salt' ListDashboardVersions' {..} =
    salt' `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDashboardVersions where
  rnf ListDashboardVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListDashboardVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListDashboardVersions where
  toPath ListDashboardVersions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/dashboards/",
        Core.toBS dashboardId,
        "/versions"
      ]

instance Core.ToQuery ListDashboardVersions where
  toQuery ListDashboardVersions' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListDashboardVersionsResponse' smart constructor.
data ListDashboardVersionsResponse = ListDashboardVersionsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains information about each version of the
    -- dashboard.
    dashboardVersionSummaryList :: Prelude.Maybe [DashboardVersionSummary],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDashboardVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'listDashboardVersionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'dashboardVersionSummaryList', 'listDashboardVersionsResponse_dashboardVersionSummaryList' - A structure that contains information about each version of the
-- dashboard.
--
-- 'nextToken', 'listDashboardVersionsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'status', 'listDashboardVersionsResponse_status' - The HTTP status of the request.
newListDashboardVersionsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListDashboardVersionsResponse
newListDashboardVersionsResponse pStatus_ =
  ListDashboardVersionsResponse'
    { requestId =
        Prelude.Nothing,
      dashboardVersionSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
listDashboardVersionsResponse_requestId :: Lens.Lens' ListDashboardVersionsResponse (Prelude.Maybe Prelude.Text)
listDashboardVersionsResponse_requestId = Lens.lens (\ListDashboardVersionsResponse' {requestId} -> requestId) (\s@ListDashboardVersionsResponse' {} a -> s {requestId = a} :: ListDashboardVersionsResponse)

-- | A structure that contains information about each version of the
-- dashboard.
listDashboardVersionsResponse_dashboardVersionSummaryList :: Lens.Lens' ListDashboardVersionsResponse (Prelude.Maybe [DashboardVersionSummary])
listDashboardVersionsResponse_dashboardVersionSummaryList = Lens.lens (\ListDashboardVersionsResponse' {dashboardVersionSummaryList} -> dashboardVersionSummaryList) (\s@ListDashboardVersionsResponse' {} a -> s {dashboardVersionSummaryList = a} :: ListDashboardVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listDashboardVersionsResponse_nextToken :: Lens.Lens' ListDashboardVersionsResponse (Prelude.Maybe Prelude.Text)
listDashboardVersionsResponse_nextToken = Lens.lens (\ListDashboardVersionsResponse' {nextToken} -> nextToken) (\s@ListDashboardVersionsResponse' {} a -> s {nextToken = a} :: ListDashboardVersionsResponse)

-- | The HTTP status of the request.
listDashboardVersionsResponse_status :: Lens.Lens' ListDashboardVersionsResponse Prelude.Int
listDashboardVersionsResponse_status = Lens.lens (\ListDashboardVersionsResponse' {status} -> status) (\s@ListDashboardVersionsResponse' {} a -> s {status = a} :: ListDashboardVersionsResponse)

instance Prelude.NFData ListDashboardVersionsResponse where
  rnf ListDashboardVersionsResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dashboardVersionSummaryList
