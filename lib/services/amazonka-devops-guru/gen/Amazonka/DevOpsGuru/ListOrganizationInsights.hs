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
-- Module      : Amazonka.DevOpsGuru.ListOrganizationInsights
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of insights associated with the account or OU Id.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.ListOrganizationInsights
  ( -- * Creating a Request
    ListOrganizationInsights (..),
    newListOrganizationInsights,

    -- * Request Lenses
    listOrganizationInsights_accountIds,
    listOrganizationInsights_maxResults,
    listOrganizationInsights_nextToken,
    listOrganizationInsights_organizationalUnitIds,
    listOrganizationInsights_statusFilter,

    -- * Destructuring the Response
    ListOrganizationInsightsResponse (..),
    newListOrganizationInsightsResponse,

    -- * Response Lenses
    listOrganizationInsightsResponse_nextToken,
    listOrganizationInsightsResponse_proactiveInsights,
    listOrganizationInsightsResponse_reactiveInsights,
    listOrganizationInsightsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOrganizationInsights' smart constructor.
data ListOrganizationInsights = ListOrganizationInsights'
  { -- | The ID of the Amazon Web Services account.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the organizational unit.
    organizationalUnitIds :: Prelude.Maybe [Prelude.Text],
    statusFilter :: ListInsightsStatusFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizationInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'listOrganizationInsights_accountIds' - The ID of the Amazon Web Services account.
--
-- 'maxResults', 'listOrganizationInsights_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listOrganizationInsights_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'organizationalUnitIds', 'listOrganizationInsights_organizationalUnitIds' - The ID of the organizational unit.
--
-- 'statusFilter', 'listOrganizationInsights_statusFilter' - Undocumented member.
newListOrganizationInsights ::
  -- | 'statusFilter'
  ListInsightsStatusFilter ->
  ListOrganizationInsights
newListOrganizationInsights pStatusFilter_ =
  ListOrganizationInsights'
    { accountIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      organizationalUnitIds = Prelude.Nothing,
      statusFilter = pStatusFilter_
    }

-- | The ID of the Amazon Web Services account.
listOrganizationInsights_accountIds :: Lens.Lens' ListOrganizationInsights (Prelude.Maybe [Prelude.Text])
listOrganizationInsights_accountIds = Lens.lens (\ListOrganizationInsights' {accountIds} -> accountIds) (\s@ListOrganizationInsights' {} a -> s {accountIds = a} :: ListOrganizationInsights) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listOrganizationInsights_maxResults :: Lens.Lens' ListOrganizationInsights (Prelude.Maybe Prelude.Natural)
listOrganizationInsights_maxResults = Lens.lens (\ListOrganizationInsights' {maxResults} -> maxResults) (\s@ListOrganizationInsights' {} a -> s {maxResults = a} :: ListOrganizationInsights)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listOrganizationInsights_nextToken :: Lens.Lens' ListOrganizationInsights (Prelude.Maybe Prelude.Text)
listOrganizationInsights_nextToken = Lens.lens (\ListOrganizationInsights' {nextToken} -> nextToken) (\s@ListOrganizationInsights' {} a -> s {nextToken = a} :: ListOrganizationInsights)

-- | The ID of the organizational unit.
listOrganizationInsights_organizationalUnitIds :: Lens.Lens' ListOrganizationInsights (Prelude.Maybe [Prelude.Text])
listOrganizationInsights_organizationalUnitIds = Lens.lens (\ListOrganizationInsights' {organizationalUnitIds} -> organizationalUnitIds) (\s@ListOrganizationInsights' {} a -> s {organizationalUnitIds = a} :: ListOrganizationInsights) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listOrganizationInsights_statusFilter :: Lens.Lens' ListOrganizationInsights ListInsightsStatusFilter
listOrganizationInsights_statusFilter = Lens.lens (\ListOrganizationInsights' {statusFilter} -> statusFilter) (\s@ListOrganizationInsights' {} a -> s {statusFilter = a} :: ListOrganizationInsights)

instance Core.AWSPager ListOrganizationInsights where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOrganizationInsightsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOrganizationInsightsResponse_proactiveInsights
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOrganizationInsightsResponse_reactiveInsights
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOrganizationInsights_nextToken
          Lens..~ rs
          Lens.^? listOrganizationInsightsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListOrganizationInsights where
  type
    AWSResponse ListOrganizationInsights =
      ListOrganizationInsightsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationInsightsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ProactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "ReactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOrganizationInsights where
  hashWithSalt _salt ListOrganizationInsights' {..} =
    _salt `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` organizationalUnitIds
      `Prelude.hashWithSalt` statusFilter

instance Prelude.NFData ListOrganizationInsights where
  rnf ListOrganizationInsights' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationalUnitIds
      `Prelude.seq` Prelude.rnf statusFilter

instance Data.ToHeaders ListOrganizationInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListOrganizationInsights where
  toJSON ListOrganizationInsights' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountIds" Data..=) Prelude.<$> accountIds,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("OrganizationalUnitIds" Data..=)
              Prelude.<$> organizationalUnitIds,
            Prelude.Just ("StatusFilter" Data..= statusFilter)
          ]
      )

instance Data.ToPath ListOrganizationInsights where
  toPath = Prelude.const "/organization/insights"

instance Data.ToQuery ListOrganizationInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOrganizationInsightsResponse' smart constructor.
data ListOrganizationInsightsResponse = ListOrganizationInsightsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An integer that specifies the number of open proactive insights in your
    -- Amazon Web Services account.
    proactiveInsights :: Prelude.Maybe [ProactiveOrganizationInsightSummary],
    -- | An integer that specifies the number of open reactive insights in your
    -- Amazon Web Services account.
    reactiveInsights :: Prelude.Maybe [ReactiveOrganizationInsightSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizationInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOrganizationInsightsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'proactiveInsights', 'listOrganizationInsightsResponse_proactiveInsights' - An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account.
--
-- 'reactiveInsights', 'listOrganizationInsightsResponse_reactiveInsights' - An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account.
--
-- 'httpStatus', 'listOrganizationInsightsResponse_httpStatus' - The response's http status code.
newListOrganizationInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOrganizationInsightsResponse
newListOrganizationInsightsResponse pHttpStatus_ =
  ListOrganizationInsightsResponse'
    { nextToken =
        Prelude.Nothing,
      proactiveInsights = Prelude.Nothing,
      reactiveInsights = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listOrganizationInsightsResponse_nextToken :: Lens.Lens' ListOrganizationInsightsResponse (Prelude.Maybe Prelude.Text)
listOrganizationInsightsResponse_nextToken = Lens.lens (\ListOrganizationInsightsResponse' {nextToken} -> nextToken) (\s@ListOrganizationInsightsResponse' {} a -> s {nextToken = a} :: ListOrganizationInsightsResponse)

-- | An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account.
listOrganizationInsightsResponse_proactiveInsights :: Lens.Lens' ListOrganizationInsightsResponse (Prelude.Maybe [ProactiveOrganizationInsightSummary])
listOrganizationInsightsResponse_proactiveInsights = Lens.lens (\ListOrganizationInsightsResponse' {proactiveInsights} -> proactiveInsights) (\s@ListOrganizationInsightsResponse' {} a -> s {proactiveInsights = a} :: ListOrganizationInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account.
listOrganizationInsightsResponse_reactiveInsights :: Lens.Lens' ListOrganizationInsightsResponse (Prelude.Maybe [ReactiveOrganizationInsightSummary])
listOrganizationInsightsResponse_reactiveInsights = Lens.lens (\ListOrganizationInsightsResponse' {reactiveInsights} -> reactiveInsights) (\s@ListOrganizationInsightsResponse' {} a -> s {reactiveInsights = a} :: ListOrganizationInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOrganizationInsightsResponse_httpStatus :: Lens.Lens' ListOrganizationInsightsResponse Prelude.Int
listOrganizationInsightsResponse_httpStatus = Lens.lens (\ListOrganizationInsightsResponse' {httpStatus} -> httpStatus) (\s@ListOrganizationInsightsResponse' {} a -> s {httpStatus = a} :: ListOrganizationInsightsResponse)

instance
  Prelude.NFData
    ListOrganizationInsightsResponse
  where
  rnf ListOrganizationInsightsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf proactiveInsights
      `Prelude.seq` Prelude.rnf reactiveInsights
      `Prelude.seq` Prelude.rnf httpStatus
