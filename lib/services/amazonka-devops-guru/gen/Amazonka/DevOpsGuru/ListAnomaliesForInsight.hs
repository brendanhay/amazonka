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
-- Module      : Amazonka.DevOpsGuru.ListAnomaliesForInsight
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the anomalies that belong to an insight that you
-- specify using its ID.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.ListAnomaliesForInsight
  ( -- * Creating a Request
    ListAnomaliesForInsight (..),
    newListAnomaliesForInsight,

    -- * Request Lenses
    listAnomaliesForInsight_accountId,
    listAnomaliesForInsight_filters,
    listAnomaliesForInsight_maxResults,
    listAnomaliesForInsight_nextToken,
    listAnomaliesForInsight_startTimeRange,
    listAnomaliesForInsight_insightId,

    -- * Destructuring the Response
    ListAnomaliesForInsightResponse (..),
    newListAnomaliesForInsightResponse,

    -- * Response Lenses
    listAnomaliesForInsightResponse_nextToken,
    listAnomaliesForInsightResponse_proactiveAnomalies,
    listAnomaliesForInsightResponse_reactiveAnomalies,
    listAnomaliesForInsightResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAnomaliesForInsight' smart constructor.
data ListAnomaliesForInsight = ListAnomaliesForInsight'
  { -- | The ID of the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Specifies one or more service names that are used to list anomalies.
    filters :: Prelude.Maybe ListAnomaliesForInsightFilters,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A time range used to specify when the requested anomalies started. All
    -- returned anomalies started during this time range.
    startTimeRange :: Prelude.Maybe StartTimeRange,
    -- | The ID of the insight. The returned anomalies belong to this insight.
    insightId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomaliesForInsight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'listAnomaliesForInsight_accountId' - The ID of the Amazon Web Services account.
--
-- 'filters', 'listAnomaliesForInsight_filters' - Specifies one or more service names that are used to list anomalies.
--
-- 'maxResults', 'listAnomaliesForInsight_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listAnomaliesForInsight_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'startTimeRange', 'listAnomaliesForInsight_startTimeRange' - A time range used to specify when the requested anomalies started. All
-- returned anomalies started during this time range.
--
-- 'insightId', 'listAnomaliesForInsight_insightId' - The ID of the insight. The returned anomalies belong to this insight.
newListAnomaliesForInsight ::
  -- | 'insightId'
  Prelude.Text ->
  ListAnomaliesForInsight
newListAnomaliesForInsight pInsightId_ =
  ListAnomaliesForInsight'
    { accountId =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTimeRange = Prelude.Nothing,
      insightId = pInsightId_
    }

-- | The ID of the Amazon Web Services account.
listAnomaliesForInsight_accountId :: Lens.Lens' ListAnomaliesForInsight (Prelude.Maybe Prelude.Text)
listAnomaliesForInsight_accountId = Lens.lens (\ListAnomaliesForInsight' {accountId} -> accountId) (\s@ListAnomaliesForInsight' {} a -> s {accountId = a} :: ListAnomaliesForInsight)

-- | Specifies one or more service names that are used to list anomalies.
listAnomaliesForInsight_filters :: Lens.Lens' ListAnomaliesForInsight (Prelude.Maybe ListAnomaliesForInsightFilters)
listAnomaliesForInsight_filters = Lens.lens (\ListAnomaliesForInsight' {filters} -> filters) (\s@ListAnomaliesForInsight' {} a -> s {filters = a} :: ListAnomaliesForInsight)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listAnomaliesForInsight_maxResults :: Lens.Lens' ListAnomaliesForInsight (Prelude.Maybe Prelude.Natural)
listAnomaliesForInsight_maxResults = Lens.lens (\ListAnomaliesForInsight' {maxResults} -> maxResults) (\s@ListAnomaliesForInsight' {} a -> s {maxResults = a} :: ListAnomaliesForInsight)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listAnomaliesForInsight_nextToken :: Lens.Lens' ListAnomaliesForInsight (Prelude.Maybe Prelude.Text)
listAnomaliesForInsight_nextToken = Lens.lens (\ListAnomaliesForInsight' {nextToken} -> nextToken) (\s@ListAnomaliesForInsight' {} a -> s {nextToken = a} :: ListAnomaliesForInsight)

-- | A time range used to specify when the requested anomalies started. All
-- returned anomalies started during this time range.
listAnomaliesForInsight_startTimeRange :: Lens.Lens' ListAnomaliesForInsight (Prelude.Maybe StartTimeRange)
listAnomaliesForInsight_startTimeRange = Lens.lens (\ListAnomaliesForInsight' {startTimeRange} -> startTimeRange) (\s@ListAnomaliesForInsight' {} a -> s {startTimeRange = a} :: ListAnomaliesForInsight)

-- | The ID of the insight. The returned anomalies belong to this insight.
listAnomaliesForInsight_insightId :: Lens.Lens' ListAnomaliesForInsight Prelude.Text
listAnomaliesForInsight_insightId = Lens.lens (\ListAnomaliesForInsight' {insightId} -> insightId) (\s@ListAnomaliesForInsight' {} a -> s {insightId = a} :: ListAnomaliesForInsight)

instance Core.AWSPager ListAnomaliesForInsight where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAnomaliesForInsightResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAnomaliesForInsightResponse_reactiveAnomalies
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAnomaliesForInsightResponse_proactiveAnomalies
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAnomaliesForInsight_nextToken
          Lens..~ rs
          Lens.^? listAnomaliesForInsightResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAnomaliesForInsight where
  type
    AWSResponse ListAnomaliesForInsight =
      ListAnomaliesForInsightResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnomaliesForInsightResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ProactiveAnomalies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "ReactiveAnomalies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAnomaliesForInsight where
  hashWithSalt _salt ListAnomaliesForInsight' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTimeRange
      `Prelude.hashWithSalt` insightId

instance Prelude.NFData ListAnomaliesForInsight where
  rnf ListAnomaliesForInsight' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTimeRange
      `Prelude.seq` Prelude.rnf insightId

instance Data.ToHeaders ListAnomaliesForInsight where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAnomaliesForInsight where
  toJSON ListAnomaliesForInsight' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StartTimeRange" Data..=)
              Prelude.<$> startTimeRange
          ]
      )

instance Data.ToPath ListAnomaliesForInsight where
  toPath ListAnomaliesForInsight' {..} =
    Prelude.mconcat
      ["/anomalies/insight/", Data.toBS insightId]

instance Data.ToQuery ListAnomaliesForInsight where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAnomaliesForInsightResponse' smart constructor.
data ListAnomaliesForInsightResponse = ListAnomaliesForInsightResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @ProactiveAnomalySummary@ objects that represent the
    -- requested anomalies
    proactiveAnomalies :: Prelude.Maybe [ProactiveAnomalySummary],
    -- | An array of @ReactiveAnomalySummary@ objects that represent the
    -- requested anomalies
    reactiveAnomalies :: Prelude.Maybe [ReactiveAnomalySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomaliesForInsightResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAnomaliesForInsightResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'proactiveAnomalies', 'listAnomaliesForInsightResponse_proactiveAnomalies' - An array of @ProactiveAnomalySummary@ objects that represent the
-- requested anomalies
--
-- 'reactiveAnomalies', 'listAnomaliesForInsightResponse_reactiveAnomalies' - An array of @ReactiveAnomalySummary@ objects that represent the
-- requested anomalies
--
-- 'httpStatus', 'listAnomaliesForInsightResponse_httpStatus' - The response's http status code.
newListAnomaliesForInsightResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAnomaliesForInsightResponse
newListAnomaliesForInsightResponse pHttpStatus_ =
  ListAnomaliesForInsightResponse'
    { nextToken =
        Prelude.Nothing,
      proactiveAnomalies = Prelude.Nothing,
      reactiveAnomalies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listAnomaliesForInsightResponse_nextToken :: Lens.Lens' ListAnomaliesForInsightResponse (Prelude.Maybe Prelude.Text)
listAnomaliesForInsightResponse_nextToken = Lens.lens (\ListAnomaliesForInsightResponse' {nextToken} -> nextToken) (\s@ListAnomaliesForInsightResponse' {} a -> s {nextToken = a} :: ListAnomaliesForInsightResponse)

-- | An array of @ProactiveAnomalySummary@ objects that represent the
-- requested anomalies
listAnomaliesForInsightResponse_proactiveAnomalies :: Lens.Lens' ListAnomaliesForInsightResponse (Prelude.Maybe [ProactiveAnomalySummary])
listAnomaliesForInsightResponse_proactiveAnomalies = Lens.lens (\ListAnomaliesForInsightResponse' {proactiveAnomalies} -> proactiveAnomalies) (\s@ListAnomaliesForInsightResponse' {} a -> s {proactiveAnomalies = a} :: ListAnomaliesForInsightResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ReactiveAnomalySummary@ objects that represent the
-- requested anomalies
listAnomaliesForInsightResponse_reactiveAnomalies :: Lens.Lens' ListAnomaliesForInsightResponse (Prelude.Maybe [ReactiveAnomalySummary])
listAnomaliesForInsightResponse_reactiveAnomalies = Lens.lens (\ListAnomaliesForInsightResponse' {reactiveAnomalies} -> reactiveAnomalies) (\s@ListAnomaliesForInsightResponse' {} a -> s {reactiveAnomalies = a} :: ListAnomaliesForInsightResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAnomaliesForInsightResponse_httpStatus :: Lens.Lens' ListAnomaliesForInsightResponse Prelude.Int
listAnomaliesForInsightResponse_httpStatus = Lens.lens (\ListAnomaliesForInsightResponse' {httpStatus} -> httpStatus) (\s@ListAnomaliesForInsightResponse' {} a -> s {httpStatus = a} :: ListAnomaliesForInsightResponse)

instance
  Prelude.NFData
    ListAnomaliesForInsightResponse
  where
  rnf ListAnomaliesForInsightResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf proactiveAnomalies
      `Prelude.seq` Prelude.rnf reactiveAnomalies
      `Prelude.seq` Prelude.rnf httpStatus
