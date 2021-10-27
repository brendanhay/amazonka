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
-- Module      : Network.AWS.DevOpsGuru.ListAnomaliesForInsight
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the anomalies that belong to an insight that you
-- specify using its ID.
--
-- This operation returns paginated results.
module Network.AWS.DevOpsGuru.ListAnomaliesForInsight
  ( -- * Creating a Request
    ListAnomaliesForInsight (..),
    newListAnomaliesForInsight,

    -- * Request Lenses
    listAnomaliesForInsight_startTimeRange,
    listAnomaliesForInsight_nextToken,
    listAnomaliesForInsight_maxResults,
    listAnomaliesForInsight_insightId,

    -- * Destructuring the Response
    ListAnomaliesForInsightResponse (..),
    newListAnomaliesForInsightResponse,

    -- * Response Lenses
    listAnomaliesForInsightResponse_proactiveAnomalies,
    listAnomaliesForInsightResponse_nextToken,
    listAnomaliesForInsightResponse_reactiveAnomalies,
    listAnomaliesForInsightResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAnomaliesForInsight' smart constructor.
data ListAnomaliesForInsight = ListAnomaliesForInsight'
  { -- | A time range used to specify when the requested anomalies started. All
    -- returned anomalies started during this time range.
    startTimeRange :: Prelude.Maybe StartTimeRange,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'startTimeRange', 'listAnomaliesForInsight_startTimeRange' - A time range used to specify when the requested anomalies started. All
-- returned anomalies started during this time range.
--
-- 'nextToken', 'listAnomaliesForInsight_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'maxResults', 'listAnomaliesForInsight_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'insightId', 'listAnomaliesForInsight_insightId' - The ID of the insight. The returned anomalies belong to this insight.
newListAnomaliesForInsight ::
  -- | 'insightId'
  Prelude.Text ->
  ListAnomaliesForInsight
newListAnomaliesForInsight pInsightId_ =
  ListAnomaliesForInsight'
    { startTimeRange =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      insightId = pInsightId_
    }

-- | A time range used to specify when the requested anomalies started. All
-- returned anomalies started during this time range.
listAnomaliesForInsight_startTimeRange :: Lens.Lens' ListAnomaliesForInsight (Prelude.Maybe StartTimeRange)
listAnomaliesForInsight_startTimeRange = Lens.lens (\ListAnomaliesForInsight' {startTimeRange} -> startTimeRange) (\s@ListAnomaliesForInsight' {} a -> s {startTimeRange = a} :: ListAnomaliesForInsight)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listAnomaliesForInsight_nextToken :: Lens.Lens' ListAnomaliesForInsight (Prelude.Maybe Prelude.Text)
listAnomaliesForInsight_nextToken = Lens.lens (\ListAnomaliesForInsight' {nextToken} -> nextToken) (\s@ListAnomaliesForInsight' {} a -> s {nextToken = a} :: ListAnomaliesForInsight)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listAnomaliesForInsight_maxResults :: Lens.Lens' ListAnomaliesForInsight (Prelude.Maybe Prelude.Natural)
listAnomaliesForInsight_maxResults = Lens.lens (\ListAnomaliesForInsight' {maxResults} -> maxResults) (\s@ListAnomaliesForInsight' {} a -> s {maxResults = a} :: ListAnomaliesForInsight)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& listAnomaliesForInsight_nextToken
          Lens..~ rs
          Lens.^? listAnomaliesForInsightResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAnomaliesForInsight where
  type
    AWSResponse ListAnomaliesForInsight =
      ListAnomaliesForInsightResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnomaliesForInsightResponse'
            Prelude.<$> ( x Core..?> "ProactiveAnomalies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ReactiveAnomalies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAnomaliesForInsight

instance Prelude.NFData ListAnomaliesForInsight

instance Core.ToHeaders ListAnomaliesForInsight where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAnomaliesForInsight where
  toJSON ListAnomaliesForInsight' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StartTimeRange" Core..=)
              Prelude.<$> startTimeRange,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListAnomaliesForInsight where
  toPath ListAnomaliesForInsight' {..} =
    Prelude.mconcat
      ["/anomalies/insight/", Core.toBS insightId]

instance Core.ToQuery ListAnomaliesForInsight where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAnomaliesForInsightResponse' smart constructor.
data ListAnomaliesForInsightResponse = ListAnomaliesForInsightResponse'
  { -- | An array of @ProactiveAnomalySummary@ objects that represent the
    -- requested anomalies
    proactiveAnomalies :: Prelude.Maybe [ProactiveAnomalySummary],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'proactiveAnomalies', 'listAnomaliesForInsightResponse_proactiveAnomalies' - An array of @ProactiveAnomalySummary@ objects that represent the
-- requested anomalies
--
-- 'nextToken', 'listAnomaliesForInsightResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
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
    { proactiveAnomalies =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      reactiveAnomalies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @ProactiveAnomalySummary@ objects that represent the
-- requested anomalies
listAnomaliesForInsightResponse_proactiveAnomalies :: Lens.Lens' ListAnomaliesForInsightResponse (Prelude.Maybe [ProactiveAnomalySummary])
listAnomaliesForInsightResponse_proactiveAnomalies = Lens.lens (\ListAnomaliesForInsightResponse' {proactiveAnomalies} -> proactiveAnomalies) (\s@ListAnomaliesForInsightResponse' {} a -> s {proactiveAnomalies = a} :: ListAnomaliesForInsightResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listAnomaliesForInsightResponse_nextToken :: Lens.Lens' ListAnomaliesForInsightResponse (Prelude.Maybe Prelude.Text)
listAnomaliesForInsightResponse_nextToken = Lens.lens (\ListAnomaliesForInsightResponse' {nextToken} -> nextToken) (\s@ListAnomaliesForInsightResponse' {} a -> s {nextToken = a} :: ListAnomaliesForInsightResponse)

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
