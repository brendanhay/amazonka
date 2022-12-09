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
-- Module      : Amazonka.SageMaker.ListMonitoringAlertHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of past alerts in a model monitoring schedule.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListMonitoringAlertHistory
  ( -- * Creating a Request
    ListMonitoringAlertHistory (..),
    newListMonitoringAlertHistory,

    -- * Request Lenses
    listMonitoringAlertHistory_creationTimeAfter,
    listMonitoringAlertHistory_creationTimeBefore,
    listMonitoringAlertHistory_maxResults,
    listMonitoringAlertHistory_monitoringAlertName,
    listMonitoringAlertHistory_monitoringScheduleName,
    listMonitoringAlertHistory_nextToken,
    listMonitoringAlertHistory_sortBy,
    listMonitoringAlertHistory_sortOrder,
    listMonitoringAlertHistory_statusEquals,

    -- * Destructuring the Response
    ListMonitoringAlertHistoryResponse (..),
    newListMonitoringAlertHistoryResponse,

    -- * Response Lenses
    listMonitoringAlertHistoryResponse_monitoringAlertHistory,
    listMonitoringAlertHistoryResponse_nextToken,
    listMonitoringAlertHistoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListMonitoringAlertHistory' smart constructor.
data ListMonitoringAlertHistory = ListMonitoringAlertHistory'
  { -- | A filter that returns only alerts created on or after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only alerts created on or before the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of results to display. The default is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of a monitoring alert.
    monitoringAlertName :: Prelude.Maybe Prelude.Text,
    -- | The name of a monitoring schedule.
    monitoringScheduleName :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous @ListMonitoringAlertHistory@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of alerts in the history, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The field used to sort results. The default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringAlertHistorySortKey,
    -- | The sort order, whether @Ascending@ or @Descending@, of the alert
    -- history. The default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that retrieves only alerts with a specific status.
    statusEquals :: Prelude.Maybe MonitoringAlertStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitoringAlertHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listMonitoringAlertHistory_creationTimeAfter' - A filter that returns only alerts created on or after the specified
-- time.
--
-- 'creationTimeBefore', 'listMonitoringAlertHistory_creationTimeBefore' - A filter that returns only alerts created on or before the specified
-- time.
--
-- 'maxResults', 'listMonitoringAlertHistory_maxResults' - The maximum number of results to display. The default is 100.
--
-- 'monitoringAlertName', 'listMonitoringAlertHistory_monitoringAlertName' - The name of a monitoring alert.
--
-- 'monitoringScheduleName', 'listMonitoringAlertHistory_monitoringScheduleName' - The name of a monitoring schedule.
--
-- 'nextToken', 'listMonitoringAlertHistory_nextToken' - If the result of the previous @ListMonitoringAlertHistory@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of alerts in the history, use the token in the next request.
--
-- 'sortBy', 'listMonitoringAlertHistory_sortBy' - The field used to sort results. The default is @CreationTime@.
--
-- 'sortOrder', 'listMonitoringAlertHistory_sortOrder' - The sort order, whether @Ascending@ or @Descending@, of the alert
-- history. The default is @Descending@.
--
-- 'statusEquals', 'listMonitoringAlertHistory_statusEquals' - A filter that retrieves only alerts with a specific status.
newListMonitoringAlertHistory ::
  ListMonitoringAlertHistory
newListMonitoringAlertHistory =
  ListMonitoringAlertHistory'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      monitoringAlertName = Prelude.Nothing,
      monitoringScheduleName = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | A filter that returns only alerts created on or after the specified
-- time.
listMonitoringAlertHistory_creationTimeAfter :: Lens.Lens' ListMonitoringAlertHistory (Prelude.Maybe Prelude.UTCTime)
listMonitoringAlertHistory_creationTimeAfter = Lens.lens (\ListMonitoringAlertHistory' {creationTimeAfter} -> creationTimeAfter) (\s@ListMonitoringAlertHistory' {} a -> s {creationTimeAfter = a} :: ListMonitoringAlertHistory) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only alerts created on or before the specified
-- time.
listMonitoringAlertHistory_creationTimeBefore :: Lens.Lens' ListMonitoringAlertHistory (Prelude.Maybe Prelude.UTCTime)
listMonitoringAlertHistory_creationTimeBefore = Lens.lens (\ListMonitoringAlertHistory' {creationTimeBefore} -> creationTimeBefore) (\s@ListMonitoringAlertHistory' {} a -> s {creationTimeBefore = a} :: ListMonitoringAlertHistory) Prelude.. Lens.mapping Data._Time

-- | The maximum number of results to display. The default is 100.
listMonitoringAlertHistory_maxResults :: Lens.Lens' ListMonitoringAlertHistory (Prelude.Maybe Prelude.Natural)
listMonitoringAlertHistory_maxResults = Lens.lens (\ListMonitoringAlertHistory' {maxResults} -> maxResults) (\s@ListMonitoringAlertHistory' {} a -> s {maxResults = a} :: ListMonitoringAlertHistory)

-- | The name of a monitoring alert.
listMonitoringAlertHistory_monitoringAlertName :: Lens.Lens' ListMonitoringAlertHistory (Prelude.Maybe Prelude.Text)
listMonitoringAlertHistory_monitoringAlertName = Lens.lens (\ListMonitoringAlertHistory' {monitoringAlertName} -> monitoringAlertName) (\s@ListMonitoringAlertHistory' {} a -> s {monitoringAlertName = a} :: ListMonitoringAlertHistory)

-- | The name of a monitoring schedule.
listMonitoringAlertHistory_monitoringScheduleName :: Lens.Lens' ListMonitoringAlertHistory (Prelude.Maybe Prelude.Text)
listMonitoringAlertHistory_monitoringScheduleName = Lens.lens (\ListMonitoringAlertHistory' {monitoringScheduleName} -> monitoringScheduleName) (\s@ListMonitoringAlertHistory' {} a -> s {monitoringScheduleName = a} :: ListMonitoringAlertHistory)

-- | If the result of the previous @ListMonitoringAlertHistory@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of alerts in the history, use the token in the next request.
listMonitoringAlertHistory_nextToken :: Lens.Lens' ListMonitoringAlertHistory (Prelude.Maybe Prelude.Text)
listMonitoringAlertHistory_nextToken = Lens.lens (\ListMonitoringAlertHistory' {nextToken} -> nextToken) (\s@ListMonitoringAlertHistory' {} a -> s {nextToken = a} :: ListMonitoringAlertHistory)

-- | The field used to sort results. The default is @CreationTime@.
listMonitoringAlertHistory_sortBy :: Lens.Lens' ListMonitoringAlertHistory (Prelude.Maybe MonitoringAlertHistorySortKey)
listMonitoringAlertHistory_sortBy = Lens.lens (\ListMonitoringAlertHistory' {sortBy} -> sortBy) (\s@ListMonitoringAlertHistory' {} a -> s {sortBy = a} :: ListMonitoringAlertHistory)

-- | The sort order, whether @Ascending@ or @Descending@, of the alert
-- history. The default is @Descending@.
listMonitoringAlertHistory_sortOrder :: Lens.Lens' ListMonitoringAlertHistory (Prelude.Maybe SortOrder)
listMonitoringAlertHistory_sortOrder = Lens.lens (\ListMonitoringAlertHistory' {sortOrder} -> sortOrder) (\s@ListMonitoringAlertHistory' {} a -> s {sortOrder = a} :: ListMonitoringAlertHistory)

-- | A filter that retrieves only alerts with a specific status.
listMonitoringAlertHistory_statusEquals :: Lens.Lens' ListMonitoringAlertHistory (Prelude.Maybe MonitoringAlertStatus)
listMonitoringAlertHistory_statusEquals = Lens.lens (\ListMonitoringAlertHistory' {statusEquals} -> statusEquals) (\s@ListMonitoringAlertHistory' {} a -> s {statusEquals = a} :: ListMonitoringAlertHistory)

instance Core.AWSPager ListMonitoringAlertHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMonitoringAlertHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMonitoringAlertHistoryResponse_monitoringAlertHistory
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMonitoringAlertHistory_nextToken
          Lens..~ rs
          Lens.^? listMonitoringAlertHistoryResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMonitoringAlertHistory where
  type
    AWSResponse ListMonitoringAlertHistory =
      ListMonitoringAlertHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitoringAlertHistoryResponse'
            Prelude.<$> ( x Data..?> "MonitoringAlertHistory"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMonitoringAlertHistory where
  hashWithSalt _salt ListMonitoringAlertHistory' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` monitoringAlertName
      `Prelude.hashWithSalt` monitoringScheduleName
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals

instance Prelude.NFData ListMonitoringAlertHistory where
  rnf ListMonitoringAlertHistory' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf monitoringAlertName
      `Prelude.seq` Prelude.rnf monitoringScheduleName
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals

instance Data.ToHeaders ListMonitoringAlertHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListMonitoringAlertHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMonitoringAlertHistory where
  toJSON ListMonitoringAlertHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("MonitoringAlertName" Data..=)
              Prelude.<$> monitoringAlertName,
            ("MonitoringScheduleName" Data..=)
              Prelude.<$> monitoringScheduleName,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
          ]
      )

instance Data.ToPath ListMonitoringAlertHistory where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMonitoringAlertHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMonitoringAlertHistoryResponse' smart constructor.
data ListMonitoringAlertHistoryResponse = ListMonitoringAlertHistoryResponse'
  { -- | An alert history for a model monitoring schedule.
    monitoringAlertHistory :: Prelude.Maybe [MonitoringAlertHistorySummary],
    -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of alerts, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitoringAlertHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringAlertHistory', 'listMonitoringAlertHistoryResponse_monitoringAlertHistory' - An alert history for a model monitoring schedule.
--
-- 'nextToken', 'listMonitoringAlertHistoryResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of alerts, use it in the subsequent request.
--
-- 'httpStatus', 'listMonitoringAlertHistoryResponse_httpStatus' - The response's http status code.
newListMonitoringAlertHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMonitoringAlertHistoryResponse
newListMonitoringAlertHistoryResponse pHttpStatus_ =
  ListMonitoringAlertHistoryResponse'
    { monitoringAlertHistory =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An alert history for a model monitoring schedule.
listMonitoringAlertHistoryResponse_monitoringAlertHistory :: Lens.Lens' ListMonitoringAlertHistoryResponse (Prelude.Maybe [MonitoringAlertHistorySummary])
listMonitoringAlertHistoryResponse_monitoringAlertHistory = Lens.lens (\ListMonitoringAlertHistoryResponse' {monitoringAlertHistory} -> monitoringAlertHistory) (\s@ListMonitoringAlertHistoryResponse' {} a -> s {monitoringAlertHistory = a} :: ListMonitoringAlertHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of alerts, use it in the subsequent request.
listMonitoringAlertHistoryResponse_nextToken :: Lens.Lens' ListMonitoringAlertHistoryResponse (Prelude.Maybe Prelude.Text)
listMonitoringAlertHistoryResponse_nextToken = Lens.lens (\ListMonitoringAlertHistoryResponse' {nextToken} -> nextToken) (\s@ListMonitoringAlertHistoryResponse' {} a -> s {nextToken = a} :: ListMonitoringAlertHistoryResponse)

-- | The response's http status code.
listMonitoringAlertHistoryResponse_httpStatus :: Lens.Lens' ListMonitoringAlertHistoryResponse Prelude.Int
listMonitoringAlertHistoryResponse_httpStatus = Lens.lens (\ListMonitoringAlertHistoryResponse' {httpStatus} -> httpStatus) (\s@ListMonitoringAlertHistoryResponse' {} a -> s {httpStatus = a} :: ListMonitoringAlertHistoryResponse)

instance
  Prelude.NFData
    ListMonitoringAlertHistoryResponse
  where
  rnf ListMonitoringAlertHistoryResponse' {..} =
    Prelude.rnf monitoringAlertHistory
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
