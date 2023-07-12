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
-- Module      : Amazonka.SageMaker.ListMonitoringAlerts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the alerts for a single monitoring schedule.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListMonitoringAlerts
  ( -- * Creating a Request
    ListMonitoringAlerts (..),
    newListMonitoringAlerts,

    -- * Request Lenses
    listMonitoringAlerts_maxResults,
    listMonitoringAlerts_nextToken,
    listMonitoringAlerts_monitoringScheduleName,

    -- * Destructuring the Response
    ListMonitoringAlertsResponse (..),
    newListMonitoringAlertsResponse,

    -- * Response Lenses
    listMonitoringAlertsResponse_monitoringAlertSummaries,
    listMonitoringAlertsResponse_nextToken,
    listMonitoringAlertsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListMonitoringAlerts' smart constructor.
data ListMonitoringAlerts = ListMonitoringAlerts'
  { -- | The maximum number of results to display. The default is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous @ListMonitoringAlerts@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of alerts in the history, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of a monitoring schedule.
    monitoringScheduleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitoringAlerts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMonitoringAlerts_maxResults' - The maximum number of results to display. The default is 100.
--
-- 'nextToken', 'listMonitoringAlerts_nextToken' - If the result of the previous @ListMonitoringAlerts@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of alerts in the history, use the token in the next request.
--
-- 'monitoringScheduleName', 'listMonitoringAlerts_monitoringScheduleName' - The name of a monitoring schedule.
newListMonitoringAlerts ::
  -- | 'monitoringScheduleName'
  Prelude.Text ->
  ListMonitoringAlerts
newListMonitoringAlerts pMonitoringScheduleName_ =
  ListMonitoringAlerts'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      monitoringScheduleName = pMonitoringScheduleName_
    }

-- | The maximum number of results to display. The default is 100.
listMonitoringAlerts_maxResults :: Lens.Lens' ListMonitoringAlerts (Prelude.Maybe Prelude.Natural)
listMonitoringAlerts_maxResults = Lens.lens (\ListMonitoringAlerts' {maxResults} -> maxResults) (\s@ListMonitoringAlerts' {} a -> s {maxResults = a} :: ListMonitoringAlerts)

-- | If the result of the previous @ListMonitoringAlerts@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of alerts in the history, use the token in the next request.
listMonitoringAlerts_nextToken :: Lens.Lens' ListMonitoringAlerts (Prelude.Maybe Prelude.Text)
listMonitoringAlerts_nextToken = Lens.lens (\ListMonitoringAlerts' {nextToken} -> nextToken) (\s@ListMonitoringAlerts' {} a -> s {nextToken = a} :: ListMonitoringAlerts)

-- | The name of a monitoring schedule.
listMonitoringAlerts_monitoringScheduleName :: Lens.Lens' ListMonitoringAlerts Prelude.Text
listMonitoringAlerts_monitoringScheduleName = Lens.lens (\ListMonitoringAlerts' {monitoringScheduleName} -> monitoringScheduleName) (\s@ListMonitoringAlerts' {} a -> s {monitoringScheduleName = a} :: ListMonitoringAlerts)

instance Core.AWSPager ListMonitoringAlerts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMonitoringAlertsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMonitoringAlertsResponse_monitoringAlertSummaries
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listMonitoringAlerts_nextToken
          Lens..~ rs
          Lens.^? listMonitoringAlertsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListMonitoringAlerts where
  type
    AWSResponse ListMonitoringAlerts =
      ListMonitoringAlertsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitoringAlertsResponse'
            Prelude.<$> (x Data..?> "MonitoringAlertSummaries")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMonitoringAlerts where
  hashWithSalt _salt ListMonitoringAlerts' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` monitoringScheduleName

instance Prelude.NFData ListMonitoringAlerts where
  rnf ListMonitoringAlerts' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf monitoringScheduleName

instance Data.ToHeaders ListMonitoringAlerts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListMonitoringAlerts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMonitoringAlerts where
  toJSON ListMonitoringAlerts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "MonitoringScheduleName"
                  Data..= monitoringScheduleName
              )
          ]
      )

instance Data.ToPath ListMonitoringAlerts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMonitoringAlerts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMonitoringAlertsResponse' smart constructor.
data ListMonitoringAlertsResponse = ListMonitoringAlertsResponse'
  { -- | A JSON array where each element is a summary for a monitoring alert.
    monitoringAlertSummaries :: Prelude.Maybe (Prelude.NonEmpty MonitoringAlertSummary),
    -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of alerts, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitoringAlertsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringAlertSummaries', 'listMonitoringAlertsResponse_monitoringAlertSummaries' - A JSON array where each element is a summary for a monitoring alert.
--
-- 'nextToken', 'listMonitoringAlertsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of alerts, use it in the subsequent request.
--
-- 'httpStatus', 'listMonitoringAlertsResponse_httpStatus' - The response's http status code.
newListMonitoringAlertsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMonitoringAlertsResponse
newListMonitoringAlertsResponse pHttpStatus_ =
  ListMonitoringAlertsResponse'
    { monitoringAlertSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JSON array where each element is a summary for a monitoring alert.
listMonitoringAlertsResponse_monitoringAlertSummaries :: Lens.Lens' ListMonitoringAlertsResponse (Prelude.Maybe (Prelude.NonEmpty MonitoringAlertSummary))
listMonitoringAlertsResponse_monitoringAlertSummaries = Lens.lens (\ListMonitoringAlertsResponse' {monitoringAlertSummaries} -> monitoringAlertSummaries) (\s@ListMonitoringAlertsResponse' {} a -> s {monitoringAlertSummaries = a} :: ListMonitoringAlertsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of alerts, use it in the subsequent request.
listMonitoringAlertsResponse_nextToken :: Lens.Lens' ListMonitoringAlertsResponse (Prelude.Maybe Prelude.Text)
listMonitoringAlertsResponse_nextToken = Lens.lens (\ListMonitoringAlertsResponse' {nextToken} -> nextToken) (\s@ListMonitoringAlertsResponse' {} a -> s {nextToken = a} :: ListMonitoringAlertsResponse)

-- | The response's http status code.
listMonitoringAlertsResponse_httpStatus :: Lens.Lens' ListMonitoringAlertsResponse Prelude.Int
listMonitoringAlertsResponse_httpStatus = Lens.lens (\ListMonitoringAlertsResponse' {httpStatus} -> httpStatus) (\s@ListMonitoringAlertsResponse' {} a -> s {httpStatus = a} :: ListMonitoringAlertsResponse)

instance Prelude.NFData ListMonitoringAlertsResponse where
  rnf ListMonitoringAlertsResponse' {..} =
    Prelude.rnf monitoringAlertSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
