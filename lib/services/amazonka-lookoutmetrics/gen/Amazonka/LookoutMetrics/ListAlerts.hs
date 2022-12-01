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
-- Module      : Amazonka.LookoutMetrics.ListAlerts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the alerts attached to a detector.
--
-- Amazon Lookout for Metrics API actions are eventually consistent. If you
-- do a read operation on a resource immediately after creating or
-- modifying it, use retries to allow time for the write operation to
-- complete.
module Amazonka.LookoutMetrics.ListAlerts
  ( -- * Creating a Request
    ListAlerts (..),
    newListAlerts,

    -- * Request Lenses
    listAlerts_nextToken,
    listAlerts_anomalyDetectorArn,
    listAlerts_maxResults,

    -- * Destructuring the Response
    ListAlertsResponse (..),
    newListAlertsResponse,

    -- * Response Lenses
    listAlertsResponse_nextToken,
    listAlertsResponse_alertSummaryList,
    listAlertsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAlerts' smart constructor.
data ListAlerts = ListAlerts'
  { -- | If the result of the previous request is truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the alert\'s detector.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that will be displayed by the request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlerts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAlerts_nextToken' - If the result of the previous request is truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
--
-- 'anomalyDetectorArn', 'listAlerts_anomalyDetectorArn' - The ARN of the alert\'s detector.
--
-- 'maxResults', 'listAlerts_maxResults' - The maximum number of results that will be displayed by the request.
newListAlerts ::
  ListAlerts
newListAlerts =
  ListAlerts'
    { nextToken = Prelude.Nothing,
      anomalyDetectorArn = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the result of the previous request is truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listAlerts_nextToken :: Lens.Lens' ListAlerts (Prelude.Maybe Prelude.Text)
listAlerts_nextToken = Lens.lens (\ListAlerts' {nextToken} -> nextToken) (\s@ListAlerts' {} a -> s {nextToken = a} :: ListAlerts)

-- | The ARN of the alert\'s detector.
listAlerts_anomalyDetectorArn :: Lens.Lens' ListAlerts (Prelude.Maybe Prelude.Text)
listAlerts_anomalyDetectorArn = Lens.lens (\ListAlerts' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@ListAlerts' {} a -> s {anomalyDetectorArn = a} :: ListAlerts)

-- | The maximum number of results that will be displayed by the request.
listAlerts_maxResults :: Lens.Lens' ListAlerts (Prelude.Maybe Prelude.Natural)
listAlerts_maxResults = Lens.lens (\ListAlerts' {maxResults} -> maxResults) (\s@ListAlerts' {} a -> s {maxResults = a} :: ListAlerts)

instance Core.AWSRequest ListAlerts where
  type AWSResponse ListAlerts = ListAlertsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlertsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "AlertSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAlerts where
  hashWithSalt _salt ListAlerts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAlerts where
  rnf ListAlerts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListAlerts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAlerts where
  toJSON ListAlerts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("AnomalyDetectorArn" Core..=)
              Prelude.<$> anomalyDetectorArn,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListAlerts where
  toPath = Prelude.const "/ListAlerts"

instance Core.ToQuery ListAlerts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAlertsResponse' smart constructor.
data ListAlertsResponse = ListAlertsResponse'
  { -- | If the response is truncated, the service returns this token. To
    -- retrieve the next set of results, use this token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains information about an alert.
    alertSummaryList :: Prelude.Maybe [AlertSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlertsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAlertsResponse_nextToken' - If the response is truncated, the service returns this token. To
-- retrieve the next set of results, use this token in the next request.
--
-- 'alertSummaryList', 'listAlertsResponse_alertSummaryList' - Contains information about an alert.
--
-- 'httpStatus', 'listAlertsResponse_httpStatus' - The response's http status code.
newListAlertsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAlertsResponse
newListAlertsResponse pHttpStatus_ =
  ListAlertsResponse'
    { nextToken = Prelude.Nothing,
      alertSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, the service returns this token. To
-- retrieve the next set of results, use this token in the next request.
listAlertsResponse_nextToken :: Lens.Lens' ListAlertsResponse (Prelude.Maybe Prelude.Text)
listAlertsResponse_nextToken = Lens.lens (\ListAlertsResponse' {nextToken} -> nextToken) (\s@ListAlertsResponse' {} a -> s {nextToken = a} :: ListAlertsResponse)

-- | Contains information about an alert.
listAlertsResponse_alertSummaryList :: Lens.Lens' ListAlertsResponse (Prelude.Maybe [AlertSummary])
listAlertsResponse_alertSummaryList = Lens.lens (\ListAlertsResponse' {alertSummaryList} -> alertSummaryList) (\s@ListAlertsResponse' {} a -> s {alertSummaryList = a} :: ListAlertsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAlertsResponse_httpStatus :: Lens.Lens' ListAlertsResponse Prelude.Int
listAlertsResponse_httpStatus = Lens.lens (\ListAlertsResponse' {httpStatus} -> httpStatus) (\s@ListAlertsResponse' {} a -> s {httpStatus = a} :: ListAlertsResponse)

instance Prelude.NFData ListAlertsResponse where
  rnf ListAlertsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf alertSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
