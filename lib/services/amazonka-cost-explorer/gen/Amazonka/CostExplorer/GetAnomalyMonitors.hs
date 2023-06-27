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
-- Module      : Amazonka.CostExplorer.GetAnomalyMonitors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly monitor definitions for your account. You can
-- filter using a list of cost anomaly monitor Amazon Resource Names
-- (ARNs).
module Amazonka.CostExplorer.GetAnomalyMonitors
  ( -- * Creating a Request
    GetAnomalyMonitors (..),
    newGetAnomalyMonitors,

    -- * Request Lenses
    getAnomalyMonitors_maxResults,
    getAnomalyMonitors_monitorArnList,
    getAnomalyMonitors_nextPageToken,

    -- * Destructuring the Response
    GetAnomalyMonitorsResponse (..),
    newGetAnomalyMonitorsResponse,

    -- * Response Lenses
    getAnomalyMonitorsResponse_nextPageToken,
    getAnomalyMonitorsResponse_httpStatus,
    getAnomalyMonitorsResponse_anomalyMonitors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAnomalyMonitors' smart constructor.
data GetAnomalyMonitors = GetAnomalyMonitors'
  { -- | The number of entries that a paginated response contains.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A list of cost anomaly monitor ARNs.
    monitorArnList :: Prelude.Maybe [Prelude.Text],
    -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnomalyMonitors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getAnomalyMonitors_maxResults' - The number of entries that a paginated response contains.
--
-- 'monitorArnList', 'getAnomalyMonitors_monitorArnList' - A list of cost anomaly monitor ARNs.
--
-- 'nextPageToken', 'getAnomalyMonitors_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
newGetAnomalyMonitors ::
  GetAnomalyMonitors
newGetAnomalyMonitors =
  GetAnomalyMonitors'
    { maxResults = Prelude.Nothing,
      monitorArnList = Prelude.Nothing,
      nextPageToken = Prelude.Nothing
    }

-- | The number of entries that a paginated response contains.
getAnomalyMonitors_maxResults :: Lens.Lens' GetAnomalyMonitors (Prelude.Maybe Prelude.Int)
getAnomalyMonitors_maxResults = Lens.lens (\GetAnomalyMonitors' {maxResults} -> maxResults) (\s@GetAnomalyMonitors' {} a -> s {maxResults = a} :: GetAnomalyMonitors)

-- | A list of cost anomaly monitor ARNs.
getAnomalyMonitors_monitorArnList :: Lens.Lens' GetAnomalyMonitors (Prelude.Maybe [Prelude.Text])
getAnomalyMonitors_monitorArnList = Lens.lens (\GetAnomalyMonitors' {monitorArnList} -> monitorArnList) (\s@GetAnomalyMonitors' {} a -> s {monitorArnList = a} :: GetAnomalyMonitors) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getAnomalyMonitors_nextPageToken :: Lens.Lens' GetAnomalyMonitors (Prelude.Maybe Prelude.Text)
getAnomalyMonitors_nextPageToken = Lens.lens (\GetAnomalyMonitors' {nextPageToken} -> nextPageToken) (\s@GetAnomalyMonitors' {} a -> s {nextPageToken = a} :: GetAnomalyMonitors)

instance Core.AWSRequest GetAnomalyMonitors where
  type
    AWSResponse GetAnomalyMonitors =
      GetAnomalyMonitorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnomalyMonitorsResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "AnomalyMonitors"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetAnomalyMonitors where
  hashWithSalt _salt GetAnomalyMonitors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` monitorArnList
      `Prelude.hashWithSalt` nextPageToken

instance Prelude.NFData GetAnomalyMonitors where
  rnf GetAnomalyMonitors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf monitorArnList
      `Prelude.seq` Prelude.rnf nextPageToken

instance Data.ToHeaders GetAnomalyMonitors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetAnomalyMonitors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAnomalyMonitors where
  toJSON GetAnomalyMonitors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("MonitorArnList" Data..=)
              Prelude.<$> monitorArnList,
            ("NextPageToken" Data..=) Prelude.<$> nextPageToken
          ]
      )

instance Data.ToPath GetAnomalyMonitors where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAnomalyMonitors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAnomalyMonitorsResponse' smart constructor.
data GetAnomalyMonitorsResponse = GetAnomalyMonitorsResponse'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of cost anomaly monitors that includes the detailed metadata for
    -- each monitor.
    anomalyMonitors :: [AnomalyMonitor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnomalyMonitorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getAnomalyMonitorsResponse_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'httpStatus', 'getAnomalyMonitorsResponse_httpStatus' - The response's http status code.
--
-- 'anomalyMonitors', 'getAnomalyMonitorsResponse_anomalyMonitors' - A list of cost anomaly monitors that includes the detailed metadata for
-- each monitor.
newGetAnomalyMonitorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAnomalyMonitorsResponse
newGetAnomalyMonitorsResponse pHttpStatus_ =
  GetAnomalyMonitorsResponse'
    { nextPageToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      anomalyMonitors = Prelude.mempty
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getAnomalyMonitorsResponse_nextPageToken :: Lens.Lens' GetAnomalyMonitorsResponse (Prelude.Maybe Prelude.Text)
getAnomalyMonitorsResponse_nextPageToken = Lens.lens (\GetAnomalyMonitorsResponse' {nextPageToken} -> nextPageToken) (\s@GetAnomalyMonitorsResponse' {} a -> s {nextPageToken = a} :: GetAnomalyMonitorsResponse)

-- | The response's http status code.
getAnomalyMonitorsResponse_httpStatus :: Lens.Lens' GetAnomalyMonitorsResponse Prelude.Int
getAnomalyMonitorsResponse_httpStatus = Lens.lens (\GetAnomalyMonitorsResponse' {httpStatus} -> httpStatus) (\s@GetAnomalyMonitorsResponse' {} a -> s {httpStatus = a} :: GetAnomalyMonitorsResponse)

-- | A list of cost anomaly monitors that includes the detailed metadata for
-- each monitor.
getAnomalyMonitorsResponse_anomalyMonitors :: Lens.Lens' GetAnomalyMonitorsResponse [AnomalyMonitor]
getAnomalyMonitorsResponse_anomalyMonitors = Lens.lens (\GetAnomalyMonitorsResponse' {anomalyMonitors} -> anomalyMonitors) (\s@GetAnomalyMonitorsResponse' {} a -> s {anomalyMonitors = a} :: GetAnomalyMonitorsResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetAnomalyMonitorsResponse where
  rnf GetAnomalyMonitorsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf anomalyMonitors
