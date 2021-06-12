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
-- Module      : Network.AWS.CostExplorer.GetAnomalyMonitors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly monitor definitions for your account. You can
-- filter using a list of cost anomaly monitor Amazon Resource Names
-- (ARNs).
module Network.AWS.CostExplorer.GetAnomalyMonitors
  ( -- * Creating a Request
    GetAnomalyMonitors (..),
    newGetAnomalyMonitors,

    -- * Request Lenses
    getAnomalyMonitors_maxResults,
    getAnomalyMonitors_nextPageToken,
    getAnomalyMonitors_monitorArnList,

    -- * Destructuring the Response
    GetAnomalyMonitorsResponse (..),
    newGetAnomalyMonitorsResponse,

    -- * Response Lenses
    getAnomalyMonitorsResponse_nextPageToken,
    getAnomalyMonitorsResponse_httpStatus,
    getAnomalyMonitorsResponse_anomalyMonitors,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAnomalyMonitors' smart constructor.
data GetAnomalyMonitors = GetAnomalyMonitors'
  { -- | The number of entries a paginated response contains.
    maxResults :: Core.Maybe Core.Int,
    -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | A list of cost anomaly monitor ARNs.
    monitorArnList :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAnomalyMonitors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getAnomalyMonitors_maxResults' - The number of entries a paginated response contains.
--
-- 'nextPageToken', 'getAnomalyMonitors_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'monitorArnList', 'getAnomalyMonitors_monitorArnList' - A list of cost anomaly monitor ARNs.
newGetAnomalyMonitors ::
  GetAnomalyMonitors
newGetAnomalyMonitors =
  GetAnomalyMonitors'
    { maxResults = Core.Nothing,
      nextPageToken = Core.Nothing,
      monitorArnList = Core.Nothing
    }

-- | The number of entries a paginated response contains.
getAnomalyMonitors_maxResults :: Lens.Lens' GetAnomalyMonitors (Core.Maybe Core.Int)
getAnomalyMonitors_maxResults = Lens.lens (\GetAnomalyMonitors' {maxResults} -> maxResults) (\s@GetAnomalyMonitors' {} a -> s {maxResults = a} :: GetAnomalyMonitors)

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getAnomalyMonitors_nextPageToken :: Lens.Lens' GetAnomalyMonitors (Core.Maybe Core.Text)
getAnomalyMonitors_nextPageToken = Lens.lens (\GetAnomalyMonitors' {nextPageToken} -> nextPageToken) (\s@GetAnomalyMonitors' {} a -> s {nextPageToken = a} :: GetAnomalyMonitors)

-- | A list of cost anomaly monitor ARNs.
getAnomalyMonitors_monitorArnList :: Lens.Lens' GetAnomalyMonitors (Core.Maybe [Core.Text])
getAnomalyMonitors_monitorArnList = Lens.lens (\GetAnomalyMonitors' {monitorArnList} -> monitorArnList) (\s@GetAnomalyMonitors' {} a -> s {monitorArnList = a} :: GetAnomalyMonitors) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest GetAnomalyMonitors where
  type
    AWSResponse GetAnomalyMonitors =
      GetAnomalyMonitorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnomalyMonitorsResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "AnomalyMonitors" Core..!@ Core.mempty)
      )

instance Core.Hashable GetAnomalyMonitors

instance Core.NFData GetAnomalyMonitors

instance Core.ToHeaders GetAnomalyMonitors where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetAnomalyMonitors" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAnomalyMonitors where
  toJSON GetAnomalyMonitors' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("MonitorArnList" Core..=) Core.<$> monitorArnList
          ]
      )

instance Core.ToPath GetAnomalyMonitors where
  toPath = Core.const "/"

instance Core.ToQuery GetAnomalyMonitors where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAnomalyMonitorsResponse' smart constructor.
data GetAnomalyMonitorsResponse = GetAnomalyMonitorsResponse'
  { -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of cost anomaly monitors that includes the detailed metadata for
    -- each monitor.
    anomalyMonitors :: [AnomalyMonitor]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAnomalyMonitorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getAnomalyMonitorsResponse_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'httpStatus', 'getAnomalyMonitorsResponse_httpStatus' - The response's http status code.
--
-- 'anomalyMonitors', 'getAnomalyMonitorsResponse_anomalyMonitors' - A list of cost anomaly monitors that includes the detailed metadata for
-- each monitor.
newGetAnomalyMonitorsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAnomalyMonitorsResponse
newGetAnomalyMonitorsResponse pHttpStatus_ =
  GetAnomalyMonitorsResponse'
    { nextPageToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      anomalyMonitors = Core.mempty
    }

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getAnomalyMonitorsResponse_nextPageToken :: Lens.Lens' GetAnomalyMonitorsResponse (Core.Maybe Core.Text)
getAnomalyMonitorsResponse_nextPageToken = Lens.lens (\GetAnomalyMonitorsResponse' {nextPageToken} -> nextPageToken) (\s@GetAnomalyMonitorsResponse' {} a -> s {nextPageToken = a} :: GetAnomalyMonitorsResponse)

-- | The response's http status code.
getAnomalyMonitorsResponse_httpStatus :: Lens.Lens' GetAnomalyMonitorsResponse Core.Int
getAnomalyMonitorsResponse_httpStatus = Lens.lens (\GetAnomalyMonitorsResponse' {httpStatus} -> httpStatus) (\s@GetAnomalyMonitorsResponse' {} a -> s {httpStatus = a} :: GetAnomalyMonitorsResponse)

-- | A list of cost anomaly monitors that includes the detailed metadata for
-- each monitor.
getAnomalyMonitorsResponse_anomalyMonitors :: Lens.Lens' GetAnomalyMonitorsResponse [AnomalyMonitor]
getAnomalyMonitorsResponse_anomalyMonitors = Lens.lens (\GetAnomalyMonitorsResponse' {anomalyMonitors} -> anomalyMonitors) (\s@GetAnomalyMonitorsResponse' {} a -> s {anomalyMonitors = a} :: GetAnomalyMonitorsResponse) Core.. Lens._Coerce

instance Core.NFData GetAnomalyMonitorsResponse
