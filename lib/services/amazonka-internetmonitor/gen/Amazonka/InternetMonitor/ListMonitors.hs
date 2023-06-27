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
-- Module      : Amazonka.InternetMonitor.ListMonitors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of your monitors for Amazon CloudWatch Internet Monitor and
-- their statuses, along with the Amazon Resource Name (ARN) and name of
-- each monitor.
--
-- This operation returns paginated results.
module Amazonka.InternetMonitor.ListMonitors
  ( -- * Creating a Request
    ListMonitors (..),
    newListMonitors,

    -- * Request Lenses
    listMonitors_maxResults,
    listMonitors_monitorStatus,
    listMonitors_nextToken,

    -- * Destructuring the Response
    ListMonitorsResponse (..),
    newListMonitorsResponse,

    -- * Response Lenses
    listMonitorsResponse_nextToken,
    listMonitorsResponse_httpStatus,
    listMonitorsResponse_monitors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMonitors' smart constructor.
data ListMonitors = ListMonitors'
  { -- | The number of monitor objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The status of a monitor. This includes the status of the data processing
    -- for the monitor and the status of the monitor itself.
    --
    -- For information about the statuses for a monitor, see
    -- <https://docs.aws.amazon.com/internet-monitor/latest/api/API_Monitor.html Monitor>.
    monitorStatus :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMonitors_maxResults' - The number of monitor objects that you want to return with this call.
--
-- 'monitorStatus', 'listMonitors_monitorStatus' - The status of a monitor. This includes the status of the data processing
-- for the monitor and the status of the monitor itself.
--
-- For information about the statuses for a monitor, see
-- <https://docs.aws.amazon.com/internet-monitor/latest/api/API_Monitor.html Monitor>.
--
-- 'nextToken', 'listMonitors_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
newListMonitors ::
  ListMonitors
newListMonitors =
  ListMonitors'
    { maxResults = Prelude.Nothing,
      monitorStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of monitor objects that you want to return with this call.
listMonitors_maxResults :: Lens.Lens' ListMonitors (Prelude.Maybe Prelude.Natural)
listMonitors_maxResults = Lens.lens (\ListMonitors' {maxResults} -> maxResults) (\s@ListMonitors' {} a -> s {maxResults = a} :: ListMonitors)

-- | The status of a monitor. This includes the status of the data processing
-- for the monitor and the status of the monitor itself.
--
-- For information about the statuses for a monitor, see
-- <https://docs.aws.amazon.com/internet-monitor/latest/api/API_Monitor.html Monitor>.
listMonitors_monitorStatus :: Lens.Lens' ListMonitors (Prelude.Maybe Prelude.Text)
listMonitors_monitorStatus = Lens.lens (\ListMonitors' {monitorStatus} -> monitorStatus) (\s@ListMonitors' {} a -> s {monitorStatus = a} :: ListMonitors)

-- | The token for the next set of results. You receive this token from a
-- previous call.
listMonitors_nextToken :: Lens.Lens' ListMonitors (Prelude.Maybe Prelude.Text)
listMonitors_nextToken = Lens.lens (\ListMonitors' {nextToken} -> nextToken) (\s@ListMonitors' {} a -> s {nextToken = a} :: ListMonitors)

instance Core.AWSPager ListMonitors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMonitorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listMonitorsResponse_monitors) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listMonitors_nextToken
          Lens..~ rs
          Lens.^? listMonitorsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListMonitors where
  type AWSResponse ListMonitors = ListMonitorsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitorsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Monitors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListMonitors where
  hashWithSalt _salt ListMonitors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` monitorStatus
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMonitors where
  rnf ListMonitors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf monitorStatus
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListMonitors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListMonitors where
  toPath = Prelude.const "/v20210603/Monitors"

instance Data.ToQuery ListMonitors where
  toQuery ListMonitors' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "MonitorStatus" Data.=: monitorStatus,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListMonitorsResponse' smart constructor.
data ListMonitorsResponse = ListMonitorsResponse'
  { -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of monitors.
    monitors :: [Monitor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMonitorsResponse_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'httpStatus', 'listMonitorsResponse_httpStatus' - The response's http status code.
--
-- 'monitors', 'listMonitorsResponse_monitors' - A list of monitors.
newListMonitorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMonitorsResponse
newListMonitorsResponse pHttpStatus_ =
  ListMonitorsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      monitors = Prelude.mempty
    }

-- | The token for the next set of results. You receive this token from a
-- previous call.
listMonitorsResponse_nextToken :: Lens.Lens' ListMonitorsResponse (Prelude.Maybe Prelude.Text)
listMonitorsResponse_nextToken = Lens.lens (\ListMonitorsResponse' {nextToken} -> nextToken) (\s@ListMonitorsResponse' {} a -> s {nextToken = a} :: ListMonitorsResponse)

-- | The response's http status code.
listMonitorsResponse_httpStatus :: Lens.Lens' ListMonitorsResponse Prelude.Int
listMonitorsResponse_httpStatus = Lens.lens (\ListMonitorsResponse' {httpStatus} -> httpStatus) (\s@ListMonitorsResponse' {} a -> s {httpStatus = a} :: ListMonitorsResponse)

-- | A list of monitors.
listMonitorsResponse_monitors :: Lens.Lens' ListMonitorsResponse [Monitor]
listMonitorsResponse_monitors = Lens.lens (\ListMonitorsResponse' {monitors} -> monitors) (\s@ListMonitorsResponse' {} a -> s {monitors = a} :: ListMonitorsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListMonitorsResponse where
  rnf ListMonitorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf monitors
