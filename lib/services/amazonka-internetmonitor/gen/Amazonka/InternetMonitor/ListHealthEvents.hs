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
-- Module      : Amazonka.InternetMonitor.ListHealthEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all health events for a monitor in Amazon CloudWatch Internet
-- Monitor. Returns all information for health events including the client
-- location information the network cause and status, event start and end
-- time, percentage of total traffic impacted, and status.
--
-- Health events that have start times during the time frame that is
-- requested are not included in the list of health events.
--
-- This operation returns paginated results.
module Amazonka.InternetMonitor.ListHealthEvents
  ( -- * Creating a Request
    ListHealthEvents (..),
    newListHealthEvents,

    -- * Request Lenses
    listHealthEvents_endTime,
    listHealthEvents_eventStatus,
    listHealthEvents_maxResults,
    listHealthEvents_nextToken,
    listHealthEvents_startTime,
    listHealthEvents_monitorName,

    -- * Destructuring the Response
    ListHealthEventsResponse (..),
    newListHealthEventsResponse,

    -- * Response Lenses
    listHealthEventsResponse_nextToken,
    listHealthEventsResponse_httpStatus,
    listHealthEventsResponse_healthEvents,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListHealthEvents' smart constructor.
data ListHealthEvents = ListHealthEvents'
  { -- | The time when a health event ended. If the health event is still
    -- ongoing, then the end time is not set.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The status of a health event.
    eventStatus :: Prelude.Maybe HealthEventStatus,
    -- | The number of health event objects that you want to return with this
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time when a health event started.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the monitor.
    monitorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHealthEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'listHealthEvents_endTime' - The time when a health event ended. If the health event is still
-- ongoing, then the end time is not set.
--
-- 'eventStatus', 'listHealthEvents_eventStatus' - The status of a health event.
--
-- 'maxResults', 'listHealthEvents_maxResults' - The number of health event objects that you want to return with this
-- call.
--
-- 'nextToken', 'listHealthEvents_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'startTime', 'listHealthEvents_startTime' - The time when a health event started.
--
-- 'monitorName', 'listHealthEvents_monitorName' - The name of the monitor.
newListHealthEvents ::
  -- | 'monitorName'
  Prelude.Text ->
  ListHealthEvents
newListHealthEvents pMonitorName_ =
  ListHealthEvents'
    { endTime = Prelude.Nothing,
      eventStatus = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing,
      monitorName = pMonitorName_
    }

-- | The time when a health event ended. If the health event is still
-- ongoing, then the end time is not set.
listHealthEvents_endTime :: Lens.Lens' ListHealthEvents (Prelude.Maybe Prelude.UTCTime)
listHealthEvents_endTime = Lens.lens (\ListHealthEvents' {endTime} -> endTime) (\s@ListHealthEvents' {} a -> s {endTime = a} :: ListHealthEvents) Prelude.. Lens.mapping Data._Time

-- | The status of a health event.
listHealthEvents_eventStatus :: Lens.Lens' ListHealthEvents (Prelude.Maybe HealthEventStatus)
listHealthEvents_eventStatus = Lens.lens (\ListHealthEvents' {eventStatus} -> eventStatus) (\s@ListHealthEvents' {} a -> s {eventStatus = a} :: ListHealthEvents)

-- | The number of health event objects that you want to return with this
-- call.
listHealthEvents_maxResults :: Lens.Lens' ListHealthEvents (Prelude.Maybe Prelude.Natural)
listHealthEvents_maxResults = Lens.lens (\ListHealthEvents' {maxResults} -> maxResults) (\s@ListHealthEvents' {} a -> s {maxResults = a} :: ListHealthEvents)

-- | The token for the next set of results. You receive this token from a
-- previous call.
listHealthEvents_nextToken :: Lens.Lens' ListHealthEvents (Prelude.Maybe Prelude.Text)
listHealthEvents_nextToken = Lens.lens (\ListHealthEvents' {nextToken} -> nextToken) (\s@ListHealthEvents' {} a -> s {nextToken = a} :: ListHealthEvents)

-- | The time when a health event started.
listHealthEvents_startTime :: Lens.Lens' ListHealthEvents (Prelude.Maybe Prelude.UTCTime)
listHealthEvents_startTime = Lens.lens (\ListHealthEvents' {startTime} -> startTime) (\s@ListHealthEvents' {} a -> s {startTime = a} :: ListHealthEvents) Prelude.. Lens.mapping Data._Time

-- | The name of the monitor.
listHealthEvents_monitorName :: Lens.Lens' ListHealthEvents Prelude.Text
listHealthEvents_monitorName = Lens.lens (\ListHealthEvents' {monitorName} -> monitorName) (\s@ListHealthEvents' {} a -> s {monitorName = a} :: ListHealthEvents)

instance Core.AWSPager ListHealthEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHealthEventsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listHealthEventsResponse_healthEvents) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listHealthEvents_nextToken
          Lens..~ rs
          Lens.^? listHealthEventsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListHealthEvents where
  type
    AWSResponse ListHealthEvents =
      ListHealthEventsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHealthEventsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "HealthEvents" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListHealthEvents where
  hashWithSalt _salt ListHealthEvents' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` eventStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` monitorName

instance Prelude.NFData ListHealthEvents where
  rnf ListHealthEvents' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf eventStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf monitorName

instance Data.ToHeaders ListHealthEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListHealthEvents where
  toPath ListHealthEvents' {..} =
    Prelude.mconcat
      [ "/v20210603/Monitors/",
        Data.toBS monitorName,
        "/HealthEvents"
      ]

instance Data.ToQuery ListHealthEvents where
  toQuery ListHealthEvents' {..} =
    Prelude.mconcat
      [ "EndTime" Data.=: endTime,
        "EventStatus" Data.=: eventStatus,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "StartTime" Data.=: startTime
      ]

-- | /See:/ 'newListHealthEventsResponse' smart constructor.
data ListHealthEventsResponse = ListHealthEventsResponse'
  { -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of health events.
    healthEvents :: [HealthEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHealthEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHealthEventsResponse_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'httpStatus', 'listHealthEventsResponse_httpStatus' - The response's http status code.
--
-- 'healthEvents', 'listHealthEventsResponse_healthEvents' - A list of health events.
newListHealthEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHealthEventsResponse
newListHealthEventsResponse pHttpStatus_ =
  ListHealthEventsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      healthEvents = Prelude.mempty
    }

-- | The token for the next set of results. You receive this token from a
-- previous call.
listHealthEventsResponse_nextToken :: Lens.Lens' ListHealthEventsResponse (Prelude.Maybe Prelude.Text)
listHealthEventsResponse_nextToken = Lens.lens (\ListHealthEventsResponse' {nextToken} -> nextToken) (\s@ListHealthEventsResponse' {} a -> s {nextToken = a} :: ListHealthEventsResponse)

-- | The response's http status code.
listHealthEventsResponse_httpStatus :: Lens.Lens' ListHealthEventsResponse Prelude.Int
listHealthEventsResponse_httpStatus = Lens.lens (\ListHealthEventsResponse' {httpStatus} -> httpStatus) (\s@ListHealthEventsResponse' {} a -> s {httpStatus = a} :: ListHealthEventsResponse)

-- | A list of health events.
listHealthEventsResponse_healthEvents :: Lens.Lens' ListHealthEventsResponse [HealthEvent]
listHealthEventsResponse_healthEvents = Lens.lens (\ListHealthEventsResponse' {healthEvents} -> healthEvents) (\s@ListHealthEventsResponse' {} a -> s {healthEvents = a} :: ListHealthEventsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListHealthEventsResponse where
  rnf ListHealthEventsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf healthEvents
