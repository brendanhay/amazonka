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
-- Module      : Network.AWS.XRay.GetInsightEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- X-Ray reevaluates insights periodically until they\'re resolved, and
-- records each intermediate state as an event. You can review an
-- insight\'s events in the Impact Timeline on the Inspect page in the
-- X-Ray console.
module Network.AWS.XRay.GetInsightEvents
  ( -- * Creating a Request
    GetInsightEvents (..),
    newGetInsightEvents,

    -- * Request Lenses
    getInsightEvents_nextToken,
    getInsightEvents_maxResults,
    getInsightEvents_insightId,

    -- * Destructuring the Response
    GetInsightEventsResponse (..),
    newGetInsightEventsResponse,

    -- * Response Lenses
    getInsightEventsResponse_nextToken,
    getInsightEventsResponse_insightEvents,
    getInsightEventsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetInsightEvents' smart constructor.
data GetInsightEvents = GetInsightEvents'
  { -- | Specify the pagination token returned by a previous request to retrieve
    -- the next page of events.
    nextToken :: Core.Maybe Core.Text,
    -- | Used to retrieve at most the specified value of events.
    maxResults :: Core.Maybe Core.Natural,
    -- | The insight\'s unique identifier. Use the GetInsightSummaries action to
    -- retrieve an InsightId.
    insightId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInsightEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInsightEvents_nextToken' - Specify the pagination token returned by a previous request to retrieve
-- the next page of events.
--
-- 'maxResults', 'getInsightEvents_maxResults' - Used to retrieve at most the specified value of events.
--
-- 'insightId', 'getInsightEvents_insightId' - The insight\'s unique identifier. Use the GetInsightSummaries action to
-- retrieve an InsightId.
newGetInsightEvents ::
  -- | 'insightId'
  Core.Text ->
  GetInsightEvents
newGetInsightEvents pInsightId_ =
  GetInsightEvents'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      insightId = pInsightId_
    }

-- | Specify the pagination token returned by a previous request to retrieve
-- the next page of events.
getInsightEvents_nextToken :: Lens.Lens' GetInsightEvents (Core.Maybe Core.Text)
getInsightEvents_nextToken = Lens.lens (\GetInsightEvents' {nextToken} -> nextToken) (\s@GetInsightEvents' {} a -> s {nextToken = a} :: GetInsightEvents)

-- | Used to retrieve at most the specified value of events.
getInsightEvents_maxResults :: Lens.Lens' GetInsightEvents (Core.Maybe Core.Natural)
getInsightEvents_maxResults = Lens.lens (\GetInsightEvents' {maxResults} -> maxResults) (\s@GetInsightEvents' {} a -> s {maxResults = a} :: GetInsightEvents)

-- | The insight\'s unique identifier. Use the GetInsightSummaries action to
-- retrieve an InsightId.
getInsightEvents_insightId :: Lens.Lens' GetInsightEvents Core.Text
getInsightEvents_insightId = Lens.lens (\GetInsightEvents' {insightId} -> insightId) (\s@GetInsightEvents' {} a -> s {insightId = a} :: GetInsightEvents)

instance Core.AWSRequest GetInsightEvents where
  type
    AWSResponse GetInsightEvents =
      GetInsightEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightEventsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "InsightEvents" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInsightEvents

instance Core.NFData GetInsightEvents

instance Core.ToHeaders GetInsightEvents where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetInsightEvents where
  toJSON GetInsightEvents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("InsightId" Core..= insightId)
          ]
      )

instance Core.ToPath GetInsightEvents where
  toPath = Core.const "/InsightEvents"

instance Core.ToQuery GetInsightEvents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInsightEventsResponse' smart constructor.
data GetInsightEventsResponse = GetInsightEventsResponse'
  { -- | Use this token to retrieve the next page of insight events.
    nextToken :: Core.Maybe Core.Text,
    -- | A detailed description of the event. This includes the time of the
    -- event, client and root cause impact statistics, and the top anomalous
    -- service at the time of the event.
    insightEvents :: Core.Maybe [InsightEvent],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInsightEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInsightEventsResponse_nextToken' - Use this token to retrieve the next page of insight events.
--
-- 'insightEvents', 'getInsightEventsResponse_insightEvents' - A detailed description of the event. This includes the time of the
-- event, client and root cause impact statistics, and the top anomalous
-- service at the time of the event.
--
-- 'httpStatus', 'getInsightEventsResponse_httpStatus' - The response's http status code.
newGetInsightEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInsightEventsResponse
newGetInsightEventsResponse pHttpStatus_ =
  GetInsightEventsResponse'
    { nextToken = Core.Nothing,
      insightEvents = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Use this token to retrieve the next page of insight events.
getInsightEventsResponse_nextToken :: Lens.Lens' GetInsightEventsResponse (Core.Maybe Core.Text)
getInsightEventsResponse_nextToken = Lens.lens (\GetInsightEventsResponse' {nextToken} -> nextToken) (\s@GetInsightEventsResponse' {} a -> s {nextToken = a} :: GetInsightEventsResponse)

-- | A detailed description of the event. This includes the time of the
-- event, client and root cause impact statistics, and the top anomalous
-- service at the time of the event.
getInsightEventsResponse_insightEvents :: Lens.Lens' GetInsightEventsResponse (Core.Maybe [InsightEvent])
getInsightEventsResponse_insightEvents = Lens.lens (\GetInsightEventsResponse' {insightEvents} -> insightEvents) (\s@GetInsightEventsResponse' {} a -> s {insightEvents = a} :: GetInsightEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getInsightEventsResponse_httpStatus :: Lens.Lens' GetInsightEventsResponse Core.Int
getInsightEventsResponse_httpStatus = Lens.lens (\GetInsightEventsResponse' {httpStatus} -> httpStatus) (\s@GetInsightEventsResponse' {} a -> s {httpStatus = a} :: GetInsightEventsResponse)

instance Core.NFData GetInsightEventsResponse
