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
-- Module      : Amazonka.XRay.GetInsightEvents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- X-Ray reevaluates insights periodically until they\'re resolved, and
-- records each intermediate state as an event. You can review an
-- insight\'s events in the Impact Timeline on the Inspect page in the
-- X-Ray console.
module Amazonka.XRay.GetInsightEvents
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newGetInsightEvents' smart constructor.
data GetInsightEvents = GetInsightEvents'
  { -- | Specify the pagination token returned by a previous request to retrieve
    -- the next page of events.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Used to retrieve at most the specified value of events.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The insight\'s unique identifier. Use the GetInsightSummaries action to
    -- retrieve an InsightId.
    insightId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetInsightEvents
newGetInsightEvents pInsightId_ =
  GetInsightEvents'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      insightId = pInsightId_
    }

-- | Specify the pagination token returned by a previous request to retrieve
-- the next page of events.
getInsightEvents_nextToken :: Lens.Lens' GetInsightEvents (Prelude.Maybe Prelude.Text)
getInsightEvents_nextToken = Lens.lens (\GetInsightEvents' {nextToken} -> nextToken) (\s@GetInsightEvents' {} a -> s {nextToken = a} :: GetInsightEvents)

-- | Used to retrieve at most the specified value of events.
getInsightEvents_maxResults :: Lens.Lens' GetInsightEvents (Prelude.Maybe Prelude.Natural)
getInsightEvents_maxResults = Lens.lens (\GetInsightEvents' {maxResults} -> maxResults) (\s@GetInsightEvents' {} a -> s {maxResults = a} :: GetInsightEvents)

-- | The insight\'s unique identifier. Use the GetInsightSummaries action to
-- retrieve an InsightId.
getInsightEvents_insightId :: Lens.Lens' GetInsightEvents Prelude.Text
getInsightEvents_insightId = Lens.lens (\GetInsightEvents' {insightId} -> insightId) (\s@GetInsightEvents' {} a -> s {insightId = a} :: GetInsightEvents)

instance Core.AWSRequest GetInsightEvents where
  type
    AWSResponse GetInsightEvents =
      GetInsightEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightEventsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "InsightEvents" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsightEvents where
  hashWithSalt _salt GetInsightEvents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` insightId

instance Prelude.NFData GetInsightEvents where
  rnf GetInsightEvents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf insightId

instance Core.ToHeaders GetInsightEvents where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetInsightEvents where
  toJSON GetInsightEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("InsightId" Core..= insightId)
          ]
      )

instance Core.ToPath GetInsightEvents where
  toPath = Prelude.const "/InsightEvents"

instance Core.ToQuery GetInsightEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightEventsResponse' smart constructor.
data GetInsightEventsResponse = GetInsightEventsResponse'
  { -- | Use this token to retrieve the next page of insight events.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A detailed description of the event. This includes the time of the
    -- event, client and root cause impact statistics, and the top anomalous
    -- service at the time of the event.
    insightEvents :: Prelude.Maybe [InsightEvent],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetInsightEventsResponse
newGetInsightEventsResponse pHttpStatus_ =
  GetInsightEventsResponse'
    { nextToken =
        Prelude.Nothing,
      insightEvents = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Use this token to retrieve the next page of insight events.
getInsightEventsResponse_nextToken :: Lens.Lens' GetInsightEventsResponse (Prelude.Maybe Prelude.Text)
getInsightEventsResponse_nextToken = Lens.lens (\GetInsightEventsResponse' {nextToken} -> nextToken) (\s@GetInsightEventsResponse' {} a -> s {nextToken = a} :: GetInsightEventsResponse)

-- | A detailed description of the event. This includes the time of the
-- event, client and root cause impact statistics, and the top anomalous
-- service at the time of the event.
getInsightEventsResponse_insightEvents :: Lens.Lens' GetInsightEventsResponse (Prelude.Maybe [InsightEvent])
getInsightEventsResponse_insightEvents = Lens.lens (\GetInsightEventsResponse' {insightEvents} -> insightEvents) (\s@GetInsightEventsResponse' {} a -> s {insightEvents = a} :: GetInsightEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getInsightEventsResponse_httpStatus :: Lens.Lens' GetInsightEventsResponse Prelude.Int
getInsightEventsResponse_httpStatus = Lens.lens (\GetInsightEventsResponse' {httpStatus} -> httpStatus) (\s@GetInsightEventsResponse' {} a -> s {httpStatus = a} :: GetInsightEventsResponse)

instance Prelude.NFData GetInsightEventsResponse where
  rnf GetInsightEventsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf insightEvents
      `Prelude.seq` Prelude.rnf httpStatus
