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
-- Module      : Amazonka.SSMIncidents.ListTimelineEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists timeline events of the specified incident record.
--
-- This operation returns paginated results.
module Amazonka.SSMIncidents.ListTimelineEvents
  ( -- * Creating a Request
    ListTimelineEvents (..),
    newListTimelineEvents,

    -- * Request Lenses
    listTimelineEvents_filters,
    listTimelineEvents_sortOrder,
    listTimelineEvents_nextToken,
    listTimelineEvents_maxResults,
    listTimelineEvents_sortBy,
    listTimelineEvents_incidentRecordArn,

    -- * Destructuring the Response
    ListTimelineEventsResponse (..),
    newListTimelineEventsResponse,

    -- * Response Lenses
    listTimelineEventsResponse_nextToken,
    listTimelineEventsResponse_httpStatus,
    listTimelineEventsResponse_eventSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newListTimelineEvents' smart constructor.
data ListTimelineEvents = ListTimelineEvents'
  { -- | Filters the timeline events based on the provided conditional values.
    -- You can filter timeline events using the following keys:
    --
    -- -   @eventTime@
    --
    -- -   @eventType@
    filters :: Prelude.Maybe [Filter],
    -- | Sorts the order of timeline events by the value specified in the
    -- @sortBy@ field.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Sort by the specified key value pair.
    sortBy :: Prelude.Maybe TimelineEventSort,
    -- | The Amazon Resource Name (ARN) of the incident that the event is part
    -- of.
    incidentRecordArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTimelineEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listTimelineEvents_filters' - Filters the timeline events based on the provided conditional values.
-- You can filter timeline events using the following keys:
--
-- -   @eventTime@
--
-- -   @eventType@
--
-- 'sortOrder', 'listTimelineEvents_sortOrder' - Sorts the order of timeline events by the value specified in the
-- @sortBy@ field.
--
-- 'nextToken', 'listTimelineEvents_nextToken' - The pagination token to continue to the next page of results.
--
-- 'maxResults', 'listTimelineEvents_maxResults' - The maximum number of results per page.
--
-- 'sortBy', 'listTimelineEvents_sortBy' - Sort by the specified key value pair.
--
-- 'incidentRecordArn', 'listTimelineEvents_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident that the event is part
-- of.
newListTimelineEvents ::
  -- | 'incidentRecordArn'
  Prelude.Text ->
  ListTimelineEvents
newListTimelineEvents pIncidentRecordArn_ =
  ListTimelineEvents'
    { filters = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      incidentRecordArn = pIncidentRecordArn_
    }

-- | Filters the timeline events based on the provided conditional values.
-- You can filter timeline events using the following keys:
--
-- -   @eventTime@
--
-- -   @eventType@
listTimelineEvents_filters :: Lens.Lens' ListTimelineEvents (Prelude.Maybe [Filter])
listTimelineEvents_filters = Lens.lens (\ListTimelineEvents' {filters} -> filters) (\s@ListTimelineEvents' {} a -> s {filters = a} :: ListTimelineEvents) Prelude.. Lens.mapping Lens.coerced

-- | Sorts the order of timeline events by the value specified in the
-- @sortBy@ field.
listTimelineEvents_sortOrder :: Lens.Lens' ListTimelineEvents (Prelude.Maybe SortOrder)
listTimelineEvents_sortOrder = Lens.lens (\ListTimelineEvents' {sortOrder} -> sortOrder) (\s@ListTimelineEvents' {} a -> s {sortOrder = a} :: ListTimelineEvents)

-- | The pagination token to continue to the next page of results.
listTimelineEvents_nextToken :: Lens.Lens' ListTimelineEvents (Prelude.Maybe Prelude.Text)
listTimelineEvents_nextToken = Lens.lens (\ListTimelineEvents' {nextToken} -> nextToken) (\s@ListTimelineEvents' {} a -> s {nextToken = a} :: ListTimelineEvents)

-- | The maximum number of results per page.
listTimelineEvents_maxResults :: Lens.Lens' ListTimelineEvents (Prelude.Maybe Prelude.Natural)
listTimelineEvents_maxResults = Lens.lens (\ListTimelineEvents' {maxResults} -> maxResults) (\s@ListTimelineEvents' {} a -> s {maxResults = a} :: ListTimelineEvents)

-- | Sort by the specified key value pair.
listTimelineEvents_sortBy :: Lens.Lens' ListTimelineEvents (Prelude.Maybe TimelineEventSort)
listTimelineEvents_sortBy = Lens.lens (\ListTimelineEvents' {sortBy} -> sortBy) (\s@ListTimelineEvents' {} a -> s {sortBy = a} :: ListTimelineEvents)

-- | The Amazon Resource Name (ARN) of the incident that the event is part
-- of.
listTimelineEvents_incidentRecordArn :: Lens.Lens' ListTimelineEvents Prelude.Text
listTimelineEvents_incidentRecordArn = Lens.lens (\ListTimelineEvents' {incidentRecordArn} -> incidentRecordArn) (\s@ListTimelineEvents' {} a -> s {incidentRecordArn = a} :: ListTimelineEvents)

instance Core.AWSPager ListTimelineEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTimelineEventsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listTimelineEventsResponse_eventSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTimelineEvents_nextToken
          Lens..~ rs
          Lens.^? listTimelineEventsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTimelineEvents where
  type
    AWSResponse ListTimelineEvents =
      ListTimelineEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTimelineEventsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "eventSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListTimelineEvents where
  hashWithSalt salt' ListTimelineEvents' {..} =
    salt' `Prelude.hashWithSalt` incidentRecordArn
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` filters

instance Prelude.NFData ListTimelineEvents where
  rnf ListTimelineEvents' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf incidentRecordArn
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortOrder

instance Core.ToHeaders ListTimelineEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTimelineEvents where
  toJSON ListTimelineEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("sortOrder" Core..=) Prelude.<$> sortOrder,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            Prelude.Just
              ("incidentRecordArn" Core..= incidentRecordArn)
          ]
      )

instance Core.ToPath ListTimelineEvents where
  toPath = Prelude.const "/listTimelineEvents"

instance Core.ToQuery ListTimelineEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTimelineEventsResponse' smart constructor.
data ListTimelineEventsResponse = ListTimelineEventsResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details about each event that occurred during the incident.
    eventSummaries :: [EventSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTimelineEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTimelineEventsResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'listTimelineEventsResponse_httpStatus' - The response's http status code.
--
-- 'eventSummaries', 'listTimelineEventsResponse_eventSummaries' - Details about each event that occurred during the incident.
newListTimelineEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTimelineEventsResponse
newListTimelineEventsResponse pHttpStatus_ =
  ListTimelineEventsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      eventSummaries = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
listTimelineEventsResponse_nextToken :: Lens.Lens' ListTimelineEventsResponse (Prelude.Maybe Prelude.Text)
listTimelineEventsResponse_nextToken = Lens.lens (\ListTimelineEventsResponse' {nextToken} -> nextToken) (\s@ListTimelineEventsResponse' {} a -> s {nextToken = a} :: ListTimelineEventsResponse)

-- | The response's http status code.
listTimelineEventsResponse_httpStatus :: Lens.Lens' ListTimelineEventsResponse Prelude.Int
listTimelineEventsResponse_httpStatus = Lens.lens (\ListTimelineEventsResponse' {httpStatus} -> httpStatus) (\s@ListTimelineEventsResponse' {} a -> s {httpStatus = a} :: ListTimelineEventsResponse)

-- | Details about each event that occurred during the incident.
listTimelineEventsResponse_eventSummaries :: Lens.Lens' ListTimelineEventsResponse [EventSummary]
listTimelineEventsResponse_eventSummaries = Lens.lens (\ListTimelineEventsResponse' {eventSummaries} -> eventSummaries) (\s@ListTimelineEventsResponse' {} a -> s {eventSummaries = a} :: ListTimelineEventsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTimelineEventsResponse where
  rnf ListTimelineEventsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventSummaries
      `Prelude.seq` Prelude.rnf httpStatus
