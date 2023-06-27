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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists timeline events for the specified incident record.
--
-- This operation returns paginated results.
module Amazonka.SSMIncidents.ListTimelineEvents
  ( -- * Creating a Request
    ListTimelineEvents (..),
    newListTimelineEvents,

    -- * Request Lenses
    listTimelineEvents_filters,
    listTimelineEvents_maxResults,
    listTimelineEvents_nextToken,
    listTimelineEvents_sortBy,
    listTimelineEvents_sortOrder,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newListTimelineEvents' smart constructor.
data ListTimelineEvents = ListTimelineEvents'
  { -- | Filters the timeline events based on the provided conditional values.
    -- You can filter timeline events with the following keys:
    --
    -- -   @eventTime@
    --
    -- -   @eventType@
    --
    -- Note the following when deciding how to use Filters:
    --
    -- -   If you don\'t specify a Filter, the response includes all timeline
    --     events.
    --
    -- -   If you specify more than one filter in a single request, the
    --     response returns timeline events that match all filters.
    --
    -- -   If you specify a filter with more than one value, the response
    --     returns timeline events that match any of the values provided.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort timeline events by the specified key value pair.
    sortBy :: Prelude.Maybe TimelineEventSort,
    -- | Sorts the order of timeline events by the value specified in the
    -- @sortBy@ field.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The Amazon Resource Name (ARN) of the incident that includes the
    -- timeline event.
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
-- You can filter timeline events with the following keys:
--
-- -   @eventTime@
--
-- -   @eventType@
--
-- Note the following when deciding how to use Filters:
--
-- -   If you don\'t specify a Filter, the response includes all timeline
--     events.
--
-- -   If you specify more than one filter in a single request, the
--     response returns timeline events that match all filters.
--
-- -   If you specify a filter with more than one value, the response
--     returns timeline events that match any of the values provided.
--
-- 'maxResults', 'listTimelineEvents_maxResults' - The maximum number of results per page.
--
-- 'nextToken', 'listTimelineEvents_nextToken' - The pagination token to continue to the next page of results.
--
-- 'sortBy', 'listTimelineEvents_sortBy' - Sort timeline events by the specified key value pair.
--
-- 'sortOrder', 'listTimelineEvents_sortOrder' - Sorts the order of timeline events by the value specified in the
-- @sortBy@ field.
--
-- 'incidentRecordArn', 'listTimelineEvents_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident that includes the
-- timeline event.
newListTimelineEvents ::
  -- | 'incidentRecordArn'
  Prelude.Text ->
  ListTimelineEvents
newListTimelineEvents pIncidentRecordArn_ =
  ListTimelineEvents'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      incidentRecordArn = pIncidentRecordArn_
    }

-- | Filters the timeline events based on the provided conditional values.
-- You can filter timeline events with the following keys:
--
-- -   @eventTime@
--
-- -   @eventType@
--
-- Note the following when deciding how to use Filters:
--
-- -   If you don\'t specify a Filter, the response includes all timeline
--     events.
--
-- -   If you specify more than one filter in a single request, the
--     response returns timeline events that match all filters.
--
-- -   If you specify a filter with more than one value, the response
--     returns timeline events that match any of the values provided.
listTimelineEvents_filters :: Lens.Lens' ListTimelineEvents (Prelude.Maybe [Filter])
listTimelineEvents_filters = Lens.lens (\ListTimelineEvents' {filters} -> filters) (\s@ListTimelineEvents' {} a -> s {filters = a} :: ListTimelineEvents) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results per page.
listTimelineEvents_maxResults :: Lens.Lens' ListTimelineEvents (Prelude.Maybe Prelude.Natural)
listTimelineEvents_maxResults = Lens.lens (\ListTimelineEvents' {maxResults} -> maxResults) (\s@ListTimelineEvents' {} a -> s {maxResults = a} :: ListTimelineEvents)

-- | The pagination token to continue to the next page of results.
listTimelineEvents_nextToken :: Lens.Lens' ListTimelineEvents (Prelude.Maybe Prelude.Text)
listTimelineEvents_nextToken = Lens.lens (\ListTimelineEvents' {nextToken} -> nextToken) (\s@ListTimelineEvents' {} a -> s {nextToken = a} :: ListTimelineEvents)

-- | Sort timeline events by the specified key value pair.
listTimelineEvents_sortBy :: Lens.Lens' ListTimelineEvents (Prelude.Maybe TimelineEventSort)
listTimelineEvents_sortBy = Lens.lens (\ListTimelineEvents' {sortBy} -> sortBy) (\s@ListTimelineEvents' {} a -> s {sortBy = a} :: ListTimelineEvents)

-- | Sorts the order of timeline events by the value specified in the
-- @sortBy@ field.
listTimelineEvents_sortOrder :: Lens.Lens' ListTimelineEvents (Prelude.Maybe SortOrder)
listTimelineEvents_sortOrder = Lens.lens (\ListTimelineEvents' {sortOrder} -> sortOrder) (\s@ListTimelineEvents' {} a -> s {sortOrder = a} :: ListTimelineEvents)

-- | The Amazon Resource Name (ARN) of the incident that includes the
-- timeline event.
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
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTimelineEvents_nextToken
          Lens..~ rs
          Lens.^? listTimelineEventsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTimelineEvents where
  type
    AWSResponse ListTimelineEvents =
      ListTimelineEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTimelineEventsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "eventSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListTimelineEvents where
  hashWithSalt _salt ListTimelineEvents' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` incidentRecordArn

instance Prelude.NFData ListTimelineEvents where
  rnf ListTimelineEvents' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf incidentRecordArn

instance Data.ToHeaders ListTimelineEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTimelineEvents where
  toJSON ListTimelineEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder,
            Prelude.Just
              ("incidentRecordArn" Data..= incidentRecordArn)
          ]
      )

instance Data.ToPath ListTimelineEvents where
  toPath = Prelude.const "/listTimelineEvents"

instance Data.ToQuery ListTimelineEvents where
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
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf eventSummaries
