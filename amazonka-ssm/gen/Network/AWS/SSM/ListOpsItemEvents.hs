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
-- Module      : Network.AWS.SSM.ListOpsItemEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all OpsItem events in the current AWS account and
-- Region. You can limit the results to events associated with specific
-- OpsItems by specifying a filter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListOpsItemEvents
  ( -- * Creating a Request
    ListOpsItemEvents (..),
    newListOpsItemEvents,

    -- * Request Lenses
    listOpsItemEvents_nextToken,
    listOpsItemEvents_maxResults,
    listOpsItemEvents_filters,

    -- * Destructuring the Response
    ListOpsItemEventsResponse (..),
    newListOpsItemEventsResponse,

    -- * Response Lenses
    listOpsItemEventsResponse_nextToken,
    listOpsItemEventsResponse_summaries,
    listOpsItemEventsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListOpsItemEvents' smart constructor.
data ListOpsItemEvents = ListOpsItemEvents'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more OpsItem filters. Use a filter to return a more specific list
    -- of results.
    filters :: Core.Maybe [OpsItemEventFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOpsItemEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOpsItemEvents_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'listOpsItemEvents_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'listOpsItemEvents_filters' - One or more OpsItem filters. Use a filter to return a more specific list
-- of results.
newListOpsItemEvents ::
  ListOpsItemEvents
newListOpsItemEvents =
  ListOpsItemEvents'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listOpsItemEvents_nextToken :: Lens.Lens' ListOpsItemEvents (Core.Maybe Core.Text)
listOpsItemEvents_nextToken = Lens.lens (\ListOpsItemEvents' {nextToken} -> nextToken) (\s@ListOpsItemEvents' {} a -> s {nextToken = a} :: ListOpsItemEvents)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listOpsItemEvents_maxResults :: Lens.Lens' ListOpsItemEvents (Core.Maybe Core.Natural)
listOpsItemEvents_maxResults = Lens.lens (\ListOpsItemEvents' {maxResults} -> maxResults) (\s@ListOpsItemEvents' {} a -> s {maxResults = a} :: ListOpsItemEvents)

-- | One or more OpsItem filters. Use a filter to return a more specific list
-- of results.
listOpsItemEvents_filters :: Lens.Lens' ListOpsItemEvents (Core.Maybe [OpsItemEventFilter])
listOpsItemEvents_filters = Lens.lens (\ListOpsItemEvents' {filters} -> filters) (\s@ListOpsItemEvents' {} a -> s {filters = a} :: ListOpsItemEvents) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListOpsItemEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOpsItemEventsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOpsItemEventsResponse_summaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOpsItemEvents_nextToken
          Lens..~ rs
          Lens.^? listOpsItemEventsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListOpsItemEvents where
  type
    AWSResponse ListOpsItemEvents =
      ListOpsItemEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOpsItemEventsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Summaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOpsItemEvents

instance Core.NFData ListOpsItemEvents

instance Core.ToHeaders ListOpsItemEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.ListOpsItemEvents" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListOpsItemEvents where
  toJSON ListOpsItemEvents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListOpsItemEvents where
  toPath = Core.const "/"

instance Core.ToQuery ListOpsItemEvents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListOpsItemEventsResponse' smart constructor.
data ListOpsItemEventsResponse = ListOpsItemEventsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of event information for the specified OpsItems.
    summaries :: Core.Maybe [OpsItemEventSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOpsItemEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOpsItemEventsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'summaries', 'listOpsItemEventsResponse_summaries' - A list of event information for the specified OpsItems.
--
-- 'httpStatus', 'listOpsItemEventsResponse_httpStatus' - The response's http status code.
newListOpsItemEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOpsItemEventsResponse
newListOpsItemEventsResponse pHttpStatus_ =
  ListOpsItemEventsResponse'
    { nextToken =
        Core.Nothing,
      summaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listOpsItemEventsResponse_nextToken :: Lens.Lens' ListOpsItemEventsResponse (Core.Maybe Core.Text)
listOpsItemEventsResponse_nextToken = Lens.lens (\ListOpsItemEventsResponse' {nextToken} -> nextToken) (\s@ListOpsItemEventsResponse' {} a -> s {nextToken = a} :: ListOpsItemEventsResponse)

-- | A list of event information for the specified OpsItems.
listOpsItemEventsResponse_summaries :: Lens.Lens' ListOpsItemEventsResponse (Core.Maybe [OpsItemEventSummary])
listOpsItemEventsResponse_summaries = Lens.lens (\ListOpsItemEventsResponse' {summaries} -> summaries) (\s@ListOpsItemEventsResponse' {} a -> s {summaries = a} :: ListOpsItemEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOpsItemEventsResponse_httpStatus :: Lens.Lens' ListOpsItemEventsResponse Core.Int
listOpsItemEventsResponse_httpStatus = Lens.lens (\ListOpsItemEventsResponse' {httpStatus} -> httpStatus) (\s@ListOpsItemEventsResponse' {} a -> s {httpStatus = a} :: ListOpsItemEventsResponse)

instance Core.NFData ListOpsItemEventsResponse
