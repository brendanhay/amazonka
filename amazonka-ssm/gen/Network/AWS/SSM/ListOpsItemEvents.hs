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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListOpsItemEvents' smart constructor.
data ListOpsItemEvents = ListOpsItemEvents'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more OpsItem filters. Use a filter to return a more specific list
    -- of results.
    filters :: Prelude.Maybe [OpsItemEventFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listOpsItemEvents_nextToken :: Lens.Lens' ListOpsItemEvents (Prelude.Maybe Prelude.Text)
listOpsItemEvents_nextToken = Lens.lens (\ListOpsItemEvents' {nextToken} -> nextToken) (\s@ListOpsItemEvents' {} a -> s {nextToken = a} :: ListOpsItemEvents)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listOpsItemEvents_maxResults :: Lens.Lens' ListOpsItemEvents (Prelude.Maybe Prelude.Natural)
listOpsItemEvents_maxResults = Lens.lens (\ListOpsItemEvents' {maxResults} -> maxResults) (\s@ListOpsItemEvents' {} a -> s {maxResults = a} :: ListOpsItemEvents)

-- | One or more OpsItem filters. Use a filter to return a more specific list
-- of results.
listOpsItemEvents_filters :: Lens.Lens' ListOpsItemEvents (Prelude.Maybe [OpsItemEventFilter])
listOpsItemEvents_filters = Lens.lens (\ListOpsItemEvents' {filters} -> filters) (\s@ListOpsItemEvents' {} a -> s {filters = a} :: ListOpsItemEvents) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListOpsItemEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOpsItemEventsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOpsItemEventsResponse_summaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOpsItemEvents_nextToken
          Lens..~ rs
          Lens.^? listOpsItemEventsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListOpsItemEvents where
  type
    AWSResponse ListOpsItemEvents =
      ListOpsItemEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOpsItemEventsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOpsItemEvents

instance Prelude.NFData ListOpsItemEvents

instance Core.ToHeaders ListOpsItemEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListOpsItemEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListOpsItemEvents where
  toJSON ListOpsItemEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath ListOpsItemEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery ListOpsItemEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOpsItemEventsResponse' smart constructor.
data ListOpsItemEventsResponse = ListOpsItemEventsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of event information for the specified OpsItems.
    summaries :: Prelude.Maybe [OpsItemEventSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListOpsItemEventsResponse
newListOpsItemEventsResponse pHttpStatus_ =
  ListOpsItemEventsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listOpsItemEventsResponse_nextToken :: Lens.Lens' ListOpsItemEventsResponse (Prelude.Maybe Prelude.Text)
listOpsItemEventsResponse_nextToken = Lens.lens (\ListOpsItemEventsResponse' {nextToken} -> nextToken) (\s@ListOpsItemEventsResponse' {} a -> s {nextToken = a} :: ListOpsItemEventsResponse)

-- | A list of event information for the specified OpsItems.
listOpsItemEventsResponse_summaries :: Lens.Lens' ListOpsItemEventsResponse (Prelude.Maybe [OpsItemEventSummary])
listOpsItemEventsResponse_summaries = Lens.lens (\ListOpsItemEventsResponse' {summaries} -> summaries) (\s@ListOpsItemEventsResponse' {} a -> s {summaries = a} :: ListOpsItemEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOpsItemEventsResponse_httpStatus :: Lens.Lens' ListOpsItemEventsResponse Prelude.Int
listOpsItemEventsResponse_httpStatus = Lens.lens (\ListOpsItemEventsResponse' {httpStatus} -> httpStatus) (\s@ListOpsItemEventsResponse' {} a -> s {httpStatus = a} :: ListOpsItemEventsResponse)

instance Prelude.NFData ListOpsItemEventsResponse
