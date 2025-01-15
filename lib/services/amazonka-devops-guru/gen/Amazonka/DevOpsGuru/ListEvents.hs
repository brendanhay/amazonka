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
-- Module      : Amazonka.DevOpsGuru.ListEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the events emitted by the resources that are evaluated
-- by DevOps Guru. You can use filters to specify which events are
-- returned.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.ListEvents
  ( -- * Creating a Request
    ListEvents (..),
    newListEvents,

    -- * Request Lenses
    listEvents_accountId,
    listEvents_maxResults,
    listEvents_nextToken,
    listEvents_filters,

    -- * Destructuring the Response
    ListEventsResponse (..),
    newListEventsResponse,

    -- * Response Lenses
    listEventsResponse_nextToken,
    listEventsResponse_httpStatus,
    listEventsResponse_events,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEvents' smart constructor.
data ListEvents = ListEvents'
  { -- | The ID of the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A @ListEventsFilters@ object used to specify which events to return.
    filters :: ListEventsFilters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'listEvents_accountId' - The ID of the Amazon Web Services account.
--
-- 'maxResults', 'listEvents_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listEvents_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'filters', 'listEvents_filters' - A @ListEventsFilters@ object used to specify which events to return.
newListEvents ::
  -- | 'filters'
  ListEventsFilters ->
  ListEvents
newListEvents pFilters_ =
  ListEvents'
    { accountId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = pFilters_
    }

-- | The ID of the Amazon Web Services account.
listEvents_accountId :: Lens.Lens' ListEvents (Prelude.Maybe Prelude.Text)
listEvents_accountId = Lens.lens (\ListEvents' {accountId} -> accountId) (\s@ListEvents' {} a -> s {accountId = a} :: ListEvents)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listEvents_maxResults :: Lens.Lens' ListEvents (Prelude.Maybe Prelude.Natural)
listEvents_maxResults = Lens.lens (\ListEvents' {maxResults} -> maxResults) (\s@ListEvents' {} a -> s {maxResults = a} :: ListEvents)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listEvents_nextToken :: Lens.Lens' ListEvents (Prelude.Maybe Prelude.Text)
listEvents_nextToken = Lens.lens (\ListEvents' {nextToken} -> nextToken) (\s@ListEvents' {} a -> s {nextToken = a} :: ListEvents)

-- | A @ListEventsFilters@ object used to specify which events to return.
listEvents_filters :: Lens.Lens' ListEvents ListEventsFilters
listEvents_filters = Lens.lens (\ListEvents' {filters} -> filters) (\s@ListEvents' {} a -> s {filters = a} :: ListEvents)

instance Core.AWSPager ListEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEventsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listEventsResponse_events) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listEvents_nextToken
              Lens..~ rs
              Lens.^? listEventsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListEvents where
  type AWSResponse ListEvents = ListEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Events" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListEvents where
  hashWithSalt _salt ListEvents' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData ListEvents where
  rnf ListEvents' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf filters

instance Data.ToHeaders ListEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEvents where
  toJSON ListEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Filters" Data..= filters)
          ]
      )

instance Data.ToPath ListEvents where
  toPath = Prelude.const "/events"

instance Data.ToQuery ListEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEventsResponse' smart constructor.
data ListEventsResponse = ListEventsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of the requested events.
    events :: [Event]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'listEventsResponse_httpStatus' - The response's http status code.
--
-- 'events', 'listEventsResponse_events' - A list of the requested events.
newListEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventsResponse
newListEventsResponse pHttpStatus_ =
  ListEventsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      events = Prelude.mempty
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listEventsResponse_nextToken :: Lens.Lens' ListEventsResponse (Prelude.Maybe Prelude.Text)
listEventsResponse_nextToken = Lens.lens (\ListEventsResponse' {nextToken} -> nextToken) (\s@ListEventsResponse' {} a -> s {nextToken = a} :: ListEventsResponse)

-- | The response's http status code.
listEventsResponse_httpStatus :: Lens.Lens' ListEventsResponse Prelude.Int
listEventsResponse_httpStatus = Lens.lens (\ListEventsResponse' {httpStatus} -> httpStatus) (\s@ListEventsResponse' {} a -> s {httpStatus = a} :: ListEventsResponse)

-- | A list of the requested events.
listEventsResponse_events :: Lens.Lens' ListEventsResponse [Event]
listEventsResponse_events = Lens.lens (\ListEventsResponse' {events} -> events) (\s@ListEventsResponse' {} a -> s {events = a} :: ListEventsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEventsResponse where
  rnf ListEventsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf events
