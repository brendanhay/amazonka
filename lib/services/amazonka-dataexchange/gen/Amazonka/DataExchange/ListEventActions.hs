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
-- Module      : Amazonka.DataExchange.ListEventActions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists your event actions.
--
-- This operation returns paginated results.
module Amazonka.DataExchange.ListEventActions
  ( -- * Creating a Request
    ListEventActions (..),
    newListEventActions,

    -- * Request Lenses
    listEventActions_nextToken,
    listEventActions_maxResults,
    listEventActions_eventSourceId,

    -- * Destructuring the Response
    ListEventActionsResponse (..),
    newListEventActionsResponse,

    -- * Response Lenses
    listEventActionsResponse_nextToken,
    listEventActionsResponse_eventActions,
    listEventActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventActions' smart constructor.
data ListEventActions = ListEventActions'
  { -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier for the event source.
    eventSourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventActions_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'maxResults', 'listEventActions_maxResults' - The maximum number of results returned by a single call.
--
-- 'eventSourceId', 'listEventActions_eventSourceId' - The unique identifier for the event source.
newListEventActions ::
  ListEventActions
newListEventActions =
  ListEventActions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      eventSourceId = Prelude.Nothing
    }

-- | The token value retrieved from a previous call to access the next page
-- of results.
listEventActions_nextToken :: Lens.Lens' ListEventActions (Prelude.Maybe Prelude.Text)
listEventActions_nextToken = Lens.lens (\ListEventActions' {nextToken} -> nextToken) (\s@ListEventActions' {} a -> s {nextToken = a} :: ListEventActions)

-- | The maximum number of results returned by a single call.
listEventActions_maxResults :: Lens.Lens' ListEventActions (Prelude.Maybe Prelude.Natural)
listEventActions_maxResults = Lens.lens (\ListEventActions' {maxResults} -> maxResults) (\s@ListEventActions' {} a -> s {maxResults = a} :: ListEventActions)

-- | The unique identifier for the event source.
listEventActions_eventSourceId :: Lens.Lens' ListEventActions (Prelude.Maybe Prelude.Text)
listEventActions_eventSourceId = Lens.lens (\ListEventActions' {eventSourceId} -> eventSourceId) (\s@ListEventActions' {} a -> s {eventSourceId = a} :: ListEventActions)

instance Core.AWSPager ListEventActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEventActionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEventActionsResponse_eventActions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEventActions_nextToken
          Lens..~ rs
          Lens.^? listEventActionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEventActions where
  type
    AWSResponse ListEventActions =
      ListEventActionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventActionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "EventActions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventActions where
  hashWithSalt _salt ListEventActions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` eventSourceId

instance Prelude.NFData ListEventActions where
  rnf ListEventActions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf eventSourceId

instance Data.ToHeaders ListEventActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEventActions where
  toPath = Prelude.const "/v1/event-actions"

instance Data.ToQuery ListEventActions where
  toQuery ListEventActions' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults,
        "eventSourceId" Data.=: eventSourceId
      ]

-- | /See:/ 'newListEventActionsResponse' smart constructor.
data ListEventActionsResponse = ListEventActionsResponse'
  { -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The event action objects listed by the request.
    eventActions :: Prelude.Maybe [EventActionEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventActionsResponse_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'eventActions', 'listEventActionsResponse_eventActions' - The event action objects listed by the request.
--
-- 'httpStatus', 'listEventActionsResponse_httpStatus' - The response's http status code.
newListEventActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventActionsResponse
newListEventActionsResponse pHttpStatus_ =
  ListEventActionsResponse'
    { nextToken =
        Prelude.Nothing,
      eventActions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token value retrieved from a previous call to access the next page
-- of results.
listEventActionsResponse_nextToken :: Lens.Lens' ListEventActionsResponse (Prelude.Maybe Prelude.Text)
listEventActionsResponse_nextToken = Lens.lens (\ListEventActionsResponse' {nextToken} -> nextToken) (\s@ListEventActionsResponse' {} a -> s {nextToken = a} :: ListEventActionsResponse)

-- | The event action objects listed by the request.
listEventActionsResponse_eventActions :: Lens.Lens' ListEventActionsResponse (Prelude.Maybe [EventActionEntry])
listEventActionsResponse_eventActions = Lens.lens (\ListEventActionsResponse' {eventActions} -> eventActions) (\s@ListEventActionsResponse' {} a -> s {eventActions = a} :: ListEventActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEventActionsResponse_httpStatus :: Lens.Lens' ListEventActionsResponse Prelude.Int
listEventActionsResponse_httpStatus = Lens.lens (\ListEventActionsResponse' {httpStatus} -> httpStatus) (\s@ListEventActionsResponse' {} a -> s {httpStatus = a} :: ListEventActionsResponse)

instance Prelude.NFData ListEventActionsResponse where
  rnf ListEventActionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventActions
      `Prelude.seq` Prelude.rnf httpStatus
