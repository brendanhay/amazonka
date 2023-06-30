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
-- Module      : Amazonka.CloudWatchEvents.ListEventSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this to see all the partner event sources that have been
-- shared with your Amazon Web Services account. For more information about
-- partner event sources, see
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_CreateEventBus.html CreateEventBus>.
module Amazonka.CloudWatchEvents.ListEventSources
  ( -- * Creating a Request
    ListEventSources (..),
    newListEventSources,

    -- * Request Lenses
    listEventSources_limit,
    listEventSources_namePrefix,
    listEventSources_nextToken,

    -- * Destructuring the Response
    ListEventSourcesResponse (..),
    newListEventSourcesResponse,

    -- * Response Lenses
    listEventSourcesResponse_eventSources,
    listEventSourcesResponse_nextToken,
    listEventSourcesResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventSources' smart constructor.
data ListEventSources = ListEventSources'
  { -- | Specifying this limits the number of results returned by this operation.
    -- The operation also returns a NextToken which you can use in a subsequent
    -- operation to retrieve the next set of results.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Specifying this limits the results to only those partner event sources
    -- with names that start with the specified prefix.
    namePrefix :: Prelude.Maybe Prelude.Text,
    -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listEventSources_limit' - Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
--
-- 'namePrefix', 'listEventSources_namePrefix' - Specifying this limits the results to only those partner event sources
-- with names that start with the specified prefix.
--
-- 'nextToken', 'listEventSources_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
newListEventSources ::
  ListEventSources
newListEventSources =
  ListEventSources'
    { limit = Prelude.Nothing,
      namePrefix = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
listEventSources_limit :: Lens.Lens' ListEventSources (Prelude.Maybe Prelude.Natural)
listEventSources_limit = Lens.lens (\ListEventSources' {limit} -> limit) (\s@ListEventSources' {} a -> s {limit = a} :: ListEventSources)

-- | Specifying this limits the results to only those partner event sources
-- with names that start with the specified prefix.
listEventSources_namePrefix :: Lens.Lens' ListEventSources (Prelude.Maybe Prelude.Text)
listEventSources_namePrefix = Lens.lens (\ListEventSources' {namePrefix} -> namePrefix) (\s@ListEventSources' {} a -> s {namePrefix = a} :: ListEventSources)

-- | The token returned by a previous call to retrieve the next set of
-- results.
listEventSources_nextToken :: Lens.Lens' ListEventSources (Prelude.Maybe Prelude.Text)
listEventSources_nextToken = Lens.lens (\ListEventSources' {nextToken} -> nextToken) (\s@ListEventSources' {} a -> s {nextToken = a} :: ListEventSources)

instance Core.AWSRequest ListEventSources where
  type
    AWSResponse ListEventSources =
      ListEventSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventSourcesResponse'
            Prelude.<$> (x Data..?> "EventSources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventSources where
  hashWithSalt _salt ListEventSources' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` namePrefix
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEventSources where
  rnf ListEventSources' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf namePrefix
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEventSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.ListEventSources" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEventSources where
  toJSON ListEventSources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NamePrefix" Data..=) Prelude.<$> namePrefix,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEventSources where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEventSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEventSourcesResponse' smart constructor.
data ListEventSourcesResponse = ListEventSourcesResponse'
  { -- | The list of event sources.
    eventSources :: Prelude.Maybe [EventSource],
    -- | A token you can use in a subsequent operation to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSources', 'listEventSourcesResponse_eventSources' - The list of event sources.
--
-- 'nextToken', 'listEventSourcesResponse_nextToken' - A token you can use in a subsequent operation to retrieve the next set
-- of results.
--
-- 'httpStatus', 'listEventSourcesResponse_httpStatus' - The response's http status code.
newListEventSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventSourcesResponse
newListEventSourcesResponse pHttpStatus_ =
  ListEventSourcesResponse'
    { eventSources =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of event sources.
listEventSourcesResponse_eventSources :: Lens.Lens' ListEventSourcesResponse (Prelude.Maybe [EventSource])
listEventSourcesResponse_eventSources = Lens.lens (\ListEventSourcesResponse' {eventSources} -> eventSources) (\s@ListEventSourcesResponse' {} a -> s {eventSources = a} :: ListEventSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token you can use in a subsequent operation to retrieve the next set
-- of results.
listEventSourcesResponse_nextToken :: Lens.Lens' ListEventSourcesResponse (Prelude.Maybe Prelude.Text)
listEventSourcesResponse_nextToken = Lens.lens (\ListEventSourcesResponse' {nextToken} -> nextToken) (\s@ListEventSourcesResponse' {} a -> s {nextToken = a} :: ListEventSourcesResponse)

-- | The response's http status code.
listEventSourcesResponse_httpStatus :: Lens.Lens' ListEventSourcesResponse Prelude.Int
listEventSourcesResponse_httpStatus = Lens.lens (\ListEventSourcesResponse' {httpStatus} -> httpStatus) (\s@ListEventSourcesResponse' {} a -> s {httpStatus = a} :: ListEventSourcesResponse)

instance Prelude.NFData ListEventSourcesResponse where
  rnf ListEventSourcesResponse' {..} =
    Prelude.rnf eventSources
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
