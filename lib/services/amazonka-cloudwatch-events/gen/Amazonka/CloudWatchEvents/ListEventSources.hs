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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    listEventSources_nextToken,
    listEventSources_limit,
    listEventSources_namePrefix,

    -- * Destructuring the Response
    ListEventSourcesResponse (..),
    newListEventSourcesResponse,

    -- * Response Lenses
    listEventSourcesResponse_nextToken,
    listEventSourcesResponse_eventSources,
    listEventSourcesResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventSources' smart constructor.
data ListEventSources = ListEventSources'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifying this limits the number of results returned by this operation.
    -- The operation also returns a NextToken which you can use in a subsequent
    -- operation to retrieve the next set of results.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Specifying this limits the results to only those partner event sources
    -- with names that start with the specified prefix.
    namePrefix :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'listEventSources_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'limit', 'listEventSources_limit' - Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
--
-- 'namePrefix', 'listEventSources_namePrefix' - Specifying this limits the results to only those partner event sources
-- with names that start with the specified prefix.
newListEventSources ::
  ListEventSources
newListEventSources =
  ListEventSources'
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      namePrefix = Prelude.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listEventSources_nextToken :: Lens.Lens' ListEventSources (Prelude.Maybe Prelude.Text)
listEventSources_nextToken = Lens.lens (\ListEventSources' {nextToken} -> nextToken) (\s@ListEventSources' {} a -> s {nextToken = a} :: ListEventSources)

-- | Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
listEventSources_limit :: Lens.Lens' ListEventSources (Prelude.Maybe Prelude.Natural)
listEventSources_limit = Lens.lens (\ListEventSources' {limit} -> limit) (\s@ListEventSources' {} a -> s {limit = a} :: ListEventSources)

-- | Specifying this limits the results to only those partner event sources
-- with names that start with the specified prefix.
listEventSources_namePrefix :: Lens.Lens' ListEventSources (Prelude.Maybe Prelude.Text)
listEventSources_namePrefix = Lens.lens (\ListEventSources' {namePrefix} -> namePrefix) (\s@ListEventSources' {} a -> s {namePrefix = a} :: ListEventSources)

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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "EventSources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventSources where
  hashWithSalt _salt ListEventSources' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` namePrefix

instance Prelude.NFData ListEventSources where
  rnf ListEventSources' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf namePrefix

instance Core.ToHeaders ListEventSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.ListEventSources" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEventSources where
  toJSON ListEventSources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            ("NamePrefix" Core..=) Prelude.<$> namePrefix
          ]
      )

instance Core.ToPath ListEventSources where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEventSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEventSourcesResponse' smart constructor.
data ListEventSourcesResponse = ListEventSourcesResponse'
  { -- | A token you can use in a subsequent operation to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of event sources.
    eventSources :: Prelude.Maybe [EventSource],
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
-- 'nextToken', 'listEventSourcesResponse_nextToken' - A token you can use in a subsequent operation to retrieve the next set
-- of results.
--
-- 'eventSources', 'listEventSourcesResponse_eventSources' - The list of event sources.
--
-- 'httpStatus', 'listEventSourcesResponse_httpStatus' - The response's http status code.
newListEventSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventSourcesResponse
newListEventSourcesResponse pHttpStatus_ =
  ListEventSourcesResponse'
    { nextToken =
        Prelude.Nothing,
      eventSources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token you can use in a subsequent operation to retrieve the next set
-- of results.
listEventSourcesResponse_nextToken :: Lens.Lens' ListEventSourcesResponse (Prelude.Maybe Prelude.Text)
listEventSourcesResponse_nextToken = Lens.lens (\ListEventSourcesResponse' {nextToken} -> nextToken) (\s@ListEventSourcesResponse' {} a -> s {nextToken = a} :: ListEventSourcesResponse)

-- | The list of event sources.
listEventSourcesResponse_eventSources :: Lens.Lens' ListEventSourcesResponse (Prelude.Maybe [EventSource])
listEventSourcesResponse_eventSources = Lens.lens (\ListEventSourcesResponse' {eventSources} -> eventSources) (\s@ListEventSourcesResponse' {} a -> s {eventSources = a} :: ListEventSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEventSourcesResponse_httpStatus :: Lens.Lens' ListEventSourcesResponse Prelude.Int
listEventSourcesResponse_httpStatus = Lens.lens (\ListEventSourcesResponse' {httpStatus} -> httpStatus) (\s@ListEventSourcesResponse' {} a -> s {httpStatus = a} :: ListEventSourcesResponse)

instance Prelude.NFData ListEventSourcesResponse where
  rnf ListEventSourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventSources
      `Prelude.seq` Prelude.rnf httpStatus
