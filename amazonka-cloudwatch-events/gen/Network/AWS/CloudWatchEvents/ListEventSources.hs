{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchEvents.ListEventSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this to see all the partner event sources that have been
-- shared with your AWS account. For more information about partner event
-- sources, see CreateEventBus.
module Network.AWS.CloudWatchEvents.ListEventSources
  ( -- * Creating a Request
    ListEventSources (..),
    newListEventSources,

    -- * Request Lenses
    listEventSources_nextToken,
    listEventSources_namePrefix,
    listEventSources_limit,

    -- * Destructuring the Response
    ListEventSourcesResponse (..),
    newListEventSourcesResponse,

    -- * Response Lenses
    listEventSourcesResponse_nextToken,
    listEventSourcesResponse_eventSources,
    listEventSourcesResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEventSources' smart constructor.
data ListEventSources = ListEventSources'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifying this limits the results to only those partner event sources
    -- with names that start with the specified prefix.
    namePrefix :: Prelude.Maybe Prelude.Text,
    -- | Specifying this limits the number of results returned by this operation.
    -- The operation also returns a NextToken which you can use in a subsequent
    -- operation to retrieve the next set of results.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'namePrefix', 'listEventSources_namePrefix' - Specifying this limits the results to only those partner event sources
-- with names that start with the specified prefix.
--
-- 'limit', 'listEventSources_limit' - Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
newListEventSources ::
  ListEventSources
newListEventSources =
  ListEventSources'
    { nextToken = Prelude.Nothing,
      namePrefix = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listEventSources_nextToken :: Lens.Lens' ListEventSources (Prelude.Maybe Prelude.Text)
listEventSources_nextToken = Lens.lens (\ListEventSources' {nextToken} -> nextToken) (\s@ListEventSources' {} a -> s {nextToken = a} :: ListEventSources)

-- | Specifying this limits the results to only those partner event sources
-- with names that start with the specified prefix.
listEventSources_namePrefix :: Lens.Lens' ListEventSources (Prelude.Maybe Prelude.Text)
listEventSources_namePrefix = Lens.lens (\ListEventSources' {namePrefix} -> namePrefix) (\s@ListEventSources' {} a -> s {namePrefix = a} :: ListEventSources)

-- | Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
listEventSources_limit :: Lens.Lens' ListEventSources (Prelude.Maybe Prelude.Natural)
listEventSources_limit = Lens.lens (\ListEventSources' {limit} -> limit) (\s@ListEventSources' {} a -> s {limit = a} :: ListEventSources)

instance Prelude.AWSRequest ListEventSources where
  type Rs ListEventSources = ListEventSourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventSourcesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "EventSources"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventSources

instance Prelude.NFData ListEventSources

instance Prelude.ToHeaders ListEventSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSEvents.ListEventSources" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListEventSources where
  toJSON ListEventSources' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("NamePrefix" Prelude..=) Prelude.<$> namePrefix,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath ListEventSources where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListEventSources where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listEventSourcesResponse_eventSources = Lens.lens (\ListEventSourcesResponse' {eventSources} -> eventSources) (\s@ListEventSourcesResponse' {} a -> s {eventSources = a} :: ListEventSourcesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listEventSourcesResponse_httpStatus :: Lens.Lens' ListEventSourcesResponse Prelude.Int
listEventSourcesResponse_httpStatus = Lens.lens (\ListEventSourcesResponse' {httpStatus} -> httpStatus) (\s@ListEventSourcesResponse' {} a -> s {httpStatus = a} :: ListEventSourcesResponse)

instance Prelude.NFData ListEventSourcesResponse
