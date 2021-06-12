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
-- Module      : Network.AWS.CloudWatchEvents.ListEventBuses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the event buses in your account, including the default event
-- bus, custom event buses, and partner event buses.
module Network.AWS.CloudWatchEvents.ListEventBuses
  ( -- * Creating a Request
    ListEventBuses (..),
    newListEventBuses,

    -- * Request Lenses
    listEventBuses_nextToken,
    listEventBuses_namePrefix,
    listEventBuses_limit,

    -- * Destructuring the Response
    ListEventBusesResponse (..),
    newListEventBusesResponse,

    -- * Response Lenses
    listEventBusesResponse_nextToken,
    listEventBusesResponse_eventBuses,
    listEventBusesResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEventBuses' smart constructor.
data ListEventBuses = ListEventBuses'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Specifying this limits the results to only those event buses with names
    -- that start with the specified prefix.
    namePrefix :: Core.Maybe Core.Text,
    -- | Specifying this limits the number of results returned by this operation.
    -- The operation also returns a NextToken which you can use in a subsequent
    -- operation to retrieve the next set of results.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEventBuses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventBuses_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'namePrefix', 'listEventBuses_namePrefix' - Specifying this limits the results to only those event buses with names
-- that start with the specified prefix.
--
-- 'limit', 'listEventBuses_limit' - Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
newListEventBuses ::
  ListEventBuses
newListEventBuses =
  ListEventBuses'
    { nextToken = Core.Nothing,
      namePrefix = Core.Nothing,
      limit = Core.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listEventBuses_nextToken :: Lens.Lens' ListEventBuses (Core.Maybe Core.Text)
listEventBuses_nextToken = Lens.lens (\ListEventBuses' {nextToken} -> nextToken) (\s@ListEventBuses' {} a -> s {nextToken = a} :: ListEventBuses)

-- | Specifying this limits the results to only those event buses with names
-- that start with the specified prefix.
listEventBuses_namePrefix :: Lens.Lens' ListEventBuses (Core.Maybe Core.Text)
listEventBuses_namePrefix = Lens.lens (\ListEventBuses' {namePrefix} -> namePrefix) (\s@ListEventBuses' {} a -> s {namePrefix = a} :: ListEventBuses)

-- | Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
listEventBuses_limit :: Lens.Lens' ListEventBuses (Core.Maybe Core.Natural)
listEventBuses_limit = Lens.lens (\ListEventBuses' {limit} -> limit) (\s@ListEventBuses' {} a -> s {limit = a} :: ListEventBuses)

instance Core.AWSRequest ListEventBuses where
  type
    AWSResponse ListEventBuses =
      ListEventBusesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventBusesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "EventBuses" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListEventBuses

instance Core.NFData ListEventBuses

instance Core.ToHeaders ListEventBuses where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.ListEventBuses" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListEventBuses where
  toJSON ListEventBuses' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("NamePrefix" Core..=) Core.<$> namePrefix,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListEventBuses where
  toPath = Core.const "/"

instance Core.ToQuery ListEventBuses where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListEventBusesResponse' smart constructor.
data ListEventBusesResponse = ListEventBusesResponse'
  { -- | A token you can use in a subsequent operation to retrieve the next set
    -- of results.
    nextToken :: Core.Maybe Core.Text,
    -- | This list of event buses.
    eventBuses :: Core.Maybe [EventBus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEventBusesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventBusesResponse_nextToken' - A token you can use in a subsequent operation to retrieve the next set
-- of results.
--
-- 'eventBuses', 'listEventBusesResponse_eventBuses' - This list of event buses.
--
-- 'httpStatus', 'listEventBusesResponse_httpStatus' - The response's http status code.
newListEventBusesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListEventBusesResponse
newListEventBusesResponse pHttpStatus_ =
  ListEventBusesResponse'
    { nextToken = Core.Nothing,
      eventBuses = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token you can use in a subsequent operation to retrieve the next set
-- of results.
listEventBusesResponse_nextToken :: Lens.Lens' ListEventBusesResponse (Core.Maybe Core.Text)
listEventBusesResponse_nextToken = Lens.lens (\ListEventBusesResponse' {nextToken} -> nextToken) (\s@ListEventBusesResponse' {} a -> s {nextToken = a} :: ListEventBusesResponse)

-- | This list of event buses.
listEventBusesResponse_eventBuses :: Lens.Lens' ListEventBusesResponse (Core.Maybe [EventBus])
listEventBusesResponse_eventBuses = Lens.lens (\ListEventBusesResponse' {eventBuses} -> eventBuses) (\s@ListEventBusesResponse' {} a -> s {eventBuses = a} :: ListEventBusesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listEventBusesResponse_httpStatus :: Lens.Lens' ListEventBusesResponse Core.Int
listEventBusesResponse_httpStatus = Lens.lens (\ListEventBusesResponse' {httpStatus} -> httpStatus) (\s@ListEventBusesResponse' {} a -> s {httpStatus = a} :: ListEventBusesResponse)

instance Core.NFData ListEventBusesResponse
