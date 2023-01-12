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
-- Module      : Amazonka.CloudWatchEvents.ListEventBuses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the event buses in your account, including the default event
-- bus, custom event buses, and partner event buses.
module Amazonka.CloudWatchEvents.ListEventBuses
  ( -- * Creating a Request
    ListEventBuses (..),
    newListEventBuses,

    -- * Request Lenses
    listEventBuses_limit,
    listEventBuses_namePrefix,
    listEventBuses_nextToken,

    -- * Destructuring the Response
    ListEventBusesResponse (..),
    newListEventBusesResponse,

    -- * Response Lenses
    listEventBusesResponse_eventBuses,
    listEventBusesResponse_nextToken,
    listEventBusesResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventBuses' smart constructor.
data ListEventBuses = ListEventBuses'
  { -- | Specifying this limits the number of results returned by this operation.
    -- The operation also returns a NextToken which you can use in a subsequent
    -- operation to retrieve the next set of results.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Specifying this limits the results to only those event buses with names
    -- that start with the specified prefix.
    namePrefix :: Prelude.Maybe Prelude.Text,
    -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventBuses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listEventBuses_limit' - Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
--
-- 'namePrefix', 'listEventBuses_namePrefix' - Specifying this limits the results to only those event buses with names
-- that start with the specified prefix.
--
-- 'nextToken', 'listEventBuses_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
newListEventBuses ::
  ListEventBuses
newListEventBuses =
  ListEventBuses'
    { limit = Prelude.Nothing,
      namePrefix = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
listEventBuses_limit :: Lens.Lens' ListEventBuses (Prelude.Maybe Prelude.Natural)
listEventBuses_limit = Lens.lens (\ListEventBuses' {limit} -> limit) (\s@ListEventBuses' {} a -> s {limit = a} :: ListEventBuses)

-- | Specifying this limits the results to only those event buses with names
-- that start with the specified prefix.
listEventBuses_namePrefix :: Lens.Lens' ListEventBuses (Prelude.Maybe Prelude.Text)
listEventBuses_namePrefix = Lens.lens (\ListEventBuses' {namePrefix} -> namePrefix) (\s@ListEventBuses' {} a -> s {namePrefix = a} :: ListEventBuses)

-- | The token returned by a previous call to retrieve the next set of
-- results.
listEventBuses_nextToken :: Lens.Lens' ListEventBuses (Prelude.Maybe Prelude.Text)
listEventBuses_nextToken = Lens.lens (\ListEventBuses' {nextToken} -> nextToken) (\s@ListEventBuses' {} a -> s {nextToken = a} :: ListEventBuses)

instance Core.AWSRequest ListEventBuses where
  type
    AWSResponse ListEventBuses =
      ListEventBusesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventBusesResponse'
            Prelude.<$> (x Data..?> "EventBuses" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventBuses where
  hashWithSalt _salt ListEventBuses' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` namePrefix
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEventBuses where
  rnf ListEventBuses' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf namePrefix
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEventBuses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.ListEventBuses" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEventBuses where
  toJSON ListEventBuses' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NamePrefix" Data..=) Prelude.<$> namePrefix,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEventBuses where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEventBuses where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEventBusesResponse' smart constructor.
data ListEventBusesResponse = ListEventBusesResponse'
  { -- | This list of event buses.
    eventBuses :: Prelude.Maybe [EventBus],
    -- | A token you can use in a subsequent operation to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventBusesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBuses', 'listEventBusesResponse_eventBuses' - This list of event buses.
--
-- 'nextToken', 'listEventBusesResponse_nextToken' - A token you can use in a subsequent operation to retrieve the next set
-- of results.
--
-- 'httpStatus', 'listEventBusesResponse_httpStatus' - The response's http status code.
newListEventBusesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventBusesResponse
newListEventBusesResponse pHttpStatus_ =
  ListEventBusesResponse'
    { eventBuses =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This list of event buses.
listEventBusesResponse_eventBuses :: Lens.Lens' ListEventBusesResponse (Prelude.Maybe [EventBus])
listEventBusesResponse_eventBuses = Lens.lens (\ListEventBusesResponse' {eventBuses} -> eventBuses) (\s@ListEventBusesResponse' {} a -> s {eventBuses = a} :: ListEventBusesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token you can use in a subsequent operation to retrieve the next set
-- of results.
listEventBusesResponse_nextToken :: Lens.Lens' ListEventBusesResponse (Prelude.Maybe Prelude.Text)
listEventBusesResponse_nextToken = Lens.lens (\ListEventBusesResponse' {nextToken} -> nextToken) (\s@ListEventBusesResponse' {} a -> s {nextToken = a} :: ListEventBusesResponse)

-- | The response's http status code.
listEventBusesResponse_httpStatus :: Lens.Lens' ListEventBusesResponse Prelude.Int
listEventBusesResponse_httpStatus = Lens.lens (\ListEventBusesResponse' {httpStatus} -> httpStatus) (\s@ListEventBusesResponse' {} a -> s {httpStatus = a} :: ListEventBusesResponse)

instance Prelude.NFData ListEventBusesResponse where
  rnf ListEventBusesResponse' {..} =
    Prelude.rnf eventBuses
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
