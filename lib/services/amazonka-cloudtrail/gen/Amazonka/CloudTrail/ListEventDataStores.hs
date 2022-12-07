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
-- Module      : Amazonka.CloudTrail.ListEventDataStores
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all event data stores in the account, in the
-- current region.
module Amazonka.CloudTrail.ListEventDataStores
  ( -- * Creating a Request
    ListEventDataStores (..),
    newListEventDataStores,

    -- * Request Lenses
    listEventDataStores_nextToken,
    listEventDataStores_maxResults,

    -- * Destructuring the Response
    ListEventDataStoresResponse (..),
    newListEventDataStoresResponse,

    -- * Response Lenses
    listEventDataStoresResponse_nextToken,
    listEventDataStoresResponse_eventDataStores,
    listEventDataStoresResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventDataStores' smart constructor.
data ListEventDataStores = ListEventDataStores'
  { -- | A token you can use to get the next page of event data store results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of event data stores to display on a single page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventDataStores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventDataStores_nextToken' - A token you can use to get the next page of event data store results.
--
-- 'maxResults', 'listEventDataStores_maxResults' - The maximum number of event data stores to display on a single page.
newListEventDataStores ::
  ListEventDataStores
newListEventDataStores =
  ListEventDataStores'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token you can use to get the next page of event data store results.
listEventDataStores_nextToken :: Lens.Lens' ListEventDataStores (Prelude.Maybe Prelude.Text)
listEventDataStores_nextToken = Lens.lens (\ListEventDataStores' {nextToken} -> nextToken) (\s@ListEventDataStores' {} a -> s {nextToken = a} :: ListEventDataStores)

-- | The maximum number of event data stores to display on a single page.
listEventDataStores_maxResults :: Lens.Lens' ListEventDataStores (Prelude.Maybe Prelude.Natural)
listEventDataStores_maxResults = Lens.lens (\ListEventDataStores' {maxResults} -> maxResults) (\s@ListEventDataStores' {} a -> s {maxResults = a} :: ListEventDataStores)

instance Core.AWSRequest ListEventDataStores where
  type
    AWSResponse ListEventDataStores =
      ListEventDataStoresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventDataStoresResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "EventDataStores"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventDataStores where
  hashWithSalt _salt ListEventDataStores' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListEventDataStores where
  rnf ListEventDataStores' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListEventDataStores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListEventDataStores" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEventDataStores where
  toJSON ListEventDataStores' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListEventDataStores where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEventDataStores where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEventDataStoresResponse' smart constructor.
data ListEventDataStoresResponse = ListEventDataStoresResponse'
  { -- | A token you can use to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains information about event data stores in the account, in the
    -- current region.
    eventDataStores :: Prelude.Maybe [EventDataStore],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventDataStoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventDataStoresResponse_nextToken' - A token you can use to get the next page of results.
--
-- 'eventDataStores', 'listEventDataStoresResponse_eventDataStores' - Contains information about event data stores in the account, in the
-- current region.
--
-- 'httpStatus', 'listEventDataStoresResponse_httpStatus' - The response's http status code.
newListEventDataStoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventDataStoresResponse
newListEventDataStoresResponse pHttpStatus_ =
  ListEventDataStoresResponse'
    { nextToken =
        Prelude.Nothing,
      eventDataStores = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token you can use to get the next page of results.
listEventDataStoresResponse_nextToken :: Lens.Lens' ListEventDataStoresResponse (Prelude.Maybe Prelude.Text)
listEventDataStoresResponse_nextToken = Lens.lens (\ListEventDataStoresResponse' {nextToken} -> nextToken) (\s@ListEventDataStoresResponse' {} a -> s {nextToken = a} :: ListEventDataStoresResponse)

-- | Contains information about event data stores in the account, in the
-- current region.
listEventDataStoresResponse_eventDataStores :: Lens.Lens' ListEventDataStoresResponse (Prelude.Maybe [EventDataStore])
listEventDataStoresResponse_eventDataStores = Lens.lens (\ListEventDataStoresResponse' {eventDataStores} -> eventDataStores) (\s@ListEventDataStoresResponse' {} a -> s {eventDataStores = a} :: ListEventDataStoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEventDataStoresResponse_httpStatus :: Lens.Lens' ListEventDataStoresResponse Prelude.Int
listEventDataStoresResponse_httpStatus = Lens.lens (\ListEventDataStoresResponse' {httpStatus} -> httpStatus) (\s@ListEventDataStoresResponse' {} a -> s {httpStatus = a} :: ListEventDataStoresResponse)

instance Prelude.NFData ListEventDataStoresResponse where
  rnf ListEventDataStoresResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventDataStores
      `Prelude.seq` Prelude.rnf httpStatus
