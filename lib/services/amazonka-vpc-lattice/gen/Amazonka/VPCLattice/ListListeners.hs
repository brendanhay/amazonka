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
-- Module      : Amazonka.VPCLattice.ListListeners
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the listeners for the specified service.
--
-- This operation returns paginated results.
module Amazonka.VPCLattice.ListListeners
  ( -- * Creating a Request
    ListListeners (..),
    newListListeners,

    -- * Request Lenses
    listListeners_maxResults,
    listListeners_nextToken,
    listListeners_serviceIdentifier,

    -- * Destructuring the Response
    ListListenersResponse (..),
    newListListenersResponse,

    -- * Response Lenses
    listListenersResponse_nextToken,
    listListenersResponse_httpStatus,
    listListenersResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newListListeners' smart constructor.
data ListListeners = ListListeners'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListListeners' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listListeners_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listListeners_nextToken' - A pagination token for the next page of results.
--
-- 'serviceIdentifier', 'listListeners_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newListListeners ::
  -- | 'serviceIdentifier'
  Prelude.Text ->
  ListListeners
newListListeners pServiceIdentifier_ =
  ListListeners'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceIdentifier = pServiceIdentifier_
    }

-- | The maximum number of results to return.
listListeners_maxResults :: Lens.Lens' ListListeners (Prelude.Maybe Prelude.Natural)
listListeners_maxResults = Lens.lens (\ListListeners' {maxResults} -> maxResults) (\s@ListListeners' {} a -> s {maxResults = a} :: ListListeners)

-- | A pagination token for the next page of results.
listListeners_nextToken :: Lens.Lens' ListListeners (Prelude.Maybe Prelude.Text)
listListeners_nextToken = Lens.lens (\ListListeners' {nextToken} -> nextToken) (\s@ListListeners' {} a -> s {nextToken = a} :: ListListeners)

-- | The ID or Amazon Resource Name (ARN) of the service.
listListeners_serviceIdentifier :: Lens.Lens' ListListeners Prelude.Text
listListeners_serviceIdentifier = Lens.lens (\ListListeners' {serviceIdentifier} -> serviceIdentifier) (\s@ListListeners' {} a -> s {serviceIdentifier = a} :: ListListeners)

instance Core.AWSPager ListListeners where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listListenersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listListenersResponse_items) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listListeners_nextToken
          Lens..~ rs
          Lens.^? listListenersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListListeners where
  type
    AWSResponse ListListeners =
      ListListenersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListListenersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListListeners where
  hashWithSalt _salt ListListeners' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData ListListeners where
  rnf ListListeners' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders ListListeners where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListListeners where
  toPath ListListeners' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners"
      ]

instance Data.ToQuery ListListeners where
  toQuery ListListeners' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListListenersResponse' smart constructor.
data ListListenersResponse = ListListenersResponse'
  { -- | If there are additional results, a pagination token for the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the listeners.
    items :: [ListenerSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListListenersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listListenersResponse_nextToken' - If there are additional results, a pagination token for the next page of
-- results.
--
-- 'httpStatus', 'listListenersResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listListenersResponse_items' - Information about the listeners.
newListListenersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListListenersResponse
newListListenersResponse pHttpStatus_ =
  ListListenersResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | If there are additional results, a pagination token for the next page of
-- results.
listListenersResponse_nextToken :: Lens.Lens' ListListenersResponse (Prelude.Maybe Prelude.Text)
listListenersResponse_nextToken = Lens.lens (\ListListenersResponse' {nextToken} -> nextToken) (\s@ListListenersResponse' {} a -> s {nextToken = a} :: ListListenersResponse)

-- | The response's http status code.
listListenersResponse_httpStatus :: Lens.Lens' ListListenersResponse Prelude.Int
listListenersResponse_httpStatus = Lens.lens (\ListListenersResponse' {httpStatus} -> httpStatus) (\s@ListListenersResponse' {} a -> s {httpStatus = a} :: ListListenersResponse)

-- | Information about the listeners.
listListenersResponse_items :: Lens.Lens' ListListenersResponse [ListenerSummary]
listListenersResponse_items = Lens.lens (\ListListenersResponse' {items} -> items) (\s@ListListenersResponse' {} a -> s {items = a} :: ListListenersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListListenersResponse where
  rnf ListListenersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
