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
-- Module      : Amazonka.MGN.ListWaves
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all waves or multiple waves by ID.
--
-- This operation returns paginated results.
module Amazonka.MGN.ListWaves
  ( -- * Creating a Request
    ListWaves (..),
    newListWaves,

    -- * Request Lenses
    listWaves_filters,
    listWaves_maxResults,
    listWaves_nextToken,

    -- * Destructuring the Response
    ListWavesResponse (..),
    newListWavesResponse,

    -- * Response Lenses
    listWavesResponse_items,
    listWavesResponse_nextToken,
    listWavesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWaves' smart constructor.
data ListWaves = ListWaves'
  { -- | Waves list filters.
    filters :: Prelude.Maybe ListWavesRequestFilters,
    -- | Maximum results to return when listing waves.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Request next token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWaves' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listWaves_filters' - Waves list filters.
--
-- 'maxResults', 'listWaves_maxResults' - Maximum results to return when listing waves.
--
-- 'nextToken', 'listWaves_nextToken' - Request next token.
newListWaves ::
  ListWaves
newListWaves =
  ListWaves'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Waves list filters.
listWaves_filters :: Lens.Lens' ListWaves (Prelude.Maybe ListWavesRequestFilters)
listWaves_filters = Lens.lens (\ListWaves' {filters} -> filters) (\s@ListWaves' {} a -> s {filters = a} :: ListWaves)

-- | Maximum results to return when listing waves.
listWaves_maxResults :: Lens.Lens' ListWaves (Prelude.Maybe Prelude.Natural)
listWaves_maxResults = Lens.lens (\ListWaves' {maxResults} -> maxResults) (\s@ListWaves' {} a -> s {maxResults = a} :: ListWaves)

-- | Request next token.
listWaves_nextToken :: Lens.Lens' ListWaves (Prelude.Maybe Prelude.Text)
listWaves_nextToken = Lens.lens (\ListWaves' {nextToken} -> nextToken) (\s@ListWaves' {} a -> s {nextToken = a} :: ListWaves)

instance Core.AWSPager ListWaves where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWavesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWavesResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWaves_nextToken
          Lens..~ rs
          Lens.^? listWavesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListWaves where
  type AWSResponse ListWaves = ListWavesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWavesResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWaves where
  hashWithSalt _salt ListWaves' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListWaves where
  rnf ListWaves' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListWaves where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWaves where
  toJSON ListWaves' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListWaves where
  toPath = Prelude.const "/ListWaves"

instance Data.ToQuery ListWaves where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWavesResponse' smart constructor.
data ListWavesResponse = ListWavesResponse'
  { -- | Waves list.
    items :: Prelude.Maybe [Wave],
    -- | Response next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWavesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listWavesResponse_items' - Waves list.
--
-- 'nextToken', 'listWavesResponse_nextToken' - Response next token.
--
-- 'httpStatus', 'listWavesResponse_httpStatus' - The response's http status code.
newListWavesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWavesResponse
newListWavesResponse pHttpStatus_ =
  ListWavesResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Waves list.
listWavesResponse_items :: Lens.Lens' ListWavesResponse (Prelude.Maybe [Wave])
listWavesResponse_items = Lens.lens (\ListWavesResponse' {items} -> items) (\s@ListWavesResponse' {} a -> s {items = a} :: ListWavesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Response next token.
listWavesResponse_nextToken :: Lens.Lens' ListWavesResponse (Prelude.Maybe Prelude.Text)
listWavesResponse_nextToken = Lens.lens (\ListWavesResponse' {nextToken} -> nextToken) (\s@ListWavesResponse' {} a -> s {nextToken = a} :: ListWavesResponse)

-- | The response's http status code.
listWavesResponse_httpStatus :: Lens.Lens' ListWavesResponse Prelude.Int
listWavesResponse_httpStatus = Lens.lens (\ListWavesResponse' {httpStatus} -> httpStatus) (\s@ListWavesResponse' {} a -> s {httpStatus = a} :: ListWavesResponse)

instance Prelude.NFData ListWavesResponse where
  rnf ListWavesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
