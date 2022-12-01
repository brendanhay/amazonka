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
-- Module      : Amazonka.MacieV2.ListCustomDataIdentifiers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a subset of information about all the custom data identifiers
-- for an account.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListCustomDataIdentifiers
  ( -- * Creating a Request
    ListCustomDataIdentifiers (..),
    newListCustomDataIdentifiers,

    -- * Request Lenses
    listCustomDataIdentifiers_nextToken,
    listCustomDataIdentifiers_maxResults,

    -- * Destructuring the Response
    ListCustomDataIdentifiersResponse (..),
    newListCustomDataIdentifiersResponse,

    -- * Response Lenses
    listCustomDataIdentifiersResponse_items,
    listCustomDataIdentifiersResponse_nextToken,
    listCustomDataIdentifiersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomDataIdentifiers' smart constructor.
data ListCustomDataIdentifiers = ListCustomDataIdentifiers'
  { -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of the response.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomDataIdentifiers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomDataIdentifiers_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'maxResults', 'listCustomDataIdentifiers_maxResults' - The maximum number of items to include in each page of the response.
newListCustomDataIdentifiers ::
  ListCustomDataIdentifiers
newListCustomDataIdentifiers =
  ListCustomDataIdentifiers'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listCustomDataIdentifiers_nextToken :: Lens.Lens' ListCustomDataIdentifiers (Prelude.Maybe Prelude.Text)
listCustomDataIdentifiers_nextToken = Lens.lens (\ListCustomDataIdentifiers' {nextToken} -> nextToken) (\s@ListCustomDataIdentifiers' {} a -> s {nextToken = a} :: ListCustomDataIdentifiers)

-- | The maximum number of items to include in each page of the response.
listCustomDataIdentifiers_maxResults :: Lens.Lens' ListCustomDataIdentifiers (Prelude.Maybe Prelude.Int)
listCustomDataIdentifiers_maxResults = Lens.lens (\ListCustomDataIdentifiers' {maxResults} -> maxResults) (\s@ListCustomDataIdentifiers' {} a -> s {maxResults = a} :: ListCustomDataIdentifiers)

instance Core.AWSPager ListCustomDataIdentifiers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomDataIdentifiersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomDataIdentifiersResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCustomDataIdentifiers_nextToken
          Lens..~ rs
          Lens.^? listCustomDataIdentifiersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCustomDataIdentifiers where
  type
    AWSResponse ListCustomDataIdentifiers =
      ListCustomDataIdentifiersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomDataIdentifiersResponse'
            Prelude.<$> (x Core..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCustomDataIdentifiers where
  hashWithSalt _salt ListCustomDataIdentifiers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCustomDataIdentifiers where
  rnf ListCustomDataIdentifiers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListCustomDataIdentifiers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCustomDataIdentifiers where
  toJSON ListCustomDataIdentifiers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListCustomDataIdentifiers where
  toPath =
    Prelude.const "/custom-data-identifiers/list"

instance Core.ToQuery ListCustomDataIdentifiers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomDataIdentifiersResponse' smart constructor.
data ListCustomDataIdentifiersResponse = ListCustomDataIdentifiersResponse'
  { -- | An array of objects, one for each custom data identifier.
    items :: Prelude.Maybe [CustomDataIdentifierSummary],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomDataIdentifiersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listCustomDataIdentifiersResponse_items' - An array of objects, one for each custom data identifier.
--
-- 'nextToken', 'listCustomDataIdentifiersResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'httpStatus', 'listCustomDataIdentifiersResponse_httpStatus' - The response's http status code.
newListCustomDataIdentifiersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomDataIdentifiersResponse
newListCustomDataIdentifiersResponse pHttpStatus_ =
  ListCustomDataIdentifiersResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each custom data identifier.
listCustomDataIdentifiersResponse_items :: Lens.Lens' ListCustomDataIdentifiersResponse (Prelude.Maybe [CustomDataIdentifierSummary])
listCustomDataIdentifiersResponse_items = Lens.lens (\ListCustomDataIdentifiersResponse' {items} -> items) (\s@ListCustomDataIdentifiersResponse' {} a -> s {items = a} :: ListCustomDataIdentifiersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listCustomDataIdentifiersResponse_nextToken :: Lens.Lens' ListCustomDataIdentifiersResponse (Prelude.Maybe Prelude.Text)
listCustomDataIdentifiersResponse_nextToken = Lens.lens (\ListCustomDataIdentifiersResponse' {nextToken} -> nextToken) (\s@ListCustomDataIdentifiersResponse' {} a -> s {nextToken = a} :: ListCustomDataIdentifiersResponse)

-- | The response's http status code.
listCustomDataIdentifiersResponse_httpStatus :: Lens.Lens' ListCustomDataIdentifiersResponse Prelude.Int
listCustomDataIdentifiersResponse_httpStatus = Lens.lens (\ListCustomDataIdentifiersResponse' {httpStatus} -> httpStatus) (\s@ListCustomDataIdentifiersResponse' {} a -> s {httpStatus = a} :: ListCustomDataIdentifiersResponse)

instance
  Prelude.NFData
    ListCustomDataIdentifiersResponse
  where
  rnf ListCustomDataIdentifiersResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
