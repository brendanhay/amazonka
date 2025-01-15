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
-- Module      : Amazonka.Omics.ListVariantStores
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of variant stores.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListVariantStores
  ( -- * Creating a Request
    ListVariantStores (..),
    newListVariantStores,

    -- * Request Lenses
    listVariantStores_filter,
    listVariantStores_ids,
    listVariantStores_maxResults,
    listVariantStores_nextToken,

    -- * Destructuring the Response
    ListVariantStoresResponse (..),
    newListVariantStoresResponse,

    -- * Response Lenses
    listVariantStoresResponse_nextToken,
    listVariantStoresResponse_variantStores,
    listVariantStoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVariantStores' smart constructor.
data ListVariantStores = ListVariantStores'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ListVariantStoresFilter,
    -- | A list of store IDs.
    ids :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of stores to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVariantStores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listVariantStores_filter' - A filter to apply to the list.
--
-- 'ids', 'listVariantStores_ids' - A list of store IDs.
--
-- 'maxResults', 'listVariantStores_maxResults' - The maximum number of stores to return in one page of results.
--
-- 'nextToken', 'listVariantStores_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListVariantStores ::
  ListVariantStores
newListVariantStores =
  ListVariantStores'
    { filter' = Prelude.Nothing,
      ids = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A filter to apply to the list.
listVariantStores_filter :: Lens.Lens' ListVariantStores (Prelude.Maybe ListVariantStoresFilter)
listVariantStores_filter = Lens.lens (\ListVariantStores' {filter'} -> filter') (\s@ListVariantStores' {} a -> s {filter' = a} :: ListVariantStores)

-- | A list of store IDs.
listVariantStores_ids :: Lens.Lens' ListVariantStores (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listVariantStores_ids = Lens.lens (\ListVariantStores' {ids} -> ids) (\s@ListVariantStores' {} a -> s {ids = a} :: ListVariantStores) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of stores to return in one page of results.
listVariantStores_maxResults :: Lens.Lens' ListVariantStores (Prelude.Maybe Prelude.Natural)
listVariantStores_maxResults = Lens.lens (\ListVariantStores' {maxResults} -> maxResults) (\s@ListVariantStores' {} a -> s {maxResults = a} :: ListVariantStores)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listVariantStores_nextToken :: Lens.Lens' ListVariantStores (Prelude.Maybe Prelude.Text)
listVariantStores_nextToken = Lens.lens (\ListVariantStores' {nextToken} -> nextToken) (\s@ListVariantStores' {} a -> s {nextToken = a} :: ListVariantStores)

instance Core.AWSPager ListVariantStores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVariantStoresResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVariantStoresResponse_variantStores
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listVariantStores_nextToken
              Lens..~ rs
              Lens.^? listVariantStoresResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListVariantStores where
  type
    AWSResponse ListVariantStores =
      ListVariantStoresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVariantStoresResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "variantStores" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVariantStores where
  hashWithSalt _salt ListVariantStores' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` ids
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListVariantStores where
  rnf ListVariantStores' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf ids `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken

instance Data.ToHeaders ListVariantStores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVariantStores where
  toJSON ListVariantStores' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("ids" Data..=) Prelude.<$> ids
          ]
      )

instance Data.ToPath ListVariantStores where
  toPath = Prelude.const "/variantStores"

instance Data.ToQuery ListVariantStores where
  toQuery ListVariantStores' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListVariantStoresResponse' smart constructor.
data ListVariantStoresResponse = ListVariantStoresResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of variant stores.
    variantStores :: Prelude.Maybe [VariantStoreItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVariantStoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVariantStoresResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'variantStores', 'listVariantStoresResponse_variantStores' - A list of variant stores.
--
-- 'httpStatus', 'listVariantStoresResponse_httpStatus' - The response's http status code.
newListVariantStoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVariantStoresResponse
newListVariantStoresResponse pHttpStatus_ =
  ListVariantStoresResponse'
    { nextToken =
        Prelude.Nothing,
      variantStores = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token that\'s included if more results are available.
listVariantStoresResponse_nextToken :: Lens.Lens' ListVariantStoresResponse (Prelude.Maybe Prelude.Text)
listVariantStoresResponse_nextToken = Lens.lens (\ListVariantStoresResponse' {nextToken} -> nextToken) (\s@ListVariantStoresResponse' {} a -> s {nextToken = a} :: ListVariantStoresResponse)

-- | A list of variant stores.
listVariantStoresResponse_variantStores :: Lens.Lens' ListVariantStoresResponse (Prelude.Maybe [VariantStoreItem])
listVariantStoresResponse_variantStores = Lens.lens (\ListVariantStoresResponse' {variantStores} -> variantStores) (\s@ListVariantStoresResponse' {} a -> s {variantStores = a} :: ListVariantStoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVariantStoresResponse_httpStatus :: Lens.Lens' ListVariantStoresResponse Prelude.Int
listVariantStoresResponse_httpStatus = Lens.lens (\ListVariantStoresResponse' {httpStatus} -> httpStatus) (\s@ListVariantStoresResponse' {} a -> s {httpStatus = a} :: ListVariantStoresResponse)

instance Prelude.NFData ListVariantStoresResponse where
  rnf ListVariantStoresResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf variantStores `Prelude.seq`
        Prelude.rnf httpStatus
