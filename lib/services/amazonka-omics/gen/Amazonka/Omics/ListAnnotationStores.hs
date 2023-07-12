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
-- Module      : Amazonka.Omics.ListAnnotationStores
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of annotation stores.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListAnnotationStores
  ( -- * Creating a Request
    ListAnnotationStores (..),
    newListAnnotationStores,

    -- * Request Lenses
    listAnnotationStores_filter,
    listAnnotationStores_ids,
    listAnnotationStores_maxResults,
    listAnnotationStores_nextToken,

    -- * Destructuring the Response
    ListAnnotationStoresResponse (..),
    newListAnnotationStoresResponse,

    -- * Response Lenses
    listAnnotationStoresResponse_annotationStores,
    listAnnotationStoresResponse_nextToken,
    listAnnotationStoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAnnotationStores' smart constructor.
data ListAnnotationStores = ListAnnotationStores'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ListAnnotationStoresFilter,
    -- | IDs of stores to list.
    ids :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of stores to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnnotationStores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listAnnotationStores_filter' - A filter to apply to the list.
--
-- 'ids', 'listAnnotationStores_ids' - IDs of stores to list.
--
-- 'maxResults', 'listAnnotationStores_maxResults' - The maximum number of stores to return in one page of results.
--
-- 'nextToken', 'listAnnotationStores_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListAnnotationStores ::
  ListAnnotationStores
newListAnnotationStores =
  ListAnnotationStores'
    { filter' = Prelude.Nothing,
      ids = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A filter to apply to the list.
listAnnotationStores_filter :: Lens.Lens' ListAnnotationStores (Prelude.Maybe ListAnnotationStoresFilter)
listAnnotationStores_filter = Lens.lens (\ListAnnotationStores' {filter'} -> filter') (\s@ListAnnotationStores' {} a -> s {filter' = a} :: ListAnnotationStores)

-- | IDs of stores to list.
listAnnotationStores_ids :: Lens.Lens' ListAnnotationStores (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listAnnotationStores_ids = Lens.lens (\ListAnnotationStores' {ids} -> ids) (\s@ListAnnotationStores' {} a -> s {ids = a} :: ListAnnotationStores) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of stores to return in one page of results.
listAnnotationStores_maxResults :: Lens.Lens' ListAnnotationStores (Prelude.Maybe Prelude.Natural)
listAnnotationStores_maxResults = Lens.lens (\ListAnnotationStores' {maxResults} -> maxResults) (\s@ListAnnotationStores' {} a -> s {maxResults = a} :: ListAnnotationStores)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listAnnotationStores_nextToken :: Lens.Lens' ListAnnotationStores (Prelude.Maybe Prelude.Text)
listAnnotationStores_nextToken = Lens.lens (\ListAnnotationStores' {nextToken} -> nextToken) (\s@ListAnnotationStores' {} a -> s {nextToken = a} :: ListAnnotationStores)

instance Core.AWSPager ListAnnotationStores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAnnotationStoresResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAnnotationStoresResponse_annotationStores
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAnnotationStores_nextToken
          Lens..~ rs
          Lens.^? listAnnotationStoresResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAnnotationStores where
  type
    AWSResponse ListAnnotationStores =
      ListAnnotationStoresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAnnotationStoresResponse'
            Prelude.<$> ( x
                            Data..?> "annotationStores"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAnnotationStores where
  hashWithSalt _salt ListAnnotationStores' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` ids
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAnnotationStores where
  rnf ListAnnotationStores' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf ids
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAnnotationStores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAnnotationStores where
  toJSON ListAnnotationStores' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("ids" Data..=) Prelude.<$> ids
          ]
      )

instance Data.ToPath ListAnnotationStores where
  toPath = Prelude.const "/annotationStores"

instance Data.ToQuery ListAnnotationStores where
  toQuery ListAnnotationStores' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAnnotationStoresResponse' smart constructor.
data ListAnnotationStoresResponse = ListAnnotationStoresResponse'
  { -- | A list of stores.
    annotationStores :: Prelude.Maybe [AnnotationStoreItem],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnnotationStoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotationStores', 'listAnnotationStoresResponse_annotationStores' - A list of stores.
--
-- 'nextToken', 'listAnnotationStoresResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listAnnotationStoresResponse_httpStatus' - The response's http status code.
newListAnnotationStoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAnnotationStoresResponse
newListAnnotationStoresResponse pHttpStatus_ =
  ListAnnotationStoresResponse'
    { annotationStores =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of stores.
listAnnotationStoresResponse_annotationStores :: Lens.Lens' ListAnnotationStoresResponse (Prelude.Maybe [AnnotationStoreItem])
listAnnotationStoresResponse_annotationStores = Lens.lens (\ListAnnotationStoresResponse' {annotationStores} -> annotationStores) (\s@ListAnnotationStoresResponse' {} a -> s {annotationStores = a} :: ListAnnotationStoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listAnnotationStoresResponse_nextToken :: Lens.Lens' ListAnnotationStoresResponse (Prelude.Maybe Prelude.Text)
listAnnotationStoresResponse_nextToken = Lens.lens (\ListAnnotationStoresResponse' {nextToken} -> nextToken) (\s@ListAnnotationStoresResponse' {} a -> s {nextToken = a} :: ListAnnotationStoresResponse)

-- | The response's http status code.
listAnnotationStoresResponse_httpStatus :: Lens.Lens' ListAnnotationStoresResponse Prelude.Int
listAnnotationStoresResponse_httpStatus = Lens.lens (\ListAnnotationStoresResponse' {httpStatus} -> httpStatus) (\s@ListAnnotationStoresResponse' {} a -> s {httpStatus = a} :: ListAnnotationStoresResponse)

instance Prelude.NFData ListAnnotationStoresResponse where
  rnf ListAnnotationStoresResponse' {..} =
    Prelude.rnf annotationStores
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
