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
-- Module      : Amazonka.Omics.ListSequenceStores
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of sequence stores.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListSequenceStores
  ( -- * Creating a Request
    ListSequenceStores (..),
    newListSequenceStores,

    -- * Request Lenses
    listSequenceStores_filter,
    listSequenceStores_maxResults,
    listSequenceStores_nextToken,

    -- * Destructuring the Response
    ListSequenceStoresResponse (..),
    newListSequenceStoresResponse,

    -- * Response Lenses
    listSequenceStoresResponse_nextToken,
    listSequenceStoresResponse_httpStatus,
    listSequenceStoresResponse_sequenceStores,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSequenceStores' smart constructor.
data ListSequenceStores = ListSequenceStores'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe SequenceStoreFilter,
    -- | The maximum number of stores to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSequenceStores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listSequenceStores_filter' - A filter to apply to the list.
--
-- 'maxResults', 'listSequenceStores_maxResults' - The maximum number of stores to return in one page of results.
--
-- 'nextToken', 'listSequenceStores_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListSequenceStores ::
  ListSequenceStores
newListSequenceStores =
  ListSequenceStores'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A filter to apply to the list.
listSequenceStores_filter :: Lens.Lens' ListSequenceStores (Prelude.Maybe SequenceStoreFilter)
listSequenceStores_filter = Lens.lens (\ListSequenceStores' {filter'} -> filter') (\s@ListSequenceStores' {} a -> s {filter' = a} :: ListSequenceStores)

-- | The maximum number of stores to return in one page of results.
listSequenceStores_maxResults :: Lens.Lens' ListSequenceStores (Prelude.Maybe Prelude.Natural)
listSequenceStores_maxResults = Lens.lens (\ListSequenceStores' {maxResults} -> maxResults) (\s@ListSequenceStores' {} a -> s {maxResults = a} :: ListSequenceStores)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listSequenceStores_nextToken :: Lens.Lens' ListSequenceStores (Prelude.Maybe Prelude.Text)
listSequenceStores_nextToken = Lens.lens (\ListSequenceStores' {nextToken} -> nextToken) (\s@ListSequenceStores' {} a -> s {nextToken = a} :: ListSequenceStores)

instance Core.AWSPager ListSequenceStores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSequenceStoresResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listSequenceStoresResponse_sequenceStores
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSequenceStores_nextToken
          Lens..~ rs
          Lens.^? listSequenceStoresResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSequenceStores where
  type
    AWSResponse ListSequenceStores =
      ListSequenceStoresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSequenceStoresResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "sequenceStores"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListSequenceStores where
  hashWithSalt _salt ListSequenceStores' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSequenceStores where
  rnf ListSequenceStores' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSequenceStores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSequenceStores where
  toJSON ListSequenceStores' {..} =
    Data.object
      ( Prelude.catMaybes
          [("filter" Data..=) Prelude.<$> filter']
      )

instance Data.ToPath ListSequenceStores where
  toPath = Prelude.const "/sequencestores"

instance Data.ToQuery ListSequenceStores where
  toQuery ListSequenceStores' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSequenceStoresResponse' smart constructor.
data ListSequenceStoresResponse = ListSequenceStoresResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of sequence stores.
    sequenceStores :: [SequenceStoreDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSequenceStoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSequenceStoresResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listSequenceStoresResponse_httpStatus' - The response's http status code.
--
-- 'sequenceStores', 'listSequenceStoresResponse_sequenceStores' - A list of sequence stores.
newListSequenceStoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSequenceStoresResponse
newListSequenceStoresResponse pHttpStatus_ =
  ListSequenceStoresResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      sequenceStores = Prelude.mempty
    }

-- | A pagination token that\'s included if more results are available.
listSequenceStoresResponse_nextToken :: Lens.Lens' ListSequenceStoresResponse (Prelude.Maybe Prelude.Text)
listSequenceStoresResponse_nextToken = Lens.lens (\ListSequenceStoresResponse' {nextToken} -> nextToken) (\s@ListSequenceStoresResponse' {} a -> s {nextToken = a} :: ListSequenceStoresResponse)

-- | The response's http status code.
listSequenceStoresResponse_httpStatus :: Lens.Lens' ListSequenceStoresResponse Prelude.Int
listSequenceStoresResponse_httpStatus = Lens.lens (\ListSequenceStoresResponse' {httpStatus} -> httpStatus) (\s@ListSequenceStoresResponse' {} a -> s {httpStatus = a} :: ListSequenceStoresResponse)

-- | A list of sequence stores.
listSequenceStoresResponse_sequenceStores :: Lens.Lens' ListSequenceStoresResponse [SequenceStoreDetail]
listSequenceStoresResponse_sequenceStores = Lens.lens (\ListSequenceStoresResponse' {sequenceStores} -> sequenceStores) (\s@ListSequenceStoresResponse' {} a -> s {sequenceStores = a} :: ListSequenceStoresResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListSequenceStoresResponse where
  rnf ListSequenceStoresResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf sequenceStores
