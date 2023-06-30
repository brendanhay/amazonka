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
-- Module      : Amazonka.Omics.ListReferenceStores
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of reference stores.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListReferenceStores
  ( -- * Creating a Request
    ListReferenceStores (..),
    newListReferenceStores,

    -- * Request Lenses
    listReferenceStores_filter,
    listReferenceStores_maxResults,
    listReferenceStores_nextToken,

    -- * Destructuring the Response
    ListReferenceStoresResponse (..),
    newListReferenceStoresResponse,

    -- * Response Lenses
    listReferenceStoresResponse_nextToken,
    listReferenceStoresResponse_httpStatus,
    listReferenceStoresResponse_referenceStores,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReferenceStores' smart constructor.
data ListReferenceStores = ListReferenceStores'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ReferenceStoreFilter,
    -- | The maximum number of stores to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReferenceStores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReferenceStores_filter' - A filter to apply to the list.
--
-- 'maxResults', 'listReferenceStores_maxResults' - The maximum number of stores to return in one page of results.
--
-- 'nextToken', 'listReferenceStores_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListReferenceStores ::
  ListReferenceStores
newListReferenceStores =
  ListReferenceStores'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A filter to apply to the list.
listReferenceStores_filter :: Lens.Lens' ListReferenceStores (Prelude.Maybe ReferenceStoreFilter)
listReferenceStores_filter = Lens.lens (\ListReferenceStores' {filter'} -> filter') (\s@ListReferenceStores' {} a -> s {filter' = a} :: ListReferenceStores)

-- | The maximum number of stores to return in one page of results.
listReferenceStores_maxResults :: Lens.Lens' ListReferenceStores (Prelude.Maybe Prelude.Natural)
listReferenceStores_maxResults = Lens.lens (\ListReferenceStores' {maxResults} -> maxResults) (\s@ListReferenceStores' {} a -> s {maxResults = a} :: ListReferenceStores)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listReferenceStores_nextToken :: Lens.Lens' ListReferenceStores (Prelude.Maybe Prelude.Text)
listReferenceStores_nextToken = Lens.lens (\ListReferenceStores' {nextToken} -> nextToken) (\s@ListReferenceStores' {} a -> s {nextToken = a} :: ListReferenceStores)

instance Core.AWSPager ListReferenceStores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReferenceStoresResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listReferenceStoresResponse_referenceStores
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listReferenceStores_nextToken
          Lens..~ rs
          Lens.^? listReferenceStoresResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListReferenceStores where
  type
    AWSResponse ListReferenceStores =
      ListReferenceStoresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReferenceStoresResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "referenceStores"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListReferenceStores where
  hashWithSalt _salt ListReferenceStores' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListReferenceStores where
  rnf ListReferenceStores' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListReferenceStores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReferenceStores where
  toJSON ListReferenceStores' {..} =
    Data.object
      ( Prelude.catMaybes
          [("filter" Data..=) Prelude.<$> filter']
      )

instance Data.ToPath ListReferenceStores where
  toPath = Prelude.const "/referencestores"

instance Data.ToQuery ListReferenceStores where
  toQuery ListReferenceStores' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReferenceStoresResponse' smart constructor.
data ListReferenceStoresResponse = ListReferenceStoresResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of reference stores.
    referenceStores :: [ReferenceStoreDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReferenceStoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReferenceStoresResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listReferenceStoresResponse_httpStatus' - The response's http status code.
--
-- 'referenceStores', 'listReferenceStoresResponse_referenceStores' - A list of reference stores.
newListReferenceStoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReferenceStoresResponse
newListReferenceStoresResponse pHttpStatus_ =
  ListReferenceStoresResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      referenceStores = Prelude.mempty
    }

-- | A pagination token that\'s included if more results are available.
listReferenceStoresResponse_nextToken :: Lens.Lens' ListReferenceStoresResponse (Prelude.Maybe Prelude.Text)
listReferenceStoresResponse_nextToken = Lens.lens (\ListReferenceStoresResponse' {nextToken} -> nextToken) (\s@ListReferenceStoresResponse' {} a -> s {nextToken = a} :: ListReferenceStoresResponse)

-- | The response's http status code.
listReferenceStoresResponse_httpStatus :: Lens.Lens' ListReferenceStoresResponse Prelude.Int
listReferenceStoresResponse_httpStatus = Lens.lens (\ListReferenceStoresResponse' {httpStatus} -> httpStatus) (\s@ListReferenceStoresResponse' {} a -> s {httpStatus = a} :: ListReferenceStoresResponse)

-- | A list of reference stores.
listReferenceStoresResponse_referenceStores :: Lens.Lens' ListReferenceStoresResponse [ReferenceStoreDetail]
listReferenceStoresResponse_referenceStores = Lens.lens (\ListReferenceStoresResponse' {referenceStores} -> referenceStores) (\s@ListReferenceStoresResponse' {} a -> s {referenceStores = a} :: ListReferenceStoresResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListReferenceStoresResponse where
  rnf ListReferenceStoresResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf referenceStores
