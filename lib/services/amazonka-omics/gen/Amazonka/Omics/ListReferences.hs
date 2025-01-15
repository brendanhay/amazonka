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
-- Module      : Amazonka.Omics.ListReferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of references.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListReferences
  ( -- * Creating a Request
    ListReferences (..),
    newListReferences,

    -- * Request Lenses
    listReferences_filter,
    listReferences_maxResults,
    listReferences_nextToken,
    listReferences_referenceStoreId,

    -- * Destructuring the Response
    ListReferencesResponse (..),
    newListReferencesResponse,

    -- * Response Lenses
    listReferencesResponse_nextToken,
    listReferencesResponse_httpStatus,
    listReferencesResponse_references,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReferences' smart constructor.
data ListReferences = ListReferences'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ReferenceFilter,
    -- | The maximum number of references to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The references\' reference store ID.
    referenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReferences_filter' - A filter to apply to the list.
--
-- 'maxResults', 'listReferences_maxResults' - The maximum number of references to return in one page of results.
--
-- 'nextToken', 'listReferences_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'referenceStoreId', 'listReferences_referenceStoreId' - The references\' reference store ID.
newListReferences ::
  -- | 'referenceStoreId'
  Prelude.Text ->
  ListReferences
newListReferences pReferenceStoreId_ =
  ListReferences'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      referenceStoreId = pReferenceStoreId_
    }

-- | A filter to apply to the list.
listReferences_filter :: Lens.Lens' ListReferences (Prelude.Maybe ReferenceFilter)
listReferences_filter = Lens.lens (\ListReferences' {filter'} -> filter') (\s@ListReferences' {} a -> s {filter' = a} :: ListReferences)

-- | The maximum number of references to return in one page of results.
listReferences_maxResults :: Lens.Lens' ListReferences (Prelude.Maybe Prelude.Natural)
listReferences_maxResults = Lens.lens (\ListReferences' {maxResults} -> maxResults) (\s@ListReferences' {} a -> s {maxResults = a} :: ListReferences)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listReferences_nextToken :: Lens.Lens' ListReferences (Prelude.Maybe Prelude.Text)
listReferences_nextToken = Lens.lens (\ListReferences' {nextToken} -> nextToken) (\s@ListReferences' {} a -> s {nextToken = a} :: ListReferences)

-- | The references\' reference store ID.
listReferences_referenceStoreId :: Lens.Lens' ListReferences Prelude.Text
listReferences_referenceStoreId = Lens.lens (\ListReferences' {referenceStoreId} -> referenceStoreId) (\s@ListReferences' {} a -> s {referenceStoreId = a} :: ListReferences)

instance Core.AWSPager ListReferences where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReferencesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listReferencesResponse_references) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listReferences_nextToken
              Lens..~ rs
              Lens.^? listReferencesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListReferences where
  type
    AWSResponse ListReferences =
      ListReferencesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReferencesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "references" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListReferences where
  hashWithSalt _salt ListReferences' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` referenceStoreId

instance Prelude.NFData ListReferences where
  rnf ListReferences' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf referenceStoreId

instance Data.ToHeaders ListReferences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReferences where
  toJSON ListReferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [("filter" Data..=) Prelude.<$> filter']
      )

instance Data.ToPath ListReferences where
  toPath ListReferences' {..} =
    Prelude.mconcat
      [ "/referencestore/",
        Data.toBS referenceStoreId,
        "/references"
      ]

instance Data.ToQuery ListReferences where
  toQuery ListReferences' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReferencesResponse' smart constructor.
data ListReferencesResponse = ListReferencesResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of references.
    references :: [ReferenceListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReferencesResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listReferencesResponse_httpStatus' - The response's http status code.
--
-- 'references', 'listReferencesResponse_references' - A list of references.
newListReferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReferencesResponse
newListReferencesResponse pHttpStatus_ =
  ListReferencesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      references = Prelude.mempty
    }

-- | A pagination token that\'s included if more results are available.
listReferencesResponse_nextToken :: Lens.Lens' ListReferencesResponse (Prelude.Maybe Prelude.Text)
listReferencesResponse_nextToken = Lens.lens (\ListReferencesResponse' {nextToken} -> nextToken) (\s@ListReferencesResponse' {} a -> s {nextToken = a} :: ListReferencesResponse)

-- | The response's http status code.
listReferencesResponse_httpStatus :: Lens.Lens' ListReferencesResponse Prelude.Int
listReferencesResponse_httpStatus = Lens.lens (\ListReferencesResponse' {httpStatus} -> httpStatus) (\s@ListReferencesResponse' {} a -> s {httpStatus = a} :: ListReferencesResponse)

-- | A list of references.
listReferencesResponse_references :: Lens.Lens' ListReferencesResponse [ReferenceListItem]
listReferencesResponse_references = Lens.lens (\ListReferencesResponse' {references} -> references) (\s@ListReferencesResponse' {} a -> s {references = a} :: ListReferencesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListReferencesResponse where
  rnf ListReferencesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf references
