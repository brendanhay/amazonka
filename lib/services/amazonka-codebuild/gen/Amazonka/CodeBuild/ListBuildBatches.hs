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
-- Module      : Amazonka.CodeBuild.ListBuildBatches
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of your build batches in the current region.
--
-- This operation returns paginated results.
module Amazonka.CodeBuild.ListBuildBatches
  ( -- * Creating a Request
    ListBuildBatches (..),
    newListBuildBatches,

    -- * Request Lenses
    listBuildBatches_filter,
    listBuildBatches_maxResults,
    listBuildBatches_nextToken,
    listBuildBatches_sortOrder,

    -- * Destructuring the Response
    ListBuildBatchesResponse (..),
    newListBuildBatchesResponse,

    -- * Response Lenses
    listBuildBatchesResponse_ids,
    listBuildBatchesResponse_nextToken,
    listBuildBatchesResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBuildBatches' smart constructor.
data ListBuildBatches = ListBuildBatches'
  { -- | A @BuildBatchFilter@ object that specifies the filters for the search.
    filter' :: Prelude.Maybe BuildBatchFilter,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ value returned from a previous call to
    -- @ListBuildBatches@. This specifies the next item to return. To return
    -- the beginning of the list, exclude this parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the sort order of the returned items. Valid values include:
    --
    -- -   @ASCENDING@: List the batch build identifiers in ascending order by
    --     identifier.
    --
    -- -   @DESCENDING@: List the batch build identifiers in descending order
    --     by identifier.
    sortOrder :: Prelude.Maybe SortOrderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuildBatches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listBuildBatches_filter' - A @BuildBatchFilter@ object that specifies the filters for the search.
--
-- 'maxResults', 'listBuildBatches_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listBuildBatches_nextToken' - The @nextToken@ value returned from a previous call to
-- @ListBuildBatches@. This specifies the next item to return. To return
-- the beginning of the list, exclude this parameter.
--
-- 'sortOrder', 'listBuildBatches_sortOrder' - Specifies the sort order of the returned items. Valid values include:
--
-- -   @ASCENDING@: List the batch build identifiers in ascending order by
--     identifier.
--
-- -   @DESCENDING@: List the batch build identifiers in descending order
--     by identifier.
newListBuildBatches ::
  ListBuildBatches
newListBuildBatches =
  ListBuildBatches'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
listBuildBatches_filter :: Lens.Lens' ListBuildBatches (Prelude.Maybe BuildBatchFilter)
listBuildBatches_filter = Lens.lens (\ListBuildBatches' {filter'} -> filter') (\s@ListBuildBatches' {} a -> s {filter' = a} :: ListBuildBatches)

-- | The maximum number of results to return.
listBuildBatches_maxResults :: Lens.Lens' ListBuildBatches (Prelude.Maybe Prelude.Natural)
listBuildBatches_maxResults = Lens.lens (\ListBuildBatches' {maxResults} -> maxResults) (\s@ListBuildBatches' {} a -> s {maxResults = a} :: ListBuildBatches)

-- | The @nextToken@ value returned from a previous call to
-- @ListBuildBatches@. This specifies the next item to return. To return
-- the beginning of the list, exclude this parameter.
listBuildBatches_nextToken :: Lens.Lens' ListBuildBatches (Prelude.Maybe Prelude.Text)
listBuildBatches_nextToken = Lens.lens (\ListBuildBatches' {nextToken} -> nextToken) (\s@ListBuildBatches' {} a -> s {nextToken = a} :: ListBuildBatches)

-- | Specifies the sort order of the returned items. Valid values include:
--
-- -   @ASCENDING@: List the batch build identifiers in ascending order by
--     identifier.
--
-- -   @DESCENDING@: List the batch build identifiers in descending order
--     by identifier.
listBuildBatches_sortOrder :: Lens.Lens' ListBuildBatches (Prelude.Maybe SortOrderType)
listBuildBatches_sortOrder = Lens.lens (\ListBuildBatches' {sortOrder} -> sortOrder) (\s@ListBuildBatches' {} a -> s {sortOrder = a} :: ListBuildBatches)

instance Core.AWSPager ListBuildBatches where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildBatchesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildBatchesResponse_ids Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBuildBatches_nextToken
          Lens..~ rs
          Lens.^? listBuildBatchesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBuildBatches where
  type
    AWSResponse ListBuildBatches =
      ListBuildBatchesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildBatchesResponse'
            Prelude.<$> (x Data..?> "ids" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBuildBatches where
  hashWithSalt _salt ListBuildBatches' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListBuildBatches where
  rnf ListBuildBatches' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListBuildBatches where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.ListBuildBatches" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBuildBatches where
  toJSON ListBuildBatches' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListBuildBatches where
  toPath = Prelude.const "/"

instance Data.ToQuery ListBuildBatches where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBuildBatchesResponse' smart constructor.
data ListBuildBatchesResponse = ListBuildBatchesResponse'
  { -- | An array of strings that contains the batch build identifiers.
    ids :: Prelude.Maybe [Prelude.Text],
    -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to @ListBuildBatches@ to retrieve the next set of
    -- items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuildBatchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'listBuildBatchesResponse_ids' - An array of strings that contains the batch build identifiers.
--
-- 'nextToken', 'listBuildBatchesResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to @ListBuildBatches@ to retrieve the next set of
-- items.
--
-- 'httpStatus', 'listBuildBatchesResponse_httpStatus' - The response's http status code.
newListBuildBatchesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBuildBatchesResponse
newListBuildBatchesResponse pHttpStatus_ =
  ListBuildBatchesResponse'
    { ids = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of strings that contains the batch build identifiers.
listBuildBatchesResponse_ids :: Lens.Lens' ListBuildBatchesResponse (Prelude.Maybe [Prelude.Text])
listBuildBatchesResponse_ids = Lens.lens (\ListBuildBatchesResponse' {ids} -> ids) (\s@ListBuildBatchesResponse' {} a -> s {ids = a} :: ListBuildBatchesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to @ListBuildBatches@ to retrieve the next set of
-- items.
listBuildBatchesResponse_nextToken :: Lens.Lens' ListBuildBatchesResponse (Prelude.Maybe Prelude.Text)
listBuildBatchesResponse_nextToken = Lens.lens (\ListBuildBatchesResponse' {nextToken} -> nextToken) (\s@ListBuildBatchesResponse' {} a -> s {nextToken = a} :: ListBuildBatchesResponse)

-- | The response's http status code.
listBuildBatchesResponse_httpStatus :: Lens.Lens' ListBuildBatchesResponse Prelude.Int
listBuildBatchesResponse_httpStatus = Lens.lens (\ListBuildBatchesResponse' {httpStatus} -> httpStatus) (\s@ListBuildBatchesResponse' {} a -> s {httpStatus = a} :: ListBuildBatchesResponse)

instance Prelude.NFData ListBuildBatchesResponse where
  rnf ListBuildBatchesResponse' {..} =
    Prelude.rnf ids
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
