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
-- Module      : Network.AWS.CodeBuild.ListBuildBatches
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of your build batches in the current region.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildBatches
  ( -- * Creating a Request
    ListBuildBatches (..),
    newListBuildBatches,

    -- * Request Lenses
    listBuildBatches_sortOrder,
    listBuildBatches_nextToken,
    listBuildBatches_maxResults,
    listBuildBatches_filter,

    -- * Destructuring the Response
    ListBuildBatchesResponse (..),
    newListBuildBatchesResponse,

    -- * Response Lenses
    listBuildBatchesResponse_nextToken,
    listBuildBatchesResponse_ids,
    listBuildBatchesResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBuildBatches' smart constructor.
data ListBuildBatches = ListBuildBatches'
  { -- | Specifies the sort order of the returned items. Valid values include:
    --
    -- -   @ASCENDING@: List the batch build identifiers in ascending order by
    --     identifier.
    --
    -- -   @DESCENDING@: List the batch build identifiers in descending order
    --     by identifier.
    sortOrder :: Core.Maybe SortOrderType,
    -- | The @nextToken@ value returned from a previous call to
    -- @ListBuildBatches@. This specifies the next item to return. To return
    -- the beginning of the list, exclude this parameter.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A @BuildBatchFilter@ object that specifies the filters for the search.
    filter' :: Core.Maybe BuildBatchFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuildBatches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listBuildBatches_sortOrder' - Specifies the sort order of the returned items. Valid values include:
--
-- -   @ASCENDING@: List the batch build identifiers in ascending order by
--     identifier.
--
-- -   @DESCENDING@: List the batch build identifiers in descending order
--     by identifier.
--
-- 'nextToken', 'listBuildBatches_nextToken' - The @nextToken@ value returned from a previous call to
-- @ListBuildBatches@. This specifies the next item to return. To return
-- the beginning of the list, exclude this parameter.
--
-- 'maxResults', 'listBuildBatches_maxResults' - The maximum number of results to return.
--
-- 'filter'', 'listBuildBatches_filter' - A @BuildBatchFilter@ object that specifies the filters for the search.
newListBuildBatches ::
  ListBuildBatches
newListBuildBatches =
  ListBuildBatches'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | Specifies the sort order of the returned items. Valid values include:
--
-- -   @ASCENDING@: List the batch build identifiers in ascending order by
--     identifier.
--
-- -   @DESCENDING@: List the batch build identifiers in descending order
--     by identifier.
listBuildBatches_sortOrder :: Lens.Lens' ListBuildBatches (Core.Maybe SortOrderType)
listBuildBatches_sortOrder = Lens.lens (\ListBuildBatches' {sortOrder} -> sortOrder) (\s@ListBuildBatches' {} a -> s {sortOrder = a} :: ListBuildBatches)

-- | The @nextToken@ value returned from a previous call to
-- @ListBuildBatches@. This specifies the next item to return. To return
-- the beginning of the list, exclude this parameter.
listBuildBatches_nextToken :: Lens.Lens' ListBuildBatches (Core.Maybe Core.Text)
listBuildBatches_nextToken = Lens.lens (\ListBuildBatches' {nextToken} -> nextToken) (\s@ListBuildBatches' {} a -> s {nextToken = a} :: ListBuildBatches)

-- | The maximum number of results to return.
listBuildBatches_maxResults :: Lens.Lens' ListBuildBatches (Core.Maybe Core.Natural)
listBuildBatches_maxResults = Lens.lens (\ListBuildBatches' {maxResults} -> maxResults) (\s@ListBuildBatches' {} a -> s {maxResults = a} :: ListBuildBatches)

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
listBuildBatches_filter :: Lens.Lens' ListBuildBatches (Core.Maybe BuildBatchFilter)
listBuildBatches_filter = Lens.lens (\ListBuildBatches' {filter'} -> filter') (\s@ListBuildBatches' {} a -> s {filter' = a} :: ListBuildBatches)

instance Core.AWSPager ListBuildBatches where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildBatchesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildBatchesResponse_ids Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBuildBatches_nextToken
          Lens..~ rs
          Lens.^? listBuildBatchesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListBuildBatches where
  type
    AWSResponse ListBuildBatches =
      ListBuildBatchesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildBatchesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "ids" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBuildBatches

instance Core.NFData ListBuildBatches

instance Core.ToHeaders ListBuildBatches where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListBuildBatches" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBuildBatches where
  toJSON ListBuildBatches' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListBuildBatches where
  toPath = Core.const "/"

instance Core.ToQuery ListBuildBatches where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListBuildBatchesResponse' smart constructor.
data ListBuildBatchesResponse = ListBuildBatchesResponse'
  { -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to @ListBuildBatches@ to retrieve the next set of
    -- items.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of strings that contains the batch build identifiers.
    ids :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuildBatchesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBuildBatchesResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to @ListBuildBatches@ to retrieve the next set of
-- items.
--
-- 'ids', 'listBuildBatchesResponse_ids' - An array of strings that contains the batch build identifiers.
--
-- 'httpStatus', 'listBuildBatchesResponse_httpStatus' - The response's http status code.
newListBuildBatchesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBuildBatchesResponse
newListBuildBatchesResponse pHttpStatus_ =
  ListBuildBatchesResponse'
    { nextToken = Core.Nothing,
      ids = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to @ListBuildBatches@ to retrieve the next set of
-- items.
listBuildBatchesResponse_nextToken :: Lens.Lens' ListBuildBatchesResponse (Core.Maybe Core.Text)
listBuildBatchesResponse_nextToken = Lens.lens (\ListBuildBatchesResponse' {nextToken} -> nextToken) (\s@ListBuildBatchesResponse' {} a -> s {nextToken = a} :: ListBuildBatchesResponse)

-- | An array of strings that contains the batch build identifiers.
listBuildBatchesResponse_ids :: Lens.Lens' ListBuildBatchesResponse (Core.Maybe [Core.Text])
listBuildBatchesResponse_ids = Lens.lens (\ListBuildBatchesResponse' {ids} -> ids) (\s@ListBuildBatchesResponse' {} a -> s {ids = a} :: ListBuildBatchesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBuildBatchesResponse_httpStatus :: Lens.Lens' ListBuildBatchesResponse Core.Int
listBuildBatchesResponse_httpStatus = Lens.lens (\ListBuildBatchesResponse' {httpStatus} -> httpStatus) (\s@ListBuildBatchesResponse' {} a -> s {httpStatus = a} :: ListBuildBatchesResponse)

instance Core.NFData ListBuildBatchesResponse
