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
-- Module      : Network.AWS.CodeBuild.ListBuildBatchesForProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of the build batches for a specific project.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildBatchesForProject
  ( -- * Creating a Request
    ListBuildBatchesForProject (..),
    newListBuildBatchesForProject,

    -- * Request Lenses
    listBuildBatchesForProject_sortOrder,
    listBuildBatchesForProject_nextToken,
    listBuildBatchesForProject_maxResults,
    listBuildBatchesForProject_projectName,
    listBuildBatchesForProject_filter,

    -- * Destructuring the Response
    ListBuildBatchesForProjectResponse (..),
    newListBuildBatchesForProjectResponse,

    -- * Response Lenses
    listBuildBatchesForProjectResponse_nextToken,
    listBuildBatchesForProjectResponse_ids,
    listBuildBatchesForProjectResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBuildBatchesForProject' smart constructor.
data ListBuildBatchesForProject = ListBuildBatchesForProject'
  { -- | Specifies the sort order of the returned items. Valid values include:
    --
    -- -   @ASCENDING@: List the batch build identifiers in ascending order by
    --     identifier.
    --
    -- -   @DESCENDING@: List the batch build identifiers in descending order
    --     by identifier.
    sortOrder :: Core.Maybe SortOrderType,
    -- | The @nextToken@ value returned from a previous call to
    -- @ListBuildBatchesForProject@. This specifies the next item to return. To
    -- return the beginning of the list, exclude this parameter.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the project.
    projectName :: Core.Maybe Core.Text,
    -- | A @BuildBatchFilter@ object that specifies the filters for the search.
    filter' :: Core.Maybe BuildBatchFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuildBatchesForProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listBuildBatchesForProject_sortOrder' - Specifies the sort order of the returned items. Valid values include:
--
-- -   @ASCENDING@: List the batch build identifiers in ascending order by
--     identifier.
--
-- -   @DESCENDING@: List the batch build identifiers in descending order
--     by identifier.
--
-- 'nextToken', 'listBuildBatchesForProject_nextToken' - The @nextToken@ value returned from a previous call to
-- @ListBuildBatchesForProject@. This specifies the next item to return. To
-- return the beginning of the list, exclude this parameter.
--
-- 'maxResults', 'listBuildBatchesForProject_maxResults' - The maximum number of results to return.
--
-- 'projectName', 'listBuildBatchesForProject_projectName' - The name of the project.
--
-- 'filter'', 'listBuildBatchesForProject_filter' - A @BuildBatchFilter@ object that specifies the filters for the search.
newListBuildBatchesForProject ::
  ListBuildBatchesForProject
newListBuildBatchesForProject =
  ListBuildBatchesForProject'
    { sortOrder =
        Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      projectName = Core.Nothing,
      filter' = Core.Nothing
    }

-- | Specifies the sort order of the returned items. Valid values include:
--
-- -   @ASCENDING@: List the batch build identifiers in ascending order by
--     identifier.
--
-- -   @DESCENDING@: List the batch build identifiers in descending order
--     by identifier.
listBuildBatchesForProject_sortOrder :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe SortOrderType)
listBuildBatchesForProject_sortOrder = Lens.lens (\ListBuildBatchesForProject' {sortOrder} -> sortOrder) (\s@ListBuildBatchesForProject' {} a -> s {sortOrder = a} :: ListBuildBatchesForProject)

-- | The @nextToken@ value returned from a previous call to
-- @ListBuildBatchesForProject@. This specifies the next item to return. To
-- return the beginning of the list, exclude this parameter.
listBuildBatchesForProject_nextToken :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe Core.Text)
listBuildBatchesForProject_nextToken = Lens.lens (\ListBuildBatchesForProject' {nextToken} -> nextToken) (\s@ListBuildBatchesForProject' {} a -> s {nextToken = a} :: ListBuildBatchesForProject)

-- | The maximum number of results to return.
listBuildBatchesForProject_maxResults :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe Core.Natural)
listBuildBatchesForProject_maxResults = Lens.lens (\ListBuildBatchesForProject' {maxResults} -> maxResults) (\s@ListBuildBatchesForProject' {} a -> s {maxResults = a} :: ListBuildBatchesForProject)

-- | The name of the project.
listBuildBatchesForProject_projectName :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe Core.Text)
listBuildBatchesForProject_projectName = Lens.lens (\ListBuildBatchesForProject' {projectName} -> projectName) (\s@ListBuildBatchesForProject' {} a -> s {projectName = a} :: ListBuildBatchesForProject)

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
listBuildBatchesForProject_filter :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe BuildBatchFilter)
listBuildBatchesForProject_filter = Lens.lens (\ListBuildBatchesForProject' {filter'} -> filter') (\s@ListBuildBatchesForProject' {} a -> s {filter' = a} :: ListBuildBatchesForProject)

instance Core.AWSPager ListBuildBatchesForProject where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildBatchesForProjectResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildBatchesForProjectResponse_ids
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBuildBatchesForProject_nextToken
          Lens..~ rs
          Lens.^? listBuildBatchesForProjectResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListBuildBatchesForProject where
  type
    AWSResponse ListBuildBatchesForProject =
      ListBuildBatchesForProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildBatchesForProjectResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "ids" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBuildBatchesForProject

instance Core.NFData ListBuildBatchesForProject

instance Core.ToHeaders ListBuildBatchesForProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListBuildBatchesForProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBuildBatchesForProject where
  toJSON ListBuildBatchesForProject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("projectName" Core..=) Core.<$> projectName,
            ("filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListBuildBatchesForProject where
  toPath = Core.const "/"

instance Core.ToQuery ListBuildBatchesForProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListBuildBatchesForProjectResponse' smart constructor.
data ListBuildBatchesForProjectResponse = ListBuildBatchesForProjectResponse'
  { -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to @ListBuildBatchesForProject@ to retrieve the
    -- next set of items.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of strings that contains the batch build identifiers.
    ids :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuildBatchesForProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBuildBatchesForProjectResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to @ListBuildBatchesForProject@ to retrieve the
-- next set of items.
--
-- 'ids', 'listBuildBatchesForProjectResponse_ids' - An array of strings that contains the batch build identifiers.
--
-- 'httpStatus', 'listBuildBatchesForProjectResponse_httpStatus' - The response's http status code.
newListBuildBatchesForProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBuildBatchesForProjectResponse
newListBuildBatchesForProjectResponse pHttpStatus_ =
  ListBuildBatchesForProjectResponse'
    { nextToken =
        Core.Nothing,
      ids = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to @ListBuildBatchesForProject@ to retrieve the
-- next set of items.
listBuildBatchesForProjectResponse_nextToken :: Lens.Lens' ListBuildBatchesForProjectResponse (Core.Maybe Core.Text)
listBuildBatchesForProjectResponse_nextToken = Lens.lens (\ListBuildBatchesForProjectResponse' {nextToken} -> nextToken) (\s@ListBuildBatchesForProjectResponse' {} a -> s {nextToken = a} :: ListBuildBatchesForProjectResponse)

-- | An array of strings that contains the batch build identifiers.
listBuildBatchesForProjectResponse_ids :: Lens.Lens' ListBuildBatchesForProjectResponse (Core.Maybe [Core.Text])
listBuildBatchesForProjectResponse_ids = Lens.lens (\ListBuildBatchesForProjectResponse' {ids} -> ids) (\s@ListBuildBatchesForProjectResponse' {} a -> s {ids = a} :: ListBuildBatchesForProjectResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBuildBatchesForProjectResponse_httpStatus :: Lens.Lens' ListBuildBatchesForProjectResponse Core.Int
listBuildBatchesForProjectResponse_httpStatus = Lens.lens (\ListBuildBatchesForProjectResponse' {httpStatus} -> httpStatus) (\s@ListBuildBatchesForProjectResponse' {} a -> s {httpStatus = a} :: ListBuildBatchesForProjectResponse)

instance
  Core.NFData
    ListBuildBatchesForProjectResponse
