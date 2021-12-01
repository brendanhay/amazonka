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
-- Module      : Amazonka.CodeBuild.ListBuildBatchesForProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of the build batches for a specific project.
--
-- This operation returns paginated results.
module Amazonka.CodeBuild.ListBuildBatchesForProject
  ( -- * Creating a Request
    ListBuildBatchesForProject (..),
    newListBuildBatchesForProject,

    -- * Request Lenses
    listBuildBatchesForProject_sortOrder,
    listBuildBatchesForProject_nextToken,
    listBuildBatchesForProject_projectName,
    listBuildBatchesForProject_filter,
    listBuildBatchesForProject_maxResults,

    -- * Destructuring the Response
    ListBuildBatchesForProjectResponse (..),
    newListBuildBatchesForProjectResponse,

    -- * Response Lenses
    listBuildBatchesForProjectResponse_ids,
    listBuildBatchesForProjectResponse_nextToken,
    listBuildBatchesForProjectResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBuildBatchesForProject' smart constructor.
data ListBuildBatchesForProject = ListBuildBatchesForProject'
  { -- | Specifies the sort order of the returned items. Valid values include:
    --
    -- -   @ASCENDING@: List the batch build identifiers in ascending order by
    --     identifier.
    --
    -- -   @DESCENDING@: List the batch build identifiers in descending order
    --     by identifier.
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | The @nextToken@ value returned from a previous call to
    -- @ListBuildBatchesForProject@. This specifies the next item to return. To
    -- return the beginning of the list, exclude this parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the project.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | A @BuildBatchFilter@ object that specifies the filters for the search.
    filter' :: Prelude.Maybe BuildBatchFilter,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'projectName', 'listBuildBatchesForProject_projectName' - The name of the project.
--
-- 'filter'', 'listBuildBatchesForProject_filter' - A @BuildBatchFilter@ object that specifies the filters for the search.
--
-- 'maxResults', 'listBuildBatchesForProject_maxResults' - The maximum number of results to return.
newListBuildBatchesForProject ::
  ListBuildBatchesForProject
newListBuildBatchesForProject =
  ListBuildBatchesForProject'
    { sortOrder =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      projectName = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Specifies the sort order of the returned items. Valid values include:
--
-- -   @ASCENDING@: List the batch build identifiers in ascending order by
--     identifier.
--
-- -   @DESCENDING@: List the batch build identifiers in descending order
--     by identifier.
listBuildBatchesForProject_sortOrder :: Lens.Lens' ListBuildBatchesForProject (Prelude.Maybe SortOrderType)
listBuildBatchesForProject_sortOrder = Lens.lens (\ListBuildBatchesForProject' {sortOrder} -> sortOrder) (\s@ListBuildBatchesForProject' {} a -> s {sortOrder = a} :: ListBuildBatchesForProject)

-- | The @nextToken@ value returned from a previous call to
-- @ListBuildBatchesForProject@. This specifies the next item to return. To
-- return the beginning of the list, exclude this parameter.
listBuildBatchesForProject_nextToken :: Lens.Lens' ListBuildBatchesForProject (Prelude.Maybe Prelude.Text)
listBuildBatchesForProject_nextToken = Lens.lens (\ListBuildBatchesForProject' {nextToken} -> nextToken) (\s@ListBuildBatchesForProject' {} a -> s {nextToken = a} :: ListBuildBatchesForProject)

-- | The name of the project.
listBuildBatchesForProject_projectName :: Lens.Lens' ListBuildBatchesForProject (Prelude.Maybe Prelude.Text)
listBuildBatchesForProject_projectName = Lens.lens (\ListBuildBatchesForProject' {projectName} -> projectName) (\s@ListBuildBatchesForProject' {} a -> s {projectName = a} :: ListBuildBatchesForProject)

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
listBuildBatchesForProject_filter :: Lens.Lens' ListBuildBatchesForProject (Prelude.Maybe BuildBatchFilter)
listBuildBatchesForProject_filter = Lens.lens (\ListBuildBatchesForProject' {filter'} -> filter') (\s@ListBuildBatchesForProject' {} a -> s {filter' = a} :: ListBuildBatchesForProject)

-- | The maximum number of results to return.
listBuildBatchesForProject_maxResults :: Lens.Lens' ListBuildBatchesForProject (Prelude.Maybe Prelude.Natural)
listBuildBatchesForProject_maxResults = Lens.lens (\ListBuildBatchesForProject' {maxResults} -> maxResults) (\s@ListBuildBatchesForProject' {} a -> s {maxResults = a} :: ListBuildBatchesForProject)

instance Core.AWSPager ListBuildBatchesForProject where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildBatchesForProjectResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildBatchesForProjectResponse_ids
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBuildBatchesForProject_nextToken
          Lens..~ rs
          Lens.^? listBuildBatchesForProjectResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBuildBatchesForProject where
  type
    AWSResponse ListBuildBatchesForProject =
      ListBuildBatchesForProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildBatchesForProjectResponse'
            Prelude.<$> (x Core..?> "ids" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBuildBatchesForProject where
  hashWithSalt salt' ListBuildBatchesForProject' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListBuildBatchesForProject where
  rnf ListBuildBatchesForProject' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ListBuildBatchesForProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListBuildBatchesForProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBuildBatchesForProject where
  toJSON ListBuildBatchesForProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sortOrder" Core..=) Prelude.<$> sortOrder,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("projectName" Core..=) Prelude.<$> projectName,
            ("filter" Core..=) Prelude.<$> filter',
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListBuildBatchesForProject where
  toPath = Prelude.const "/"

instance Core.ToQuery ListBuildBatchesForProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBuildBatchesForProjectResponse' smart constructor.
data ListBuildBatchesForProjectResponse = ListBuildBatchesForProjectResponse'
  { -- | An array of strings that contains the batch build identifiers.
    ids :: Prelude.Maybe [Prelude.Text],
    -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to @ListBuildBatchesForProject@ to retrieve the
    -- next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBuildBatchesForProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'listBuildBatchesForProjectResponse_ids' - An array of strings that contains the batch build identifiers.
--
-- 'nextToken', 'listBuildBatchesForProjectResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to @ListBuildBatchesForProject@ to retrieve the
-- next set of items.
--
-- 'httpStatus', 'listBuildBatchesForProjectResponse_httpStatus' - The response's http status code.
newListBuildBatchesForProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBuildBatchesForProjectResponse
newListBuildBatchesForProjectResponse pHttpStatus_ =
  ListBuildBatchesForProjectResponse'
    { ids =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of strings that contains the batch build identifiers.
listBuildBatchesForProjectResponse_ids :: Lens.Lens' ListBuildBatchesForProjectResponse (Prelude.Maybe [Prelude.Text])
listBuildBatchesForProjectResponse_ids = Lens.lens (\ListBuildBatchesForProjectResponse' {ids} -> ids) (\s@ListBuildBatchesForProjectResponse' {} a -> s {ids = a} :: ListBuildBatchesForProjectResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to @ListBuildBatchesForProject@ to retrieve the
-- next set of items.
listBuildBatchesForProjectResponse_nextToken :: Lens.Lens' ListBuildBatchesForProjectResponse (Prelude.Maybe Prelude.Text)
listBuildBatchesForProjectResponse_nextToken = Lens.lens (\ListBuildBatchesForProjectResponse' {nextToken} -> nextToken) (\s@ListBuildBatchesForProjectResponse' {} a -> s {nextToken = a} :: ListBuildBatchesForProjectResponse)

-- | The response's http status code.
listBuildBatchesForProjectResponse_httpStatus :: Lens.Lens' ListBuildBatchesForProjectResponse Prelude.Int
listBuildBatchesForProjectResponse_httpStatus = Lens.lens (\ListBuildBatchesForProjectResponse' {httpStatus} -> httpStatus) (\s@ListBuildBatchesForProjectResponse' {} a -> s {httpStatus = a} :: ListBuildBatchesForProjectResponse)

instance
  Prelude.NFData
    ListBuildBatchesForProjectResponse
  where
  rnf ListBuildBatchesForProjectResponse' {..} =
    Prelude.rnf ids
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
