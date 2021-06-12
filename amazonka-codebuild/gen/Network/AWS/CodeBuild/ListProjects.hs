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
-- Module      : Network.AWS.CodeBuild.ListProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build project names, with each build project name
-- representing a single build project.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListProjects
  ( -- * Creating a Request
    ListProjects (..),
    newListProjects,

    -- * Request Lenses
    listProjects_sortOrder,
    listProjects_nextToken,
    listProjects_sortBy,

    -- * Destructuring the Response
    ListProjectsResponse (..),
    newListProjectsResponse,

    -- * Response Lenses
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProjects' smart constructor.
data ListProjects = ListProjects'
  { -- | The order in which to list build projects. Valid values include:
    --
    -- -   @ASCENDING@: List in ascending order.
    --
    -- -   @DESCENDING@: List in descending order.
    --
    -- Use @sortBy@ to specify the criterion to be used to list build project
    -- names.
    sortOrder :: Core.Maybe SortOrderType,
    -- | During a previous call, if there are more than 100 items in the list,
    -- only the first 100 items are returned, along with a unique string called
    -- a /nextToken/. To get the next batch of items in the list, call this
    -- operation again, adding the next token to the call. To get all of the
    -- items in the list, keep calling this operation with each subsequent next
    -- token that is returned, until no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The criterion to be used to list build project names. Valid values
    -- include:
    --
    -- -   @CREATED_TIME@: List based on when each build project was created.
    --
    -- -   @LAST_MODIFIED_TIME@: List based on when information about each
    --     build project was last changed.
    --
    -- -   @NAME@: List based on each build project\'s name.
    --
    -- Use @sortOrder@ to specify in what order to list the build project names
    -- based on the preceding criteria.
    sortBy :: Core.Maybe ProjectSortByType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listProjects_sortOrder' - The order in which to list build projects. Valid values include:
--
-- -   @ASCENDING@: List in ascending order.
--
-- -   @DESCENDING@: List in descending order.
--
-- Use @sortBy@ to specify the criterion to be used to list build project
-- names.
--
-- 'nextToken', 'listProjects_nextToken' - During a previous call, if there are more than 100 items in the list,
-- only the first 100 items are returned, along with a unique string called
-- a /nextToken/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
--
-- 'sortBy', 'listProjects_sortBy' - The criterion to be used to list build project names. Valid values
-- include:
--
-- -   @CREATED_TIME@: List based on when each build project was created.
--
-- -   @LAST_MODIFIED_TIME@: List based on when information about each
--     build project was last changed.
--
-- -   @NAME@: List based on each build project\'s name.
--
-- Use @sortOrder@ to specify in what order to list the build project names
-- based on the preceding criteria.
newListProjects ::
  ListProjects
newListProjects =
  ListProjects'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | The order in which to list build projects. Valid values include:
--
-- -   @ASCENDING@: List in ascending order.
--
-- -   @DESCENDING@: List in descending order.
--
-- Use @sortBy@ to specify the criterion to be used to list build project
-- names.
listProjects_sortOrder :: Lens.Lens' ListProjects (Core.Maybe SortOrderType)
listProjects_sortOrder = Lens.lens (\ListProjects' {sortOrder} -> sortOrder) (\s@ListProjects' {} a -> s {sortOrder = a} :: ListProjects)

-- | During a previous call, if there are more than 100 items in the list,
-- only the first 100 items are returned, along with a unique string called
-- a /nextToken/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
listProjects_nextToken :: Lens.Lens' ListProjects (Core.Maybe Core.Text)
listProjects_nextToken = Lens.lens (\ListProjects' {nextToken} -> nextToken) (\s@ListProjects' {} a -> s {nextToken = a} :: ListProjects)

-- | The criterion to be used to list build project names. Valid values
-- include:
--
-- -   @CREATED_TIME@: List based on when each build project was created.
--
-- -   @LAST_MODIFIED_TIME@: List based on when information about each
--     build project was last changed.
--
-- -   @NAME@: List based on each build project\'s name.
--
-- Use @sortOrder@ to specify in what order to list the build project names
-- based on the preceding criteria.
listProjects_sortBy :: Lens.Lens' ListProjects (Core.Maybe ProjectSortByType)
listProjects_sortBy = Lens.lens (\ListProjects' {sortBy} -> sortBy) (\s@ListProjects' {} a -> s {sortBy = a} :: ListProjects)

instance Core.AWSPager ListProjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_projects Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProjects_nextToken
          Lens..~ rs
          Lens.^? listProjectsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListProjects where
  type AWSResponse ListProjects = ListProjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "projects")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListProjects

instance Core.NFData ListProjects

instance Core.ToHeaders ListProjects where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListProjects" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListProjects where
  toJSON ListProjects' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("sortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.ToPath ListProjects where
  toPath = Core.const "/"

instance Core.ToQuery ListProjects where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { -- | If there are more than 100 items in the list, only the first 100 items
    -- are returned, along with a unique string called a /nextToken/. To get
    -- the next batch of items in the list, call this operation again, adding
    -- the next token to the call.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of build project names, with each build project name
    -- representing a single build project.
    projects :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjectsResponse_nextToken' - If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
--
-- 'projects', 'listProjectsResponse_projects' - The list of build project names, with each build project name
-- representing a single build project.
--
-- 'httpStatus', 'listProjectsResponse_httpStatus' - The response's http status code.
newListProjectsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListProjectsResponse
newListProjectsResponse pHttpStatus_ =
  ListProjectsResponse'
    { nextToken = Core.Nothing,
      projects = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
listProjectsResponse_nextToken :: Lens.Lens' ListProjectsResponse (Core.Maybe Core.Text)
listProjectsResponse_nextToken = Lens.lens (\ListProjectsResponse' {nextToken} -> nextToken) (\s@ListProjectsResponse' {} a -> s {nextToken = a} :: ListProjectsResponse)

-- | The list of build project names, with each build project name
-- representing a single build project.
listProjectsResponse_projects :: Lens.Lens' ListProjectsResponse (Core.Maybe (Core.NonEmpty Core.Text))
listProjectsResponse_projects = Lens.lens (\ListProjectsResponse' {projects} -> projects) (\s@ListProjectsResponse' {} a -> s {projects = a} :: ListProjectsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProjectsResponse_httpStatus :: Lens.Lens' ListProjectsResponse Core.Int
listProjectsResponse_httpStatus = Lens.lens (\ListProjectsResponse' {httpStatus} -> httpStatus) (\s@ListProjectsResponse' {} a -> s {httpStatus = a} :: ListProjectsResponse)

instance Core.NFData ListProjectsResponse
