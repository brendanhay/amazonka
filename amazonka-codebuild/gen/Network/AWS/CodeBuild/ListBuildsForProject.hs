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
-- Module      : Network.AWS.CodeBuild.ListBuildsForProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of build identifiers for the specified build project, with
-- each build identifier representing a single build.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildsForProject
  ( -- * Creating a Request
    ListBuildsForProject (..),
    newListBuildsForProject,

    -- * Request Lenses
    listBuildsForProject_sortOrder,
    listBuildsForProject_nextToken,
    listBuildsForProject_projectName,

    -- * Destructuring the Response
    ListBuildsForProjectResponse (..),
    newListBuildsForProjectResponse,

    -- * Response Lenses
    listBuildsForProjectResponse_nextToken,
    listBuildsForProjectResponse_ids,
    listBuildsForProjectResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBuildsForProject' smart constructor.
data ListBuildsForProject = ListBuildsForProject'
  { -- | The order to list results in. The results are sorted by build number,
    -- not the build identifier.
    --
    -- Valid values include:
    --
    -- -   @ASCENDING@: List the build IDs in ascending order by build ID.
    --
    -- -   @DESCENDING@: List the build IDs in descending order by build ID.
    --
    -- If the project has more than 100 builds, setting the sort order will
    -- result in an error.
    sortOrder :: Core.Maybe SortOrderType,
    -- | During a previous call, if there are more than 100 items in the list,
    -- only the first 100 items are returned, along with a unique string called
    -- a /nextToken/. To get the next batch of items in the list, call this
    -- operation again, adding the next token to the call. To get all of the
    -- items in the list, keep calling this operation with each subsequent next
    -- token that is returned, until no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the AWS CodeBuild project.
    projectName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuildsForProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listBuildsForProject_sortOrder' - The order to list results in. The results are sorted by build number,
-- not the build identifier.
--
-- Valid values include:
--
-- -   @ASCENDING@: List the build IDs in ascending order by build ID.
--
-- -   @DESCENDING@: List the build IDs in descending order by build ID.
--
-- If the project has more than 100 builds, setting the sort order will
-- result in an error.
--
-- 'nextToken', 'listBuildsForProject_nextToken' - During a previous call, if there are more than 100 items in the list,
-- only the first 100 items are returned, along with a unique string called
-- a /nextToken/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
--
-- 'projectName', 'listBuildsForProject_projectName' - The name of the AWS CodeBuild project.
newListBuildsForProject ::
  -- | 'projectName'
  Core.Text ->
  ListBuildsForProject
newListBuildsForProject pProjectName_ =
  ListBuildsForProject'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      projectName = pProjectName_
    }

-- | The order to list results in. The results are sorted by build number,
-- not the build identifier.
--
-- Valid values include:
--
-- -   @ASCENDING@: List the build IDs in ascending order by build ID.
--
-- -   @DESCENDING@: List the build IDs in descending order by build ID.
--
-- If the project has more than 100 builds, setting the sort order will
-- result in an error.
listBuildsForProject_sortOrder :: Lens.Lens' ListBuildsForProject (Core.Maybe SortOrderType)
listBuildsForProject_sortOrder = Lens.lens (\ListBuildsForProject' {sortOrder} -> sortOrder) (\s@ListBuildsForProject' {} a -> s {sortOrder = a} :: ListBuildsForProject)

-- | During a previous call, if there are more than 100 items in the list,
-- only the first 100 items are returned, along with a unique string called
-- a /nextToken/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
listBuildsForProject_nextToken :: Lens.Lens' ListBuildsForProject (Core.Maybe Core.Text)
listBuildsForProject_nextToken = Lens.lens (\ListBuildsForProject' {nextToken} -> nextToken) (\s@ListBuildsForProject' {} a -> s {nextToken = a} :: ListBuildsForProject)

-- | The name of the AWS CodeBuild project.
listBuildsForProject_projectName :: Lens.Lens' ListBuildsForProject Core.Text
listBuildsForProject_projectName = Lens.lens (\ListBuildsForProject' {projectName} -> projectName) (\s@ListBuildsForProject' {} a -> s {projectName = a} :: ListBuildsForProject)

instance Core.AWSPager ListBuildsForProject where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBuildsForProjectResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBuildsForProjectResponse_ids Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBuildsForProject_nextToken
          Lens..~ rs
          Lens.^? listBuildsForProjectResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListBuildsForProject where
  type
    AWSResponse ListBuildsForProject =
      ListBuildsForProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildsForProjectResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "ids")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBuildsForProject

instance Core.NFData ListBuildsForProject

instance Core.ToHeaders ListBuildsForProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListBuildsForProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBuildsForProject where
  toJSON ListBuildsForProject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just ("projectName" Core..= projectName)
          ]
      )

instance Core.ToPath ListBuildsForProject where
  toPath = Core.const "/"

instance Core.ToQuery ListBuildsForProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListBuildsForProjectResponse' smart constructor.
data ListBuildsForProjectResponse = ListBuildsForProjectResponse'
  { -- | If there are more than 100 items in the list, only the first 100 items
    -- are returned, along with a unique string called a /nextToken/. To get
    -- the next batch of items in the list, call this operation again, adding
    -- the next token to the call.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of build IDs for the specified build project, with each build ID
    -- representing a single build.
    ids :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuildsForProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBuildsForProjectResponse_nextToken' - If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
--
-- 'ids', 'listBuildsForProjectResponse_ids' - A list of build IDs for the specified build project, with each build ID
-- representing a single build.
--
-- 'httpStatus', 'listBuildsForProjectResponse_httpStatus' - The response's http status code.
newListBuildsForProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBuildsForProjectResponse
newListBuildsForProjectResponse pHttpStatus_ =
  ListBuildsForProjectResponse'
    { nextToken =
        Core.Nothing,
      ids = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more than 100 items in the list, only the first 100 items
-- are returned, along with a unique string called a /nextToken/. To get
-- the next batch of items in the list, call this operation again, adding
-- the next token to the call.
listBuildsForProjectResponse_nextToken :: Lens.Lens' ListBuildsForProjectResponse (Core.Maybe Core.Text)
listBuildsForProjectResponse_nextToken = Lens.lens (\ListBuildsForProjectResponse' {nextToken} -> nextToken) (\s@ListBuildsForProjectResponse' {} a -> s {nextToken = a} :: ListBuildsForProjectResponse)

-- | A list of build IDs for the specified build project, with each build ID
-- representing a single build.
listBuildsForProjectResponse_ids :: Lens.Lens' ListBuildsForProjectResponse (Core.Maybe (Core.NonEmpty Core.Text))
listBuildsForProjectResponse_ids = Lens.lens (\ListBuildsForProjectResponse' {ids} -> ids) (\s@ListBuildsForProjectResponse' {} a -> s {ids = a} :: ListBuildsForProjectResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBuildsForProjectResponse_httpStatus :: Lens.Lens' ListBuildsForProjectResponse Core.Int
listBuildsForProjectResponse_httpStatus = Lens.lens (\ListBuildsForProjectResponse' {httpStatus} -> httpStatus) (\s@ListBuildsForProjectResponse' {} a -> s {httpStatus = a} :: ListBuildsForProjectResponse)

instance Core.NFData ListBuildsForProjectResponse
