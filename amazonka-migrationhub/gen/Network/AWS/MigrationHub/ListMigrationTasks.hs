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
-- Module      : Network.AWS.MigrationHub.ListMigrationTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all, or filtered by resource name, migration tasks associated with
-- the user account making this call. This API has the following traits:
--
-- -   Can show a summary list of the most recent migration tasks.
--
-- -   Can show a summary list of migration tasks associated with a given
--     discovered resource.
--
-- -   Lists migration tasks in a paginated interface.
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListMigrationTasks
  ( -- * Creating a Request
    ListMigrationTasks (..),
    newListMigrationTasks,

    -- * Request Lenses
    listMigrationTasks_nextToken,
    listMigrationTasks_maxResults,
    listMigrationTasks_resourceName,

    -- * Destructuring the Response
    ListMigrationTasksResponse (..),
    newListMigrationTasksResponse,

    -- * Response Lenses
    listMigrationTasksResponse_migrationTaskSummaryList,
    listMigrationTasksResponse_nextToken,
    listMigrationTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListMigrationTasks' smart constructor.
data ListMigrationTasks = ListMigrationTasks'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Core.Maybe Core.Text,
    -- | Value to specify how many results are returned per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filter migration tasks by discovered resource name.
    resourceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMigrationTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMigrationTasks_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'maxResults', 'listMigrationTasks_maxResults' - Value to specify how many results are returned per page.
--
-- 'resourceName', 'listMigrationTasks_resourceName' - Filter migration tasks by discovered resource name.
newListMigrationTasks ::
  ListMigrationTasks
newListMigrationTasks =
  ListMigrationTasks'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      resourceName = Core.Nothing
    }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listMigrationTasks_nextToken :: Lens.Lens' ListMigrationTasks (Core.Maybe Core.Text)
listMigrationTasks_nextToken = Lens.lens (\ListMigrationTasks' {nextToken} -> nextToken) (\s@ListMigrationTasks' {} a -> s {nextToken = a} :: ListMigrationTasks)

-- | Value to specify how many results are returned per page.
listMigrationTasks_maxResults :: Lens.Lens' ListMigrationTasks (Core.Maybe Core.Natural)
listMigrationTasks_maxResults = Lens.lens (\ListMigrationTasks' {maxResults} -> maxResults) (\s@ListMigrationTasks' {} a -> s {maxResults = a} :: ListMigrationTasks)

-- | Filter migration tasks by discovered resource name.
listMigrationTasks_resourceName :: Lens.Lens' ListMigrationTasks (Core.Maybe Core.Text)
listMigrationTasks_resourceName = Lens.lens (\ListMigrationTasks' {resourceName} -> resourceName) (\s@ListMigrationTasks' {} a -> s {resourceName = a} :: ListMigrationTasks)

instance Core.AWSPager ListMigrationTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMigrationTasksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listMigrationTasksResponse_migrationTaskSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listMigrationTasks_nextToken
          Lens..~ rs
          Lens.^? listMigrationTasksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListMigrationTasks where
  type
    AWSResponse ListMigrationTasks =
      ListMigrationTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMigrationTasksResponse'
            Core.<$> ( x Core..?> "MigrationTaskSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMigrationTasks

instance Core.NFData ListMigrationTasks

instance Core.ToHeaders ListMigrationTasks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.ListMigrationTasks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListMigrationTasks where
  toJSON ListMigrationTasks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ResourceName" Core..=) Core.<$> resourceName
          ]
      )

instance Core.ToPath ListMigrationTasks where
  toPath = Core.const "/"

instance Core.ToQuery ListMigrationTasks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListMigrationTasksResponse' smart constructor.
data ListMigrationTasksResponse = ListMigrationTasksResponse'
  { -- | Lists the migration task\'s summary which includes: @MigrationTaskName@,
    -- @ProgressPercent@, @ProgressUpdateStream@, @Status@, and the
    -- @UpdateDateTime@ for each task.
    migrationTaskSummaryList :: Core.Maybe [MigrationTaskSummary],
    -- | If there are more migration tasks than the max result, return the next
    -- token to be passed to the next call as a bookmark of where to start
    -- from.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMigrationTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'migrationTaskSummaryList', 'listMigrationTasksResponse_migrationTaskSummaryList' - Lists the migration task\'s summary which includes: @MigrationTaskName@,
-- @ProgressPercent@, @ProgressUpdateStream@, @Status@, and the
-- @UpdateDateTime@ for each task.
--
-- 'nextToken', 'listMigrationTasksResponse_nextToken' - If there are more migration tasks than the max result, return the next
-- token to be passed to the next call as a bookmark of where to start
-- from.
--
-- 'httpStatus', 'listMigrationTasksResponse_httpStatus' - The response's http status code.
newListMigrationTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMigrationTasksResponse
newListMigrationTasksResponse pHttpStatus_ =
  ListMigrationTasksResponse'
    { migrationTaskSummaryList =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the migration task\'s summary which includes: @MigrationTaskName@,
-- @ProgressPercent@, @ProgressUpdateStream@, @Status@, and the
-- @UpdateDateTime@ for each task.
listMigrationTasksResponse_migrationTaskSummaryList :: Lens.Lens' ListMigrationTasksResponse (Core.Maybe [MigrationTaskSummary])
listMigrationTasksResponse_migrationTaskSummaryList = Lens.lens (\ListMigrationTasksResponse' {migrationTaskSummaryList} -> migrationTaskSummaryList) (\s@ListMigrationTasksResponse' {} a -> s {migrationTaskSummaryList = a} :: ListMigrationTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | If there are more migration tasks than the max result, return the next
-- token to be passed to the next call as a bookmark of where to start
-- from.
listMigrationTasksResponse_nextToken :: Lens.Lens' ListMigrationTasksResponse (Core.Maybe Core.Text)
listMigrationTasksResponse_nextToken = Lens.lens (\ListMigrationTasksResponse' {nextToken} -> nextToken) (\s@ListMigrationTasksResponse' {} a -> s {nextToken = a} :: ListMigrationTasksResponse)

-- | The response's http status code.
listMigrationTasksResponse_httpStatus :: Lens.Lens' ListMigrationTasksResponse Core.Int
listMigrationTasksResponse_httpStatus = Lens.lens (\ListMigrationTasksResponse' {httpStatus} -> httpStatus) (\s@ListMigrationTasksResponse' {} a -> s {httpStatus = a} :: ListMigrationTasksResponse)

instance Core.NFData ListMigrationTasksResponse
