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
-- Module      : Amazonka.MigrationHub.ListMigrationTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.MigrationHub.ListMigrationTasks
  ( -- * Creating a Request
    ListMigrationTasks (..),
    newListMigrationTasks,

    -- * Request Lenses
    listMigrationTasks_maxResults,
    listMigrationTasks_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMigrationTasks' smart constructor.
data ListMigrationTasks = ListMigrationTasks'
  { -- | Value to specify how many results are returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter migration tasks by discovered resource name.
    resourceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMigrationTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMigrationTasks_maxResults' - Value to specify how many results are returned per page.
--
-- 'nextToken', 'listMigrationTasks_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'resourceName', 'listMigrationTasks_resourceName' - Filter migration tasks by discovered resource name.
newListMigrationTasks ::
  ListMigrationTasks
newListMigrationTasks =
  ListMigrationTasks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceName = Prelude.Nothing
    }

-- | Value to specify how many results are returned per page.
listMigrationTasks_maxResults :: Lens.Lens' ListMigrationTasks (Prelude.Maybe Prelude.Natural)
listMigrationTasks_maxResults = Lens.lens (\ListMigrationTasks' {maxResults} -> maxResults) (\s@ListMigrationTasks' {} a -> s {maxResults = a} :: ListMigrationTasks)

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listMigrationTasks_nextToken :: Lens.Lens' ListMigrationTasks (Prelude.Maybe Prelude.Text)
listMigrationTasks_nextToken = Lens.lens (\ListMigrationTasks' {nextToken} -> nextToken) (\s@ListMigrationTasks' {} a -> s {nextToken = a} :: ListMigrationTasks)

-- | Filter migration tasks by discovered resource name.
listMigrationTasks_resourceName :: Lens.Lens' ListMigrationTasks (Prelude.Maybe Prelude.Text)
listMigrationTasks_resourceName = Lens.lens (\ListMigrationTasks' {resourceName} -> resourceName) (\s@ListMigrationTasks' {} a -> s {resourceName = a} :: ListMigrationTasks)

instance Core.AWSPager ListMigrationTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMigrationTasksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMigrationTasksResponse_migrationTaskSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listMigrationTasks_nextToken
              Lens..~ rs
              Lens.^? listMigrationTasksResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListMigrationTasks where
  type
    AWSResponse ListMigrationTasks =
      ListMigrationTasksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMigrationTasksResponse'
            Prelude.<$> ( x
                            Data..?> "MigrationTaskSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMigrationTasks where
  hashWithSalt _salt ListMigrationTasks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData ListMigrationTasks where
  rnf ListMigrationTasks' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf resourceName

instance Data.ToHeaders ListMigrationTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHub.ListMigrationTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMigrationTasks where
  toJSON ListMigrationTasks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceName" Data..=) Prelude.<$> resourceName
          ]
      )

instance Data.ToPath ListMigrationTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMigrationTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMigrationTasksResponse' smart constructor.
data ListMigrationTasksResponse = ListMigrationTasksResponse'
  { -- | Lists the migration task\'s summary which includes: @MigrationTaskName@,
    -- @ProgressPercent@, @ProgressUpdateStream@, @Status@, and the
    -- @UpdateDateTime@ for each task.
    migrationTaskSummaryList :: Prelude.Maybe [MigrationTaskSummary],
    -- | If there are more migration tasks than the max result, return the next
    -- token to be passed to the next call as a bookmark of where to start
    -- from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListMigrationTasksResponse
newListMigrationTasksResponse pHttpStatus_ =
  ListMigrationTasksResponse'
    { migrationTaskSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the migration task\'s summary which includes: @MigrationTaskName@,
-- @ProgressPercent@, @ProgressUpdateStream@, @Status@, and the
-- @UpdateDateTime@ for each task.
listMigrationTasksResponse_migrationTaskSummaryList :: Lens.Lens' ListMigrationTasksResponse (Prelude.Maybe [MigrationTaskSummary])
listMigrationTasksResponse_migrationTaskSummaryList = Lens.lens (\ListMigrationTasksResponse' {migrationTaskSummaryList} -> migrationTaskSummaryList) (\s@ListMigrationTasksResponse' {} a -> s {migrationTaskSummaryList = a} :: ListMigrationTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more migration tasks than the max result, return the next
-- token to be passed to the next call as a bookmark of where to start
-- from.
listMigrationTasksResponse_nextToken :: Lens.Lens' ListMigrationTasksResponse (Prelude.Maybe Prelude.Text)
listMigrationTasksResponse_nextToken = Lens.lens (\ListMigrationTasksResponse' {nextToken} -> nextToken) (\s@ListMigrationTasksResponse' {} a -> s {nextToken = a} :: ListMigrationTasksResponse)

-- | The response's http status code.
listMigrationTasksResponse_httpStatus :: Lens.Lens' ListMigrationTasksResponse Prelude.Int
listMigrationTasksResponse_httpStatus = Lens.lens (\ListMigrationTasksResponse' {httpStatus} -> httpStatus) (\s@ListMigrationTasksResponse' {} a -> s {httpStatus = a} :: ListMigrationTasksResponse)

instance Prelude.NFData ListMigrationTasksResponse where
  rnf ListMigrationTasksResponse' {..} =
    Prelude.rnf migrationTaskSummaryList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
