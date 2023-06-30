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
-- Module      : Amazonka.Glue.GetMLTaskRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of runs for a machine learning transform. Machine learning
-- task runs are asynchronous tasks that Glue runs on your behalf as part
-- of various machine learning workflows. You can get a sortable,
-- filterable list of machine learning task runs by calling @GetMLTaskRuns@
-- with their parent transform\'s @TransformID@ and other optional
-- parameters as documented in this section.
--
-- This operation returns a list of historic runs and must be paginated.
module Amazonka.Glue.GetMLTaskRuns
  ( -- * Creating a Request
    GetMLTaskRuns (..),
    newGetMLTaskRuns,

    -- * Request Lenses
    getMLTaskRuns_filter,
    getMLTaskRuns_maxResults,
    getMLTaskRuns_nextToken,
    getMLTaskRuns_sort,
    getMLTaskRuns_transformId,

    -- * Destructuring the Response
    GetMLTaskRunsResponse (..),
    newGetMLTaskRunsResponse,

    -- * Response Lenses
    getMLTaskRunsResponse_nextToken,
    getMLTaskRunsResponse_taskRuns,
    getMLTaskRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMLTaskRuns' smart constructor.
data GetMLTaskRuns = GetMLTaskRuns'
  { -- | The filter criteria, in the @TaskRunFilterCriteria@ structure, for the
    -- task run.
    filter' :: Prelude.Maybe TaskRunFilterCriteria,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token for pagination of the results. The default is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sorting criteria, in the @TaskRunSortCriteria@ structure, for the
    -- task run.
    sort :: Prelude.Maybe TaskRunSortCriteria,
    -- | The unique identifier of the machine learning transform.
    transformId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMLTaskRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'getMLTaskRuns_filter' - The filter criteria, in the @TaskRunFilterCriteria@ structure, for the
-- task run.
--
-- 'maxResults', 'getMLTaskRuns_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getMLTaskRuns_nextToken' - A token for pagination of the results. The default is empty.
--
-- 'sort', 'getMLTaskRuns_sort' - The sorting criteria, in the @TaskRunSortCriteria@ structure, for the
-- task run.
--
-- 'transformId', 'getMLTaskRuns_transformId' - The unique identifier of the machine learning transform.
newGetMLTaskRuns ::
  -- | 'transformId'
  Prelude.Text ->
  GetMLTaskRuns
newGetMLTaskRuns pTransformId_ =
  GetMLTaskRuns'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sort = Prelude.Nothing,
      transformId = pTransformId_
    }

-- | The filter criteria, in the @TaskRunFilterCriteria@ structure, for the
-- task run.
getMLTaskRuns_filter :: Lens.Lens' GetMLTaskRuns (Prelude.Maybe TaskRunFilterCriteria)
getMLTaskRuns_filter = Lens.lens (\GetMLTaskRuns' {filter'} -> filter') (\s@GetMLTaskRuns' {} a -> s {filter' = a} :: GetMLTaskRuns)

-- | The maximum number of results to return.
getMLTaskRuns_maxResults :: Lens.Lens' GetMLTaskRuns (Prelude.Maybe Prelude.Natural)
getMLTaskRuns_maxResults = Lens.lens (\GetMLTaskRuns' {maxResults} -> maxResults) (\s@GetMLTaskRuns' {} a -> s {maxResults = a} :: GetMLTaskRuns)

-- | A token for pagination of the results. The default is empty.
getMLTaskRuns_nextToken :: Lens.Lens' GetMLTaskRuns (Prelude.Maybe Prelude.Text)
getMLTaskRuns_nextToken = Lens.lens (\GetMLTaskRuns' {nextToken} -> nextToken) (\s@GetMLTaskRuns' {} a -> s {nextToken = a} :: GetMLTaskRuns)

-- | The sorting criteria, in the @TaskRunSortCriteria@ structure, for the
-- task run.
getMLTaskRuns_sort :: Lens.Lens' GetMLTaskRuns (Prelude.Maybe TaskRunSortCriteria)
getMLTaskRuns_sort = Lens.lens (\GetMLTaskRuns' {sort} -> sort) (\s@GetMLTaskRuns' {} a -> s {sort = a} :: GetMLTaskRuns)

-- | The unique identifier of the machine learning transform.
getMLTaskRuns_transformId :: Lens.Lens' GetMLTaskRuns Prelude.Text
getMLTaskRuns_transformId = Lens.lens (\GetMLTaskRuns' {transformId} -> transformId) (\s@GetMLTaskRuns' {} a -> s {transformId = a} :: GetMLTaskRuns)

instance Core.AWSRequest GetMLTaskRuns where
  type
    AWSResponse GetMLTaskRuns =
      GetMLTaskRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMLTaskRunsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TaskRuns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMLTaskRuns where
  hashWithSalt _salt GetMLTaskRuns' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` transformId

instance Prelude.NFData GetMLTaskRuns where
  rnf GetMLTaskRuns' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sort
      `Prelude.seq` Prelude.rnf transformId

instance Data.ToHeaders GetMLTaskRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetMLTaskRuns" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMLTaskRuns where
  toJSON GetMLTaskRuns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Sort" Data..=) Prelude.<$> sort,
            Prelude.Just ("TransformId" Data..= transformId)
          ]
      )

instance Data.ToPath GetMLTaskRuns where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMLTaskRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMLTaskRunsResponse' smart constructor.
data GetMLTaskRunsResponse = GetMLTaskRunsResponse'
  { -- | A pagination token, if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of task runs that are associated with the transform.
    taskRuns :: Prelude.Maybe [TaskRun],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMLTaskRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getMLTaskRunsResponse_nextToken' - A pagination token, if more results are available.
--
-- 'taskRuns', 'getMLTaskRunsResponse_taskRuns' - A list of task runs that are associated with the transform.
--
-- 'httpStatus', 'getMLTaskRunsResponse_httpStatus' - The response's http status code.
newGetMLTaskRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMLTaskRunsResponse
newGetMLTaskRunsResponse pHttpStatus_ =
  GetMLTaskRunsResponse'
    { nextToken = Prelude.Nothing,
      taskRuns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token, if more results are available.
getMLTaskRunsResponse_nextToken :: Lens.Lens' GetMLTaskRunsResponse (Prelude.Maybe Prelude.Text)
getMLTaskRunsResponse_nextToken = Lens.lens (\GetMLTaskRunsResponse' {nextToken} -> nextToken) (\s@GetMLTaskRunsResponse' {} a -> s {nextToken = a} :: GetMLTaskRunsResponse)

-- | A list of task runs that are associated with the transform.
getMLTaskRunsResponse_taskRuns :: Lens.Lens' GetMLTaskRunsResponse (Prelude.Maybe [TaskRun])
getMLTaskRunsResponse_taskRuns = Lens.lens (\GetMLTaskRunsResponse' {taskRuns} -> taskRuns) (\s@GetMLTaskRunsResponse' {} a -> s {taskRuns = a} :: GetMLTaskRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getMLTaskRunsResponse_httpStatus :: Lens.Lens' GetMLTaskRunsResponse Prelude.Int
getMLTaskRunsResponse_httpStatus = Lens.lens (\GetMLTaskRunsResponse' {httpStatus} -> httpStatus) (\s@GetMLTaskRunsResponse' {} a -> s {httpStatus = a} :: GetMLTaskRunsResponse)

instance Prelude.NFData GetMLTaskRunsResponse where
  rnf GetMLTaskRunsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskRuns
      `Prelude.seq` Prelude.rnf httpStatus
