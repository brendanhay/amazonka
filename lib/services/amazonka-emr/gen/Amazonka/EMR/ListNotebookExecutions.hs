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
-- Module      : Amazonka.EMR.ListNotebookExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summaries of all notebook executions. You can filter the list
-- based on multiple criteria such as status, time range, and editor id.
-- Returns a maximum of 50 notebook executions and a marker to track the
-- paging of a longer notebook execution list across multiple
-- @ListNotebookExecution@ calls.
--
-- This operation returns paginated results.
module Amazonka.EMR.ListNotebookExecutions
  ( -- * Creating a Request
    ListNotebookExecutions (..),
    newListNotebookExecutions,

    -- * Request Lenses
    listNotebookExecutions_editorId,
    listNotebookExecutions_from,
    listNotebookExecutions_marker,
    listNotebookExecutions_status,
    listNotebookExecutions_to,

    -- * Destructuring the Response
    ListNotebookExecutionsResponse (..),
    newListNotebookExecutionsResponse,

    -- * Response Lenses
    listNotebookExecutionsResponse_marker,
    listNotebookExecutionsResponse_notebookExecutions,
    listNotebookExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNotebookExecutions' smart constructor.
data ListNotebookExecutions = ListNotebookExecutions'
  { -- | The unique ID of the editor associated with the notebook execution.
    editorId :: Prelude.Maybe Prelude.Text,
    -- | The beginning of time range filter for listing notebook executions. The
    -- default is the timestamp of 30 days ago.
    from :: Prelude.Maybe Data.POSIX,
    -- | The pagination token, returned by a previous @ListNotebookExecutions@
    -- call, that indicates the start of the list for this
    -- @ListNotebookExecutions@ call.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The status filter for listing notebook executions.
    --
    -- -   @START_PENDING@ indicates that the cluster has received the
    --     execution request but execution has not begun.
    --
    -- -   @STARTING@ indicates that the execution is starting on the cluster.
    --
    -- -   @RUNNING@ indicates that the execution is being processed by the
    --     cluster.
    --
    -- -   @FINISHING@ indicates that execution processing is in the final
    --     stages.
    --
    -- -   @FINISHED@ indicates that the execution has completed without error.
    --
    -- -   @FAILING@ indicates that the execution is failing and will not
    --     finish successfully.
    --
    -- -   @FAILED@ indicates that the execution failed.
    --
    -- -   @STOP_PENDING@ indicates that the cluster has received a
    --     @StopNotebookExecution@ request and the stop is pending.
    --
    -- -   @STOPPING@ indicates that the cluster is in the process of stopping
    --     the execution as a result of a @StopNotebookExecution@ request.
    --
    -- -   @STOPPED@ indicates that the execution stopped because of a
    --     @StopNotebookExecution@ request.
    status :: Prelude.Maybe NotebookExecutionStatus,
    -- | The end of time range filter for listing notebook executions. The
    -- default is the current timestamp.
    to :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotebookExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'editorId', 'listNotebookExecutions_editorId' - The unique ID of the editor associated with the notebook execution.
--
-- 'from', 'listNotebookExecutions_from' - The beginning of time range filter for listing notebook executions. The
-- default is the timestamp of 30 days ago.
--
-- 'marker', 'listNotebookExecutions_marker' - The pagination token, returned by a previous @ListNotebookExecutions@
-- call, that indicates the start of the list for this
-- @ListNotebookExecutions@ call.
--
-- 'status', 'listNotebookExecutions_status' - The status filter for listing notebook executions.
--
-- -   @START_PENDING@ indicates that the cluster has received the
--     execution request but execution has not begun.
--
-- -   @STARTING@ indicates that the execution is starting on the cluster.
--
-- -   @RUNNING@ indicates that the execution is being processed by the
--     cluster.
--
-- -   @FINISHING@ indicates that execution processing is in the final
--     stages.
--
-- -   @FINISHED@ indicates that the execution has completed without error.
--
-- -   @FAILING@ indicates that the execution is failing and will not
--     finish successfully.
--
-- -   @FAILED@ indicates that the execution failed.
--
-- -   @STOP_PENDING@ indicates that the cluster has received a
--     @StopNotebookExecution@ request and the stop is pending.
--
-- -   @STOPPING@ indicates that the cluster is in the process of stopping
--     the execution as a result of a @StopNotebookExecution@ request.
--
-- -   @STOPPED@ indicates that the execution stopped because of a
--     @StopNotebookExecution@ request.
--
-- 'to', 'listNotebookExecutions_to' - The end of time range filter for listing notebook executions. The
-- default is the current timestamp.
newListNotebookExecutions ::
  ListNotebookExecutions
newListNotebookExecutions =
  ListNotebookExecutions'
    { editorId = Prelude.Nothing,
      from = Prelude.Nothing,
      marker = Prelude.Nothing,
      status = Prelude.Nothing,
      to = Prelude.Nothing
    }

-- | The unique ID of the editor associated with the notebook execution.
listNotebookExecutions_editorId :: Lens.Lens' ListNotebookExecutions (Prelude.Maybe Prelude.Text)
listNotebookExecutions_editorId = Lens.lens (\ListNotebookExecutions' {editorId} -> editorId) (\s@ListNotebookExecutions' {} a -> s {editorId = a} :: ListNotebookExecutions)

-- | The beginning of time range filter for listing notebook executions. The
-- default is the timestamp of 30 days ago.
listNotebookExecutions_from :: Lens.Lens' ListNotebookExecutions (Prelude.Maybe Prelude.UTCTime)
listNotebookExecutions_from = Lens.lens (\ListNotebookExecutions' {from} -> from) (\s@ListNotebookExecutions' {} a -> s {from = a} :: ListNotebookExecutions) Prelude.. Lens.mapping Data._Time

-- | The pagination token, returned by a previous @ListNotebookExecutions@
-- call, that indicates the start of the list for this
-- @ListNotebookExecutions@ call.
listNotebookExecutions_marker :: Lens.Lens' ListNotebookExecutions (Prelude.Maybe Prelude.Text)
listNotebookExecutions_marker = Lens.lens (\ListNotebookExecutions' {marker} -> marker) (\s@ListNotebookExecutions' {} a -> s {marker = a} :: ListNotebookExecutions)

-- | The status filter for listing notebook executions.
--
-- -   @START_PENDING@ indicates that the cluster has received the
--     execution request but execution has not begun.
--
-- -   @STARTING@ indicates that the execution is starting on the cluster.
--
-- -   @RUNNING@ indicates that the execution is being processed by the
--     cluster.
--
-- -   @FINISHING@ indicates that execution processing is in the final
--     stages.
--
-- -   @FINISHED@ indicates that the execution has completed without error.
--
-- -   @FAILING@ indicates that the execution is failing and will not
--     finish successfully.
--
-- -   @FAILED@ indicates that the execution failed.
--
-- -   @STOP_PENDING@ indicates that the cluster has received a
--     @StopNotebookExecution@ request and the stop is pending.
--
-- -   @STOPPING@ indicates that the cluster is in the process of stopping
--     the execution as a result of a @StopNotebookExecution@ request.
--
-- -   @STOPPED@ indicates that the execution stopped because of a
--     @StopNotebookExecution@ request.
listNotebookExecutions_status :: Lens.Lens' ListNotebookExecutions (Prelude.Maybe NotebookExecutionStatus)
listNotebookExecutions_status = Lens.lens (\ListNotebookExecutions' {status} -> status) (\s@ListNotebookExecutions' {} a -> s {status = a} :: ListNotebookExecutions)

-- | The end of time range filter for listing notebook executions. The
-- default is the current timestamp.
listNotebookExecutions_to :: Lens.Lens' ListNotebookExecutions (Prelude.Maybe Prelude.UTCTime)
listNotebookExecutions_to = Lens.lens (\ListNotebookExecutions' {to} -> to) (\s@ListNotebookExecutions' {} a -> s {to = a} :: ListNotebookExecutions) Prelude.. Lens.mapping Data._Time

instance Core.AWSPager ListNotebookExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNotebookExecutionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNotebookExecutionsResponse_notebookExecutions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNotebookExecutions_marker
          Lens..~ rs
          Lens.^? listNotebookExecutionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest ListNotebookExecutions where
  type
    AWSResponse ListNotebookExecutions =
      ListNotebookExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNotebookExecutionsResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> ( x Data..?> "NotebookExecutions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNotebookExecutions where
  hashWithSalt _salt ListNotebookExecutions' {..} =
    _salt `Prelude.hashWithSalt` editorId
      `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` to

instance Prelude.NFData ListNotebookExecutions where
  rnf ListNotebookExecutions' {..} =
    Prelude.rnf editorId
      `Prelude.seq` Prelude.rnf from
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf to

instance Data.ToHeaders ListNotebookExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.ListNotebookExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListNotebookExecutions where
  toJSON ListNotebookExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EditorId" Data..=) Prelude.<$> editorId,
            ("From" Data..=) Prelude.<$> from,
            ("Marker" Data..=) Prelude.<$> marker,
            ("Status" Data..=) Prelude.<$> status,
            ("To" Data..=) Prelude.<$> to
          ]
      )

instance Data.ToPath ListNotebookExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListNotebookExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNotebookExecutionsResponse' smart constructor.
data ListNotebookExecutionsResponse = ListNotebookExecutionsResponse'
  { -- | A pagination token that a subsequent @ListNotebookExecutions@ can use to
    -- determine the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of notebook executions.
    notebookExecutions :: Prelude.Maybe [NotebookExecutionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotebookExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listNotebookExecutionsResponse_marker' - A pagination token that a subsequent @ListNotebookExecutions@ can use to
-- determine the next set of results to retrieve.
--
-- 'notebookExecutions', 'listNotebookExecutionsResponse_notebookExecutions' - A list of notebook executions.
--
-- 'httpStatus', 'listNotebookExecutionsResponse_httpStatus' - The response's http status code.
newListNotebookExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNotebookExecutionsResponse
newListNotebookExecutionsResponse pHttpStatus_ =
  ListNotebookExecutionsResponse'
    { marker =
        Prelude.Nothing,
      notebookExecutions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token that a subsequent @ListNotebookExecutions@ can use to
-- determine the next set of results to retrieve.
listNotebookExecutionsResponse_marker :: Lens.Lens' ListNotebookExecutionsResponse (Prelude.Maybe Prelude.Text)
listNotebookExecutionsResponse_marker = Lens.lens (\ListNotebookExecutionsResponse' {marker} -> marker) (\s@ListNotebookExecutionsResponse' {} a -> s {marker = a} :: ListNotebookExecutionsResponse)

-- | A list of notebook executions.
listNotebookExecutionsResponse_notebookExecutions :: Lens.Lens' ListNotebookExecutionsResponse (Prelude.Maybe [NotebookExecutionSummary])
listNotebookExecutionsResponse_notebookExecutions = Lens.lens (\ListNotebookExecutionsResponse' {notebookExecutions} -> notebookExecutions) (\s@ListNotebookExecutionsResponse' {} a -> s {notebookExecutions = a} :: ListNotebookExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNotebookExecutionsResponse_httpStatus :: Lens.Lens' ListNotebookExecutionsResponse Prelude.Int
listNotebookExecutionsResponse_httpStatus = Lens.lens (\ListNotebookExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListNotebookExecutionsResponse' {} a -> s {httpStatus = a} :: ListNotebookExecutionsResponse)

instance
  Prelude.NFData
    ListNotebookExecutionsResponse
  where
  rnf ListNotebookExecutionsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf notebookExecutions
      `Prelude.seq` Prelude.rnf httpStatus
