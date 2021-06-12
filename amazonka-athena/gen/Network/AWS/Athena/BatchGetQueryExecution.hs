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
-- Module      : Network.AWS.Athena.BatchGetQueryExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a single query execution or a list of up to 50
-- query executions, which you provide as an array of query execution ID
-- strings. Requires you to have access to the workgroup in which the
-- queries ran. To get a list of query execution IDs, use
-- ListQueryExecutionsInput$WorkGroup. Query executions differ from named
-- (saved) queries. Use BatchGetNamedQueryInput to get details about named
-- queries.
module Network.AWS.Athena.BatchGetQueryExecution
  ( -- * Creating a Request
    BatchGetQueryExecution (..),
    newBatchGetQueryExecution,

    -- * Request Lenses
    batchGetQueryExecution_queryExecutionIds,

    -- * Destructuring the Response
    BatchGetQueryExecutionResponse (..),
    newBatchGetQueryExecutionResponse,

    -- * Response Lenses
    batchGetQueryExecutionResponse_queryExecutions,
    batchGetQueryExecutionResponse_unprocessedQueryExecutionIds,
    batchGetQueryExecutionResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetQueryExecution' smart constructor.
data BatchGetQueryExecution = BatchGetQueryExecution'
  { -- | An array of query execution IDs.
    queryExecutionIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetQueryExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecutionIds', 'batchGetQueryExecution_queryExecutionIds' - An array of query execution IDs.
newBatchGetQueryExecution ::
  -- | 'queryExecutionIds'
  Core.NonEmpty Core.Text ->
  BatchGetQueryExecution
newBatchGetQueryExecution pQueryExecutionIds_ =
  BatchGetQueryExecution'
    { queryExecutionIds =
        Lens._Coerce Lens.# pQueryExecutionIds_
    }

-- | An array of query execution IDs.
batchGetQueryExecution_queryExecutionIds :: Lens.Lens' BatchGetQueryExecution (Core.NonEmpty Core.Text)
batchGetQueryExecution_queryExecutionIds = Lens.lens (\BatchGetQueryExecution' {queryExecutionIds} -> queryExecutionIds) (\s@BatchGetQueryExecution' {} a -> s {queryExecutionIds = a} :: BatchGetQueryExecution) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetQueryExecution where
  type
    AWSResponse BatchGetQueryExecution =
      BatchGetQueryExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetQueryExecutionResponse'
            Core.<$> (x Core..?> "QueryExecutions" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "UnprocessedQueryExecutionIds"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetQueryExecution

instance Core.NFData BatchGetQueryExecution

instance Core.ToHeaders BatchGetQueryExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonAthena.BatchGetQueryExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetQueryExecution where
  toJSON BatchGetQueryExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QueryExecutionIds" Core..= queryExecutionIds)
          ]
      )

instance Core.ToPath BatchGetQueryExecution where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetQueryExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetQueryExecutionResponse' smart constructor.
data BatchGetQueryExecutionResponse = BatchGetQueryExecutionResponse'
  { -- | Information about a query execution.
    queryExecutions :: Core.Maybe [QueryExecution],
    -- | Information about the query executions that failed to run.
    unprocessedQueryExecutionIds :: Core.Maybe [UnprocessedQueryExecutionId],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetQueryExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecutions', 'batchGetQueryExecutionResponse_queryExecutions' - Information about a query execution.
--
-- 'unprocessedQueryExecutionIds', 'batchGetQueryExecutionResponse_unprocessedQueryExecutionIds' - Information about the query executions that failed to run.
--
-- 'httpStatus', 'batchGetQueryExecutionResponse_httpStatus' - The response's http status code.
newBatchGetQueryExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetQueryExecutionResponse
newBatchGetQueryExecutionResponse pHttpStatus_ =
  BatchGetQueryExecutionResponse'
    { queryExecutions =
        Core.Nothing,
      unprocessedQueryExecutionIds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a query execution.
batchGetQueryExecutionResponse_queryExecutions :: Lens.Lens' BatchGetQueryExecutionResponse (Core.Maybe [QueryExecution])
batchGetQueryExecutionResponse_queryExecutions = Lens.lens (\BatchGetQueryExecutionResponse' {queryExecutions} -> queryExecutions) (\s@BatchGetQueryExecutionResponse' {} a -> s {queryExecutions = a} :: BatchGetQueryExecutionResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the query executions that failed to run.
batchGetQueryExecutionResponse_unprocessedQueryExecutionIds :: Lens.Lens' BatchGetQueryExecutionResponse (Core.Maybe [UnprocessedQueryExecutionId])
batchGetQueryExecutionResponse_unprocessedQueryExecutionIds = Lens.lens (\BatchGetQueryExecutionResponse' {unprocessedQueryExecutionIds} -> unprocessedQueryExecutionIds) (\s@BatchGetQueryExecutionResponse' {} a -> s {unprocessedQueryExecutionIds = a} :: BatchGetQueryExecutionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetQueryExecutionResponse_httpStatus :: Lens.Lens' BatchGetQueryExecutionResponse Core.Int
batchGetQueryExecutionResponse_httpStatus = Lens.lens (\BatchGetQueryExecutionResponse' {httpStatus} -> httpStatus) (\s@BatchGetQueryExecutionResponse' {} a -> s {httpStatus = a} :: BatchGetQueryExecutionResponse)

instance Core.NFData BatchGetQueryExecutionResponse
