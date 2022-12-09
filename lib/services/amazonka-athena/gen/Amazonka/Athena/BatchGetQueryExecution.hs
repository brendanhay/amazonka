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
-- Module      : Amazonka.Athena.BatchGetQueryExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Athena.BatchGetQueryExecution
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

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains an array of query execution IDs.
--
-- /See:/ 'newBatchGetQueryExecution' smart constructor.
data BatchGetQueryExecution = BatchGetQueryExecution'
  { -- | An array of query execution IDs.
    queryExecutionIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  BatchGetQueryExecution
newBatchGetQueryExecution pQueryExecutionIds_ =
  BatchGetQueryExecution'
    { queryExecutionIds =
        Lens.coerced Lens.# pQueryExecutionIds_
    }

-- | An array of query execution IDs.
batchGetQueryExecution_queryExecutionIds :: Lens.Lens' BatchGetQueryExecution (Prelude.NonEmpty Prelude.Text)
batchGetQueryExecution_queryExecutionIds = Lens.lens (\BatchGetQueryExecution' {queryExecutionIds} -> queryExecutionIds) (\s@BatchGetQueryExecution' {} a -> s {queryExecutionIds = a} :: BatchGetQueryExecution) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetQueryExecution where
  type
    AWSResponse BatchGetQueryExecution =
      BatchGetQueryExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetQueryExecutionResponse'
            Prelude.<$> ( x Data..?> "QueryExecutions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "UnprocessedQueryExecutionIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetQueryExecution where
  hashWithSalt _salt BatchGetQueryExecution' {..} =
    _salt `Prelude.hashWithSalt` queryExecutionIds

instance Prelude.NFData BatchGetQueryExecution where
  rnf BatchGetQueryExecution' {..} =
    Prelude.rnf queryExecutionIds

instance Data.ToHeaders BatchGetQueryExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.BatchGetQueryExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetQueryExecution where
  toJSON BatchGetQueryExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QueryExecutionIds" Data..= queryExecutionIds)
          ]
      )

instance Data.ToPath BatchGetQueryExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetQueryExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetQueryExecutionResponse' smart constructor.
data BatchGetQueryExecutionResponse = BatchGetQueryExecutionResponse'
  { -- | Information about a query execution.
    queryExecutions :: Prelude.Maybe [QueryExecution],
    -- | Information about the query executions that failed to run.
    unprocessedQueryExecutionIds :: Prelude.Maybe [UnprocessedQueryExecutionId],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchGetQueryExecutionResponse
newBatchGetQueryExecutionResponse pHttpStatus_ =
  BatchGetQueryExecutionResponse'
    { queryExecutions =
        Prelude.Nothing,
      unprocessedQueryExecutionIds =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a query execution.
batchGetQueryExecutionResponse_queryExecutions :: Lens.Lens' BatchGetQueryExecutionResponse (Prelude.Maybe [QueryExecution])
batchGetQueryExecutionResponse_queryExecutions = Lens.lens (\BatchGetQueryExecutionResponse' {queryExecutions} -> queryExecutions) (\s@BatchGetQueryExecutionResponse' {} a -> s {queryExecutions = a} :: BatchGetQueryExecutionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the query executions that failed to run.
batchGetQueryExecutionResponse_unprocessedQueryExecutionIds :: Lens.Lens' BatchGetQueryExecutionResponse (Prelude.Maybe [UnprocessedQueryExecutionId])
batchGetQueryExecutionResponse_unprocessedQueryExecutionIds = Lens.lens (\BatchGetQueryExecutionResponse' {unprocessedQueryExecutionIds} -> unprocessedQueryExecutionIds) (\s@BatchGetQueryExecutionResponse' {} a -> s {unprocessedQueryExecutionIds = a} :: BatchGetQueryExecutionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetQueryExecutionResponse_httpStatus :: Lens.Lens' BatchGetQueryExecutionResponse Prelude.Int
batchGetQueryExecutionResponse_httpStatus = Lens.lens (\BatchGetQueryExecutionResponse' {httpStatus} -> httpStatus) (\s@BatchGetQueryExecutionResponse' {} a -> s {httpStatus = a} :: BatchGetQueryExecutionResponse)

instance
  Prelude.NFData
    BatchGetQueryExecutionResponse
  where
  rnf BatchGetQueryExecutionResponse' {..} =
    Prelude.rnf queryExecutions
      `Prelude.seq` Prelude.rnf unprocessedQueryExecutionIds
      `Prelude.seq` Prelude.rnf httpStatus
