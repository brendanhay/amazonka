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
-- Module      : Amazonka.DynamoDB.BatchExecuteStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform batch reads or writes on data
-- stored in DynamoDB, using PartiQL. Each read statement in a
-- @BatchExecuteStatement@ must specify an equality condition on all key
-- attributes. This enforces that each @SELECT@ statement in a batch
-- returns at most a single item.
--
-- The entire batch must consist of either read statements or write
-- statements, you cannot mix both in one batch.
--
-- A HTTP 200 response does not mean that all statements in the
-- BatchExecuteStatement succeeded. Error details for individual statements
-- can be found under the
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_BatchStatementResponse.html#DDB-Type-BatchStatementResponse-Error Error>
-- field of the @BatchStatementResponse@ for each statement.
module Amazonka.DynamoDB.BatchExecuteStatement
  ( -- * Creating a Request
    BatchExecuteStatement (..),
    newBatchExecuteStatement,

    -- * Request Lenses
    batchExecuteStatement_returnConsumedCapacity,
    batchExecuteStatement_statements,

    -- * Destructuring the Response
    BatchExecuteStatementResponse (..),
    newBatchExecuteStatementResponse,

    -- * Response Lenses
    batchExecuteStatementResponse_consumedCapacity,
    batchExecuteStatementResponse_responses,
    batchExecuteStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchExecuteStatement' smart constructor.
data BatchExecuteStatement = BatchExecuteStatement'
  { returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | The list of PartiQL statements representing the batch to run.
    statements :: Prelude.NonEmpty BatchStatementRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchExecuteStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnConsumedCapacity', 'batchExecuteStatement_returnConsumedCapacity' - Undocumented member.
--
-- 'statements', 'batchExecuteStatement_statements' - The list of PartiQL statements representing the batch to run.
newBatchExecuteStatement ::
  -- | 'statements'
  Prelude.NonEmpty BatchStatementRequest ->
  BatchExecuteStatement
newBatchExecuteStatement pStatements_ =
  BatchExecuteStatement'
    { returnConsumedCapacity =
        Prelude.Nothing,
      statements = Lens.coerced Lens.# pStatements_
    }

-- | Undocumented member.
batchExecuteStatement_returnConsumedCapacity :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe ReturnConsumedCapacity)
batchExecuteStatement_returnConsumedCapacity = Lens.lens (\BatchExecuteStatement' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@BatchExecuteStatement' {} a -> s {returnConsumedCapacity = a} :: BatchExecuteStatement)

-- | The list of PartiQL statements representing the batch to run.
batchExecuteStatement_statements :: Lens.Lens' BatchExecuteStatement (Prelude.NonEmpty BatchStatementRequest)
batchExecuteStatement_statements = Lens.lens (\BatchExecuteStatement' {statements} -> statements) (\s@BatchExecuteStatement' {} a -> s {statements = a} :: BatchExecuteStatement) Prelude.. Lens.coerced

instance Core.AWSRequest BatchExecuteStatement where
  type
    AWSResponse BatchExecuteStatement =
      BatchExecuteStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchExecuteStatementResponse'
            Prelude.<$> ( x Data..?> "ConsumedCapacity"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Responses" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchExecuteStatement where
  hashWithSalt _salt BatchExecuteStatement' {..} =
    _salt `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` statements

instance Prelude.NFData BatchExecuteStatement where
  rnf BatchExecuteStatement' {..} =
    Prelude.rnf returnConsumedCapacity
      `Prelude.seq` Prelude.rnf statements

instance Data.ToHeaders BatchExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.BatchExecuteStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReturnConsumedCapacity" Data..=)
              Prelude.<$> returnConsumedCapacity,
            Prelude.Just ("Statements" Data..= statements)
          ]
      )

instance Data.ToPath BatchExecuteStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { -- | The capacity units consumed by the entire operation. The values of the
    -- list are ordered according to the ordering of the statements.
    consumedCapacity :: Prelude.Maybe [ConsumedCapacity],
    -- | The response to each PartiQL statement in the batch.
    responses :: Prelude.Maybe [BatchStatementResponse],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchExecuteStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedCapacity', 'batchExecuteStatementResponse_consumedCapacity' - The capacity units consumed by the entire operation. The values of the
-- list are ordered according to the ordering of the statements.
--
-- 'responses', 'batchExecuteStatementResponse_responses' - The response to each PartiQL statement in the batch.
--
-- 'httpStatus', 'batchExecuteStatementResponse_httpStatus' - The response's http status code.
newBatchExecuteStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchExecuteStatementResponse
newBatchExecuteStatementResponse pHttpStatus_ =
  BatchExecuteStatementResponse'
    { consumedCapacity =
        Prelude.Nothing,
      responses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The capacity units consumed by the entire operation. The values of the
-- list are ordered according to the ordering of the statements.
batchExecuteStatementResponse_consumedCapacity :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe [ConsumedCapacity])
batchExecuteStatementResponse_consumedCapacity = Lens.lens (\BatchExecuteStatementResponse' {consumedCapacity} -> consumedCapacity) (\s@BatchExecuteStatementResponse' {} a -> s {consumedCapacity = a} :: BatchExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response to each PartiQL statement in the batch.
batchExecuteStatementResponse_responses :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe [BatchStatementResponse])
batchExecuteStatementResponse_responses = Lens.lens (\BatchExecuteStatementResponse' {responses} -> responses) (\s@BatchExecuteStatementResponse' {} a -> s {responses = a} :: BatchExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchExecuteStatementResponse_httpStatus :: Lens.Lens' BatchExecuteStatementResponse Prelude.Int
batchExecuteStatementResponse_httpStatus = Lens.lens (\BatchExecuteStatementResponse' {httpStatus} -> httpStatus) (\s@BatchExecuteStatementResponse' {} a -> s {httpStatus = a} :: BatchExecuteStatementResponse)

instance Prelude.NFData BatchExecuteStatementResponse where
  rnf BatchExecuteStatementResponse' {..} =
    Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf responses
      `Prelude.seq` Prelude.rnf httpStatus
