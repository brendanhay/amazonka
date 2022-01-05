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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform batch reads and writes on data
-- stored in DynamoDB, using PartiQL.
module Amazonka.DynamoDB.BatchExecuteStatement
  ( -- * Creating a Request
    BatchExecuteStatement (..),
    newBatchExecuteStatement,

    -- * Request Lenses
    batchExecuteStatement_statements,

    -- * Destructuring the Response
    BatchExecuteStatementResponse (..),
    newBatchExecuteStatementResponse,

    -- * Response Lenses
    batchExecuteStatementResponse_responses,
    batchExecuteStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchExecuteStatement' smart constructor.
data BatchExecuteStatement = BatchExecuteStatement'
  { -- | The list of PartiQL statements representing the batch to run.
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
-- 'statements', 'batchExecuteStatement_statements' - The list of PartiQL statements representing the batch to run.
newBatchExecuteStatement ::
  -- | 'statements'
  Prelude.NonEmpty BatchStatementRequest ->
  BatchExecuteStatement
newBatchExecuteStatement pStatements_ =
  BatchExecuteStatement'
    { statements =
        Lens.coerced Lens.# pStatements_
    }

-- | The list of PartiQL statements representing the batch to run.
batchExecuteStatement_statements :: Lens.Lens' BatchExecuteStatement (Prelude.NonEmpty BatchStatementRequest)
batchExecuteStatement_statements = Lens.lens (\BatchExecuteStatement' {statements} -> statements) (\s@BatchExecuteStatement' {} a -> s {statements = a} :: BatchExecuteStatement) Prelude.. Lens.coerced

instance Core.AWSRequest BatchExecuteStatement where
  type
    AWSResponse BatchExecuteStatement =
      BatchExecuteStatementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchExecuteStatementResponse'
            Prelude.<$> (x Core..?> "Responses" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchExecuteStatement where
  hashWithSalt _salt BatchExecuteStatement' {..} =
    _salt `Prelude.hashWithSalt` statements

instance Prelude.NFData BatchExecuteStatement where
  rnf BatchExecuteStatement' {..} =
    Prelude.rnf statements

instance Core.ToHeaders BatchExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.BatchExecuteStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Statements" Core..= statements)]
      )

instance Core.ToPath BatchExecuteStatement where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { -- | The response to each PartiQL statement in the batch.
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
-- 'responses', 'batchExecuteStatementResponse_responses' - The response to each PartiQL statement in the batch.
--
-- 'httpStatus', 'batchExecuteStatementResponse_httpStatus' - The response's http status code.
newBatchExecuteStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchExecuteStatementResponse
newBatchExecuteStatementResponse pHttpStatus_ =
  BatchExecuteStatementResponse'
    { responses =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response to each PartiQL statement in the batch.
batchExecuteStatementResponse_responses :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe [BatchStatementResponse])
batchExecuteStatementResponse_responses = Lens.lens (\BatchExecuteStatementResponse' {responses} -> responses) (\s@BatchExecuteStatementResponse' {} a -> s {responses = a} :: BatchExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchExecuteStatementResponse_httpStatus :: Lens.Lens' BatchExecuteStatementResponse Prelude.Int
batchExecuteStatementResponse_httpStatus = Lens.lens (\BatchExecuteStatementResponse' {httpStatus} -> httpStatus) (\s@BatchExecuteStatementResponse' {} a -> s {httpStatus = a} :: BatchExecuteStatementResponse)

instance Prelude.NFData BatchExecuteStatementResponse where
  rnf BatchExecuteStatementResponse' {..} =
    Prelude.rnf responses
      `Prelude.seq` Prelude.rnf httpStatus
