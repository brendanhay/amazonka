{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DynamoDB.BatchExecuteStatement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform batch reads and writes on data
-- stored in DynamoDB, using PartiQL.
module Network.AWS.DynamoDB.BatchExecuteStatement
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

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchExecuteStatement' smart constructor.
data BatchExecuteStatement = BatchExecuteStatement'
  { -- | The list of PartiQL statements representing the batch to run.
    statements :: Prelude.NonEmpty BatchStatementRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude._Coerce Lens.# pStatements_
    }

-- | The list of PartiQL statements representing the batch to run.
batchExecuteStatement_statements :: Lens.Lens' BatchExecuteStatement (Prelude.NonEmpty BatchStatementRequest)
batchExecuteStatement_statements = Lens.lens (\BatchExecuteStatement' {statements} -> statements) (\s@BatchExecuteStatement' {} a -> s {statements = a} :: BatchExecuteStatement) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest BatchExecuteStatement where
  type
    Rs BatchExecuteStatement =
      BatchExecuteStatementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchExecuteStatementResponse'
            Prelude.<$> ( x Prelude..?> "Responses"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchExecuteStatement

instance Prelude.NFData BatchExecuteStatement

instance Prelude.ToHeaders BatchExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DynamoDB_20120810.BatchExecuteStatement" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Statements" Prelude..= statements)]
      )

instance Prelude.ToPath BatchExecuteStatement where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { -- | The response to each PartiQL statement in the batch.
    responses :: Prelude.Maybe [BatchStatementResponse],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
batchExecuteStatementResponse_responses = Lens.lens (\BatchExecuteStatementResponse' {responses} -> responses) (\s@BatchExecuteStatementResponse' {} a -> s {responses = a} :: BatchExecuteStatementResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
batchExecuteStatementResponse_httpStatus :: Lens.Lens' BatchExecuteStatementResponse Prelude.Int
batchExecuteStatementResponse_httpStatus = Lens.lens (\BatchExecuteStatementResponse' {httpStatus} -> httpStatus) (\s@BatchExecuteStatementResponse' {} a -> s {httpStatus = a} :: BatchExecuteStatementResponse)

instance Prelude.NFData BatchExecuteStatementResponse
