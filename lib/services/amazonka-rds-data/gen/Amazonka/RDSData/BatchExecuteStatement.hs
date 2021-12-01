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
-- Module      : Amazonka.RDSData.BatchExecuteStatement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs a batch SQL statement over an array of data.
--
-- You can run bulk update and insert operations for multiple records using
-- a DML statement with different parameter sets. Bulk operations can
-- provide a significant performance improvement over individual insert and
-- update operations.
--
-- If a call isn\'t part of a transaction because it doesn\'t include the
-- @transactionID@ parameter, changes that result from the call are
-- committed automatically.
module Amazonka.RDSData.BatchExecuteStatement
  ( -- * Creating a Request
    BatchExecuteStatement (..),
    newBatchExecuteStatement,

    -- * Request Lenses
    batchExecuteStatement_database,
    batchExecuteStatement_parameterSets,
    batchExecuteStatement_transactionId,
    batchExecuteStatement_schema,
    batchExecuteStatement_resourceArn,
    batchExecuteStatement_secretArn,
    batchExecuteStatement_sql,

    -- * Destructuring the Response
    BatchExecuteStatementResponse (..),
    newBatchExecuteStatementResponse,

    -- * Response Lenses
    batchExecuteStatementResponse_updateResults,
    batchExecuteStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request parameters represent the input of a SQL statement over an
-- array of data.
--
-- /See:/ 'newBatchExecuteStatement' smart constructor.
data BatchExecuteStatement = BatchExecuteStatement'
  { -- | The name of the database.
    database :: Prelude.Maybe Prelude.Text,
    -- | The parameter set for the batch operation.
    --
    -- The SQL statement is executed as many times as the number of parameter
    -- sets provided. To execute a SQL statement with no parameters, use one of
    -- the following options:
    --
    -- -   Specify one or more empty parameter sets.
    --
    -- -   Use the @ExecuteStatement@ operation instead of the
    --     @BatchExecuteStatement@ operation.
    --
    -- Array parameters are not supported.
    parameterSets :: Prelude.Maybe [[SqlParameter]],
    -- | The identifier of a transaction that was started by using the
    -- @BeginTransaction@ operation. Specify the transaction ID of the
    -- transaction that you want to include the SQL statement in.
    --
    -- If the SQL statement is not part of a transaction, don\'t set this
    -- parameter.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database schema.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
    resourceArn :: Prelude.Text,
    -- | The name or ARN of the secret that enables access to the DB cluster.
    secretArn :: Prelude.Text,
    -- | The SQL statement to run.
    sql :: Prelude.Text
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
-- 'database', 'batchExecuteStatement_database' - The name of the database.
--
-- 'parameterSets', 'batchExecuteStatement_parameterSets' - The parameter set for the batch operation.
--
-- The SQL statement is executed as many times as the number of parameter
-- sets provided. To execute a SQL statement with no parameters, use one of
-- the following options:
--
-- -   Specify one or more empty parameter sets.
--
-- -   Use the @ExecuteStatement@ operation instead of the
--     @BatchExecuteStatement@ operation.
--
-- Array parameters are not supported.
--
-- 'transactionId', 'batchExecuteStatement_transactionId' - The identifier of a transaction that was started by using the
-- @BeginTransaction@ operation. Specify the transaction ID of the
-- transaction that you want to include the SQL statement in.
--
-- If the SQL statement is not part of a transaction, don\'t set this
-- parameter.
--
-- 'schema', 'batchExecuteStatement_schema' - The name of the database schema.
--
-- 'resourceArn', 'batchExecuteStatement_resourceArn' - The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
--
-- 'secretArn', 'batchExecuteStatement_secretArn' - The name or ARN of the secret that enables access to the DB cluster.
--
-- 'sql', 'batchExecuteStatement_sql' - The SQL statement to run.
newBatchExecuteStatement ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  -- | 'sql'
  Prelude.Text ->
  BatchExecuteStatement
newBatchExecuteStatement
  pResourceArn_
  pSecretArn_
  pSql_ =
    BatchExecuteStatement'
      { database = Prelude.Nothing,
        parameterSets = Prelude.Nothing,
        transactionId = Prelude.Nothing,
        schema = Prelude.Nothing,
        resourceArn = pResourceArn_,
        secretArn = pSecretArn_,
        sql = pSql_
      }

-- | The name of the database.
batchExecuteStatement_database :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_database = Lens.lens (\BatchExecuteStatement' {database} -> database) (\s@BatchExecuteStatement' {} a -> s {database = a} :: BatchExecuteStatement)

-- | The parameter set for the batch operation.
--
-- The SQL statement is executed as many times as the number of parameter
-- sets provided. To execute a SQL statement with no parameters, use one of
-- the following options:
--
-- -   Specify one or more empty parameter sets.
--
-- -   Use the @ExecuteStatement@ operation instead of the
--     @BatchExecuteStatement@ operation.
--
-- Array parameters are not supported.
batchExecuteStatement_parameterSets :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe [[SqlParameter]])
batchExecuteStatement_parameterSets = Lens.lens (\BatchExecuteStatement' {parameterSets} -> parameterSets) (\s@BatchExecuteStatement' {} a -> s {parameterSets = a} :: BatchExecuteStatement) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of a transaction that was started by using the
-- @BeginTransaction@ operation. Specify the transaction ID of the
-- transaction that you want to include the SQL statement in.
--
-- If the SQL statement is not part of a transaction, don\'t set this
-- parameter.
batchExecuteStatement_transactionId :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_transactionId = Lens.lens (\BatchExecuteStatement' {transactionId} -> transactionId) (\s@BatchExecuteStatement' {} a -> s {transactionId = a} :: BatchExecuteStatement)

-- | The name of the database schema.
batchExecuteStatement_schema :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_schema = Lens.lens (\BatchExecuteStatement' {schema} -> schema) (\s@BatchExecuteStatement' {} a -> s {schema = a} :: BatchExecuteStatement)

-- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
batchExecuteStatement_resourceArn :: Lens.Lens' BatchExecuteStatement Prelude.Text
batchExecuteStatement_resourceArn = Lens.lens (\BatchExecuteStatement' {resourceArn} -> resourceArn) (\s@BatchExecuteStatement' {} a -> s {resourceArn = a} :: BatchExecuteStatement)

-- | The name or ARN of the secret that enables access to the DB cluster.
batchExecuteStatement_secretArn :: Lens.Lens' BatchExecuteStatement Prelude.Text
batchExecuteStatement_secretArn = Lens.lens (\BatchExecuteStatement' {secretArn} -> secretArn) (\s@BatchExecuteStatement' {} a -> s {secretArn = a} :: BatchExecuteStatement)

-- | The SQL statement to run.
batchExecuteStatement_sql :: Lens.Lens' BatchExecuteStatement Prelude.Text
batchExecuteStatement_sql = Lens.lens (\BatchExecuteStatement' {sql} -> sql) (\s@BatchExecuteStatement' {} a -> s {sql = a} :: BatchExecuteStatement)

instance Core.AWSRequest BatchExecuteStatement where
  type
    AWSResponse BatchExecuteStatement =
      BatchExecuteStatementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchExecuteStatementResponse'
            Prelude.<$> (x Core..?> "updateResults" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchExecuteStatement where
  hashWithSalt salt' BatchExecuteStatement' {..} =
    salt' `Prelude.hashWithSalt` sql
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` parameterSets
      `Prelude.hashWithSalt` database

instance Prelude.NFData BatchExecuteStatement where
  rnf BatchExecuteStatement' {..} =
    Prelude.rnf database `Prelude.seq` Prelude.rnf sql
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf parameterSets

instance Core.ToHeaders BatchExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("database" Core..=) Prelude.<$> database,
            ("parameterSets" Core..=) Prelude.<$> parameterSets,
            ("transactionId" Core..=) Prelude.<$> transactionId,
            ("schema" Core..=) Prelude.<$> schema,
            Prelude.Just ("resourceArn" Core..= resourceArn),
            Prelude.Just ("secretArn" Core..= secretArn),
            Prelude.Just ("sql" Core..= sql)
          ]
      )

instance Core.ToPath BatchExecuteStatement where
  toPath = Prelude.const "/BatchExecute"

instance Core.ToQuery BatchExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | The response elements represent the output of a SQL statement over an
-- array of data.
--
-- /See:/ 'newBatchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { -- | The execution results of each batch entry.
    updateResults :: Prelude.Maybe [UpdateResult],
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
-- 'updateResults', 'batchExecuteStatementResponse_updateResults' - The execution results of each batch entry.
--
-- 'httpStatus', 'batchExecuteStatementResponse_httpStatus' - The response's http status code.
newBatchExecuteStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchExecuteStatementResponse
newBatchExecuteStatementResponse pHttpStatus_ =
  BatchExecuteStatementResponse'
    { updateResults =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The execution results of each batch entry.
batchExecuteStatementResponse_updateResults :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe [UpdateResult])
batchExecuteStatementResponse_updateResults = Lens.lens (\BatchExecuteStatementResponse' {updateResults} -> updateResults) (\s@BatchExecuteStatementResponse' {} a -> s {updateResults = a} :: BatchExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchExecuteStatementResponse_httpStatus :: Lens.Lens' BatchExecuteStatementResponse Prelude.Int
batchExecuteStatementResponse_httpStatus = Lens.lens (\BatchExecuteStatementResponse' {httpStatus} -> httpStatus) (\s@BatchExecuteStatementResponse' {} a -> s {httpStatus = a} :: BatchExecuteStatementResponse)

instance Prelude.NFData BatchExecuteStatementResponse where
  rnf BatchExecuteStatementResponse' {..} =
    Prelude.rnf updateResults
      `Prelude.seq` Prelude.rnf httpStatus
