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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--
-- There isn\'t a fixed upper limit on the number of parameter sets.
-- However, the maximum size of the HTTP request submitted through the Data
-- API is 4 MiB. If the request exceeds this limit, the Data API returns an
-- error and doesn\'t process the request. This 4-MiB limit includes the
-- size of the HTTP headers and the JSON notation in the request. Thus, the
-- number of parameter sets that you can include depends on a combination
-- of factors, such as the size of the SQL statement and the size of each
-- parameter set.
--
-- The response size limit is 1 MiB. If the call returns more than 1 MiB of
-- response data, the call is terminated.
module Amazonka.RDSData.BatchExecuteStatement
  ( -- * Creating a Request
    BatchExecuteStatement (..),
    newBatchExecuteStatement,

    -- * Request Lenses
    batchExecuteStatement_parameterSets,
    batchExecuteStatement_database,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request parameters represent the input of a SQL statement over an
-- array of data.
--
-- /See:/ 'newBatchExecuteStatement' smart constructor.
data BatchExecuteStatement = BatchExecuteStatement'
  { -- | The parameter set for the batch operation.
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
    -- | The name of the database.
    database :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a transaction that was started by using the
    -- @BeginTransaction@ operation. Specify the transaction ID of the
    -- transaction that you want to include the SQL statement in.
    --
    -- If the SQL statement is not part of a transaction, don\'t set this
    -- parameter.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database schema.
    --
    -- Currently, the @schema@ parameter isn\'t supported.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
    resourceArn :: Prelude.Text,
    -- | The ARN of the secret that enables access to the DB cluster. Enter the
    -- database user name and password for the credentials in the secret.
    --
    -- For information about creating the secret, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/create_database_secret.html Create a database secret>.
    secretArn :: Prelude.Text,
    -- | The SQL statement to run. Don\'t include a semicolon (;) at the end of
    -- the SQL statement.
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
-- 'database', 'batchExecuteStatement_database' - The name of the database.
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
-- Currently, the @schema@ parameter isn\'t supported.
--
-- 'resourceArn', 'batchExecuteStatement_resourceArn' - The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
--
-- 'secretArn', 'batchExecuteStatement_secretArn' - The ARN of the secret that enables access to the DB cluster. Enter the
-- database user name and password for the credentials in the secret.
--
-- For information about creating the secret, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/create_database_secret.html Create a database secret>.
--
-- 'sql', 'batchExecuteStatement_sql' - The SQL statement to run. Don\'t include a semicolon (;) at the end of
-- the SQL statement.
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
      { parameterSets =
          Prelude.Nothing,
        database = Prelude.Nothing,
        transactionId = Prelude.Nothing,
        schema = Prelude.Nothing,
        resourceArn = pResourceArn_,
        secretArn = pSecretArn_,
        sql = pSql_
      }

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

-- | The name of the database.
batchExecuteStatement_database :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_database = Lens.lens (\BatchExecuteStatement' {database} -> database) (\s@BatchExecuteStatement' {} a -> s {database = a} :: BatchExecuteStatement)

-- | The identifier of a transaction that was started by using the
-- @BeginTransaction@ operation. Specify the transaction ID of the
-- transaction that you want to include the SQL statement in.
--
-- If the SQL statement is not part of a transaction, don\'t set this
-- parameter.
batchExecuteStatement_transactionId :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_transactionId = Lens.lens (\BatchExecuteStatement' {transactionId} -> transactionId) (\s@BatchExecuteStatement' {} a -> s {transactionId = a} :: BatchExecuteStatement)

-- | The name of the database schema.
--
-- Currently, the @schema@ parameter isn\'t supported.
batchExecuteStatement_schema :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_schema = Lens.lens (\BatchExecuteStatement' {schema} -> schema) (\s@BatchExecuteStatement' {} a -> s {schema = a} :: BatchExecuteStatement)

-- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
batchExecuteStatement_resourceArn :: Lens.Lens' BatchExecuteStatement Prelude.Text
batchExecuteStatement_resourceArn = Lens.lens (\BatchExecuteStatement' {resourceArn} -> resourceArn) (\s@BatchExecuteStatement' {} a -> s {resourceArn = a} :: BatchExecuteStatement)

-- | The ARN of the secret that enables access to the DB cluster. Enter the
-- database user name and password for the credentials in the secret.
--
-- For information about creating the secret, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/create_database_secret.html Create a database secret>.
batchExecuteStatement_secretArn :: Lens.Lens' BatchExecuteStatement Prelude.Text
batchExecuteStatement_secretArn = Lens.lens (\BatchExecuteStatement' {secretArn} -> secretArn) (\s@BatchExecuteStatement' {} a -> s {secretArn = a} :: BatchExecuteStatement)

-- | The SQL statement to run. Don\'t include a semicolon (;) at the end of
-- the SQL statement.
batchExecuteStatement_sql :: Lens.Lens' BatchExecuteStatement Prelude.Text
batchExecuteStatement_sql = Lens.lens (\BatchExecuteStatement' {sql} -> sql) (\s@BatchExecuteStatement' {} a -> s {sql = a} :: BatchExecuteStatement)

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
            Prelude.<$> (x Data..?> "updateResults" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchExecuteStatement where
  hashWithSalt _salt BatchExecuteStatement' {..} =
    _salt `Prelude.hashWithSalt` parameterSets
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` sql

instance Prelude.NFData BatchExecuteStatement where
  rnf BatchExecuteStatement' {..} =
    Prelude.rnf parameterSets
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf sql

instance Data.ToHeaders BatchExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("parameterSets" Data..=) Prelude.<$> parameterSets,
            ("database" Data..=) Prelude.<$> database,
            ("transactionId" Data..=) Prelude.<$> transactionId,
            ("schema" Data..=) Prelude.<$> schema,
            Prelude.Just ("resourceArn" Data..= resourceArn),
            Prelude.Just ("secretArn" Data..= secretArn),
            Prelude.Just ("sql" Data..= sql)
          ]
      )

instance Data.ToPath BatchExecuteStatement where
  toPath = Prelude.const "/BatchExecute"

instance Data.ToQuery BatchExecuteStatement where
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
