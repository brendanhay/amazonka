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
-- Module      : Amazonka.RDSData.ExecuteStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs a SQL statement against a database.
--
-- If a call isn\'t part of a transaction because it doesn\'t include the
-- @transactionID@ parameter, changes that result from the call are
-- committed automatically.
--
-- If the binary response data from the database is more than 1 MB, the
-- call is terminated.
module Amazonka.RDSData.ExecuteStatement
  ( -- * Creating a Request
    ExecuteStatement (..),
    newExecuteStatement,

    -- * Request Lenses
    executeStatement_continueAfterTimeout,
    executeStatement_database,
    executeStatement_formatRecordsAs,
    executeStatement_includeResultMetadata,
    executeStatement_parameters,
    executeStatement_resultSetOptions,
    executeStatement_schema,
    executeStatement_transactionId,
    executeStatement_resourceArn,
    executeStatement_secretArn,
    executeStatement_sql,

    -- * Destructuring the Response
    ExecuteStatementResponse (..),
    newExecuteStatementResponse,

    -- * Response Lenses
    executeStatementResponse_columnMetadata,
    executeStatementResponse_formattedRecords,
    executeStatementResponse_generatedFields,
    executeStatementResponse_numberOfRecordsUpdated,
    executeStatementResponse_records,
    executeStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request parameters represent the input of a request to run a SQL
-- statement against a database.
--
-- /See:/ 'newExecuteStatement' smart constructor.
data ExecuteStatement = ExecuteStatement'
  { -- | A value that indicates whether to continue running the statement after
    -- the call times out. By default, the statement stops running when the
    -- call times out.
    --
    -- For DDL statements, we recommend continuing to run the statement after
    -- the call times out. When a DDL statement terminates before it is
    -- finished running, it can result in errors and possibly corrupted data
    -- structures.
    continueAfterTimeout :: Prelude.Maybe Prelude.Bool,
    -- | The name of the database.
    database :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to format the result set as a single JSON
    -- string. This parameter only applies to @SELECT@ statements and is
    -- ignored for other types of statements. Allowed values are @NONE@ and
    -- @JSON@. The default value is @NONE@. The result is returned in the
    -- @formattedRecords@ field.
    --
    -- For usage information about the JSON format for result sets, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API>
    -- in the /Amazon Aurora User Guide/.
    formatRecordsAs :: Prelude.Maybe RecordsFormatType,
    -- | A value that indicates whether to include metadata in the results.
    includeResultMetadata :: Prelude.Maybe Prelude.Bool,
    -- | The parameters for the SQL statement.
    --
    -- Array parameters are not supported.
    parameters :: Prelude.Maybe [SqlParameter],
    -- | Options that control how the result set is returned.
    resultSetOptions :: Prelude.Maybe ResultSetOptions,
    -- | The name of the database schema.
    --
    -- Currently, the @schema@ parameter isn\'t supported.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a transaction that was started by using the
    -- @BeginTransaction@ operation. Specify the transaction ID of the
    -- transaction that you want to include the SQL statement in.
    --
    -- If the SQL statement is not part of a transaction, don\'t set this
    -- parameter.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
    resourceArn :: Prelude.Text,
    -- | The ARN of the secret that enables access to the DB cluster. Enter the
    -- database user name and password for the credentials in the secret.
    --
    -- For information about creating the secret, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/create_database_secret.html Create a database secret>.
    secretArn :: Prelude.Text,
    -- | The SQL statement to run.
    sql :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continueAfterTimeout', 'executeStatement_continueAfterTimeout' - A value that indicates whether to continue running the statement after
-- the call times out. By default, the statement stops running when the
-- call times out.
--
-- For DDL statements, we recommend continuing to run the statement after
-- the call times out. When a DDL statement terminates before it is
-- finished running, it can result in errors and possibly corrupted data
-- structures.
--
-- 'database', 'executeStatement_database' - The name of the database.
--
-- 'formatRecordsAs', 'executeStatement_formatRecordsAs' - A value that indicates whether to format the result set as a single JSON
-- string. This parameter only applies to @SELECT@ statements and is
-- ignored for other types of statements. Allowed values are @NONE@ and
-- @JSON@. The default value is @NONE@. The result is returned in the
-- @formattedRecords@ field.
--
-- For usage information about the JSON format for result sets, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API>
-- in the /Amazon Aurora User Guide/.
--
-- 'includeResultMetadata', 'executeStatement_includeResultMetadata' - A value that indicates whether to include metadata in the results.
--
-- 'parameters', 'executeStatement_parameters' - The parameters for the SQL statement.
--
-- Array parameters are not supported.
--
-- 'resultSetOptions', 'executeStatement_resultSetOptions' - Options that control how the result set is returned.
--
-- 'schema', 'executeStatement_schema' - The name of the database schema.
--
-- Currently, the @schema@ parameter isn\'t supported.
--
-- 'transactionId', 'executeStatement_transactionId' - The identifier of a transaction that was started by using the
-- @BeginTransaction@ operation. Specify the transaction ID of the
-- transaction that you want to include the SQL statement in.
--
-- If the SQL statement is not part of a transaction, don\'t set this
-- parameter.
--
-- 'resourceArn', 'executeStatement_resourceArn' - The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
--
-- 'secretArn', 'executeStatement_secretArn' - The ARN of the secret that enables access to the DB cluster. Enter the
-- database user name and password for the credentials in the secret.
--
-- For information about creating the secret, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/create_database_secret.html Create a database secret>.
--
-- 'sql', 'executeStatement_sql' - The SQL statement to run.
newExecuteStatement ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  -- | 'sql'
  Prelude.Text ->
  ExecuteStatement
newExecuteStatement pResourceArn_ pSecretArn_ pSql_ =
  ExecuteStatement'
    { continueAfterTimeout =
        Prelude.Nothing,
      database = Prelude.Nothing,
      formatRecordsAs = Prelude.Nothing,
      includeResultMetadata = Prelude.Nothing,
      parameters = Prelude.Nothing,
      resultSetOptions = Prelude.Nothing,
      schema = Prelude.Nothing,
      transactionId = Prelude.Nothing,
      resourceArn = pResourceArn_,
      secretArn = pSecretArn_,
      sql = pSql_
    }

-- | A value that indicates whether to continue running the statement after
-- the call times out. By default, the statement stops running when the
-- call times out.
--
-- For DDL statements, we recommend continuing to run the statement after
-- the call times out. When a DDL statement terminates before it is
-- finished running, it can result in errors and possibly corrupted data
-- structures.
executeStatement_continueAfterTimeout :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Bool)
executeStatement_continueAfterTimeout = Lens.lens (\ExecuteStatement' {continueAfterTimeout} -> continueAfterTimeout) (\s@ExecuteStatement' {} a -> s {continueAfterTimeout = a} :: ExecuteStatement)

-- | The name of the database.
executeStatement_database :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_database = Lens.lens (\ExecuteStatement' {database} -> database) (\s@ExecuteStatement' {} a -> s {database = a} :: ExecuteStatement)

-- | A value that indicates whether to format the result set as a single JSON
-- string. This parameter only applies to @SELECT@ statements and is
-- ignored for other types of statements. Allowed values are @NONE@ and
-- @JSON@. The default value is @NONE@. The result is returned in the
-- @formattedRecords@ field.
--
-- For usage information about the JSON format for result sets, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API>
-- in the /Amazon Aurora User Guide/.
executeStatement_formatRecordsAs :: Lens.Lens' ExecuteStatement (Prelude.Maybe RecordsFormatType)
executeStatement_formatRecordsAs = Lens.lens (\ExecuteStatement' {formatRecordsAs} -> formatRecordsAs) (\s@ExecuteStatement' {} a -> s {formatRecordsAs = a} :: ExecuteStatement)

-- | A value that indicates whether to include metadata in the results.
executeStatement_includeResultMetadata :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Bool)
executeStatement_includeResultMetadata = Lens.lens (\ExecuteStatement' {includeResultMetadata} -> includeResultMetadata) (\s@ExecuteStatement' {} a -> s {includeResultMetadata = a} :: ExecuteStatement)

-- | The parameters for the SQL statement.
--
-- Array parameters are not supported.
executeStatement_parameters :: Lens.Lens' ExecuteStatement (Prelude.Maybe [SqlParameter])
executeStatement_parameters = Lens.lens (\ExecuteStatement' {parameters} -> parameters) (\s@ExecuteStatement' {} a -> s {parameters = a} :: ExecuteStatement) Prelude.. Lens.mapping Lens.coerced

-- | Options that control how the result set is returned.
executeStatement_resultSetOptions :: Lens.Lens' ExecuteStatement (Prelude.Maybe ResultSetOptions)
executeStatement_resultSetOptions = Lens.lens (\ExecuteStatement' {resultSetOptions} -> resultSetOptions) (\s@ExecuteStatement' {} a -> s {resultSetOptions = a} :: ExecuteStatement)

-- | The name of the database schema.
--
-- Currently, the @schema@ parameter isn\'t supported.
executeStatement_schema :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_schema = Lens.lens (\ExecuteStatement' {schema} -> schema) (\s@ExecuteStatement' {} a -> s {schema = a} :: ExecuteStatement)

-- | The identifier of a transaction that was started by using the
-- @BeginTransaction@ operation. Specify the transaction ID of the
-- transaction that you want to include the SQL statement in.
--
-- If the SQL statement is not part of a transaction, don\'t set this
-- parameter.
executeStatement_transactionId :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_transactionId = Lens.lens (\ExecuteStatement' {transactionId} -> transactionId) (\s@ExecuteStatement' {} a -> s {transactionId = a} :: ExecuteStatement)

-- | The Amazon Resource Name (ARN) of the Aurora Serverless DB cluster.
executeStatement_resourceArn :: Lens.Lens' ExecuteStatement Prelude.Text
executeStatement_resourceArn = Lens.lens (\ExecuteStatement' {resourceArn} -> resourceArn) (\s@ExecuteStatement' {} a -> s {resourceArn = a} :: ExecuteStatement)

-- | The ARN of the secret that enables access to the DB cluster. Enter the
-- database user name and password for the credentials in the secret.
--
-- For information about creating the secret, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/create_database_secret.html Create a database secret>.
executeStatement_secretArn :: Lens.Lens' ExecuteStatement Prelude.Text
executeStatement_secretArn = Lens.lens (\ExecuteStatement' {secretArn} -> secretArn) (\s@ExecuteStatement' {} a -> s {secretArn = a} :: ExecuteStatement)

-- | The SQL statement to run.
executeStatement_sql :: Lens.Lens' ExecuteStatement Prelude.Text
executeStatement_sql = Lens.lens (\ExecuteStatement' {sql} -> sql) (\s@ExecuteStatement' {} a -> s {sql = a} :: ExecuteStatement)

instance Core.AWSRequest ExecuteStatement where
  type
    AWSResponse ExecuteStatement =
      ExecuteStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteStatementResponse'
            Prelude.<$> (x Data..?> "columnMetadata" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "formattedRecords")
            Prelude.<*> ( x
                            Data..?> "generatedFields"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "numberOfRecordsUpdated")
            Prelude.<*> (x Data..?> "records" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteStatement where
  hashWithSalt _salt ExecuteStatement' {..} =
    _salt
      `Prelude.hashWithSalt` continueAfterTimeout
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` formatRecordsAs
      `Prelude.hashWithSalt` includeResultMetadata
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` resultSetOptions
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` sql

instance Prelude.NFData ExecuteStatement where
  rnf ExecuteStatement' {..} =
    Prelude.rnf continueAfterTimeout
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf formatRecordsAs
      `Prelude.seq` Prelude.rnf includeResultMetadata
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf resultSetOptions
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf sql

instance Data.ToHeaders ExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExecuteStatement where
  toJSON ExecuteStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("continueAfterTimeout" Data..=)
              Prelude.<$> continueAfterTimeout,
            ("database" Data..=) Prelude.<$> database,
            ("formatRecordsAs" Data..=)
              Prelude.<$> formatRecordsAs,
            ("includeResultMetadata" Data..=)
              Prelude.<$> includeResultMetadata,
            ("parameters" Data..=) Prelude.<$> parameters,
            ("resultSetOptions" Data..=)
              Prelude.<$> resultSetOptions,
            ("schema" Data..=) Prelude.<$> schema,
            ("transactionId" Data..=) Prelude.<$> transactionId,
            Prelude.Just ("resourceArn" Data..= resourceArn),
            Prelude.Just ("secretArn" Data..= secretArn),
            Prelude.Just ("sql" Data..= sql)
          ]
      )

instance Data.ToPath ExecuteStatement where
  toPath = Prelude.const "/Execute"

instance Data.ToQuery ExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | The response elements represent the output of a request to run a SQL
-- statement against a database.
--
-- /See:/ 'newExecuteStatementResponse' smart constructor.
data ExecuteStatementResponse = ExecuteStatementResponse'
  { -- | Metadata for the columns included in the results. This field is blank if
    -- the @formatRecordsAs@ parameter is set to @JSON@.
    columnMetadata :: Prelude.Maybe [ColumnMetadata],
    -- | A string value that represents the result set of a @SELECT@ statement in
    -- JSON format. This value is only present when the @formatRecordsAs@
    -- parameter is set to @JSON@.
    --
    -- The size limit for this field is currently 10 MB. If the JSON-formatted
    -- string representing the result set requires more than 10 MB, the call
    -- returns an error.
    formattedRecords :: Prelude.Maybe Prelude.Text,
    -- | Values for fields generated during a DML request.
    --
    -- >  <note> <p>The <code>generatedFields</code> data isn't supported by Aurora PostgreSQL. To get the values of generated fields, use the <code>RETURNING</code> clause. For more information, see <a href="https://www.postgresql.org/docs/10/dml-returning.html">Returning Data From Modified Rows</a> in the PostgreSQL documentation.</p> </note>
    generatedFields :: Prelude.Maybe [Field],
    -- | The number of records updated by the request.
    numberOfRecordsUpdated :: Prelude.Maybe Prelude.Integer,
    -- | The records returned by the SQL statement. This field is blank if the
    -- @formatRecordsAs@ parameter is set to @JSON@.
    records :: Prelude.Maybe [[Field]],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnMetadata', 'executeStatementResponse_columnMetadata' - Metadata for the columns included in the results. This field is blank if
-- the @formatRecordsAs@ parameter is set to @JSON@.
--
-- 'formattedRecords', 'executeStatementResponse_formattedRecords' - A string value that represents the result set of a @SELECT@ statement in
-- JSON format. This value is only present when the @formatRecordsAs@
-- parameter is set to @JSON@.
--
-- The size limit for this field is currently 10 MB. If the JSON-formatted
-- string representing the result set requires more than 10 MB, the call
-- returns an error.
--
-- 'generatedFields', 'executeStatementResponse_generatedFields' - Values for fields generated during a DML request.
--
-- >  <note> <p>The <code>generatedFields</code> data isn't supported by Aurora PostgreSQL. To get the values of generated fields, use the <code>RETURNING</code> clause. For more information, see <a href="https://www.postgresql.org/docs/10/dml-returning.html">Returning Data From Modified Rows</a> in the PostgreSQL documentation.</p> </note>
--
-- 'numberOfRecordsUpdated', 'executeStatementResponse_numberOfRecordsUpdated' - The number of records updated by the request.
--
-- 'records', 'executeStatementResponse_records' - The records returned by the SQL statement. This field is blank if the
-- @formatRecordsAs@ parameter is set to @JSON@.
--
-- 'httpStatus', 'executeStatementResponse_httpStatus' - The response's http status code.
newExecuteStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExecuteStatementResponse
newExecuteStatementResponse pHttpStatus_ =
  ExecuteStatementResponse'
    { columnMetadata =
        Prelude.Nothing,
      formattedRecords = Prelude.Nothing,
      generatedFields = Prelude.Nothing,
      numberOfRecordsUpdated = Prelude.Nothing,
      records = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata for the columns included in the results. This field is blank if
-- the @formatRecordsAs@ parameter is set to @JSON@.
executeStatementResponse_columnMetadata :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe [ColumnMetadata])
executeStatementResponse_columnMetadata = Lens.lens (\ExecuteStatementResponse' {columnMetadata} -> columnMetadata) (\s@ExecuteStatementResponse' {} a -> s {columnMetadata = a} :: ExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | A string value that represents the result set of a @SELECT@ statement in
-- JSON format. This value is only present when the @formatRecordsAs@
-- parameter is set to @JSON@.
--
-- The size limit for this field is currently 10 MB. If the JSON-formatted
-- string representing the result set requires more than 10 MB, the call
-- returns an error.
executeStatementResponse_formattedRecords :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.Text)
executeStatementResponse_formattedRecords = Lens.lens (\ExecuteStatementResponse' {formattedRecords} -> formattedRecords) (\s@ExecuteStatementResponse' {} a -> s {formattedRecords = a} :: ExecuteStatementResponse)

-- | Values for fields generated during a DML request.
--
-- >  <note> <p>The <code>generatedFields</code> data isn't supported by Aurora PostgreSQL. To get the values of generated fields, use the <code>RETURNING</code> clause. For more information, see <a href="https://www.postgresql.org/docs/10/dml-returning.html">Returning Data From Modified Rows</a> in the PostgreSQL documentation.</p> </note>
executeStatementResponse_generatedFields :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe [Field])
executeStatementResponse_generatedFields = Lens.lens (\ExecuteStatementResponse' {generatedFields} -> generatedFields) (\s@ExecuteStatementResponse' {} a -> s {generatedFields = a} :: ExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of records updated by the request.
executeStatementResponse_numberOfRecordsUpdated :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.Integer)
executeStatementResponse_numberOfRecordsUpdated = Lens.lens (\ExecuteStatementResponse' {numberOfRecordsUpdated} -> numberOfRecordsUpdated) (\s@ExecuteStatementResponse' {} a -> s {numberOfRecordsUpdated = a} :: ExecuteStatementResponse)

-- | The records returned by the SQL statement. This field is blank if the
-- @formatRecordsAs@ parameter is set to @JSON@.
executeStatementResponse_records :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe [[Field]])
executeStatementResponse_records = Lens.lens (\ExecuteStatementResponse' {records} -> records) (\s@ExecuteStatementResponse' {} a -> s {records = a} :: ExecuteStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
executeStatementResponse_httpStatus :: Lens.Lens' ExecuteStatementResponse Prelude.Int
executeStatementResponse_httpStatus = Lens.lens (\ExecuteStatementResponse' {httpStatus} -> httpStatus) (\s@ExecuteStatementResponse' {} a -> s {httpStatus = a} :: ExecuteStatementResponse)

instance Prelude.NFData ExecuteStatementResponse where
  rnf ExecuteStatementResponse' {..} =
    Prelude.rnf columnMetadata
      `Prelude.seq` Prelude.rnf formattedRecords
      `Prelude.seq` Prelude.rnf generatedFields
      `Prelude.seq` Prelude.rnf numberOfRecordsUpdated
      `Prelude.seq` Prelude.rnf records
      `Prelude.seq` Prelude.rnf httpStatus
