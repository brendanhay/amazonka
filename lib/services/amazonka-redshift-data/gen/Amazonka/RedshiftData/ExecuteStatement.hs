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
-- Module      : Amazonka.RedshiftData.ExecuteStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an SQL statement, which can be data manipulation language (DML) or
-- data definition language (DDL). This statement must be a single SQL
-- statement. Depending on the authorization method, use one of the
-- following combinations of request parameters:
--
-- -   Secrets Manager - when connecting to a cluster, provide the
--     @secret-arn@ of a secret stored in Secrets Manager which has
--     @username@ and @password@. The specified secret contains credentials
--     to connect to the @database@ you specify. When you are connecting to
--     a cluster, you also supply the database name, If you provide a
--     cluster identifier (@dbClusterIdentifier@), it must match the
--     cluster identifier stored in the secret. When you are connecting to
--     a serverless workgroup, you also supply the database name.
--
-- -   Temporary credentials - when connecting to your data warehouse,
--     choose one of the following options:
--
--     -   When connecting to a serverless workgroup, specify the workgroup
--         name and database name. The database user name is derived from
--         the IAM identity. For example, @arn:iam::123456789012:user:foo@
--         has the database user name @IAM:foo@. Also, permission to call
--         the @redshift-serverless:GetCredentials@ operation is required.
--
--     -   When connecting to a cluster as an IAM identity, specify the
--         cluster identifier and the database name. The database user name
--         is derived from the IAM identity. For example,
--         @arn:iam::123456789012:user:foo@ has the database user name
--         @IAM:foo@. Also, permission to call the
--         @redshift:GetClusterCredentialsWithIAM@ operation is required.
--
--     -   When connecting to a cluster as a database user, specify the
--         cluster identifier, the database name, and the database user
--         name. Also, permission to call the
--         @redshift:GetClusterCredentials@ operation is required.
--
-- For more information about the Amazon Redshift Data API and CLI usage
-- examples, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/data-api.html Using the Amazon Redshift Data API>
-- in the /Amazon Redshift Management Guide/.
module Amazonka.RedshiftData.ExecuteStatement
  ( -- * Creating a Request
    ExecuteStatement (..),
    newExecuteStatement,

    -- * Request Lenses
    executeStatement_clientToken,
    executeStatement_clusterIdentifier,
    executeStatement_dbUser,
    executeStatement_parameters,
    executeStatement_secretArn,
    executeStatement_statementName,
    executeStatement_withEvent,
    executeStatement_workgroupName,
    executeStatement_database,
    executeStatement_sql,

    -- * Destructuring the Response
    ExecuteStatementResponse (..),
    newExecuteStatementResponse,

    -- * Response Lenses
    executeStatementResponse_clusterIdentifier,
    executeStatementResponse_createdAt,
    executeStatementResponse_database,
    executeStatementResponse_dbUser,
    executeStatementResponse_id,
    executeStatementResponse_secretArn,
    executeStatementResponse_workgroupName,
    executeStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExecuteStatement' smart constructor.
data ExecuteStatement = ExecuteStatement'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier. This parameter is required when connecting to a
    -- cluster and authenticating using either Secrets Manager or temporary
    -- credentials.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The database user name. This parameter is required when connecting to a
    -- cluster as a database user and authenticating using temporary
    -- credentials.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the SQL statement.
    parameters :: Prelude.Maybe (Prelude.NonEmpty SqlParameter),
    -- | The name or ARN of the secret that enables access to the database. This
    -- parameter is required when authenticating using Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the SQL statement. You can name the SQL statement when you
    -- create it to identify the query.
    statementName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to send an event to the Amazon
    -- EventBridge event bus after the SQL statement runs.
    withEvent :: Prelude.Maybe Prelude.Bool,
    -- | The serverless workgroup name or Amazon Resource Name (ARN). This
    -- parameter is required when connecting to a serverless workgroup and
    -- authenticating using either Secrets Manager or temporary credentials.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the database. This parameter is required when authenticating
    -- using either Secrets Manager or temporary credentials.
    database :: Prelude.Text,
    -- | The SQL statement text to run.
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
-- 'clientToken', 'executeStatement_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'clusterIdentifier', 'executeStatement_clusterIdentifier' - The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
--
-- 'dbUser', 'executeStatement_dbUser' - The database user name. This parameter is required when connecting to a
-- cluster as a database user and authenticating using temporary
-- credentials.
--
-- 'parameters', 'executeStatement_parameters' - The parameters for the SQL statement.
--
-- 'secretArn', 'executeStatement_secretArn' - The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
--
-- 'statementName', 'executeStatement_statementName' - The name of the SQL statement. You can name the SQL statement when you
-- create it to identify the query.
--
-- 'withEvent', 'executeStatement_withEvent' - A value that indicates whether to send an event to the Amazon
-- EventBridge event bus after the SQL statement runs.
--
-- 'workgroupName', 'executeStatement_workgroupName' - The serverless workgroup name or Amazon Resource Name (ARN). This
-- parameter is required when connecting to a serverless workgroup and
-- authenticating using either Secrets Manager or temporary credentials.
--
-- 'database', 'executeStatement_database' - The name of the database. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
--
-- 'sql', 'executeStatement_sql' - The SQL statement text to run.
newExecuteStatement ::
  -- | 'database'
  Prelude.Text ->
  -- | 'sql'
  Prelude.Text ->
  ExecuteStatement
newExecuteStatement pDatabase_ pSql_ =
  ExecuteStatement'
    { clientToken = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      parameters = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      statementName = Prelude.Nothing,
      withEvent = Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      database = pDatabase_,
      sql = pSql_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
executeStatement_clientToken :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_clientToken = Lens.lens (\ExecuteStatement' {clientToken} -> clientToken) (\s@ExecuteStatement' {} a -> s {clientToken = a} :: ExecuteStatement)

-- | The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
executeStatement_clusterIdentifier :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_clusterIdentifier = Lens.lens (\ExecuteStatement' {clusterIdentifier} -> clusterIdentifier) (\s@ExecuteStatement' {} a -> s {clusterIdentifier = a} :: ExecuteStatement)

-- | The database user name. This parameter is required when connecting to a
-- cluster as a database user and authenticating using temporary
-- credentials.
executeStatement_dbUser :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_dbUser = Lens.lens (\ExecuteStatement' {dbUser} -> dbUser) (\s@ExecuteStatement' {} a -> s {dbUser = a} :: ExecuteStatement)

-- | The parameters for the SQL statement.
executeStatement_parameters :: Lens.Lens' ExecuteStatement (Prelude.Maybe (Prelude.NonEmpty SqlParameter))
executeStatement_parameters = Lens.lens (\ExecuteStatement' {parameters} -> parameters) (\s@ExecuteStatement' {} a -> s {parameters = a} :: ExecuteStatement) Prelude.. Lens.mapping Lens.coerced

-- | The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
executeStatement_secretArn :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_secretArn = Lens.lens (\ExecuteStatement' {secretArn} -> secretArn) (\s@ExecuteStatement' {} a -> s {secretArn = a} :: ExecuteStatement)

-- | The name of the SQL statement. You can name the SQL statement when you
-- create it to identify the query.
executeStatement_statementName :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_statementName = Lens.lens (\ExecuteStatement' {statementName} -> statementName) (\s@ExecuteStatement' {} a -> s {statementName = a} :: ExecuteStatement)

-- | A value that indicates whether to send an event to the Amazon
-- EventBridge event bus after the SQL statement runs.
executeStatement_withEvent :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Bool)
executeStatement_withEvent = Lens.lens (\ExecuteStatement' {withEvent} -> withEvent) (\s@ExecuteStatement' {} a -> s {withEvent = a} :: ExecuteStatement)

-- | The serverless workgroup name or Amazon Resource Name (ARN). This
-- parameter is required when connecting to a serverless workgroup and
-- authenticating using either Secrets Manager or temporary credentials.
executeStatement_workgroupName :: Lens.Lens' ExecuteStatement (Prelude.Maybe Prelude.Text)
executeStatement_workgroupName = Lens.lens (\ExecuteStatement' {workgroupName} -> workgroupName) (\s@ExecuteStatement' {} a -> s {workgroupName = a} :: ExecuteStatement)

-- | The name of the database. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
executeStatement_database :: Lens.Lens' ExecuteStatement Prelude.Text
executeStatement_database = Lens.lens (\ExecuteStatement' {database} -> database) (\s@ExecuteStatement' {} a -> s {database = a} :: ExecuteStatement)

-- | The SQL statement text to run.
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
            Prelude.<$> (x Data..?> "ClusterIdentifier")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Database")
            Prelude.<*> (x Data..?> "DbUser")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "SecretArn")
            Prelude.<*> (x Data..?> "WorkgroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteStatement where
  hashWithSalt _salt ExecuteStatement' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` statementName
      `Prelude.hashWithSalt` withEvent
      `Prelude.hashWithSalt` workgroupName
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` sql

instance Prelude.NFData ExecuteStatement where
  rnf ExecuteStatement' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf statementName
      `Prelude.seq` Prelude.rnf withEvent
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf sql

instance Data.ToHeaders ExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftData.ExecuteStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExecuteStatement where
  toJSON ExecuteStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("ClusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
            ("DbUser" Data..=) Prelude.<$> dbUser,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("SecretArn" Data..=) Prelude.<$> secretArn,
            ("StatementName" Data..=) Prelude.<$> statementName,
            ("WithEvent" Data..=) Prelude.<$> withEvent,
            ("WorkgroupName" Data..=) Prelude.<$> workgroupName,
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Sql" Data..= sql)
          ]
      )

instance Data.ToPath ExecuteStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery ExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteStatementResponse' smart constructor.
data ExecuteStatementResponse = ExecuteStatementResponse'
  { -- | The cluster identifier. This element is not returned when connecting to
    -- a serverless workgroup.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The date and time (UTC) the statement was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the database.
    database :: Prelude.Maybe Prelude.Text,
    -- | The database user name.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the SQL statement whose results are to be fetched.
    -- This value is a universally unique identifier (UUID) generated by Amazon
    -- Redshift Data API.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The serverless workgroup name or Amazon Resource Name (ARN). This
    -- element is not returned when connecting to a provisioned cluster.
    workgroupName :: Prelude.Maybe Prelude.Text,
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
-- 'clusterIdentifier', 'executeStatementResponse_clusterIdentifier' - The cluster identifier. This element is not returned when connecting to
-- a serverless workgroup.
--
-- 'createdAt', 'executeStatementResponse_createdAt' - The date and time (UTC) the statement was created.
--
-- 'database', 'executeStatementResponse_database' - The name of the database.
--
-- 'dbUser', 'executeStatementResponse_dbUser' - The database user name.
--
-- 'id', 'executeStatementResponse_id' - The identifier of the SQL statement whose results are to be fetched.
-- This value is a universally unique identifier (UUID) generated by Amazon
-- Redshift Data API.
--
-- 'secretArn', 'executeStatementResponse_secretArn' - The name or ARN of the secret that enables access to the database.
--
-- 'workgroupName', 'executeStatementResponse_workgroupName' - The serverless workgroup name or Amazon Resource Name (ARN). This
-- element is not returned when connecting to a provisioned cluster.
--
-- 'httpStatus', 'executeStatementResponse_httpStatus' - The response's http status code.
newExecuteStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExecuteStatementResponse
newExecuteStatementResponse pHttpStatus_ =
  ExecuteStatementResponse'
    { clusterIdentifier =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      database = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      id = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cluster identifier. This element is not returned when connecting to
-- a serverless workgroup.
executeStatementResponse_clusterIdentifier :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.Text)
executeStatementResponse_clusterIdentifier = Lens.lens (\ExecuteStatementResponse' {clusterIdentifier} -> clusterIdentifier) (\s@ExecuteStatementResponse' {} a -> s {clusterIdentifier = a} :: ExecuteStatementResponse)

-- | The date and time (UTC) the statement was created.
executeStatementResponse_createdAt :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.UTCTime)
executeStatementResponse_createdAt = Lens.lens (\ExecuteStatementResponse' {createdAt} -> createdAt) (\s@ExecuteStatementResponse' {} a -> s {createdAt = a} :: ExecuteStatementResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the database.
executeStatementResponse_database :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.Text)
executeStatementResponse_database = Lens.lens (\ExecuteStatementResponse' {database} -> database) (\s@ExecuteStatementResponse' {} a -> s {database = a} :: ExecuteStatementResponse)

-- | The database user name.
executeStatementResponse_dbUser :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.Text)
executeStatementResponse_dbUser = Lens.lens (\ExecuteStatementResponse' {dbUser} -> dbUser) (\s@ExecuteStatementResponse' {} a -> s {dbUser = a} :: ExecuteStatementResponse)

-- | The identifier of the SQL statement whose results are to be fetched.
-- This value is a universally unique identifier (UUID) generated by Amazon
-- Redshift Data API.
executeStatementResponse_id :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.Text)
executeStatementResponse_id = Lens.lens (\ExecuteStatementResponse' {id} -> id) (\s@ExecuteStatementResponse' {} a -> s {id = a} :: ExecuteStatementResponse)

-- | The name or ARN of the secret that enables access to the database.
executeStatementResponse_secretArn :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.Text)
executeStatementResponse_secretArn = Lens.lens (\ExecuteStatementResponse' {secretArn} -> secretArn) (\s@ExecuteStatementResponse' {} a -> s {secretArn = a} :: ExecuteStatementResponse)

-- | The serverless workgroup name or Amazon Resource Name (ARN). This
-- element is not returned when connecting to a provisioned cluster.
executeStatementResponse_workgroupName :: Lens.Lens' ExecuteStatementResponse (Prelude.Maybe Prelude.Text)
executeStatementResponse_workgroupName = Lens.lens (\ExecuteStatementResponse' {workgroupName} -> workgroupName) (\s@ExecuteStatementResponse' {} a -> s {workgroupName = a} :: ExecuteStatementResponse)

-- | The response's http status code.
executeStatementResponse_httpStatus :: Lens.Lens' ExecuteStatementResponse Prelude.Int
executeStatementResponse_httpStatus = Lens.lens (\ExecuteStatementResponse' {httpStatus} -> httpStatus) (\s@ExecuteStatementResponse' {} a -> s {httpStatus = a} :: ExecuteStatementResponse)

instance Prelude.NFData ExecuteStatementResponse where
  rnf ExecuteStatementResponse' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf httpStatus
