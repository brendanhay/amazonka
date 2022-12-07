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
-- Module      : Amazonka.RedshiftData.BatchExecuteStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs one or more SQL statements, which can be data manipulation language
-- (DML) or data definition language (DDL). Depending on the authorization
-- method, use one of the following combinations of request parameters:
--
-- -   Secrets Manager - when connecting to a cluster, specify the Amazon
--     Resource Name (ARN) of the secret, the database name, and the
--     cluster identifier that matches the cluster in the secret. When
--     connecting to a serverless workgroup, specify the Amazon Resource
--     Name (ARN) of the secret and the database name.
--
-- -   Temporary credentials - when connecting to a cluster, specify the
--     cluster identifier, the database name, and the database user name.
--     Also, permission to call the @redshift:GetClusterCredentials@
--     operation is required. When connecting to a serverless workgroup,
--     specify the workgroup name and database name. Also, permission to
--     call the @redshift-serverless:GetCredentials@ operation is required.
module Amazonka.RedshiftData.BatchExecuteStatement
  ( -- * Creating a Request
    BatchExecuteStatement (..),
    newBatchExecuteStatement,

    -- * Request Lenses
    batchExecuteStatement_clusterIdentifier,
    batchExecuteStatement_workgroupName,
    batchExecuteStatement_secretArn,
    batchExecuteStatement_statementName,
    batchExecuteStatement_withEvent,
    batchExecuteStatement_dbUser,
    batchExecuteStatement_database,
    batchExecuteStatement_sqls,

    -- * Destructuring the Response
    BatchExecuteStatementResponse (..),
    newBatchExecuteStatementResponse,

    -- * Response Lenses
    batchExecuteStatementResponse_clusterIdentifier,
    batchExecuteStatementResponse_workgroupName,
    batchExecuteStatementResponse_id,
    batchExecuteStatementResponse_database,
    batchExecuteStatementResponse_secretArn,
    batchExecuteStatementResponse_dbUser,
    batchExecuteStatementResponse_createdAt,
    batchExecuteStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchExecuteStatement' smart constructor.
data BatchExecuteStatement = BatchExecuteStatement'
  { -- | The cluster identifier. This parameter is required when connecting to a
    -- cluster and authenticating using either Secrets Manager or temporary
    -- credentials.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The serverless workgroup name. This parameter is required when
    -- connecting to a serverless workgroup and authenticating using either
    -- Secrets Manager or temporary credentials.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database. This
    -- parameter is required when authenticating using Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the SQL statements. You can name the SQL statements when you
    -- create them to identify the query.
    statementName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to send an event to the Amazon
    -- EventBridge event bus after the SQL statements run.
    withEvent :: Prelude.Maybe Prelude.Bool,
    -- | The database user name. This parameter is required when connecting to a
    -- cluster and authenticating using temporary credentials.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The name of the database. This parameter is required when authenticating
    -- using either Secrets Manager or temporary credentials.
    database :: Prelude.Text,
    -- | One or more SQL statements to run.
    sqls :: Prelude.NonEmpty Prelude.Text
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
-- 'clusterIdentifier', 'batchExecuteStatement_clusterIdentifier' - The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
--
-- 'workgroupName', 'batchExecuteStatement_workgroupName' - The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
--
-- 'secretArn', 'batchExecuteStatement_secretArn' - The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
--
-- 'statementName', 'batchExecuteStatement_statementName' - The name of the SQL statements. You can name the SQL statements when you
-- create them to identify the query.
--
-- 'withEvent', 'batchExecuteStatement_withEvent' - A value that indicates whether to send an event to the Amazon
-- EventBridge event bus after the SQL statements run.
--
-- 'dbUser', 'batchExecuteStatement_dbUser' - The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
--
-- 'database', 'batchExecuteStatement_database' - The name of the database. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
--
-- 'sqls', 'batchExecuteStatement_sqls' - One or more SQL statements to run.
newBatchExecuteStatement ::
  -- | 'database'
  Prelude.Text ->
  -- | 'sqls'
  Prelude.NonEmpty Prelude.Text ->
  BatchExecuteStatement
newBatchExecuteStatement pDatabase_ pSqls_ =
  BatchExecuteStatement'
    { clusterIdentifier =
        Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      statementName = Prelude.Nothing,
      withEvent = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      database = pDatabase_,
      sqls = Lens.coerced Lens.# pSqls_
    }

-- | The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
batchExecuteStatement_clusterIdentifier :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_clusterIdentifier = Lens.lens (\BatchExecuteStatement' {clusterIdentifier} -> clusterIdentifier) (\s@BatchExecuteStatement' {} a -> s {clusterIdentifier = a} :: BatchExecuteStatement)

-- | The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
batchExecuteStatement_workgroupName :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_workgroupName = Lens.lens (\BatchExecuteStatement' {workgroupName} -> workgroupName) (\s@BatchExecuteStatement' {} a -> s {workgroupName = a} :: BatchExecuteStatement)

-- | The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
batchExecuteStatement_secretArn :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_secretArn = Lens.lens (\BatchExecuteStatement' {secretArn} -> secretArn) (\s@BatchExecuteStatement' {} a -> s {secretArn = a} :: BatchExecuteStatement)

-- | The name of the SQL statements. You can name the SQL statements when you
-- create them to identify the query.
batchExecuteStatement_statementName :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_statementName = Lens.lens (\BatchExecuteStatement' {statementName} -> statementName) (\s@BatchExecuteStatement' {} a -> s {statementName = a} :: BatchExecuteStatement)

-- | A value that indicates whether to send an event to the Amazon
-- EventBridge event bus after the SQL statements run.
batchExecuteStatement_withEvent :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Bool)
batchExecuteStatement_withEvent = Lens.lens (\BatchExecuteStatement' {withEvent} -> withEvent) (\s@BatchExecuteStatement' {} a -> s {withEvent = a} :: BatchExecuteStatement)

-- | The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
batchExecuteStatement_dbUser :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_dbUser = Lens.lens (\BatchExecuteStatement' {dbUser} -> dbUser) (\s@BatchExecuteStatement' {} a -> s {dbUser = a} :: BatchExecuteStatement)

-- | The name of the database. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
batchExecuteStatement_database :: Lens.Lens' BatchExecuteStatement Prelude.Text
batchExecuteStatement_database = Lens.lens (\BatchExecuteStatement' {database} -> database) (\s@BatchExecuteStatement' {} a -> s {database = a} :: BatchExecuteStatement)

-- | One or more SQL statements to run.
batchExecuteStatement_sqls :: Lens.Lens' BatchExecuteStatement (Prelude.NonEmpty Prelude.Text)
batchExecuteStatement_sqls = Lens.lens (\BatchExecuteStatement' {sqls} -> sqls) (\s@BatchExecuteStatement' {} a -> s {sqls = a} :: BatchExecuteStatement) Prelude.. Lens.coerced

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
            Prelude.<$> (x Data..?> "ClusterIdentifier")
            Prelude.<*> (x Data..?> "WorkgroupName")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Database")
            Prelude.<*> (x Data..?> "SecretArn")
            Prelude.<*> (x Data..?> "DbUser")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchExecuteStatement where
  hashWithSalt _salt BatchExecuteStatement' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` workgroupName
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` statementName
      `Prelude.hashWithSalt` withEvent
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` sqls

instance Prelude.NFData BatchExecuteStatement where
  rnf BatchExecuteStatement' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf statementName
      `Prelude.seq` Prelude.rnf withEvent
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf sqls

instance Data.ToHeaders BatchExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftData.BatchExecuteStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
            ("WorkgroupName" Data..=) Prelude.<$> workgroupName,
            ("SecretArn" Data..=) Prelude.<$> secretArn,
            ("StatementName" Data..=) Prelude.<$> statementName,
            ("WithEvent" Data..=) Prelude.<$> withEvent,
            ("DbUser" Data..=) Prelude.<$> dbUser,
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Sqls" Data..= sqls)
          ]
      )

instance Data.ToPath BatchExecuteStatement where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { -- | The cluster identifier. This element is not returned when connecting to
    -- a serverless workgroup.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The serverless workgroup name. This element is not returned when
    -- connecting to a provisioned cluster.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the SQL statement whose results are to be fetched.
    -- This value is a universally unique identifier (UUID) generated by Amazon
    -- Redshift Data API. This identifier is returned by
    -- @BatchExecuteStatment@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the database.
    database :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The database user name.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The date and time (UTC) the statement was created.
    createdAt :: Prelude.Maybe Data.POSIX,
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
-- 'clusterIdentifier', 'batchExecuteStatementResponse_clusterIdentifier' - The cluster identifier. This element is not returned when connecting to
-- a serverless workgroup.
--
-- 'workgroupName', 'batchExecuteStatementResponse_workgroupName' - The serverless workgroup name. This element is not returned when
-- connecting to a provisioned cluster.
--
-- 'id', 'batchExecuteStatementResponse_id' - The identifier of the SQL statement whose results are to be fetched.
-- This value is a universally unique identifier (UUID) generated by Amazon
-- Redshift Data API. This identifier is returned by
-- @BatchExecuteStatment@.
--
-- 'database', 'batchExecuteStatementResponse_database' - The name of the database.
--
-- 'secretArn', 'batchExecuteStatementResponse_secretArn' - The name or ARN of the secret that enables access to the database.
--
-- 'dbUser', 'batchExecuteStatementResponse_dbUser' - The database user name.
--
-- 'createdAt', 'batchExecuteStatementResponse_createdAt' - The date and time (UTC) the statement was created.
--
-- 'httpStatus', 'batchExecuteStatementResponse_httpStatus' - The response's http status code.
newBatchExecuteStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchExecuteStatementResponse
newBatchExecuteStatementResponse pHttpStatus_ =
  BatchExecuteStatementResponse'
    { clusterIdentifier =
        Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      id = Prelude.Nothing,
      database = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cluster identifier. This element is not returned when connecting to
-- a serverless workgroup.
batchExecuteStatementResponse_clusterIdentifier :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_clusterIdentifier = Lens.lens (\BatchExecuteStatementResponse' {clusterIdentifier} -> clusterIdentifier) (\s@BatchExecuteStatementResponse' {} a -> s {clusterIdentifier = a} :: BatchExecuteStatementResponse)

-- | The serverless workgroup name. This element is not returned when
-- connecting to a provisioned cluster.
batchExecuteStatementResponse_workgroupName :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_workgroupName = Lens.lens (\BatchExecuteStatementResponse' {workgroupName} -> workgroupName) (\s@BatchExecuteStatementResponse' {} a -> s {workgroupName = a} :: BatchExecuteStatementResponse)

-- | The identifier of the SQL statement whose results are to be fetched.
-- This value is a universally unique identifier (UUID) generated by Amazon
-- Redshift Data API. This identifier is returned by
-- @BatchExecuteStatment@.
batchExecuteStatementResponse_id :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_id = Lens.lens (\BatchExecuteStatementResponse' {id} -> id) (\s@BatchExecuteStatementResponse' {} a -> s {id = a} :: BatchExecuteStatementResponse)

-- | The name of the database.
batchExecuteStatementResponse_database :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_database = Lens.lens (\BatchExecuteStatementResponse' {database} -> database) (\s@BatchExecuteStatementResponse' {} a -> s {database = a} :: BatchExecuteStatementResponse)

-- | The name or ARN of the secret that enables access to the database.
batchExecuteStatementResponse_secretArn :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_secretArn = Lens.lens (\BatchExecuteStatementResponse' {secretArn} -> secretArn) (\s@BatchExecuteStatementResponse' {} a -> s {secretArn = a} :: BatchExecuteStatementResponse)

-- | The database user name.
batchExecuteStatementResponse_dbUser :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_dbUser = Lens.lens (\BatchExecuteStatementResponse' {dbUser} -> dbUser) (\s@BatchExecuteStatementResponse' {} a -> s {dbUser = a} :: BatchExecuteStatementResponse)

-- | The date and time (UTC) the statement was created.
batchExecuteStatementResponse_createdAt :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.UTCTime)
batchExecuteStatementResponse_createdAt = Lens.lens (\BatchExecuteStatementResponse' {createdAt} -> createdAt) (\s@BatchExecuteStatementResponse' {} a -> s {createdAt = a} :: BatchExecuteStatementResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
batchExecuteStatementResponse_httpStatus :: Lens.Lens' BatchExecuteStatementResponse Prelude.Int
batchExecuteStatementResponse_httpStatus = Lens.lens (\BatchExecuteStatementResponse' {httpStatus} -> httpStatus) (\s@BatchExecuteStatementResponse' {} a -> s {httpStatus = a} :: BatchExecuteStatementResponse)

instance Prelude.NFData BatchExecuteStatementResponse where
  rnf BatchExecuteStatementResponse' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpStatus
