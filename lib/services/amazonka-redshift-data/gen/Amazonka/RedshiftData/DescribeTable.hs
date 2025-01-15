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
-- Module      : Amazonka.RedshiftData.DescribeTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the detailed information about a table from metadata in the
-- cluster. The information includes its columns. A token is returned to
-- page through the column list. Depending on the authorization method, use
-- one of the following combinations of request parameters:
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
--
-- For more information about the Amazon Redshift Data API and CLI usage
-- examples, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/data-api.html Using the Amazon Redshift Data API>
-- in the /Amazon Redshift Management Guide/.
--
-- This operation returns paginated results.
module Amazonka.RedshiftData.DescribeTable
  ( -- * Creating a Request
    DescribeTable (..),
    newDescribeTable,

    -- * Request Lenses
    describeTable_clusterIdentifier,
    describeTable_connectedDatabase,
    describeTable_dbUser,
    describeTable_maxResults,
    describeTable_nextToken,
    describeTable_schema,
    describeTable_secretArn,
    describeTable_table,
    describeTable_workgroupName,
    describeTable_database,

    -- * Destructuring the Response
    DescribeTableResponse (..),
    newDescribeTableResponse,

    -- * Response Lenses
    describeTableResponse_columnList,
    describeTableResponse_nextToken,
    describeTableResponse_tableName,
    describeTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTable' smart constructor.
data DescribeTable = DescribeTable'
  { -- | The cluster identifier. This parameter is required when connecting to a
    -- cluster and authenticating using either Secrets Manager or temporary
    -- credentials.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A database name. The connected database is specified when you connect
    -- with your authentication credentials.
    connectedDatabase :: Prelude.Maybe Prelude.Text,
    -- | The database user name. This parameter is required when connecting to a
    -- cluster and authenticating using temporary credentials.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of tables to return in the response. If more tables
    -- exist than fit in one response, then @NextToken@ is returned to page
    -- through the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- NextToken value in the next NextToken parameter and retrying the
    -- command. If the NextToken field is empty, all response records have been
    -- retrieved for the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The schema that contains the table. If no schema is specified, then
    -- matching tables for all schemas are returned.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database. This
    -- parameter is required when authenticating using Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The table name. If no table is specified, then all tables for all
    -- matching schemas are returned. If no table and no schema is specified,
    -- then all tables for all schemas in the database are returned
    table :: Prelude.Maybe Prelude.Text,
    -- | The serverless workgroup name. This parameter is required when
    -- connecting to a serverless workgroup and authenticating using either
    -- Secrets Manager or temporary credentials.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the database that contains the tables to be described. If
    -- @ConnectedDatabase@ is not specified, this is also the database to
    -- connect to with your authentication credentials.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'describeTable_clusterIdentifier' - The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
--
-- 'connectedDatabase', 'describeTable_connectedDatabase' - A database name. The connected database is specified when you connect
-- with your authentication credentials.
--
-- 'dbUser', 'describeTable_dbUser' - The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
--
-- 'maxResults', 'describeTable_maxResults' - The maximum number of tables to return in the response. If more tables
-- exist than fit in one response, then @NextToken@ is returned to page
-- through the results.
--
-- 'nextToken', 'describeTable_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'schema', 'describeTable_schema' - The schema that contains the table. If no schema is specified, then
-- matching tables for all schemas are returned.
--
-- 'secretArn', 'describeTable_secretArn' - The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
--
-- 'table', 'describeTable_table' - The table name. If no table is specified, then all tables for all
-- matching schemas are returned. If no table and no schema is specified,
-- then all tables for all schemas in the database are returned
--
-- 'workgroupName', 'describeTable_workgroupName' - The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
--
-- 'database', 'describeTable_database' - The name of the database that contains the tables to be described. If
-- @ConnectedDatabase@ is not specified, this is also the database to
-- connect to with your authentication credentials.
newDescribeTable ::
  -- | 'database'
  Prelude.Text ->
  DescribeTable
newDescribeTable pDatabase_ =
  DescribeTable'
    { clusterIdentifier = Prelude.Nothing,
      connectedDatabase = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      schema = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      table = Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      database = pDatabase_
    }

-- | The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
describeTable_clusterIdentifier :: Lens.Lens' DescribeTable (Prelude.Maybe Prelude.Text)
describeTable_clusterIdentifier = Lens.lens (\DescribeTable' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeTable' {} a -> s {clusterIdentifier = a} :: DescribeTable)

-- | A database name. The connected database is specified when you connect
-- with your authentication credentials.
describeTable_connectedDatabase :: Lens.Lens' DescribeTable (Prelude.Maybe Prelude.Text)
describeTable_connectedDatabase = Lens.lens (\DescribeTable' {connectedDatabase} -> connectedDatabase) (\s@DescribeTable' {} a -> s {connectedDatabase = a} :: DescribeTable)

-- | The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
describeTable_dbUser :: Lens.Lens' DescribeTable (Prelude.Maybe Prelude.Text)
describeTable_dbUser = Lens.lens (\DescribeTable' {dbUser} -> dbUser) (\s@DescribeTable' {} a -> s {dbUser = a} :: DescribeTable)

-- | The maximum number of tables to return in the response. If more tables
-- exist than fit in one response, then @NextToken@ is returned to page
-- through the results.
describeTable_maxResults :: Lens.Lens' DescribeTable (Prelude.Maybe Prelude.Natural)
describeTable_maxResults = Lens.lens (\DescribeTable' {maxResults} -> maxResults) (\s@DescribeTable' {} a -> s {maxResults = a} :: DescribeTable)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
describeTable_nextToken :: Lens.Lens' DescribeTable (Prelude.Maybe Prelude.Text)
describeTable_nextToken = Lens.lens (\DescribeTable' {nextToken} -> nextToken) (\s@DescribeTable' {} a -> s {nextToken = a} :: DescribeTable)

-- | The schema that contains the table. If no schema is specified, then
-- matching tables for all schemas are returned.
describeTable_schema :: Lens.Lens' DescribeTable (Prelude.Maybe Prelude.Text)
describeTable_schema = Lens.lens (\DescribeTable' {schema} -> schema) (\s@DescribeTable' {} a -> s {schema = a} :: DescribeTable)

-- | The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
describeTable_secretArn :: Lens.Lens' DescribeTable (Prelude.Maybe Prelude.Text)
describeTable_secretArn = Lens.lens (\DescribeTable' {secretArn} -> secretArn) (\s@DescribeTable' {} a -> s {secretArn = a} :: DescribeTable)

-- | The table name. If no table is specified, then all tables for all
-- matching schemas are returned. If no table and no schema is specified,
-- then all tables for all schemas in the database are returned
describeTable_table :: Lens.Lens' DescribeTable (Prelude.Maybe Prelude.Text)
describeTable_table = Lens.lens (\DescribeTable' {table} -> table) (\s@DescribeTable' {} a -> s {table = a} :: DescribeTable)

-- | The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
describeTable_workgroupName :: Lens.Lens' DescribeTable (Prelude.Maybe Prelude.Text)
describeTable_workgroupName = Lens.lens (\DescribeTable' {workgroupName} -> workgroupName) (\s@DescribeTable' {} a -> s {workgroupName = a} :: DescribeTable)

-- | The name of the database that contains the tables to be described. If
-- @ConnectedDatabase@ is not specified, this is also the database to
-- connect to with your authentication credentials.
describeTable_database :: Lens.Lens' DescribeTable Prelude.Text
describeTable_database = Lens.lens (\DescribeTable' {database} -> database) (\s@DescribeTable' {} a -> s {database = a} :: DescribeTable)

instance Core.AWSPager DescribeTable where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTableResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTableResponse_columnList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeTable_nextToken
              Lens..~ rs
              Lens.^? describeTableResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeTable where
  type
    AWSResponse DescribeTable =
      DescribeTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTableResponse'
            Prelude.<$> (x Data..?> "ColumnList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TableName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTable where
  hashWithSalt _salt DescribeTable' {..} =
    _salt
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` connectedDatabase
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` workgroupName
      `Prelude.hashWithSalt` database

instance Prelude.NFData DescribeTable where
  rnf DescribeTable' {..} =
    Prelude.rnf clusterIdentifier `Prelude.seq`
      Prelude.rnf connectedDatabase `Prelude.seq`
        Prelude.rnf dbUser `Prelude.seq`
          Prelude.rnf maxResults `Prelude.seq`
            Prelude.rnf nextToken `Prelude.seq`
              Prelude.rnf schema `Prelude.seq`
                Prelude.rnf secretArn `Prelude.seq`
                  Prelude.rnf table `Prelude.seq`
                    Prelude.rnf workgroupName `Prelude.seq`
                      Prelude.rnf database

instance Data.ToHeaders DescribeTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("RedshiftData.DescribeTable" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTable where
  toJSON DescribeTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
            ("ConnectedDatabase" Data..=)
              Prelude.<$> connectedDatabase,
            ("DbUser" Data..=) Prelude.<$> dbUser,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Schema" Data..=) Prelude.<$> schema,
            ("SecretArn" Data..=) Prelude.<$> secretArn,
            ("Table" Data..=) Prelude.<$> table,
            ("WorkgroupName" Data..=) Prelude.<$> workgroupName,
            Prelude.Just ("Database" Data..= database)
          ]
      )

instance Data.ToPath DescribeTable where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTableResponse' smart constructor.
data DescribeTableResponse = DescribeTableResponse'
  { -- | A list of columns in the table.
    columnList :: Prelude.Maybe [ColumnMetadata],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- NextToken value in the next NextToken parameter and retrying the
    -- command. If the NextToken field is empty, all response records have been
    -- retrieved for the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The table name.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnList', 'describeTableResponse_columnList' - A list of columns in the table.
--
-- 'nextToken', 'describeTableResponse_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'tableName', 'describeTableResponse_tableName' - The table name.
--
-- 'httpStatus', 'describeTableResponse_httpStatus' - The response's http status code.
newDescribeTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTableResponse
newDescribeTableResponse pHttpStatus_ =
  DescribeTableResponse'
    { columnList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      tableName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of columns in the table.
describeTableResponse_columnList :: Lens.Lens' DescribeTableResponse (Prelude.Maybe [ColumnMetadata])
describeTableResponse_columnList = Lens.lens (\DescribeTableResponse' {columnList} -> columnList) (\s@DescribeTableResponse' {} a -> s {columnList = a} :: DescribeTableResponse) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
describeTableResponse_nextToken :: Lens.Lens' DescribeTableResponse (Prelude.Maybe Prelude.Text)
describeTableResponse_nextToken = Lens.lens (\DescribeTableResponse' {nextToken} -> nextToken) (\s@DescribeTableResponse' {} a -> s {nextToken = a} :: DescribeTableResponse)

-- | The table name.
describeTableResponse_tableName :: Lens.Lens' DescribeTableResponse (Prelude.Maybe Prelude.Text)
describeTableResponse_tableName = Lens.lens (\DescribeTableResponse' {tableName} -> tableName) (\s@DescribeTableResponse' {} a -> s {tableName = a} :: DescribeTableResponse)

-- | The response's http status code.
describeTableResponse_httpStatus :: Lens.Lens' DescribeTableResponse Prelude.Int
describeTableResponse_httpStatus = Lens.lens (\DescribeTableResponse' {httpStatus} -> httpStatus) (\s@DescribeTableResponse' {} a -> s {httpStatus = a} :: DescribeTableResponse)

instance Prelude.NFData DescribeTableResponse where
  rnf DescribeTableResponse' {..} =
    Prelude.rnf columnList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf tableName `Prelude.seq`
          Prelude.rnf httpStatus
