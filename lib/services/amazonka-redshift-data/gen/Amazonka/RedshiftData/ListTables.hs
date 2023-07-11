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
-- Module      : Amazonka.RedshiftData.ListTables
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the tables in a database. If neither @SchemaPattern@ nor
-- @TablePattern@ are specified, then all tables in the database are
-- returned. A token is returned to page through the table list. Depending
-- on the authorization method, use one of the following combinations of
-- request parameters:
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
module Amazonka.RedshiftData.ListTables
  ( -- * Creating a Request
    ListTables (..),
    newListTables,

    -- * Request Lenses
    listTables_clusterIdentifier,
    listTables_connectedDatabase,
    listTables_dbUser,
    listTables_maxResults,
    listTables_nextToken,
    listTables_schemaPattern,
    listTables_secretArn,
    listTables_tablePattern,
    listTables_workgroupName,
    listTables_database,

    -- * Destructuring the Response
    ListTablesResponse (..),
    newListTablesResponse,

    -- * Response Lenses
    listTablesResponse_nextToken,
    listTablesResponse_tables,
    listTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTables' smart constructor.
data ListTables = ListTables'
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
    -- | A pattern to filter results by schema name. Within a schema pattern,
    -- \"%\" means match any substring of 0 or more characters and \"_\" means
    -- match any one character. Only schema name entries matching the search
    -- pattern are returned. If @SchemaPattern@ is not specified, then all
    -- tables that match @TablePattern@ are returned. If neither
    -- @SchemaPattern@ or @TablePattern@ are specified, then all tables are
    -- returned.
    schemaPattern :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database. This
    -- parameter is required when authenticating using Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | A pattern to filter results by table name. Within a table pattern, \"%\"
    -- means match any substring of 0 or more characters and \"_\" means match
    -- any one character. Only table name entries matching the search pattern
    -- are returned. If @TablePattern@ is not specified, then all tables that
    -- match @SchemaPattern@are returned. If neither @SchemaPattern@ or
    -- @TablePattern@ are specified, then all tables are returned.
    tablePattern :: Prelude.Maybe Prelude.Text,
    -- | The serverless workgroup name. This parameter is required when
    -- connecting to a serverless workgroup and authenticating using either
    -- Secrets Manager or temporary credentials.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the database that contains the tables to list. If
    -- @ConnectedDatabase@ is not specified, this is also the database to
    -- connect to with your authentication credentials.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'listTables_clusterIdentifier' - The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
--
-- 'connectedDatabase', 'listTables_connectedDatabase' - A database name. The connected database is specified when you connect
-- with your authentication credentials.
--
-- 'dbUser', 'listTables_dbUser' - The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
--
-- 'maxResults', 'listTables_maxResults' - The maximum number of tables to return in the response. If more tables
-- exist than fit in one response, then @NextToken@ is returned to page
-- through the results.
--
-- 'nextToken', 'listTables_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'schemaPattern', 'listTables_schemaPattern' - A pattern to filter results by schema name. Within a schema pattern,
-- \"%\" means match any substring of 0 or more characters and \"_\" means
-- match any one character. Only schema name entries matching the search
-- pattern are returned. If @SchemaPattern@ is not specified, then all
-- tables that match @TablePattern@ are returned. If neither
-- @SchemaPattern@ or @TablePattern@ are specified, then all tables are
-- returned.
--
-- 'secretArn', 'listTables_secretArn' - The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
--
-- 'tablePattern', 'listTables_tablePattern' - A pattern to filter results by table name. Within a table pattern, \"%\"
-- means match any substring of 0 or more characters and \"_\" means match
-- any one character. Only table name entries matching the search pattern
-- are returned. If @TablePattern@ is not specified, then all tables that
-- match @SchemaPattern@are returned. If neither @SchemaPattern@ or
-- @TablePattern@ are specified, then all tables are returned.
--
-- 'workgroupName', 'listTables_workgroupName' - The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
--
-- 'database', 'listTables_database' - The name of the database that contains the tables to list. If
-- @ConnectedDatabase@ is not specified, this is also the database to
-- connect to with your authentication credentials.
newListTables ::
  -- | 'database'
  Prelude.Text ->
  ListTables
newListTables pDatabase_ =
  ListTables'
    { clusterIdentifier = Prelude.Nothing,
      connectedDatabase = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      schemaPattern = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      tablePattern = Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      database = pDatabase_
    }

-- | The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
listTables_clusterIdentifier :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_clusterIdentifier = Lens.lens (\ListTables' {clusterIdentifier} -> clusterIdentifier) (\s@ListTables' {} a -> s {clusterIdentifier = a} :: ListTables)

-- | A database name. The connected database is specified when you connect
-- with your authentication credentials.
listTables_connectedDatabase :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_connectedDatabase = Lens.lens (\ListTables' {connectedDatabase} -> connectedDatabase) (\s@ListTables' {} a -> s {connectedDatabase = a} :: ListTables)

-- | The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
listTables_dbUser :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_dbUser = Lens.lens (\ListTables' {dbUser} -> dbUser) (\s@ListTables' {} a -> s {dbUser = a} :: ListTables)

-- | The maximum number of tables to return in the response. If more tables
-- exist than fit in one response, then @NextToken@ is returned to page
-- through the results.
listTables_maxResults :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Natural)
listTables_maxResults = Lens.lens (\ListTables' {maxResults} -> maxResults) (\s@ListTables' {} a -> s {maxResults = a} :: ListTables)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listTables_nextToken :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_nextToken = Lens.lens (\ListTables' {nextToken} -> nextToken) (\s@ListTables' {} a -> s {nextToken = a} :: ListTables)

-- | A pattern to filter results by schema name. Within a schema pattern,
-- \"%\" means match any substring of 0 or more characters and \"_\" means
-- match any one character. Only schema name entries matching the search
-- pattern are returned. If @SchemaPattern@ is not specified, then all
-- tables that match @TablePattern@ are returned. If neither
-- @SchemaPattern@ or @TablePattern@ are specified, then all tables are
-- returned.
listTables_schemaPattern :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_schemaPattern = Lens.lens (\ListTables' {schemaPattern} -> schemaPattern) (\s@ListTables' {} a -> s {schemaPattern = a} :: ListTables)

-- | The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
listTables_secretArn :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_secretArn = Lens.lens (\ListTables' {secretArn} -> secretArn) (\s@ListTables' {} a -> s {secretArn = a} :: ListTables)

-- | A pattern to filter results by table name. Within a table pattern, \"%\"
-- means match any substring of 0 or more characters and \"_\" means match
-- any one character. Only table name entries matching the search pattern
-- are returned. If @TablePattern@ is not specified, then all tables that
-- match @SchemaPattern@are returned. If neither @SchemaPattern@ or
-- @TablePattern@ are specified, then all tables are returned.
listTables_tablePattern :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_tablePattern = Lens.lens (\ListTables' {tablePattern} -> tablePattern) (\s@ListTables' {} a -> s {tablePattern = a} :: ListTables)

-- | The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
listTables_workgroupName :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_workgroupName = Lens.lens (\ListTables' {workgroupName} -> workgroupName) (\s@ListTables' {} a -> s {workgroupName = a} :: ListTables)

-- | The name of the database that contains the tables to list. If
-- @ConnectedDatabase@ is not specified, this is also the database to
-- connect to with your authentication credentials.
listTables_database :: Lens.Lens' ListTables Prelude.Text
listTables_database = Lens.lens (\ListTables' {database} -> database) (\s@ListTables' {} a -> s {database = a} :: ListTables)

instance Core.AWSPager ListTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTablesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTablesResponse_tables
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTables_nextToken
          Lens..~ rs
          Lens.^? listTablesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTables where
  type AWSResponse ListTables = ListTablesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTablesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Tables" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTables where
  hashWithSalt _salt ListTables' {..} =
    _salt
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` connectedDatabase
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schemaPattern
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` tablePattern
      `Prelude.hashWithSalt` workgroupName
      `Prelude.hashWithSalt` database

instance Prelude.NFData ListTables where
  rnf ListTables' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf connectedDatabase
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaPattern
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf tablePattern
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf database

instance Data.ToHeaders ListTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("RedshiftData.ListTables" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTables where
  toJSON ListTables' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
            ("ConnectedDatabase" Data..=)
              Prelude.<$> connectedDatabase,
            ("DbUser" Data..=) Prelude.<$> dbUser,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SchemaPattern" Data..=) Prelude.<$> schemaPattern,
            ("SecretArn" Data..=) Prelude.<$> secretArn,
            ("TablePattern" Data..=) Prelude.<$> tablePattern,
            ("WorkgroupName" Data..=) Prelude.<$> workgroupName,
            Prelude.Just ("Database" Data..= database)
          ]
      )

instance Data.ToPath ListTables where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTables where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTablesResponse' smart constructor.
data ListTablesResponse = ListTablesResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- NextToken value in the next NextToken parameter and retrying the
    -- command. If the NextToken field is empty, all response records have been
    -- retrieved for the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The tables that match the request pattern.
    tables :: Prelude.Maybe [TableMember],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTablesResponse_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'tables', 'listTablesResponse_tables' - The tables that match the request pattern.
--
-- 'httpStatus', 'listTablesResponse_httpStatus' - The response's http status code.
newListTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTablesResponse
newListTablesResponse pHttpStatus_ =
  ListTablesResponse'
    { nextToken = Prelude.Nothing,
      tables = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listTablesResponse_nextToken :: Lens.Lens' ListTablesResponse (Prelude.Maybe Prelude.Text)
listTablesResponse_nextToken = Lens.lens (\ListTablesResponse' {nextToken} -> nextToken) (\s@ListTablesResponse' {} a -> s {nextToken = a} :: ListTablesResponse)

-- | The tables that match the request pattern.
listTablesResponse_tables :: Lens.Lens' ListTablesResponse (Prelude.Maybe [TableMember])
listTablesResponse_tables = Lens.lens (\ListTablesResponse' {tables} -> tables) (\s@ListTablesResponse' {} a -> s {tables = a} :: ListTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTablesResponse_httpStatus :: Lens.Lens' ListTablesResponse Prelude.Int
listTablesResponse_httpStatus = Lens.lens (\ListTablesResponse' {httpStatus} -> httpStatus) (\s@ListTablesResponse' {} a -> s {httpStatus = a} :: ListTablesResponse)

instance Prelude.NFData ListTablesResponse where
  rnf ListTablesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tables
      `Prelude.seq` Prelude.rnf httpStatus
