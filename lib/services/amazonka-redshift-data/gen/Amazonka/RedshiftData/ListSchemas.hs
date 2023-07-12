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
-- Module      : Amazonka.RedshiftData.ListSchemas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the schemas in a database. A token is returned to page through the
-- schema list. Depending on the authorization method, use one of the
-- following combinations of request parameters:
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
module Amazonka.RedshiftData.ListSchemas
  ( -- * Creating a Request
    ListSchemas (..),
    newListSchemas,

    -- * Request Lenses
    listSchemas_clusterIdentifier,
    listSchemas_connectedDatabase,
    listSchemas_dbUser,
    listSchemas_maxResults,
    listSchemas_nextToken,
    listSchemas_schemaPattern,
    listSchemas_secretArn,
    listSchemas_workgroupName,
    listSchemas_database,

    -- * Destructuring the Response
    ListSchemasResponse (..),
    newListSchemasResponse,

    -- * Response Lenses
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSchemas' smart constructor.
data ListSchemas = ListSchemas'
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
    -- | The maximum number of schemas to return in the response. If more schemas
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
    -- pattern are returned.
    schemaPattern :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database. This
    -- parameter is required when authenticating using Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The serverless workgroup name. This parameter is required when
    -- connecting to a serverless workgroup and authenticating using either
    -- Secrets Manager or temporary credentials.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the database that contains the schemas to list. If
    -- @ConnectedDatabase@ is not specified, this is also the database to
    -- connect to with your authentication credentials.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'listSchemas_clusterIdentifier' - The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
--
-- 'connectedDatabase', 'listSchemas_connectedDatabase' - A database name. The connected database is specified when you connect
-- with your authentication credentials.
--
-- 'dbUser', 'listSchemas_dbUser' - The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
--
-- 'maxResults', 'listSchemas_maxResults' - The maximum number of schemas to return in the response. If more schemas
-- exist than fit in one response, then @NextToken@ is returned to page
-- through the results.
--
-- 'nextToken', 'listSchemas_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'schemaPattern', 'listSchemas_schemaPattern' - A pattern to filter results by schema name. Within a schema pattern,
-- \"%\" means match any substring of 0 or more characters and \"_\" means
-- match any one character. Only schema name entries matching the search
-- pattern are returned.
--
-- 'secretArn', 'listSchemas_secretArn' - The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
--
-- 'workgroupName', 'listSchemas_workgroupName' - The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
--
-- 'database', 'listSchemas_database' - The name of the database that contains the schemas to list. If
-- @ConnectedDatabase@ is not specified, this is also the database to
-- connect to with your authentication credentials.
newListSchemas ::
  -- | 'database'
  Prelude.Text ->
  ListSchemas
newListSchemas pDatabase_ =
  ListSchemas'
    { clusterIdentifier = Prelude.Nothing,
      connectedDatabase = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      schemaPattern = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      database = pDatabase_
    }

-- | The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
listSchemas_clusterIdentifier :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_clusterIdentifier = Lens.lens (\ListSchemas' {clusterIdentifier} -> clusterIdentifier) (\s@ListSchemas' {} a -> s {clusterIdentifier = a} :: ListSchemas)

-- | A database name. The connected database is specified when you connect
-- with your authentication credentials.
listSchemas_connectedDatabase :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_connectedDatabase = Lens.lens (\ListSchemas' {connectedDatabase} -> connectedDatabase) (\s@ListSchemas' {} a -> s {connectedDatabase = a} :: ListSchemas)

-- | The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
listSchemas_dbUser :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_dbUser = Lens.lens (\ListSchemas' {dbUser} -> dbUser) (\s@ListSchemas' {} a -> s {dbUser = a} :: ListSchemas)

-- | The maximum number of schemas to return in the response. If more schemas
-- exist than fit in one response, then @NextToken@ is returned to page
-- through the results.
listSchemas_maxResults :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Natural)
listSchemas_maxResults = Lens.lens (\ListSchemas' {maxResults} -> maxResults) (\s@ListSchemas' {} a -> s {maxResults = a} :: ListSchemas)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listSchemas_nextToken :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_nextToken = Lens.lens (\ListSchemas' {nextToken} -> nextToken) (\s@ListSchemas' {} a -> s {nextToken = a} :: ListSchemas)

-- | A pattern to filter results by schema name. Within a schema pattern,
-- \"%\" means match any substring of 0 or more characters and \"_\" means
-- match any one character. Only schema name entries matching the search
-- pattern are returned.
listSchemas_schemaPattern :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_schemaPattern = Lens.lens (\ListSchemas' {schemaPattern} -> schemaPattern) (\s@ListSchemas' {} a -> s {schemaPattern = a} :: ListSchemas)

-- | The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
listSchemas_secretArn :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_secretArn = Lens.lens (\ListSchemas' {secretArn} -> secretArn) (\s@ListSchemas' {} a -> s {secretArn = a} :: ListSchemas)

-- | The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
listSchemas_workgroupName :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_workgroupName = Lens.lens (\ListSchemas' {workgroupName} -> workgroupName) (\s@ListSchemas' {} a -> s {workgroupName = a} :: ListSchemas)

-- | The name of the database that contains the schemas to list. If
-- @ConnectedDatabase@ is not specified, this is also the database to
-- connect to with your authentication credentials.
listSchemas_database :: Lens.Lens' ListSchemas Prelude.Text
listSchemas_database = Lens.lens (\ListSchemas' {database} -> database) (\s@ListSchemas' {} a -> s {database = a} :: ListSchemas)

instance Core.AWSPager ListSchemas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_schemas
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSchemas_nextToken
          Lens..~ rs
          Lens.^? listSchemasResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSchemas where
  type AWSResponse ListSchemas = ListSchemasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemasResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Schemas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSchemas where
  hashWithSalt _salt ListSchemas' {..} =
    _salt
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` connectedDatabase
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schemaPattern
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` workgroupName
      `Prelude.hashWithSalt` database

instance Prelude.NFData ListSchemas where
  rnf ListSchemas' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf connectedDatabase
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaPattern
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf database

instance Data.ToHeaders ListSchemas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("RedshiftData.ListSchemas" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSchemas where
  toJSON ListSchemas' {..} =
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
            ("WorkgroupName" Data..=) Prelude.<$> workgroupName,
            Prelude.Just ("Database" Data..= database)
          ]
      )

instance Data.ToPath ListSchemas where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSchemas where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- NextToken value in the next NextToken parameter and retrying the
    -- command. If the NextToken field is empty, all response records have been
    -- retrieved for the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The schemas that match the request pattern.
    schemas :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSchemasResponse_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'schemas', 'listSchemasResponse_schemas' - The schemas that match the request pattern.
--
-- 'httpStatus', 'listSchemasResponse_httpStatus' - The response's http status code.
newListSchemasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSchemasResponse
newListSchemasResponse pHttpStatus_ =
  ListSchemasResponse'
    { nextToken = Prelude.Nothing,
      schemas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listSchemasResponse_nextToken :: Lens.Lens' ListSchemasResponse (Prelude.Maybe Prelude.Text)
listSchemasResponse_nextToken = Lens.lens (\ListSchemasResponse' {nextToken} -> nextToken) (\s@ListSchemasResponse' {} a -> s {nextToken = a} :: ListSchemasResponse)

-- | The schemas that match the request pattern.
listSchemasResponse_schemas :: Lens.Lens' ListSchemasResponse (Prelude.Maybe [Prelude.Text])
listSchemasResponse_schemas = Lens.lens (\ListSchemasResponse' {schemas} -> schemas) (\s@ListSchemasResponse' {} a -> s {schemas = a} :: ListSchemasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSchemasResponse_httpStatus :: Lens.Lens' ListSchemasResponse Prelude.Int
listSchemasResponse_httpStatus = Lens.lens (\ListSchemasResponse' {httpStatus} -> httpStatus) (\s@ListSchemasResponse' {} a -> s {httpStatus = a} :: ListSchemasResponse)

instance Prelude.NFData ListSchemasResponse where
  rnf ListSchemasResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemas
      `Prelude.seq` Prelude.rnf httpStatus
