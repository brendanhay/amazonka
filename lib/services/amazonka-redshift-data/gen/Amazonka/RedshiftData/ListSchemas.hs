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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the schemas in a database. A token is returned to page through the
-- schema list. Depending on the authorization method, use one of the
-- following combinations of request parameters:
--
-- -   Secrets Manager - specify the Amazon Resource Name (ARN) of the
--     secret, the database name, and the cluster identifier that matches
--     the cluster in the secret.
--
-- -   Temporary credentials - specify the cluster identifier, the database
--     name, and the database user name. Permission to call the
--     @redshift:GetClusterCredentials@ operation is required to use this
--     method.
--
-- This operation returns paginated results.
module Amazonka.RedshiftData.ListSchemas
  ( -- * Creating a Request
    ListSchemas (..),
    newListSchemas,

    -- * Request Lenses
    listSchemas_dbUser,
    listSchemas_connectedDatabase,
    listSchemas_nextToken,
    listSchemas_secretArn,
    listSchemas_maxResults,
    listSchemas_schemaPattern,
    listSchemas_clusterIdentifier,
    listSchemas_database,

    -- * Destructuring the Response
    ListSchemasResponse (..),
    newListSchemasResponse,

    -- * Response Lenses
    listSchemasResponse_schemas,
    listSchemasResponse_nextToken,
    listSchemasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSchemas' smart constructor.
data ListSchemas = ListSchemas'
  { -- | The database user name. This parameter is required when authenticating
    -- using temporary credentials.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | A database name. The connected database is specified when you connect
    -- with your authentication credentials.
    connectedDatabase :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- NextToken value in the next NextToken parameter and retrying the
    -- command. If the NextToken field is empty, all response records have been
    -- retrieved for the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database. This
    -- parameter is required when authenticating using Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of schemas to return in the response. If more schemas
    -- exist than fit in one response, then @NextToken@ is returned to page
    -- through the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pattern to filter results by schema name. Within a schema pattern,
    -- \"%\" means match any substring of 0 or more characters and \"_\" means
    -- match any one character. Only schema name entries matching the search
    -- pattern are returned.
    schemaPattern :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier. This parameter is required when authenticating
    -- using either Secrets Manager or temporary credentials.
    clusterIdentifier :: Prelude.Text,
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
-- 'dbUser', 'listSchemas_dbUser' - The database user name. This parameter is required when authenticating
-- using temporary credentials.
--
-- 'connectedDatabase', 'listSchemas_connectedDatabase' - A database name. The connected database is specified when you connect
-- with your authentication credentials.
--
-- 'nextToken', 'listSchemas_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'secretArn', 'listSchemas_secretArn' - The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
--
-- 'maxResults', 'listSchemas_maxResults' - The maximum number of schemas to return in the response. If more schemas
-- exist than fit in one response, then @NextToken@ is returned to page
-- through the results.
--
-- 'schemaPattern', 'listSchemas_schemaPattern' - A pattern to filter results by schema name. Within a schema pattern,
-- \"%\" means match any substring of 0 or more characters and \"_\" means
-- match any one character. Only schema name entries matching the search
-- pattern are returned.
--
-- 'clusterIdentifier', 'listSchemas_clusterIdentifier' - The cluster identifier. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
--
-- 'database', 'listSchemas_database' - The name of the database that contains the schemas to list. If
-- @ConnectedDatabase@ is not specified, this is also the database to
-- connect to with your authentication credentials.
newListSchemas ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  ListSchemas
newListSchemas pClusterIdentifier_ pDatabase_ =
  ListSchemas'
    { dbUser = Prelude.Nothing,
      connectedDatabase = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      schemaPattern = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_,
      database = pDatabase_
    }

-- | The database user name. This parameter is required when authenticating
-- using temporary credentials.
listSchemas_dbUser :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_dbUser = Lens.lens (\ListSchemas' {dbUser} -> dbUser) (\s@ListSchemas' {} a -> s {dbUser = a} :: ListSchemas)

-- | A database name. The connected database is specified when you connect
-- with your authentication credentials.
listSchemas_connectedDatabase :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_connectedDatabase = Lens.lens (\ListSchemas' {connectedDatabase} -> connectedDatabase) (\s@ListSchemas' {} a -> s {connectedDatabase = a} :: ListSchemas)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listSchemas_nextToken :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_nextToken = Lens.lens (\ListSchemas' {nextToken} -> nextToken) (\s@ListSchemas' {} a -> s {nextToken = a} :: ListSchemas)

-- | The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
listSchemas_secretArn :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_secretArn = Lens.lens (\ListSchemas' {secretArn} -> secretArn) (\s@ListSchemas' {} a -> s {secretArn = a} :: ListSchemas)

-- | The maximum number of schemas to return in the response. If more schemas
-- exist than fit in one response, then @NextToken@ is returned to page
-- through the results.
listSchemas_maxResults :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Natural)
listSchemas_maxResults = Lens.lens (\ListSchemas' {maxResults} -> maxResults) (\s@ListSchemas' {} a -> s {maxResults = a} :: ListSchemas)

-- | A pattern to filter results by schema name. Within a schema pattern,
-- \"%\" means match any substring of 0 or more characters and \"_\" means
-- match any one character. Only schema name entries matching the search
-- pattern are returned.
listSchemas_schemaPattern :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_schemaPattern = Lens.lens (\ListSchemas' {schemaPattern} -> schemaPattern) (\s@ListSchemas' {} a -> s {schemaPattern = a} :: ListSchemas)

-- | The cluster identifier. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
listSchemas_clusterIdentifier :: Lens.Lens' ListSchemas Prelude.Text
listSchemas_clusterIdentifier = Lens.lens (\ListSchemas' {clusterIdentifier} -> clusterIdentifier) (\s@ListSchemas' {} a -> s {clusterIdentifier = a} :: ListSchemas)

-- | The name of the database that contains the schemas to list. If
-- @ConnectedDatabase@ is not specified, this is also the database to
-- connect to with your authentication credentials.
listSchemas_database :: Lens.Lens' ListSchemas Prelude.Text
listSchemas_database = Lens.lens (\ListSchemas' {database} -> database) (\s@ListSchemas' {} a -> s {database = a} :: ListSchemas)

instance Core.AWSPager ListSchemas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_schemas Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSchemas_nextToken
          Lens..~ rs
          Lens.^? listSchemasResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListSchemas where
  type AWSResponse ListSchemas = ListSchemasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemasResponse'
            Prelude.<$> (x Core..?> "Schemas" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSchemas where
  hashWithSalt _salt ListSchemas' {..} =
    _salt `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` connectedDatabase
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` schemaPattern
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` database

instance Prelude.NFData ListSchemas where
  rnf ListSchemas' {..} =
    Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf connectedDatabase
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf schemaPattern
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf database

instance Core.ToHeaders ListSchemas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("RedshiftData.ListSchemas" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSchemas where
  toJSON ListSchemas' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DbUser" Core..=) Prelude.<$> dbUser,
            ("ConnectedDatabase" Core..=)
              Prelude.<$> connectedDatabase,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SecretArn" Core..=) Prelude.<$> secretArn,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SchemaPattern" Core..=) Prelude.<$> schemaPattern,
            Prelude.Just
              ("ClusterIdentifier" Core..= clusterIdentifier),
            Prelude.Just ("Database" Core..= database)
          ]
      )

instance Core.ToPath ListSchemas where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSchemas where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'
  { -- | The schemas that match the request pattern.
    schemas :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- NextToken value in the next NextToken parameter and retrying the
    -- command. If the NextToken field is empty, all response records have been
    -- retrieved for the request.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'schemas', 'listSchemasResponse_schemas' - The schemas that match the request pattern.
--
-- 'nextToken', 'listSchemasResponse_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'httpStatus', 'listSchemasResponse_httpStatus' - The response's http status code.
newListSchemasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSchemasResponse
newListSchemasResponse pHttpStatus_ =
  ListSchemasResponse'
    { schemas = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The schemas that match the request pattern.
listSchemasResponse_schemas :: Lens.Lens' ListSchemasResponse (Prelude.Maybe [Prelude.Text])
listSchemasResponse_schemas = Lens.lens (\ListSchemasResponse' {schemas} -> schemas) (\s@ListSchemasResponse' {} a -> s {schemas = a} :: ListSchemasResponse) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listSchemasResponse_nextToken :: Lens.Lens' ListSchemasResponse (Prelude.Maybe Prelude.Text)
listSchemasResponse_nextToken = Lens.lens (\ListSchemasResponse' {nextToken} -> nextToken) (\s@ListSchemasResponse' {} a -> s {nextToken = a} :: ListSchemasResponse)

-- | The response's http status code.
listSchemasResponse_httpStatus :: Lens.Lens' ListSchemasResponse Prelude.Int
listSchemasResponse_httpStatus = Lens.lens (\ListSchemasResponse' {httpStatus} -> httpStatus) (\s@ListSchemasResponse' {} a -> s {httpStatus = a} :: ListSchemasResponse)

instance Prelude.NFData ListSchemasResponse where
  rnf ListSchemasResponse' {..} =
    Prelude.rnf schemas
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
