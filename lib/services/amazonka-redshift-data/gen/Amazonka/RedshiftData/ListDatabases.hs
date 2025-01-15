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
-- Module      : Amazonka.RedshiftData.ListDatabases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the databases in a cluster. A token is returned to page through the
-- database list. Depending on the authorization method, use one of the
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
module Amazonka.RedshiftData.ListDatabases
  ( -- * Creating a Request
    ListDatabases (..),
    newListDatabases,

    -- * Request Lenses
    listDatabases_clusterIdentifier,
    listDatabases_dbUser,
    listDatabases_maxResults,
    listDatabases_nextToken,
    listDatabases_secretArn,
    listDatabases_workgroupName,
    listDatabases_database,

    -- * Destructuring the Response
    ListDatabasesResponse (..),
    newListDatabasesResponse,

    -- * Response Lenses
    listDatabasesResponse_databases,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatabases' smart constructor.
data ListDatabases = ListDatabases'
  { -- | The cluster identifier. This parameter is required when connecting to a
    -- cluster and authenticating using either Secrets Manager or temporary
    -- credentials.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The database user name. This parameter is required when connecting to a
    -- cluster and authenticating using temporary credentials.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of databases to return in the response. If more
    -- databases exist than fit in one response, then @NextToken@ is returned
    -- to page through the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
    -- | The serverless workgroup name. This parameter is required when
    -- connecting to a serverless workgroup and authenticating using either
    -- Secrets Manager or temporary credentials.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the database. This parameter is required when authenticating
    -- using either Secrets Manager or temporary credentials.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'listDatabases_clusterIdentifier' - The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
--
-- 'dbUser', 'listDatabases_dbUser' - The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
--
-- 'maxResults', 'listDatabases_maxResults' - The maximum number of databases to return in the response. If more
-- databases exist than fit in one response, then @NextToken@ is returned
-- to page through the results.
--
-- 'nextToken', 'listDatabases_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'secretArn', 'listDatabases_secretArn' - The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
--
-- 'workgroupName', 'listDatabases_workgroupName' - The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
--
-- 'database', 'listDatabases_database' - The name of the database. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
newListDatabases ::
  -- | 'database'
  Prelude.Text ->
  ListDatabases
newListDatabases pDatabase_ =
  ListDatabases'
    { clusterIdentifier = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      database = pDatabase_
    }

-- | The cluster identifier. This parameter is required when connecting to a
-- cluster and authenticating using either Secrets Manager or temporary
-- credentials.
listDatabases_clusterIdentifier :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Text)
listDatabases_clusterIdentifier = Lens.lens (\ListDatabases' {clusterIdentifier} -> clusterIdentifier) (\s@ListDatabases' {} a -> s {clusterIdentifier = a} :: ListDatabases)

-- | The database user name. This parameter is required when connecting to a
-- cluster and authenticating using temporary credentials.
listDatabases_dbUser :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Text)
listDatabases_dbUser = Lens.lens (\ListDatabases' {dbUser} -> dbUser) (\s@ListDatabases' {} a -> s {dbUser = a} :: ListDatabases)

-- | The maximum number of databases to return in the response. If more
-- databases exist than fit in one response, then @NextToken@ is returned
-- to page through the results.
listDatabases_maxResults :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Natural)
listDatabases_maxResults = Lens.lens (\ListDatabases' {maxResults} -> maxResults) (\s@ListDatabases' {} a -> s {maxResults = a} :: ListDatabases)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listDatabases_nextToken :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Text)
listDatabases_nextToken = Lens.lens (\ListDatabases' {nextToken} -> nextToken) (\s@ListDatabases' {} a -> s {nextToken = a} :: ListDatabases)

-- | The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
listDatabases_secretArn :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Text)
listDatabases_secretArn = Lens.lens (\ListDatabases' {secretArn} -> secretArn) (\s@ListDatabases' {} a -> s {secretArn = a} :: ListDatabases)

-- | The serverless workgroup name. This parameter is required when
-- connecting to a serverless workgroup and authenticating using either
-- Secrets Manager or temporary credentials.
listDatabases_workgroupName :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Text)
listDatabases_workgroupName = Lens.lens (\ListDatabases' {workgroupName} -> workgroupName) (\s@ListDatabases' {} a -> s {workgroupName = a} :: ListDatabases)

-- | The name of the database. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
listDatabases_database :: Lens.Lens' ListDatabases Prelude.Text
listDatabases_database = Lens.lens (\ListDatabases' {database} -> database) (\s@ListDatabases' {} a -> s {database = a} :: ListDatabases)

instance Core.AWSPager ListDatabases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatabasesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatabasesResponse_databases
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listDatabases_nextToken
              Lens..~ rs
              Lens.^? listDatabasesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListDatabases where
  type
    AWSResponse ListDatabases =
      ListDatabasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatabasesResponse'
            Prelude.<$> (x Data..?> "Databases" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatabases where
  hashWithSalt _salt ListDatabases' {..} =
    _salt
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` workgroupName
      `Prelude.hashWithSalt` database

instance Prelude.NFData ListDatabases where
  rnf ListDatabases' {..} =
    Prelude.rnf clusterIdentifier `Prelude.seq`
      Prelude.rnf dbUser `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf secretArn `Prelude.seq`
              Prelude.rnf workgroupName `Prelude.seq`
                Prelude.rnf database

instance Data.ToHeaders ListDatabases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("RedshiftData.ListDatabases" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDatabases where
  toJSON ListDatabases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
            ("DbUser" Data..=) Prelude.<$> dbUser,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SecretArn" Data..=) Prelude.<$> secretArn,
            ("WorkgroupName" Data..=) Prelude.<$> workgroupName,
            Prelude.Just ("Database" Data..= database)
          ]
      )

instance Data.ToPath ListDatabases where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDatabases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatabasesResponse' smart constructor.
data ListDatabasesResponse = ListDatabasesResponse'
  { -- | The names of databases.
    databases :: Prelude.Maybe [Prelude.Text],
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
-- Create a value of 'ListDatabasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databases', 'listDatabasesResponse_databases' - The names of databases.
--
-- 'nextToken', 'listDatabasesResponse_nextToken' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
--
-- 'httpStatus', 'listDatabasesResponse_httpStatus' - The response's http status code.
newListDatabasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatabasesResponse
newListDatabasesResponse pHttpStatus_ =
  ListDatabasesResponse'
    { databases = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of databases.
listDatabasesResponse_databases :: Lens.Lens' ListDatabasesResponse (Prelude.Maybe [Prelude.Text])
listDatabasesResponse_databases = Lens.lens (\ListDatabasesResponse' {databases} -> databases) (\s@ListDatabasesResponse' {} a -> s {databases = a} :: ListDatabasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- NextToken value in the next NextToken parameter and retrying the
-- command. If the NextToken field is empty, all response records have been
-- retrieved for the request.
listDatabasesResponse_nextToken :: Lens.Lens' ListDatabasesResponse (Prelude.Maybe Prelude.Text)
listDatabasesResponse_nextToken = Lens.lens (\ListDatabasesResponse' {nextToken} -> nextToken) (\s@ListDatabasesResponse' {} a -> s {nextToken = a} :: ListDatabasesResponse)

-- | The response's http status code.
listDatabasesResponse_httpStatus :: Lens.Lens' ListDatabasesResponse Prelude.Int
listDatabasesResponse_httpStatus = Lens.lens (\ListDatabasesResponse' {httpStatus} -> httpStatus) (\s@ListDatabasesResponse' {} a -> s {httpStatus = a} :: ListDatabasesResponse)

instance Prelude.NFData ListDatabasesResponse where
  rnf ListDatabasesResponse' {..} =
    Prelude.rnf databases `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
