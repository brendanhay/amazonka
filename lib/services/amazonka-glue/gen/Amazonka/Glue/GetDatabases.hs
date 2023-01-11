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
-- Module      : Amazonka.Glue.GetDatabases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all databases defined in a given Data Catalog.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetDatabases
  ( -- * Creating a Request
    GetDatabases (..),
    newGetDatabases,

    -- * Request Lenses
    getDatabases_catalogId,
    getDatabases_maxResults,
    getDatabases_nextToken,
    getDatabases_resourceShareType,

    -- * Destructuring the Response
    GetDatabasesResponse (..),
    newGetDatabasesResponse,

    -- * Response Lenses
    getDatabasesResponse_nextToken,
    getDatabasesResponse_httpStatus,
    getDatabasesResponse_databaseList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDatabases' smart constructor.
data GetDatabases = GetDatabases'
  { -- | The ID of the Data Catalog from which to retrieve @Databases@. If none
    -- is provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of databases to return in one response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Allows you to specify that you want to list the databases shared with
    -- your account. The allowable values are @FOREIGN@ or @ALL@.
    --
    -- -   If set to @FOREIGN@, will list the databases shared with your
    --     account.
    --
    -- -   If set to @ALL@, will list the databases shared with your account,
    --     as well as the databases in yor local account.
    resourceShareType :: Prelude.Maybe ResourceShareType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getDatabases_catalogId' - The ID of the Data Catalog from which to retrieve @Databases@. If none
-- is provided, the Amazon Web Services account ID is used by default.
--
-- 'maxResults', 'getDatabases_maxResults' - The maximum number of databases to return in one response.
--
-- 'nextToken', 'getDatabases_nextToken' - A continuation token, if this is a continuation call.
--
-- 'resourceShareType', 'getDatabases_resourceShareType' - Allows you to specify that you want to list the databases shared with
-- your account. The allowable values are @FOREIGN@ or @ALL@.
--
-- -   If set to @FOREIGN@, will list the databases shared with your
--     account.
--
-- -   If set to @ALL@, will list the databases shared with your account,
--     as well as the databases in yor local account.
newGetDatabases ::
  GetDatabases
newGetDatabases =
  GetDatabases'
    { catalogId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceShareType = Prelude.Nothing
    }

-- | The ID of the Data Catalog from which to retrieve @Databases@. If none
-- is provided, the Amazon Web Services account ID is used by default.
getDatabases_catalogId :: Lens.Lens' GetDatabases (Prelude.Maybe Prelude.Text)
getDatabases_catalogId = Lens.lens (\GetDatabases' {catalogId} -> catalogId) (\s@GetDatabases' {} a -> s {catalogId = a} :: GetDatabases)

-- | The maximum number of databases to return in one response.
getDatabases_maxResults :: Lens.Lens' GetDatabases (Prelude.Maybe Prelude.Natural)
getDatabases_maxResults = Lens.lens (\GetDatabases' {maxResults} -> maxResults) (\s@GetDatabases' {} a -> s {maxResults = a} :: GetDatabases)

-- | A continuation token, if this is a continuation call.
getDatabases_nextToken :: Lens.Lens' GetDatabases (Prelude.Maybe Prelude.Text)
getDatabases_nextToken = Lens.lens (\GetDatabases' {nextToken} -> nextToken) (\s@GetDatabases' {} a -> s {nextToken = a} :: GetDatabases)

-- | Allows you to specify that you want to list the databases shared with
-- your account. The allowable values are @FOREIGN@ or @ALL@.
--
-- -   If set to @FOREIGN@, will list the databases shared with your
--     account.
--
-- -   If set to @ALL@, will list the databases shared with your account,
--     as well as the databases in yor local account.
getDatabases_resourceShareType :: Lens.Lens' GetDatabases (Prelude.Maybe ResourceShareType)
getDatabases_resourceShareType = Lens.lens (\GetDatabases' {resourceShareType} -> resourceShareType) (\s@GetDatabases' {} a -> s {resourceShareType = a} :: GetDatabases)

instance Core.AWSPager GetDatabases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDatabasesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. getDatabasesResponse_databaseList) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getDatabases_nextToken
          Lens..~ rs
          Lens.^? getDatabasesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetDatabases where
  type AWSResponse GetDatabases = GetDatabasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatabasesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "DatabaseList" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetDatabases where
  hashWithSalt _salt GetDatabases' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceShareType

instance Prelude.NFData GetDatabases where
  rnf GetDatabases' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceShareType

instance Data.ToHeaders GetDatabases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetDatabases" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDatabases where
  toJSON GetDatabases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceShareType" Data..=)
              Prelude.<$> resourceShareType
          ]
      )

instance Data.ToPath GetDatabases where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDatabases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDatabasesResponse' smart constructor.
data GetDatabasesResponse = GetDatabasesResponse'
  { -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @Database@ objects from the specified catalog.
    databaseList :: [Database]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatabasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDatabasesResponse_nextToken' - A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
--
-- 'httpStatus', 'getDatabasesResponse_httpStatus' - The response's http status code.
--
-- 'databaseList', 'getDatabasesResponse_databaseList' - A list of @Database@ objects from the specified catalog.
newGetDatabasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDatabasesResponse
newGetDatabasesResponse pHttpStatus_ =
  GetDatabasesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      databaseList = Prelude.mempty
    }

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
getDatabasesResponse_nextToken :: Lens.Lens' GetDatabasesResponse (Prelude.Maybe Prelude.Text)
getDatabasesResponse_nextToken = Lens.lens (\GetDatabasesResponse' {nextToken} -> nextToken) (\s@GetDatabasesResponse' {} a -> s {nextToken = a} :: GetDatabasesResponse)

-- | The response's http status code.
getDatabasesResponse_httpStatus :: Lens.Lens' GetDatabasesResponse Prelude.Int
getDatabasesResponse_httpStatus = Lens.lens (\GetDatabasesResponse' {httpStatus} -> httpStatus) (\s@GetDatabasesResponse' {} a -> s {httpStatus = a} :: GetDatabasesResponse)

-- | A list of @Database@ objects from the specified catalog.
getDatabasesResponse_databaseList :: Lens.Lens' GetDatabasesResponse [Database]
getDatabasesResponse_databaseList = Lens.lens (\GetDatabasesResponse' {databaseList} -> databaseList) (\s@GetDatabasesResponse' {} a -> s {databaseList = a} :: GetDatabasesResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetDatabasesResponse where
  rnf GetDatabasesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf databaseList
