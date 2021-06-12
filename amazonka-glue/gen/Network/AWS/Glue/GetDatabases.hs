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
-- Module      : Network.AWS.Glue.GetDatabases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all databases defined in a given Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetDatabases
  ( -- * Creating a Request
    GetDatabases (..),
    newGetDatabases,

    -- * Request Lenses
    getDatabases_nextToken,
    getDatabases_catalogId,
    getDatabases_maxResults,
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDatabases' smart constructor.
data GetDatabases = GetDatabases'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the Data Catalog from which to retrieve @Databases@. If none
    -- is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The maximum number of databases to return in one response.
    maxResults :: Core.Maybe Core.Natural,
    -- | Allows you to specify that you want to list the databases shared with
    -- your account. The allowable values are @FOREIGN@ or @ALL@.
    --
    -- -   If set to @FOREIGN@, will list the databases shared with your
    --     account.
    --
    -- -   If set to @ALL@, will list the databases shared with your account,
    --     as well as the databases in yor local account.
    resourceShareType :: Core.Maybe ResourceShareType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDatabases_nextToken' - A continuation token, if this is a continuation call.
--
-- 'catalogId', 'getDatabases_catalogId' - The ID of the Data Catalog from which to retrieve @Databases@. If none
-- is provided, the AWS account ID is used by default.
--
-- 'maxResults', 'getDatabases_maxResults' - The maximum number of databases to return in one response.
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
    { nextToken = Core.Nothing,
      catalogId = Core.Nothing,
      maxResults = Core.Nothing,
      resourceShareType = Core.Nothing
    }

-- | A continuation token, if this is a continuation call.
getDatabases_nextToken :: Lens.Lens' GetDatabases (Core.Maybe Core.Text)
getDatabases_nextToken = Lens.lens (\GetDatabases' {nextToken} -> nextToken) (\s@GetDatabases' {} a -> s {nextToken = a} :: GetDatabases)

-- | The ID of the Data Catalog from which to retrieve @Databases@. If none
-- is provided, the AWS account ID is used by default.
getDatabases_catalogId :: Lens.Lens' GetDatabases (Core.Maybe Core.Text)
getDatabases_catalogId = Lens.lens (\GetDatabases' {catalogId} -> catalogId) (\s@GetDatabases' {} a -> s {catalogId = a} :: GetDatabases)

-- | The maximum number of databases to return in one response.
getDatabases_maxResults :: Lens.Lens' GetDatabases (Core.Maybe Core.Natural)
getDatabases_maxResults = Lens.lens (\GetDatabases' {maxResults} -> maxResults) (\s@GetDatabases' {} a -> s {maxResults = a} :: GetDatabases)

-- | Allows you to specify that you want to list the databases shared with
-- your account. The allowable values are @FOREIGN@ or @ALL@.
--
-- -   If set to @FOREIGN@, will list the databases shared with your
--     account.
--
-- -   If set to @ALL@, will list the databases shared with your account,
--     as well as the databases in yor local account.
getDatabases_resourceShareType :: Lens.Lens' GetDatabases (Core.Maybe ResourceShareType)
getDatabases_resourceShareType = Lens.lens (\GetDatabases' {resourceShareType} -> resourceShareType) (\s@GetDatabases' {} a -> s {resourceShareType = a} :: GetDatabases)

instance Core.AWSPager GetDatabases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDatabasesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. getDatabasesResponse_databaseList) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getDatabases_nextToken
          Lens..~ rs
          Lens.^? getDatabasesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetDatabases where
  type AWSResponse GetDatabases = GetDatabasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatabasesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "DatabaseList" Core..!@ Core.mempty)
      )

instance Core.Hashable GetDatabases

instance Core.NFData GetDatabases

instance Core.ToHeaders GetDatabases where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetDatabases" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDatabases where
  toJSON GetDatabases' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ResourceShareType" Core..=)
              Core.<$> resourceShareType
          ]
      )

instance Core.ToPath GetDatabases where
  toPath = Core.const "/"

instance Core.ToQuery GetDatabases where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDatabasesResponse' smart constructor.
data GetDatabasesResponse = GetDatabasesResponse'
  { -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of @Database@ objects from the specified catalog.
    databaseList :: [Database]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetDatabasesResponse
newGetDatabasesResponse pHttpStatus_ =
  GetDatabasesResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      databaseList = Core.mempty
    }

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
getDatabasesResponse_nextToken :: Lens.Lens' GetDatabasesResponse (Core.Maybe Core.Text)
getDatabasesResponse_nextToken = Lens.lens (\GetDatabasesResponse' {nextToken} -> nextToken) (\s@GetDatabasesResponse' {} a -> s {nextToken = a} :: GetDatabasesResponse)

-- | The response's http status code.
getDatabasesResponse_httpStatus :: Lens.Lens' GetDatabasesResponse Core.Int
getDatabasesResponse_httpStatus = Lens.lens (\GetDatabasesResponse' {httpStatus} -> httpStatus) (\s@GetDatabasesResponse' {} a -> s {httpStatus = a} :: GetDatabasesResponse)

-- | A list of @Database@ objects from the specified catalog.
getDatabasesResponse_databaseList :: Lens.Lens' GetDatabasesResponse [Database]
getDatabasesResponse_databaseList = Lens.lens (\GetDatabasesResponse' {databaseList} -> databaseList) (\s@GetDatabasesResponse' {} a -> s {databaseList = a} :: GetDatabasesResponse) Core.. Lens._Coerce

instance Core.NFData GetDatabasesResponse
