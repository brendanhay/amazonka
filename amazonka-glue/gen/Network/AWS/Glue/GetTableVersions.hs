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
-- Module      : Network.AWS.Glue.GetTableVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of strings that identify available versions of a
-- specified table.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTableVersions
  ( -- * Creating a Request
    GetTableVersions (..),
    newGetTableVersions,

    -- * Request Lenses
    getTableVersions_nextToken,
    getTableVersions_catalogId,
    getTableVersions_maxResults,
    getTableVersions_databaseName,
    getTableVersions_tableName,

    -- * Destructuring the Response
    GetTableVersionsResponse (..),
    newGetTableVersionsResponse,

    -- * Response Lenses
    getTableVersionsResponse_nextToken,
    getTableVersionsResponse_tableVersions,
    getTableVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTableVersions' smart constructor.
data GetTableVersions = GetTableVersions'
  { -- | A continuation token, if this is not the first call.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The maximum number of table versions to return in one response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The database in the catalog in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Core.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely
    -- lowercase.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTableVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTableVersions_nextToken' - A continuation token, if this is not the first call.
--
-- 'catalogId', 'getTableVersions_catalogId' - The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
--
-- 'maxResults', 'getTableVersions_maxResults' - The maximum number of table versions to return in one response.
--
-- 'databaseName', 'getTableVersions_databaseName' - The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
--
-- 'tableName', 'getTableVersions_tableName' - The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
newGetTableVersions ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  GetTableVersions
newGetTableVersions pDatabaseName_ pTableName_ =
  GetTableVersions'
    { nextToken = Core.Nothing,
      catalogId = Core.Nothing,
      maxResults = Core.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | A continuation token, if this is not the first call.
getTableVersions_nextToken :: Lens.Lens' GetTableVersions (Core.Maybe Core.Text)
getTableVersions_nextToken = Lens.lens (\GetTableVersions' {nextToken} -> nextToken) (\s@GetTableVersions' {} a -> s {nextToken = a} :: GetTableVersions)

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
getTableVersions_catalogId :: Lens.Lens' GetTableVersions (Core.Maybe Core.Text)
getTableVersions_catalogId = Lens.lens (\GetTableVersions' {catalogId} -> catalogId) (\s@GetTableVersions' {} a -> s {catalogId = a} :: GetTableVersions)

-- | The maximum number of table versions to return in one response.
getTableVersions_maxResults :: Lens.Lens' GetTableVersions (Core.Maybe Core.Natural)
getTableVersions_maxResults = Lens.lens (\GetTableVersions' {maxResults} -> maxResults) (\s@GetTableVersions' {} a -> s {maxResults = a} :: GetTableVersions)

-- | The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
getTableVersions_databaseName :: Lens.Lens' GetTableVersions Core.Text
getTableVersions_databaseName = Lens.lens (\GetTableVersions' {databaseName} -> databaseName) (\s@GetTableVersions' {} a -> s {databaseName = a} :: GetTableVersions)

-- | The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
getTableVersions_tableName :: Lens.Lens' GetTableVersions Core.Text
getTableVersions_tableName = Lens.lens (\GetTableVersions' {tableName} -> tableName) (\s@GetTableVersions' {} a -> s {tableName = a} :: GetTableVersions)

instance Core.AWSPager GetTableVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTableVersionsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTableVersionsResponse_tableVersions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTableVersions_nextToken
          Lens..~ rs
          Lens.^? getTableVersionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetTableVersions where
  type
    AWSResponse GetTableVersions =
      GetTableVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "TableVersions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTableVersions

instance Core.NFData GetTableVersions

instance Core.ToHeaders GetTableVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetTableVersions" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTableVersions where
  toJSON GetTableVersions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath GetTableVersions where
  toPath = Core.const "/"

instance Core.ToQuery GetTableVersions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTableVersionsResponse' smart constructor.
data GetTableVersionsResponse = GetTableVersionsResponse'
  { -- | A continuation token, if the list of available versions does not include
    -- the last one.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of strings identifying available versions of the specified table.
    tableVersions :: Core.Maybe [TableVersion],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTableVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTableVersionsResponse_nextToken' - A continuation token, if the list of available versions does not include
-- the last one.
--
-- 'tableVersions', 'getTableVersionsResponse_tableVersions' - A list of strings identifying available versions of the specified table.
--
-- 'httpStatus', 'getTableVersionsResponse_httpStatus' - The response's http status code.
newGetTableVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTableVersionsResponse
newGetTableVersionsResponse pHttpStatus_ =
  GetTableVersionsResponse'
    { nextToken = Core.Nothing,
      tableVersions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the list of available versions does not include
-- the last one.
getTableVersionsResponse_nextToken :: Lens.Lens' GetTableVersionsResponse (Core.Maybe Core.Text)
getTableVersionsResponse_nextToken = Lens.lens (\GetTableVersionsResponse' {nextToken} -> nextToken) (\s@GetTableVersionsResponse' {} a -> s {nextToken = a} :: GetTableVersionsResponse)

-- | A list of strings identifying available versions of the specified table.
getTableVersionsResponse_tableVersions :: Lens.Lens' GetTableVersionsResponse (Core.Maybe [TableVersion])
getTableVersionsResponse_tableVersions = Lens.lens (\GetTableVersionsResponse' {tableVersions} -> tableVersions) (\s@GetTableVersionsResponse' {} a -> s {tableVersions = a} :: GetTableVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTableVersionsResponse_httpStatus :: Lens.Lens' GetTableVersionsResponse Core.Int
getTableVersionsResponse_httpStatus = Lens.lens (\GetTableVersionsResponse' {httpStatus} -> httpStatus) (\s@GetTableVersionsResponse' {} a -> s {httpStatus = a} :: GetTableVersionsResponse)

instance Core.NFData GetTableVersionsResponse
