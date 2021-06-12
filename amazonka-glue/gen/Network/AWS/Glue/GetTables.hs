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
-- Module      : Network.AWS.Glue.GetTables
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definitions of some or all of the tables in a given
-- @Database@.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTables
  ( -- * Creating a Request
    GetTables (..),
    newGetTables,

    -- * Request Lenses
    getTables_nextToken,
    getTables_catalogId,
    getTables_maxResults,
    getTables_expression,
    getTables_databaseName,

    -- * Destructuring the Response
    GetTablesResponse (..),
    newGetTablesResponse,

    -- * Response Lenses
    getTablesResponse_nextToken,
    getTablesResponse_tableList,
    getTablesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTables' smart constructor.
data GetTables = GetTables'
  { -- | A continuation token, included if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The maximum number of tables to return in a single response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A regular expression pattern. If present, only those tables whose names
    -- match the pattern are returned.
    expression :: Core.Maybe Core.Text,
    -- | The database in the catalog whose tables to list. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTables_nextToken' - A continuation token, included if this is a continuation call.
--
-- 'catalogId', 'getTables_catalogId' - The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
--
-- 'maxResults', 'getTables_maxResults' - The maximum number of tables to return in a single response.
--
-- 'expression', 'getTables_expression' - A regular expression pattern. If present, only those tables whose names
-- match the pattern are returned.
--
-- 'databaseName', 'getTables_databaseName' - The database in the catalog whose tables to list. For Hive
-- compatibility, this name is entirely lowercase.
newGetTables ::
  -- | 'databaseName'
  Core.Text ->
  GetTables
newGetTables pDatabaseName_ =
  GetTables'
    { nextToken = Core.Nothing,
      catalogId = Core.Nothing,
      maxResults = Core.Nothing,
      expression = Core.Nothing,
      databaseName = pDatabaseName_
    }

-- | A continuation token, included if this is a continuation call.
getTables_nextToken :: Lens.Lens' GetTables (Core.Maybe Core.Text)
getTables_nextToken = Lens.lens (\GetTables' {nextToken} -> nextToken) (\s@GetTables' {} a -> s {nextToken = a} :: GetTables)

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
getTables_catalogId :: Lens.Lens' GetTables (Core.Maybe Core.Text)
getTables_catalogId = Lens.lens (\GetTables' {catalogId} -> catalogId) (\s@GetTables' {} a -> s {catalogId = a} :: GetTables)

-- | The maximum number of tables to return in a single response.
getTables_maxResults :: Lens.Lens' GetTables (Core.Maybe Core.Natural)
getTables_maxResults = Lens.lens (\GetTables' {maxResults} -> maxResults) (\s@GetTables' {} a -> s {maxResults = a} :: GetTables)

-- | A regular expression pattern. If present, only those tables whose names
-- match the pattern are returned.
getTables_expression :: Lens.Lens' GetTables (Core.Maybe Core.Text)
getTables_expression = Lens.lens (\GetTables' {expression} -> expression) (\s@GetTables' {} a -> s {expression = a} :: GetTables)

-- | The database in the catalog whose tables to list. For Hive
-- compatibility, this name is entirely lowercase.
getTables_databaseName :: Lens.Lens' GetTables Core.Text
getTables_databaseName = Lens.lens (\GetTables' {databaseName} -> databaseName) (\s@GetTables' {} a -> s {databaseName = a} :: GetTables)

instance Core.AWSPager GetTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTablesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTablesResponse_tableList Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTables_nextToken
          Lens..~ rs
          Lens.^? getTablesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetTables where
  type AWSResponse GetTables = GetTablesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTablesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "TableList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTables

instance Core.NFData GetTables

instance Core.ToHeaders GetTables where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetTables" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTables where
  toJSON GetTables' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Expression" Core..=) Core.<$> expression,
            Core.Just ("DatabaseName" Core..= databaseName)
          ]
      )

instance Core.ToPath GetTables where
  toPath = Core.const "/"

instance Core.ToQuery GetTables where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTablesResponse' smart constructor.
data GetTablesResponse = GetTablesResponse'
  { -- | A continuation token, present if the current list segment is not the
    -- last.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of the requested @Table@ objects.
    tableList :: Core.Maybe [Table],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTablesResponse_nextToken' - A continuation token, present if the current list segment is not the
-- last.
--
-- 'tableList', 'getTablesResponse_tableList' - A list of the requested @Table@ objects.
--
-- 'httpStatus', 'getTablesResponse_httpStatus' - The response's http status code.
newGetTablesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTablesResponse
newGetTablesResponse pHttpStatus_ =
  GetTablesResponse'
    { nextToken = Core.Nothing,
      tableList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, present if the current list segment is not the
-- last.
getTablesResponse_nextToken :: Lens.Lens' GetTablesResponse (Core.Maybe Core.Text)
getTablesResponse_nextToken = Lens.lens (\GetTablesResponse' {nextToken} -> nextToken) (\s@GetTablesResponse' {} a -> s {nextToken = a} :: GetTablesResponse)

-- | A list of the requested @Table@ objects.
getTablesResponse_tableList :: Lens.Lens' GetTablesResponse (Core.Maybe [Table])
getTablesResponse_tableList = Lens.lens (\GetTablesResponse' {tableList} -> tableList) (\s@GetTablesResponse' {} a -> s {tableList = a} :: GetTablesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTablesResponse_httpStatus :: Lens.Lens' GetTablesResponse Core.Int
getTablesResponse_httpStatus = Lens.lens (\GetTablesResponse' {httpStatus} -> httpStatus) (\s@GetTablesResponse' {} a -> s {httpStatus = a} :: GetTablesResponse)

instance Core.NFData GetTablesResponse
