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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTables' smart constructor.
data GetTables = GetTables'
  { -- | A continuation token, included if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of tables to return in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A regular expression pattern. If present, only those tables whose names
    -- match the pattern are returned.
    expression :: Prelude.Maybe Prelude.Text,
    -- | The database in the catalog whose tables to list. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetTables
newGetTables pDatabaseName_ =
  GetTables'
    { nextToken = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      expression = Prelude.Nothing,
      databaseName = pDatabaseName_
    }

-- | A continuation token, included if this is a continuation call.
getTables_nextToken :: Lens.Lens' GetTables (Prelude.Maybe Prelude.Text)
getTables_nextToken = Lens.lens (\GetTables' {nextToken} -> nextToken) (\s@GetTables' {} a -> s {nextToken = a} :: GetTables)

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
getTables_catalogId :: Lens.Lens' GetTables (Prelude.Maybe Prelude.Text)
getTables_catalogId = Lens.lens (\GetTables' {catalogId} -> catalogId) (\s@GetTables' {} a -> s {catalogId = a} :: GetTables)

-- | The maximum number of tables to return in a single response.
getTables_maxResults :: Lens.Lens' GetTables (Prelude.Maybe Prelude.Natural)
getTables_maxResults = Lens.lens (\GetTables' {maxResults} -> maxResults) (\s@GetTables' {} a -> s {maxResults = a} :: GetTables)

-- | A regular expression pattern. If present, only those tables whose names
-- match the pattern are returned.
getTables_expression :: Lens.Lens' GetTables (Prelude.Maybe Prelude.Text)
getTables_expression = Lens.lens (\GetTables' {expression} -> expression) (\s@GetTables' {} a -> s {expression = a} :: GetTables)

-- | The database in the catalog whose tables to list. For Hive
-- compatibility, this name is entirely lowercase.
getTables_databaseName :: Lens.Lens' GetTables Prelude.Text
getTables_databaseName = Lens.lens (\GetTables' {databaseName} -> databaseName) (\s@GetTables' {} a -> s {databaseName = a} :: GetTables)

instance Core.AWSPager GetTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTablesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTablesResponse_tableList Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTables_nextToken
          Lens..~ rs
          Lens.^? getTablesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetTables where
  type AWSResponse GetTables = GetTablesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTablesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "TableList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTables

instance Prelude.NFData GetTables

instance Core.ToHeaders GetTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetTables" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTables where
  toJSON GetTables' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("CatalogId" Core..=) Prelude.<$> catalogId,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Expression" Core..=) Prelude.<$> expression,
            Prelude.Just ("DatabaseName" Core..= databaseName)
          ]
      )

instance Core.ToPath GetTables where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTables where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTablesResponse' smart constructor.
data GetTablesResponse = GetTablesResponse'
  { -- | A continuation token, present if the current list segment is not the
    -- last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the requested @Table@ objects.
    tableList :: Prelude.Maybe [Table],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTablesResponse
newGetTablesResponse pHttpStatus_ =
  GetTablesResponse'
    { nextToken = Prelude.Nothing,
      tableList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, present if the current list segment is not the
-- last.
getTablesResponse_nextToken :: Lens.Lens' GetTablesResponse (Prelude.Maybe Prelude.Text)
getTablesResponse_nextToken = Lens.lens (\GetTablesResponse' {nextToken} -> nextToken) (\s@GetTablesResponse' {} a -> s {nextToken = a} :: GetTablesResponse)

-- | A list of the requested @Table@ objects.
getTablesResponse_tableList :: Lens.Lens' GetTablesResponse (Prelude.Maybe [Table])
getTablesResponse_tableList = Lens.lens (\GetTablesResponse' {tableList} -> tableList) (\s@GetTablesResponse' {} a -> s {tableList = a} :: GetTablesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTablesResponse_httpStatus :: Lens.Lens' GetTablesResponse Prelude.Int
getTablesResponse_httpStatus = Lens.lens (\GetTablesResponse' {httpStatus} -> httpStatus) (\s@GetTablesResponse' {} a -> s {httpStatus = a} :: GetTablesResponse)

instance Prelude.NFData GetTablesResponse
