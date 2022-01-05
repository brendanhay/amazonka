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
-- Module      : Amazonka.Glue.GetTableVersions
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
module Amazonka.Glue.GetTableVersions
  ( -- * Creating a Request
    GetTableVersions (..),
    newGetTableVersions,

    -- * Request Lenses
    getTableVersions_catalogId,
    getTableVersions_nextToken,
    getTableVersions_maxResults,
    getTableVersions_databaseName,
    getTableVersions_tableName,

    -- * Destructuring the Response
    GetTableVersionsResponse (..),
    newGetTableVersionsResponse,

    -- * Response Lenses
    getTableVersionsResponse_tableVersions,
    getTableVersionsResponse_nextToken,
    getTableVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTableVersions' smart constructor.
data GetTableVersions = GetTableVersions'
  { -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A continuation token, if this is not the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of table versions to return in one response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The database in the catalog in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely
    -- lowercase.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getTableVersions_catalogId' - The ID of the Data Catalog where the tables reside. If none is provided,
-- the Amazon Web Services account ID is used by default.
--
-- 'nextToken', 'getTableVersions_nextToken' - A continuation token, if this is not the first call.
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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GetTableVersions
newGetTableVersions pDatabaseName_ pTableName_ =
  GetTableVersions'
    { catalogId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the Amazon Web Services account ID is used by default.
getTableVersions_catalogId :: Lens.Lens' GetTableVersions (Prelude.Maybe Prelude.Text)
getTableVersions_catalogId = Lens.lens (\GetTableVersions' {catalogId} -> catalogId) (\s@GetTableVersions' {} a -> s {catalogId = a} :: GetTableVersions)

-- | A continuation token, if this is not the first call.
getTableVersions_nextToken :: Lens.Lens' GetTableVersions (Prelude.Maybe Prelude.Text)
getTableVersions_nextToken = Lens.lens (\GetTableVersions' {nextToken} -> nextToken) (\s@GetTableVersions' {} a -> s {nextToken = a} :: GetTableVersions)

-- | The maximum number of table versions to return in one response.
getTableVersions_maxResults :: Lens.Lens' GetTableVersions (Prelude.Maybe Prelude.Natural)
getTableVersions_maxResults = Lens.lens (\GetTableVersions' {maxResults} -> maxResults) (\s@GetTableVersions' {} a -> s {maxResults = a} :: GetTableVersions)

-- | The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
getTableVersions_databaseName :: Lens.Lens' GetTableVersions Prelude.Text
getTableVersions_databaseName = Lens.lens (\GetTableVersions' {databaseName} -> databaseName) (\s@GetTableVersions' {} a -> s {databaseName = a} :: GetTableVersions)

-- | The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
getTableVersions_tableName :: Lens.Lens' GetTableVersions Prelude.Text
getTableVersions_tableName = Lens.lens (\GetTableVersions' {tableName} -> tableName) (\s@GetTableVersions' {} a -> s {tableName = a} :: GetTableVersions)

instance Core.AWSPager GetTableVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTableVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTableVersionsResponse_tableVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTableVersions_nextToken
          Lens..~ rs
          Lens.^? getTableVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetTableVersions where
  type
    AWSResponse GetTableVersions =
      GetTableVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableVersionsResponse'
            Prelude.<$> (x Core..?> "TableVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTableVersions where
  hashWithSalt _salt GetTableVersions' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData GetTableVersions where
  rnf GetTableVersions' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName

instance Core.ToHeaders GetTableVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetTableVersions" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTableVersions where
  toJSON GetTableVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath GetTableVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTableVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTableVersionsResponse' smart constructor.
data GetTableVersionsResponse = GetTableVersionsResponse'
  { -- | A list of strings identifying available versions of the specified table.
    tableVersions :: Prelude.Maybe [TableVersion],
    -- | A continuation token, if the list of available versions does not include
    -- the last one.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableVersions', 'getTableVersionsResponse_tableVersions' - A list of strings identifying available versions of the specified table.
--
-- 'nextToken', 'getTableVersionsResponse_nextToken' - A continuation token, if the list of available versions does not include
-- the last one.
--
-- 'httpStatus', 'getTableVersionsResponse_httpStatus' - The response's http status code.
newGetTableVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTableVersionsResponse
newGetTableVersionsResponse pHttpStatus_ =
  GetTableVersionsResponse'
    { tableVersions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of strings identifying available versions of the specified table.
getTableVersionsResponse_tableVersions :: Lens.Lens' GetTableVersionsResponse (Prelude.Maybe [TableVersion])
getTableVersionsResponse_tableVersions = Lens.lens (\GetTableVersionsResponse' {tableVersions} -> tableVersions) (\s@GetTableVersionsResponse' {} a -> s {tableVersions = a} :: GetTableVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if the list of available versions does not include
-- the last one.
getTableVersionsResponse_nextToken :: Lens.Lens' GetTableVersionsResponse (Prelude.Maybe Prelude.Text)
getTableVersionsResponse_nextToken = Lens.lens (\GetTableVersionsResponse' {nextToken} -> nextToken) (\s@GetTableVersionsResponse' {} a -> s {nextToken = a} :: GetTableVersionsResponse)

-- | The response's http status code.
getTableVersionsResponse_httpStatus :: Lens.Lens' GetTableVersionsResponse Prelude.Int
getTableVersionsResponse_httpStatus = Lens.lens (\GetTableVersionsResponse' {httpStatus} -> httpStatus) (\s@GetTableVersionsResponse' {} a -> s {httpStatus = a} :: GetTableVersionsResponse)

instance Prelude.NFData GetTableVersionsResponse where
  rnf GetTableVersionsResponse' {..} =
    Prelude.rnf tableVersions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
