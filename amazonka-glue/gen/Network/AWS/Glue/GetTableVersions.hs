{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTableVersions' smart constructor.
data GetTableVersions = GetTableVersions'
  { -- | A continuation token, if this is not the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of table versions to return in one response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The database in the catalog in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely
    -- lowercase.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GetTableVersions
newGetTableVersions pDatabaseName_ pTableName_ =
  GetTableVersions'
    { nextToken = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | A continuation token, if this is not the first call.
getTableVersions_nextToken :: Lens.Lens' GetTableVersions (Prelude.Maybe Prelude.Text)
getTableVersions_nextToken = Lens.lens (\GetTableVersions' {nextToken} -> nextToken) (\s@GetTableVersions' {} a -> s {nextToken = a} :: GetTableVersions)

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
getTableVersions_catalogId :: Lens.Lens' GetTableVersions (Prelude.Maybe Prelude.Text)
getTableVersions_catalogId = Lens.lens (\GetTableVersions' {catalogId} -> catalogId) (\s@GetTableVersions' {} a -> s {catalogId = a} :: GetTableVersions)

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

instance Pager.AWSPager GetTableVersions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getTableVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getTableVersionsResponse_tableVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getTableVersions_nextToken
          Lens..~ rs
          Lens.^? getTableVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetTableVersions where
  type Rs GetTableVersions = GetTableVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableVersionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "TableVersions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTableVersions

instance Prelude.NFData GetTableVersions

instance Prelude.ToHeaders GetTableVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.GetTableVersions" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetTableVersions where
  toJSON GetTableVersions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just ("TableName" Prelude..= tableName)
          ]
      )

instance Prelude.ToPath GetTableVersions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetTableVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTableVersionsResponse' smart constructor.
data GetTableVersionsResponse = GetTableVersionsResponse'
  { -- | A continuation token, if the list of available versions does not include
    -- the last one.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of strings identifying available versions of the specified table.
    tableVersions :: Prelude.Maybe [TableVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetTableVersionsResponse
newGetTableVersionsResponse pHttpStatus_ =
  GetTableVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      tableVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the list of available versions does not include
-- the last one.
getTableVersionsResponse_nextToken :: Lens.Lens' GetTableVersionsResponse (Prelude.Maybe Prelude.Text)
getTableVersionsResponse_nextToken = Lens.lens (\GetTableVersionsResponse' {nextToken} -> nextToken) (\s@GetTableVersionsResponse' {} a -> s {nextToken = a} :: GetTableVersionsResponse)

-- | A list of strings identifying available versions of the specified table.
getTableVersionsResponse_tableVersions :: Lens.Lens' GetTableVersionsResponse (Prelude.Maybe [TableVersion])
getTableVersionsResponse_tableVersions = Lens.lens (\GetTableVersionsResponse' {tableVersions} -> tableVersions) (\s@GetTableVersionsResponse' {} a -> s {tableVersions = a} :: GetTableVersionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getTableVersionsResponse_httpStatus :: Lens.Lens' GetTableVersionsResponse Prelude.Int
getTableVersionsResponse_httpStatus = Lens.lens (\GetTableVersionsResponse' {httpStatus} -> httpStatus) (\s@GetTableVersionsResponse' {} a -> s {httpStatus = a} :: GetTableVersionsResponse)

instance Prelude.NFData GetTableVersionsResponse
