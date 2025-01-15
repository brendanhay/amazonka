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
-- Module      : Amazonka.Glue.GetTables
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definitions of some or all of the tables in a given
-- @Database@.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetTables
  ( -- * Creating a Request
    GetTables (..),
    newGetTables,

    -- * Request Lenses
    getTables_catalogId,
    getTables_expression,
    getTables_maxResults,
    getTables_nextToken,
    getTables_queryAsOfTime,
    getTables_transactionId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTables' smart constructor.
data GetTables = GetTables'
  { -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A regular expression pattern. If present, only those tables whose names
    -- match the pattern are returned.
    expression :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of tables to return in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, included if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time as of when to read the table contents. If not set, the most
    -- recent transaction commit time will be used. Cannot be specified along
    -- with @TransactionId@.
    queryAsOfTime :: Prelude.Maybe Data.POSIX,
    -- | The transaction ID at which to read the table contents.
    transactionId :: Prelude.Maybe Prelude.Text,
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
-- 'catalogId', 'getTables_catalogId' - The ID of the Data Catalog where the tables reside. If none is provided,
-- the Amazon Web Services account ID is used by default.
--
-- 'expression', 'getTables_expression' - A regular expression pattern. If present, only those tables whose names
-- match the pattern are returned.
--
-- 'maxResults', 'getTables_maxResults' - The maximum number of tables to return in a single response.
--
-- 'nextToken', 'getTables_nextToken' - A continuation token, included if this is a continuation call.
--
-- 'queryAsOfTime', 'getTables_queryAsOfTime' - The time as of when to read the table contents. If not set, the most
-- recent transaction commit time will be used. Cannot be specified along
-- with @TransactionId@.
--
-- 'transactionId', 'getTables_transactionId' - The transaction ID at which to read the table contents.
--
-- 'databaseName', 'getTables_databaseName' - The database in the catalog whose tables to list. For Hive
-- compatibility, this name is entirely lowercase.
newGetTables ::
  -- | 'databaseName'
  Prelude.Text ->
  GetTables
newGetTables pDatabaseName_ =
  GetTables'
    { catalogId = Prelude.Nothing,
      expression = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      queryAsOfTime = Prelude.Nothing,
      transactionId = Prelude.Nothing,
      databaseName = pDatabaseName_
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the Amazon Web Services account ID is used by default.
getTables_catalogId :: Lens.Lens' GetTables (Prelude.Maybe Prelude.Text)
getTables_catalogId = Lens.lens (\GetTables' {catalogId} -> catalogId) (\s@GetTables' {} a -> s {catalogId = a} :: GetTables)

-- | A regular expression pattern. If present, only those tables whose names
-- match the pattern are returned.
getTables_expression :: Lens.Lens' GetTables (Prelude.Maybe Prelude.Text)
getTables_expression = Lens.lens (\GetTables' {expression} -> expression) (\s@GetTables' {} a -> s {expression = a} :: GetTables)

-- | The maximum number of tables to return in a single response.
getTables_maxResults :: Lens.Lens' GetTables (Prelude.Maybe Prelude.Natural)
getTables_maxResults = Lens.lens (\GetTables' {maxResults} -> maxResults) (\s@GetTables' {} a -> s {maxResults = a} :: GetTables)

-- | A continuation token, included if this is a continuation call.
getTables_nextToken :: Lens.Lens' GetTables (Prelude.Maybe Prelude.Text)
getTables_nextToken = Lens.lens (\GetTables' {nextToken} -> nextToken) (\s@GetTables' {} a -> s {nextToken = a} :: GetTables)

-- | The time as of when to read the table contents. If not set, the most
-- recent transaction commit time will be used. Cannot be specified along
-- with @TransactionId@.
getTables_queryAsOfTime :: Lens.Lens' GetTables (Prelude.Maybe Prelude.UTCTime)
getTables_queryAsOfTime = Lens.lens (\GetTables' {queryAsOfTime} -> queryAsOfTime) (\s@GetTables' {} a -> s {queryAsOfTime = a} :: GetTables) Prelude.. Lens.mapping Data._Time

-- | The transaction ID at which to read the table contents.
getTables_transactionId :: Lens.Lens' GetTables (Prelude.Maybe Prelude.Text)
getTables_transactionId = Lens.lens (\GetTables' {transactionId} -> transactionId) (\s@GetTables' {} a -> s {transactionId = a} :: GetTables)

-- | The database in the catalog whose tables to list. For Hive
-- compatibility, this name is entirely lowercase.
getTables_databaseName :: Lens.Lens' GetTables Prelude.Text
getTables_databaseName = Lens.lens (\GetTables' {databaseName} -> databaseName) (\s@GetTables' {} a -> s {databaseName = a} :: GetTables)

instance Core.AWSPager GetTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTablesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTablesResponse_tableList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getTables_nextToken
              Lens..~ rs
              Lens.^? getTablesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetTables where
  type AWSResponse GetTables = GetTablesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTablesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TableList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTables where
  hashWithSalt _salt GetTables' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` queryAsOfTime
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData GetTables where
  rnf GetTables' {..} =
    Prelude.rnf catalogId `Prelude.seq`
      Prelude.rnf expression `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf queryAsOfTime `Prelude.seq`
              Prelude.rnf transactionId `Prelude.seq`
                Prelude.rnf databaseName

instance Data.ToHeaders GetTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetTables" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTables where
  toJSON GetTables' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("Expression" Data..=) Prelude.<$> expression,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("QueryAsOfTime" Data..=) Prelude.<$> queryAsOfTime,
            ("TransactionId" Data..=) Prelude.<$> transactionId,
            Prelude.Just ("DatabaseName" Data..= databaseName)
          ]
      )

instance Data.ToPath GetTables where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTables where
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
getTablesResponse_tableList = Lens.lens (\GetTablesResponse' {tableList} -> tableList) (\s@GetTablesResponse' {} a -> s {tableList = a} :: GetTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTablesResponse_httpStatus :: Lens.Lens' GetTablesResponse Prelude.Int
getTablesResponse_httpStatus = Lens.lens (\GetTablesResponse' {httpStatus} -> httpStatus) (\s@GetTablesResponse' {} a -> s {httpStatus = a} :: GetTablesResponse)

instance Prelude.NFData GetTablesResponse where
  rnf GetTablesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf tableList `Prelude.seq`
        Prelude.rnf httpStatus
