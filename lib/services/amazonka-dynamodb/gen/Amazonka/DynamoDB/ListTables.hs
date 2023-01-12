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
-- Module      : Amazonka.DynamoDB.ListTables
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of table names associated with the current account and
-- endpoint. The output from @ListTables@ is paginated, with each page
-- returning a maximum of 100 table names.
--
-- This operation returns paginated results.
module Amazonka.DynamoDB.ListTables
  ( -- * Creating a Request
    ListTables (..),
    newListTables,

    -- * Request Lenses
    listTables_exclusiveStartTableName,
    listTables_limit,

    -- * Destructuring the Response
    ListTablesResponse (..),
    newListTablesResponse,

    -- * Response Lenses
    listTablesResponse_lastEvaluatedTableName,
    listTablesResponse_tableNames,
    listTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ListTables@ operation.
--
-- /See:/ 'newListTables' smart constructor.
data ListTables = ListTables'
  { -- | The first table name that this operation will evaluate. Use the value
    -- that was returned for @LastEvaluatedTableName@ in a previous operation,
    -- so that you can obtain the next page of results.
    exclusiveStartTableName :: Prelude.Maybe Prelude.Text,
    -- | A maximum number of table names to return. If this parameter is not
    -- specified, the limit is 100.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartTableName', 'listTables_exclusiveStartTableName' - The first table name that this operation will evaluate. Use the value
-- that was returned for @LastEvaluatedTableName@ in a previous operation,
-- so that you can obtain the next page of results.
--
-- 'limit', 'listTables_limit' - A maximum number of table names to return. If this parameter is not
-- specified, the limit is 100.
newListTables ::
  ListTables
newListTables =
  ListTables'
    { exclusiveStartTableName =
        Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The first table name that this operation will evaluate. Use the value
-- that was returned for @LastEvaluatedTableName@ in a previous operation,
-- so that you can obtain the next page of results.
listTables_exclusiveStartTableName :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_exclusiveStartTableName = Lens.lens (\ListTables' {exclusiveStartTableName} -> exclusiveStartTableName) (\s@ListTables' {} a -> s {exclusiveStartTableName = a} :: ListTables)

-- | A maximum number of table names to return. If this parameter is not
-- specified, the limit is 100.
listTables_limit :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Natural)
listTables_limit = Lens.lens (\ListTables' {limit} -> limit) (\s@ListTables' {} a -> s {limit = a} :: ListTables)

instance Core.AWSPager ListTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTablesResponse_lastEvaluatedTableName
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTablesResponse_tableNames Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTables_exclusiveStartTableName
          Lens..~ rs
          Lens.^? listTablesResponse_lastEvaluatedTableName
            Prelude.. Lens._Just

instance Core.AWSRequest ListTables where
  type AWSResponse ListTables = ListTablesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTablesResponse'
            Prelude.<$> (x Data..?> "LastEvaluatedTableName")
            Prelude.<*> (x Data..?> "TableNames" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTables where
  hashWithSalt _salt ListTables' {..} =
    _salt
      `Prelude.hashWithSalt` exclusiveStartTableName
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListTables where
  rnf ListTables' {..} =
    Prelude.rnf exclusiveStartTableName
      `Prelude.seq` Prelude.rnf limit

instance Data.ToHeaders ListTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ListTables" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTables where
  toJSON ListTables' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartTableName" Data..=)
              Prelude.<$> exclusiveStartTableName,
            ("Limit" Data..=) Prelude.<$> limit
          ]
      )

instance Data.ToPath ListTables where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTables where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListTables@ operation.
--
-- /See:/ 'newListTablesResponse' smart constructor.
data ListTablesResponse = ListTablesResponse'
  { -- | The name of the last table in the current page of results. Use this
    -- value as the @ExclusiveStartTableName@ in a new request to obtain the
    -- next page of results, until all the table names are returned.
    --
    -- If you do not receive a @LastEvaluatedTableName@ value in the response,
    -- this means that there are no more table names to be retrieved.
    lastEvaluatedTableName :: Prelude.Maybe Prelude.Text,
    -- | The names of the tables associated with the current account at the
    -- current endpoint. The maximum size of this array is 100.
    --
    -- If @LastEvaluatedTableName@ also appears in the output, you can use this
    -- value as the @ExclusiveStartTableName@ parameter in a subsequent
    -- @ListTables@ request and obtain the next page of results.
    tableNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastEvaluatedTableName', 'listTablesResponse_lastEvaluatedTableName' - The name of the last table in the current page of results. Use this
-- value as the @ExclusiveStartTableName@ in a new request to obtain the
-- next page of results, until all the table names are returned.
--
-- If you do not receive a @LastEvaluatedTableName@ value in the response,
-- this means that there are no more table names to be retrieved.
--
-- 'tableNames', 'listTablesResponse_tableNames' - The names of the tables associated with the current account at the
-- current endpoint. The maximum size of this array is 100.
--
-- If @LastEvaluatedTableName@ also appears in the output, you can use this
-- value as the @ExclusiveStartTableName@ parameter in a subsequent
-- @ListTables@ request and obtain the next page of results.
--
-- 'httpStatus', 'listTablesResponse_httpStatus' - The response's http status code.
newListTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTablesResponse
newListTablesResponse pHttpStatus_ =
  ListTablesResponse'
    { lastEvaluatedTableName =
        Prelude.Nothing,
      tableNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the last table in the current page of results. Use this
-- value as the @ExclusiveStartTableName@ in a new request to obtain the
-- next page of results, until all the table names are returned.
--
-- If you do not receive a @LastEvaluatedTableName@ value in the response,
-- this means that there are no more table names to be retrieved.
listTablesResponse_lastEvaluatedTableName :: Lens.Lens' ListTablesResponse (Prelude.Maybe Prelude.Text)
listTablesResponse_lastEvaluatedTableName = Lens.lens (\ListTablesResponse' {lastEvaluatedTableName} -> lastEvaluatedTableName) (\s@ListTablesResponse' {} a -> s {lastEvaluatedTableName = a} :: ListTablesResponse)

-- | The names of the tables associated with the current account at the
-- current endpoint. The maximum size of this array is 100.
--
-- If @LastEvaluatedTableName@ also appears in the output, you can use this
-- value as the @ExclusiveStartTableName@ parameter in a subsequent
-- @ListTables@ request and obtain the next page of results.
listTablesResponse_tableNames :: Lens.Lens' ListTablesResponse (Prelude.Maybe [Prelude.Text])
listTablesResponse_tableNames = Lens.lens (\ListTablesResponse' {tableNames} -> tableNames) (\s@ListTablesResponse' {} a -> s {tableNames = a} :: ListTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTablesResponse_httpStatus :: Lens.Lens' ListTablesResponse Prelude.Int
listTablesResponse_httpStatus = Lens.lens (\ListTablesResponse' {httpStatus} -> httpStatus) (\s@ListTablesResponse' {} a -> s {httpStatus = a} :: ListTablesResponse)

instance Prelude.NFData ListTablesResponse where
  rnf ListTablesResponse' {..} =
    Prelude.rnf lastEvaluatedTableName
      `Prelude.seq` Prelude.rnf tableNames
      `Prelude.seq` Prelude.rnf httpStatus
