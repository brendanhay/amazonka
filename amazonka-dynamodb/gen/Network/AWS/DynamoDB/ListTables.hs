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
-- Module      : Network.AWS.DynamoDB.ListTables
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DynamoDB.ListTables
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

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListTables@ operation.
--
-- /See:/ 'newListTables' smart constructor.
data ListTables = ListTables'
  { -- | The first table name that this operation will evaluate. Use the value
    -- that was returned for @LastEvaluatedTableName@ in a previous operation,
    -- so that you can obtain the next page of results.
    exclusiveStartTableName :: Core.Maybe Core.Text,
    -- | A maximum number of table names to return. If this parameter is not
    -- specified, the limit is 100.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { exclusiveStartTableName = Core.Nothing,
      limit = Core.Nothing
    }

-- | The first table name that this operation will evaluate. Use the value
-- that was returned for @LastEvaluatedTableName@ in a previous operation,
-- so that you can obtain the next page of results.
listTables_exclusiveStartTableName :: Lens.Lens' ListTables (Core.Maybe Core.Text)
listTables_exclusiveStartTableName = Lens.lens (\ListTables' {exclusiveStartTableName} -> exclusiveStartTableName) (\s@ListTables' {} a -> s {exclusiveStartTableName = a} :: ListTables)

-- | A maximum number of table names to return. If this parameter is not
-- specified, the limit is 100.
listTables_limit :: Lens.Lens' ListTables (Core.Maybe Core.Natural)
listTables_limit = Lens.lens (\ListTables' {limit} -> limit) (\s@ListTables' {} a -> s {limit = a} :: ListTables)

instance Core.AWSPager ListTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTablesResponse_lastEvaluatedTableName
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTablesResponse_tableNames Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTables_exclusiveStartTableName
          Lens..~ rs
          Lens.^? listTablesResponse_lastEvaluatedTableName
            Core.. Lens._Just

instance Core.AWSRequest ListTables where
  type AWSResponse ListTables = ListTablesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTablesResponse'
            Core.<$> (x Core..?> "LastEvaluatedTableName")
            Core.<*> (x Core..?> "TableNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTables

instance Core.NFData ListTables

instance Core.ToHeaders ListTables where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DynamoDB_20120810.ListTables" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTables where
  toJSON ListTables' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExclusiveStartTableName" Core..=)
              Core.<$> exclusiveStartTableName,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListTables where
  toPath = Core.const "/"

instance Core.ToQuery ListTables where
  toQuery = Core.const Core.mempty

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
    lastEvaluatedTableName :: Core.Maybe Core.Text,
    -- | The names of the tables associated with the current account at the
    -- current endpoint. The maximum size of this array is 100.
    --
    -- If @LastEvaluatedTableName@ also appears in the output, you can use this
    -- value as the @ExclusiveStartTableName@ parameter in a subsequent
    -- @ListTables@ request and obtain the next page of results.
    tableNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListTablesResponse
newListTablesResponse pHttpStatus_ =
  ListTablesResponse'
    { lastEvaluatedTableName =
        Core.Nothing,
      tableNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the last table in the current page of results. Use this
-- value as the @ExclusiveStartTableName@ in a new request to obtain the
-- next page of results, until all the table names are returned.
--
-- If you do not receive a @LastEvaluatedTableName@ value in the response,
-- this means that there are no more table names to be retrieved.
listTablesResponse_lastEvaluatedTableName :: Lens.Lens' ListTablesResponse (Core.Maybe Core.Text)
listTablesResponse_lastEvaluatedTableName = Lens.lens (\ListTablesResponse' {lastEvaluatedTableName} -> lastEvaluatedTableName) (\s@ListTablesResponse' {} a -> s {lastEvaluatedTableName = a} :: ListTablesResponse)

-- | The names of the tables associated with the current account at the
-- current endpoint. The maximum size of this array is 100.
--
-- If @LastEvaluatedTableName@ also appears in the output, you can use this
-- value as the @ExclusiveStartTableName@ parameter in a subsequent
-- @ListTables@ request and obtain the next page of results.
listTablesResponse_tableNames :: Lens.Lens' ListTablesResponse (Core.Maybe [Core.Text])
listTablesResponse_tableNames = Lens.lens (\ListTablesResponse' {tableNames} -> tableNames) (\s@ListTablesResponse' {} a -> s {tableNames = a} :: ListTablesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTablesResponse_httpStatus :: Lens.Lens' ListTablesResponse Core.Int
listTablesResponse_httpStatus = Lens.lens (\ListTablesResponse' {httpStatus} -> httpStatus) (\s@ListTablesResponse' {} a -> s {httpStatus = a} :: ListTablesResponse)

instance Core.NFData ListTablesResponse
