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
-- Module      : Network.AWS.DynamoDB.ListGlobalTables
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all global tables that have a replica in the specified Region.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V1.html Version 2017.11.29>
-- of global tables.
module Network.AWS.DynamoDB.ListGlobalTables
  ( -- * Creating a Request
    ListGlobalTables (..),
    newListGlobalTables,

    -- * Request Lenses
    listGlobalTables_regionName,
    listGlobalTables_exclusiveStartGlobalTableName,
    listGlobalTables_limit,

    -- * Destructuring the Response
    ListGlobalTablesResponse (..),
    newListGlobalTablesResponse,

    -- * Response Lenses
    listGlobalTablesResponse_lastEvaluatedGlobalTableName,
    listGlobalTablesResponse_globalTables,
    listGlobalTablesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGlobalTables' smart constructor.
data ListGlobalTables = ListGlobalTables'
  { -- | Lists the global tables in a specific Region.
    regionName :: Core.Maybe Core.Text,
    -- | The first global table name that this operation will evaluate.
    exclusiveStartGlobalTableName :: Core.Maybe Core.Text,
    -- | The maximum number of table names to return, if the parameter is not
    -- specified DynamoDB defaults to 100.
    --
    -- If the number of global tables DynamoDB finds reaches this limit, it
    -- stops the operation and returns the table names collected up to that
    -- point, with a table name in the @LastEvaluatedGlobalTableName@ to apply
    -- in a subsequent operation to the @ExclusiveStartGlobalTableName@
    -- parameter.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGlobalTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'listGlobalTables_regionName' - Lists the global tables in a specific Region.
--
-- 'exclusiveStartGlobalTableName', 'listGlobalTables_exclusiveStartGlobalTableName' - The first global table name that this operation will evaluate.
--
-- 'limit', 'listGlobalTables_limit' - The maximum number of table names to return, if the parameter is not
-- specified DynamoDB defaults to 100.
--
-- If the number of global tables DynamoDB finds reaches this limit, it
-- stops the operation and returns the table names collected up to that
-- point, with a table name in the @LastEvaluatedGlobalTableName@ to apply
-- in a subsequent operation to the @ExclusiveStartGlobalTableName@
-- parameter.
newListGlobalTables ::
  ListGlobalTables
newListGlobalTables =
  ListGlobalTables'
    { regionName = Core.Nothing,
      exclusiveStartGlobalTableName = Core.Nothing,
      limit = Core.Nothing
    }

-- | Lists the global tables in a specific Region.
listGlobalTables_regionName :: Lens.Lens' ListGlobalTables (Core.Maybe Core.Text)
listGlobalTables_regionName = Lens.lens (\ListGlobalTables' {regionName} -> regionName) (\s@ListGlobalTables' {} a -> s {regionName = a} :: ListGlobalTables)

-- | The first global table name that this operation will evaluate.
listGlobalTables_exclusiveStartGlobalTableName :: Lens.Lens' ListGlobalTables (Core.Maybe Core.Text)
listGlobalTables_exclusiveStartGlobalTableName = Lens.lens (\ListGlobalTables' {exclusiveStartGlobalTableName} -> exclusiveStartGlobalTableName) (\s@ListGlobalTables' {} a -> s {exclusiveStartGlobalTableName = a} :: ListGlobalTables)

-- | The maximum number of table names to return, if the parameter is not
-- specified DynamoDB defaults to 100.
--
-- If the number of global tables DynamoDB finds reaches this limit, it
-- stops the operation and returns the table names collected up to that
-- point, with a table name in the @LastEvaluatedGlobalTableName@ to apply
-- in a subsequent operation to the @ExclusiveStartGlobalTableName@
-- parameter.
listGlobalTables_limit :: Lens.Lens' ListGlobalTables (Core.Maybe Core.Natural)
listGlobalTables_limit = Lens.lens (\ListGlobalTables' {limit} -> limit) (\s@ListGlobalTables' {} a -> s {limit = a} :: ListGlobalTables)

instance Core.AWSRequest ListGlobalTables where
  type
    AWSResponse ListGlobalTables =
      ListGlobalTablesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGlobalTablesResponse'
            Core.<$> (x Core..?> "LastEvaluatedGlobalTableName")
            Core.<*> (x Core..?> "GlobalTables" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGlobalTables

instance Core.NFData ListGlobalTables

instance Core.ToHeaders ListGlobalTables where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.ListGlobalTables" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListGlobalTables where
  toJSON ListGlobalTables' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RegionName" Core..=) Core.<$> regionName,
            ("ExclusiveStartGlobalTableName" Core..=)
              Core.<$> exclusiveStartGlobalTableName,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListGlobalTables where
  toPath = Core.const "/"

instance Core.ToQuery ListGlobalTables where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListGlobalTablesResponse' smart constructor.
data ListGlobalTablesResponse = ListGlobalTablesResponse'
  { -- | Last evaluated global table name.
    lastEvaluatedGlobalTableName :: Core.Maybe Core.Text,
    -- | List of global table names.
    globalTables :: Core.Maybe [GlobalTable],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGlobalTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastEvaluatedGlobalTableName', 'listGlobalTablesResponse_lastEvaluatedGlobalTableName' - Last evaluated global table name.
--
-- 'globalTables', 'listGlobalTablesResponse_globalTables' - List of global table names.
--
-- 'httpStatus', 'listGlobalTablesResponse_httpStatus' - The response's http status code.
newListGlobalTablesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListGlobalTablesResponse
newListGlobalTablesResponse pHttpStatus_ =
  ListGlobalTablesResponse'
    { lastEvaluatedGlobalTableName =
        Core.Nothing,
      globalTables = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Last evaluated global table name.
listGlobalTablesResponse_lastEvaluatedGlobalTableName :: Lens.Lens' ListGlobalTablesResponse (Core.Maybe Core.Text)
listGlobalTablesResponse_lastEvaluatedGlobalTableName = Lens.lens (\ListGlobalTablesResponse' {lastEvaluatedGlobalTableName} -> lastEvaluatedGlobalTableName) (\s@ListGlobalTablesResponse' {} a -> s {lastEvaluatedGlobalTableName = a} :: ListGlobalTablesResponse)

-- | List of global table names.
listGlobalTablesResponse_globalTables :: Lens.Lens' ListGlobalTablesResponse (Core.Maybe [GlobalTable])
listGlobalTablesResponse_globalTables = Lens.lens (\ListGlobalTablesResponse' {globalTables} -> globalTables) (\s@ListGlobalTablesResponse' {} a -> s {globalTables = a} :: ListGlobalTablesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGlobalTablesResponse_httpStatus :: Lens.Lens' ListGlobalTablesResponse Core.Int
listGlobalTablesResponse_httpStatus = Lens.lens (\ListGlobalTablesResponse' {httpStatus} -> httpStatus) (\s@ListGlobalTablesResponse' {} a -> s {httpStatus = a} :: ListGlobalTablesResponse)

instance Core.NFData ListGlobalTablesResponse
