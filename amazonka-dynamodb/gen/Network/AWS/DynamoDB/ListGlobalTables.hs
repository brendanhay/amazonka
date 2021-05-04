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

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGlobalTables' smart constructor.
data ListGlobalTables = ListGlobalTables'
  { -- | Lists the global tables in a specific Region.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | The first global table name that this operation will evaluate.
    exclusiveStartGlobalTableName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of table names to return, if the parameter is not
    -- specified DynamoDB defaults to 100.
    --
    -- If the number of global tables DynamoDB finds reaches this limit, it
    -- stops the operation and returns the table names collected up to that
    -- point, with a table name in the @LastEvaluatedGlobalTableName@ to apply
    -- in a subsequent operation to the @ExclusiveStartGlobalTableName@
    -- parameter.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { regionName = Prelude.Nothing,
      exclusiveStartGlobalTableName = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | Lists the global tables in a specific Region.
listGlobalTables_regionName :: Lens.Lens' ListGlobalTables (Prelude.Maybe Prelude.Text)
listGlobalTables_regionName = Lens.lens (\ListGlobalTables' {regionName} -> regionName) (\s@ListGlobalTables' {} a -> s {regionName = a} :: ListGlobalTables)

-- | The first global table name that this operation will evaluate.
listGlobalTables_exclusiveStartGlobalTableName :: Lens.Lens' ListGlobalTables (Prelude.Maybe Prelude.Text)
listGlobalTables_exclusiveStartGlobalTableName = Lens.lens (\ListGlobalTables' {exclusiveStartGlobalTableName} -> exclusiveStartGlobalTableName) (\s@ListGlobalTables' {} a -> s {exclusiveStartGlobalTableName = a} :: ListGlobalTables)

-- | The maximum number of table names to return, if the parameter is not
-- specified DynamoDB defaults to 100.
--
-- If the number of global tables DynamoDB finds reaches this limit, it
-- stops the operation and returns the table names collected up to that
-- point, with a table name in the @LastEvaluatedGlobalTableName@ to apply
-- in a subsequent operation to the @ExclusiveStartGlobalTableName@
-- parameter.
listGlobalTables_limit :: Lens.Lens' ListGlobalTables (Prelude.Maybe Prelude.Natural)
listGlobalTables_limit = Lens.lens (\ListGlobalTables' {limit} -> limit) (\s@ListGlobalTables' {} a -> s {limit = a} :: ListGlobalTables)

instance Prelude.AWSRequest ListGlobalTables where
  type Rs ListGlobalTables = ListGlobalTablesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGlobalTablesResponse'
            Prelude.<$> (x Prelude..?> "LastEvaluatedGlobalTableName")
            Prelude.<*> ( x Prelude..?> "GlobalTables"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGlobalTables

instance Prelude.NFData ListGlobalTables

instance Prelude.ToHeaders ListGlobalTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DynamoDB_20120810.ListGlobalTables" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListGlobalTables where
  toJSON ListGlobalTables' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RegionName" Prelude..=) Prelude.<$> regionName,
            ("ExclusiveStartGlobalTableName" Prelude..=)
              Prelude.<$> exclusiveStartGlobalTableName,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath ListGlobalTables where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListGlobalTables where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGlobalTablesResponse' smart constructor.
data ListGlobalTablesResponse = ListGlobalTablesResponse'
  { -- | Last evaluated global table name.
    lastEvaluatedGlobalTableName :: Prelude.Maybe Prelude.Text,
    -- | List of global table names.
    globalTables :: Prelude.Maybe [GlobalTable],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListGlobalTablesResponse
newListGlobalTablesResponse pHttpStatus_ =
  ListGlobalTablesResponse'
    { lastEvaluatedGlobalTableName =
        Prelude.Nothing,
      globalTables = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Last evaluated global table name.
listGlobalTablesResponse_lastEvaluatedGlobalTableName :: Lens.Lens' ListGlobalTablesResponse (Prelude.Maybe Prelude.Text)
listGlobalTablesResponse_lastEvaluatedGlobalTableName = Lens.lens (\ListGlobalTablesResponse' {lastEvaluatedGlobalTableName} -> lastEvaluatedGlobalTableName) (\s@ListGlobalTablesResponse' {} a -> s {lastEvaluatedGlobalTableName = a} :: ListGlobalTablesResponse)

-- | List of global table names.
listGlobalTablesResponse_globalTables :: Lens.Lens' ListGlobalTablesResponse (Prelude.Maybe [GlobalTable])
listGlobalTablesResponse_globalTables = Lens.lens (\ListGlobalTablesResponse' {globalTables} -> globalTables) (\s@ListGlobalTablesResponse' {} a -> s {globalTables = a} :: ListGlobalTablesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listGlobalTablesResponse_httpStatus :: Lens.Lens' ListGlobalTablesResponse Prelude.Int
listGlobalTablesResponse_httpStatus = Lens.lens (\ListGlobalTablesResponse' {httpStatus} -> httpStatus) (\s@ListGlobalTablesResponse' {} a -> s {httpStatus = a} :: ListGlobalTablesResponse)

instance Prelude.NFData ListGlobalTablesResponse
