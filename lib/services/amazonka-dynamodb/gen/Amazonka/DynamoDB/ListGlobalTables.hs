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
-- Module      : Amazonka.DynamoDB.ListGlobalTables
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.DynamoDB.ListGlobalTables
  ( -- * Creating a Request
    ListGlobalTables (..),
    newListGlobalTables,

    -- * Request Lenses
    listGlobalTables_exclusiveStartGlobalTableName,
    listGlobalTables_regionName,
    listGlobalTables_limit,

    -- * Destructuring the Response
    ListGlobalTablesResponse (..),
    newListGlobalTablesResponse,

    -- * Response Lenses
    listGlobalTablesResponse_globalTables,
    listGlobalTablesResponse_lastEvaluatedGlobalTableName,
    listGlobalTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGlobalTables' smart constructor.
data ListGlobalTables = ListGlobalTables'
  { -- | The first global table name that this operation will evaluate.
    exclusiveStartGlobalTableName :: Prelude.Maybe Prelude.Text,
    -- | Lists the global tables in a specific Region.
    regionName :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGlobalTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartGlobalTableName', 'listGlobalTables_exclusiveStartGlobalTableName' - The first global table name that this operation will evaluate.
--
-- 'regionName', 'listGlobalTables_regionName' - Lists the global tables in a specific Region.
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
    { exclusiveStartGlobalTableName =
        Prelude.Nothing,
      regionName = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The first global table name that this operation will evaluate.
listGlobalTables_exclusiveStartGlobalTableName :: Lens.Lens' ListGlobalTables (Prelude.Maybe Prelude.Text)
listGlobalTables_exclusiveStartGlobalTableName = Lens.lens (\ListGlobalTables' {exclusiveStartGlobalTableName} -> exclusiveStartGlobalTableName) (\s@ListGlobalTables' {} a -> s {exclusiveStartGlobalTableName = a} :: ListGlobalTables)

-- | Lists the global tables in a specific Region.
listGlobalTables_regionName :: Lens.Lens' ListGlobalTables (Prelude.Maybe Prelude.Text)
listGlobalTables_regionName = Lens.lens (\ListGlobalTables' {regionName} -> regionName) (\s@ListGlobalTables' {} a -> s {regionName = a} :: ListGlobalTables)

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

instance Core.AWSRequest ListGlobalTables where
  type
    AWSResponse ListGlobalTables =
      ListGlobalTablesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGlobalTablesResponse'
            Prelude.<$> (x Data..?> "GlobalTables" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "LastEvaluatedGlobalTableName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGlobalTables where
  hashWithSalt _salt ListGlobalTables' {..} =
    _salt
      `Prelude.hashWithSalt` exclusiveStartGlobalTableName
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListGlobalTables where
  rnf ListGlobalTables' {..} =
    Prelude.rnf exclusiveStartGlobalTableName
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf limit

instance Data.ToHeaders ListGlobalTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ListGlobalTables" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGlobalTables where
  toJSON ListGlobalTables' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartGlobalTableName" Data..=)
              Prelude.<$> exclusiveStartGlobalTableName,
            ("RegionName" Data..=) Prelude.<$> regionName,
            ("Limit" Data..=) Prelude.<$> limit
          ]
      )

instance Data.ToPath ListGlobalTables where
  toPath = Prelude.const "/"

instance Data.ToQuery ListGlobalTables where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGlobalTablesResponse' smart constructor.
data ListGlobalTablesResponse = ListGlobalTablesResponse'
  { -- | List of global table names.
    globalTables :: Prelude.Maybe [GlobalTable],
    -- | Last evaluated global table name.
    lastEvaluatedGlobalTableName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGlobalTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTables', 'listGlobalTablesResponse_globalTables' - List of global table names.
--
-- 'lastEvaluatedGlobalTableName', 'listGlobalTablesResponse_lastEvaluatedGlobalTableName' - Last evaluated global table name.
--
-- 'httpStatus', 'listGlobalTablesResponse_httpStatus' - The response's http status code.
newListGlobalTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGlobalTablesResponse
newListGlobalTablesResponse pHttpStatus_ =
  ListGlobalTablesResponse'
    { globalTables =
        Prelude.Nothing,
      lastEvaluatedGlobalTableName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of global table names.
listGlobalTablesResponse_globalTables :: Lens.Lens' ListGlobalTablesResponse (Prelude.Maybe [GlobalTable])
listGlobalTablesResponse_globalTables = Lens.lens (\ListGlobalTablesResponse' {globalTables} -> globalTables) (\s@ListGlobalTablesResponse' {} a -> s {globalTables = a} :: ListGlobalTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Last evaluated global table name.
listGlobalTablesResponse_lastEvaluatedGlobalTableName :: Lens.Lens' ListGlobalTablesResponse (Prelude.Maybe Prelude.Text)
listGlobalTablesResponse_lastEvaluatedGlobalTableName = Lens.lens (\ListGlobalTablesResponse' {lastEvaluatedGlobalTableName} -> lastEvaluatedGlobalTableName) (\s@ListGlobalTablesResponse' {} a -> s {lastEvaluatedGlobalTableName = a} :: ListGlobalTablesResponse)

-- | The response's http status code.
listGlobalTablesResponse_httpStatus :: Lens.Lens' ListGlobalTablesResponse Prelude.Int
listGlobalTablesResponse_httpStatus = Lens.lens (\ListGlobalTablesResponse' {httpStatus} -> httpStatus) (\s@ListGlobalTablesResponse' {} a -> s {httpStatus = a} :: ListGlobalTablesResponse)

instance Prelude.NFData ListGlobalTablesResponse where
  rnf ListGlobalTablesResponse' {..} =
    Prelude.rnf globalTables
      `Prelude.seq` Prelude.rnf lastEvaluatedGlobalTableName
      `Prelude.seq` Prelude.rnf httpStatus
