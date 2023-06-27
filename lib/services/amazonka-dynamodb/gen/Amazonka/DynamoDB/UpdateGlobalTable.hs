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
-- Module      : Amazonka.DynamoDB.UpdateGlobalTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes replicas in the specified global table. The global table
-- must already exist to be able to use this operation. Any replica to be
-- added must be empty, have the same name as the global table, have the
-- same key schema, have DynamoDB Streams enabled, and have the same
-- provisioned and maximum write capacity units.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V1.html Version 2017.11.29 (Legacy)>
-- of global tables. We recommend using
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21 (Current)>
-- when creating new global tables, as it provides greater flexibility,
-- higher efficiency and consumes less write capacity than 2017.11.29
-- (Legacy). To determine which version you are using, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.DetermineVersion.html Determining the version>.
-- To update existing global tables from version 2017.11.29 (Legacy) to
-- version 2019.11.21 (Current), see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/V2globaltables_upgrade.html Updating global tables>.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V1.html Version 2017.11.29>
-- of global tables. If you are using global tables
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- you can use
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DescribeTable.html DescribeTable>
-- instead.
--
-- Although you can use @UpdateGlobalTable@ to add replicas and remove
-- replicas in a single request, for simplicity we recommend that you issue
-- separate requests for adding or removing replicas.
--
-- If global secondary indexes are specified, then the following conditions
-- must also be met:
--
-- -   The global secondary indexes must have the same name.
--
-- -   The global secondary indexes must have the same hash key and sort
--     key (if present).
--
-- -   The global secondary indexes must have the same provisioned and
--     maximum write capacity units.
module Amazonka.DynamoDB.UpdateGlobalTable
  ( -- * Creating a Request
    UpdateGlobalTable (..),
    newUpdateGlobalTable,

    -- * Request Lenses
    updateGlobalTable_globalTableName,
    updateGlobalTable_replicaUpdates,

    -- * Destructuring the Response
    UpdateGlobalTableResponse (..),
    newUpdateGlobalTableResponse,

    -- * Response Lenses
    updateGlobalTableResponse_globalTableDescription,
    updateGlobalTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGlobalTable' smart constructor.
data UpdateGlobalTable = UpdateGlobalTable'
  { -- | The global table name.
    globalTableName :: Prelude.Text,
    -- | A list of Regions that should be added or removed from the global table.
    replicaUpdates :: [ReplicaUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTableName', 'updateGlobalTable_globalTableName' - The global table name.
--
-- 'replicaUpdates', 'updateGlobalTable_replicaUpdates' - A list of Regions that should be added or removed from the global table.
newUpdateGlobalTable ::
  -- | 'globalTableName'
  Prelude.Text ->
  UpdateGlobalTable
newUpdateGlobalTable pGlobalTableName_ =
  UpdateGlobalTable'
    { globalTableName =
        pGlobalTableName_,
      replicaUpdates = Prelude.mempty
    }

-- | The global table name.
updateGlobalTable_globalTableName :: Lens.Lens' UpdateGlobalTable Prelude.Text
updateGlobalTable_globalTableName = Lens.lens (\UpdateGlobalTable' {globalTableName} -> globalTableName) (\s@UpdateGlobalTable' {} a -> s {globalTableName = a} :: UpdateGlobalTable)

-- | A list of Regions that should be added or removed from the global table.
updateGlobalTable_replicaUpdates :: Lens.Lens' UpdateGlobalTable [ReplicaUpdate]
updateGlobalTable_replicaUpdates = Lens.lens (\UpdateGlobalTable' {replicaUpdates} -> replicaUpdates) (\s@UpdateGlobalTable' {} a -> s {replicaUpdates = a} :: UpdateGlobalTable) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateGlobalTable where
  type
    AWSResponse UpdateGlobalTable =
      UpdateGlobalTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGlobalTableResponse'
            Prelude.<$> (x Data..?> "GlobalTableDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGlobalTable where
  hashWithSalt _salt UpdateGlobalTable' {..} =
    _salt
      `Prelude.hashWithSalt` globalTableName
      `Prelude.hashWithSalt` replicaUpdates

instance Prelude.NFData UpdateGlobalTable where
  rnf UpdateGlobalTable' {..} =
    Prelude.rnf globalTableName
      `Prelude.seq` Prelude.rnf replicaUpdates

instance Data.ToHeaders UpdateGlobalTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.UpdateGlobalTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGlobalTable where
  toJSON UpdateGlobalTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GlobalTableName" Data..= globalTableName),
            Prelude.Just
              ("ReplicaUpdates" Data..= replicaUpdates)
          ]
      )

instance Data.ToPath UpdateGlobalTable where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateGlobalTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGlobalTableResponse' smart constructor.
data UpdateGlobalTableResponse = UpdateGlobalTableResponse'
  { -- | Contains the details of the global table.
    globalTableDescription :: Prelude.Maybe GlobalTableDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTableDescription', 'updateGlobalTableResponse_globalTableDescription' - Contains the details of the global table.
--
-- 'httpStatus', 'updateGlobalTableResponse_httpStatus' - The response's http status code.
newUpdateGlobalTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGlobalTableResponse
newUpdateGlobalTableResponse pHttpStatus_ =
  UpdateGlobalTableResponse'
    { globalTableDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the details of the global table.
updateGlobalTableResponse_globalTableDescription :: Lens.Lens' UpdateGlobalTableResponse (Prelude.Maybe GlobalTableDescription)
updateGlobalTableResponse_globalTableDescription = Lens.lens (\UpdateGlobalTableResponse' {globalTableDescription} -> globalTableDescription) (\s@UpdateGlobalTableResponse' {} a -> s {globalTableDescription = a} :: UpdateGlobalTableResponse)

-- | The response's http status code.
updateGlobalTableResponse_httpStatus :: Lens.Lens' UpdateGlobalTableResponse Prelude.Int
updateGlobalTableResponse_httpStatus = Lens.lens (\UpdateGlobalTableResponse' {httpStatus} -> httpStatus) (\s@UpdateGlobalTableResponse' {} a -> s {httpStatus = a} :: UpdateGlobalTableResponse)

instance Prelude.NFData UpdateGlobalTableResponse where
  rnf UpdateGlobalTableResponse' {..} =
    Prelude.rnf globalTableDescription
      `Prelude.seq` Prelude.rnf httpStatus
