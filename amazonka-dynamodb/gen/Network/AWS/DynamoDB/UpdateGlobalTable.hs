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
-- Module      : Network.AWS.DynamoDB.UpdateGlobalTable
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DynamoDB.UpdateGlobalTable
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

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGlobalTable' smart constructor.
data UpdateGlobalTable = UpdateGlobalTable'
  { -- | The global table name.
    globalTableName :: Core.Text,
    -- | A list of Regions that should be added or removed from the global table.
    replicaUpdates :: [ReplicaUpdate]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateGlobalTable
newUpdateGlobalTable pGlobalTableName_ =
  UpdateGlobalTable'
    { globalTableName =
        pGlobalTableName_,
      replicaUpdates = Core.mempty
    }

-- | The global table name.
updateGlobalTable_globalTableName :: Lens.Lens' UpdateGlobalTable Core.Text
updateGlobalTable_globalTableName = Lens.lens (\UpdateGlobalTable' {globalTableName} -> globalTableName) (\s@UpdateGlobalTable' {} a -> s {globalTableName = a} :: UpdateGlobalTable)

-- | A list of Regions that should be added or removed from the global table.
updateGlobalTable_replicaUpdates :: Lens.Lens' UpdateGlobalTable [ReplicaUpdate]
updateGlobalTable_replicaUpdates = Lens.lens (\UpdateGlobalTable' {replicaUpdates} -> replicaUpdates) (\s@UpdateGlobalTable' {} a -> s {replicaUpdates = a} :: UpdateGlobalTable) Core.. Lens._Coerce

instance Core.AWSRequest UpdateGlobalTable where
  type
    AWSResponse UpdateGlobalTable =
      UpdateGlobalTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGlobalTableResponse'
            Core.<$> (x Core..?> "GlobalTableDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGlobalTable

instance Core.NFData UpdateGlobalTable

instance Core.ToHeaders UpdateGlobalTable where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.UpdateGlobalTable" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGlobalTable where
  toJSON UpdateGlobalTable' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("GlobalTableName" Core..= globalTableName),
            Core.Just ("ReplicaUpdates" Core..= replicaUpdates)
          ]
      )

instance Core.ToPath UpdateGlobalTable where
  toPath = Core.const "/"

instance Core.ToQuery UpdateGlobalTable where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGlobalTableResponse' smart constructor.
data UpdateGlobalTableResponse = UpdateGlobalTableResponse'
  { -- | Contains the details of the global table.
    globalTableDescription :: Core.Maybe GlobalTableDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateGlobalTableResponse
newUpdateGlobalTableResponse pHttpStatus_ =
  UpdateGlobalTableResponse'
    { globalTableDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the details of the global table.
updateGlobalTableResponse_globalTableDescription :: Lens.Lens' UpdateGlobalTableResponse (Core.Maybe GlobalTableDescription)
updateGlobalTableResponse_globalTableDescription = Lens.lens (\UpdateGlobalTableResponse' {globalTableDescription} -> globalTableDescription) (\s@UpdateGlobalTableResponse' {} a -> s {globalTableDescription = a} :: UpdateGlobalTableResponse)

-- | The response's http status code.
updateGlobalTableResponse_httpStatus :: Lens.Lens' UpdateGlobalTableResponse Core.Int
updateGlobalTableResponse_httpStatus = Lens.lens (\UpdateGlobalTableResponse' {httpStatus} -> httpStatus) (\s@UpdateGlobalTableResponse' {} a -> s {httpStatus = a} :: UpdateGlobalTableResponse)

instance Core.NFData UpdateGlobalTableResponse
