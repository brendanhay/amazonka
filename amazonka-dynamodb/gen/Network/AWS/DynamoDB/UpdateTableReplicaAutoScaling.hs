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
-- Module      : Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates auto scaling settings on your global tables at once.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- of global tables.
module Network.AWS.DynamoDB.UpdateTableReplicaAutoScaling
  ( -- * Creating a Request
    UpdateTableReplicaAutoScaling (..),
    newUpdateTableReplicaAutoScaling,

    -- * Request Lenses
    updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate,
    updateTableReplicaAutoScaling_globalSecondaryIndexUpdates,
    updateTableReplicaAutoScaling_replicaUpdates,
    updateTableReplicaAutoScaling_tableName,

    -- * Destructuring the Response
    UpdateTableReplicaAutoScalingResponse (..),
    newUpdateTableReplicaAutoScalingResponse,

    -- * Response Lenses
    updateTableReplicaAutoScalingResponse_tableAutoScalingDescription,
    updateTableReplicaAutoScalingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTableReplicaAutoScaling' smart constructor.
data UpdateTableReplicaAutoScaling = UpdateTableReplicaAutoScaling'
  { provisionedWriteCapacityAutoScalingUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | Represents the auto scaling settings of the global secondary indexes of
    -- the replica to be updated.
    globalSecondaryIndexUpdates :: Prelude.Maybe (Prelude.NonEmpty GlobalSecondaryIndexAutoScalingUpdate),
    -- | Represents the auto scaling settings of replicas of the table that will
    -- be modified.
    replicaUpdates :: Prelude.Maybe (Prelude.NonEmpty ReplicaAutoScalingUpdate),
    -- | The name of the global table to be updated.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableReplicaAutoScaling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedWriteCapacityAutoScalingUpdate', 'updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate' - Undocumented member.
--
-- 'globalSecondaryIndexUpdates', 'updateTableReplicaAutoScaling_globalSecondaryIndexUpdates' - Represents the auto scaling settings of the global secondary indexes of
-- the replica to be updated.
--
-- 'replicaUpdates', 'updateTableReplicaAutoScaling_replicaUpdates' - Represents the auto scaling settings of replicas of the table that will
-- be modified.
--
-- 'tableName', 'updateTableReplicaAutoScaling_tableName' - The name of the global table to be updated.
newUpdateTableReplicaAutoScaling ::
  -- | 'tableName'
  Prelude.Text ->
  UpdateTableReplicaAutoScaling
newUpdateTableReplicaAutoScaling pTableName_ =
  UpdateTableReplicaAutoScaling'
    { provisionedWriteCapacityAutoScalingUpdate =
        Prelude.Nothing,
      globalSecondaryIndexUpdates =
        Prelude.Nothing,
      replicaUpdates = Prelude.Nothing,
      tableName = pTableName_
    }

-- | Undocumented member.
updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate :: Lens.Lens' UpdateTableReplicaAutoScaling (Prelude.Maybe AutoScalingSettingsUpdate)
updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate = Lens.lens (\UpdateTableReplicaAutoScaling' {provisionedWriteCapacityAutoScalingUpdate} -> provisionedWriteCapacityAutoScalingUpdate) (\s@UpdateTableReplicaAutoScaling' {} a -> s {provisionedWriteCapacityAutoScalingUpdate = a} :: UpdateTableReplicaAutoScaling)

-- | Represents the auto scaling settings of the global secondary indexes of
-- the replica to be updated.
updateTableReplicaAutoScaling_globalSecondaryIndexUpdates :: Lens.Lens' UpdateTableReplicaAutoScaling (Prelude.Maybe (Prelude.NonEmpty GlobalSecondaryIndexAutoScalingUpdate))
updateTableReplicaAutoScaling_globalSecondaryIndexUpdates = Lens.lens (\UpdateTableReplicaAutoScaling' {globalSecondaryIndexUpdates} -> globalSecondaryIndexUpdates) (\s@UpdateTableReplicaAutoScaling' {} a -> s {globalSecondaryIndexUpdates = a} :: UpdateTableReplicaAutoScaling) Prelude.. Lens.mapping Lens._Coerce

-- | Represents the auto scaling settings of replicas of the table that will
-- be modified.
updateTableReplicaAutoScaling_replicaUpdates :: Lens.Lens' UpdateTableReplicaAutoScaling (Prelude.Maybe (Prelude.NonEmpty ReplicaAutoScalingUpdate))
updateTableReplicaAutoScaling_replicaUpdates = Lens.lens (\UpdateTableReplicaAutoScaling' {replicaUpdates} -> replicaUpdates) (\s@UpdateTableReplicaAutoScaling' {} a -> s {replicaUpdates = a} :: UpdateTableReplicaAutoScaling) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the global table to be updated.
updateTableReplicaAutoScaling_tableName :: Lens.Lens' UpdateTableReplicaAutoScaling Prelude.Text
updateTableReplicaAutoScaling_tableName = Lens.lens (\UpdateTableReplicaAutoScaling' {tableName} -> tableName) (\s@UpdateTableReplicaAutoScaling' {} a -> s {tableName = a} :: UpdateTableReplicaAutoScaling)

instance
  Core.AWSRequest
    UpdateTableReplicaAutoScaling
  where
  type
    AWSResponse UpdateTableReplicaAutoScaling =
      UpdateTableReplicaAutoScalingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTableReplicaAutoScalingResponse'
            Prelude.<$> (x Core..?> "TableAutoScalingDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateTableReplicaAutoScaling

instance Prelude.NFData UpdateTableReplicaAutoScaling

instance Core.ToHeaders UpdateTableReplicaAutoScaling where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.UpdateTableReplicaAutoScaling" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateTableReplicaAutoScaling where
  toJSON UpdateTableReplicaAutoScaling' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ( "ProvisionedWriteCapacityAutoScalingUpdate"
                Core..=
            )
              Prelude.<$> provisionedWriteCapacityAutoScalingUpdate,
            ("GlobalSecondaryIndexUpdates" Core..=)
              Prelude.<$> globalSecondaryIndexUpdates,
            ("ReplicaUpdates" Core..=)
              Prelude.<$> replicaUpdates,
            Prelude.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath UpdateTableReplicaAutoScaling where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateTableReplicaAutoScaling where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTableReplicaAutoScalingResponse' smart constructor.
data UpdateTableReplicaAutoScalingResponse = UpdateTableReplicaAutoScalingResponse'
  { -- | Returns information about the auto scaling settings of a table with
    -- replicas.
    tableAutoScalingDescription :: Prelude.Maybe TableAutoScalingDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableReplicaAutoScalingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableAutoScalingDescription', 'updateTableReplicaAutoScalingResponse_tableAutoScalingDescription' - Returns information about the auto scaling settings of a table with
-- replicas.
--
-- 'httpStatus', 'updateTableReplicaAutoScalingResponse_httpStatus' - The response's http status code.
newUpdateTableReplicaAutoScalingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTableReplicaAutoScalingResponse
newUpdateTableReplicaAutoScalingResponse pHttpStatus_ =
  UpdateTableReplicaAutoScalingResponse'
    { tableAutoScalingDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns information about the auto scaling settings of a table with
-- replicas.
updateTableReplicaAutoScalingResponse_tableAutoScalingDescription :: Lens.Lens' UpdateTableReplicaAutoScalingResponse (Prelude.Maybe TableAutoScalingDescription)
updateTableReplicaAutoScalingResponse_tableAutoScalingDescription = Lens.lens (\UpdateTableReplicaAutoScalingResponse' {tableAutoScalingDescription} -> tableAutoScalingDescription) (\s@UpdateTableReplicaAutoScalingResponse' {} a -> s {tableAutoScalingDescription = a} :: UpdateTableReplicaAutoScalingResponse)

-- | The response's http status code.
updateTableReplicaAutoScalingResponse_httpStatus :: Lens.Lens' UpdateTableReplicaAutoScalingResponse Prelude.Int
updateTableReplicaAutoScalingResponse_httpStatus = Lens.lens (\UpdateTableReplicaAutoScalingResponse' {httpStatus} -> httpStatus) (\s@UpdateTableReplicaAutoScalingResponse' {} a -> s {httpStatus = a} :: UpdateTableReplicaAutoScalingResponse)

instance
  Prelude.NFData
    UpdateTableReplicaAutoScalingResponse
