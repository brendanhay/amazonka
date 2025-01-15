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
-- Module      : Amazonka.DynamoDB.UpdateTableReplicaAutoScaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates auto scaling settings on your global tables at once.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- of global tables.
module Amazonka.DynamoDB.UpdateTableReplicaAutoScaling
  ( -- * Creating a Request
    UpdateTableReplicaAutoScaling (..),
    newUpdateTableReplicaAutoScaling,

    -- * Request Lenses
    updateTableReplicaAutoScaling_globalSecondaryIndexUpdates,
    updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTableReplicaAutoScaling' smart constructor.
data UpdateTableReplicaAutoScaling = UpdateTableReplicaAutoScaling'
  { -- | Represents the auto scaling settings of the global secondary indexes of
    -- the replica to be updated.
    globalSecondaryIndexUpdates :: Prelude.Maybe (Prelude.NonEmpty GlobalSecondaryIndexAutoScalingUpdate),
    provisionedWriteCapacityAutoScalingUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
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
-- 'globalSecondaryIndexUpdates', 'updateTableReplicaAutoScaling_globalSecondaryIndexUpdates' - Represents the auto scaling settings of the global secondary indexes of
-- the replica to be updated.
--
-- 'provisionedWriteCapacityAutoScalingUpdate', 'updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate' - Undocumented member.
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
    { globalSecondaryIndexUpdates =
        Prelude.Nothing,
      provisionedWriteCapacityAutoScalingUpdate =
        Prelude.Nothing,
      replicaUpdates = Prelude.Nothing,
      tableName = pTableName_
    }

-- | Represents the auto scaling settings of the global secondary indexes of
-- the replica to be updated.
updateTableReplicaAutoScaling_globalSecondaryIndexUpdates :: Lens.Lens' UpdateTableReplicaAutoScaling (Prelude.Maybe (Prelude.NonEmpty GlobalSecondaryIndexAutoScalingUpdate))
updateTableReplicaAutoScaling_globalSecondaryIndexUpdates = Lens.lens (\UpdateTableReplicaAutoScaling' {globalSecondaryIndexUpdates} -> globalSecondaryIndexUpdates) (\s@UpdateTableReplicaAutoScaling' {} a -> s {globalSecondaryIndexUpdates = a} :: UpdateTableReplicaAutoScaling) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate :: Lens.Lens' UpdateTableReplicaAutoScaling (Prelude.Maybe AutoScalingSettingsUpdate)
updateTableReplicaAutoScaling_provisionedWriteCapacityAutoScalingUpdate = Lens.lens (\UpdateTableReplicaAutoScaling' {provisionedWriteCapacityAutoScalingUpdate} -> provisionedWriteCapacityAutoScalingUpdate) (\s@UpdateTableReplicaAutoScaling' {} a -> s {provisionedWriteCapacityAutoScalingUpdate = a} :: UpdateTableReplicaAutoScaling)

-- | Represents the auto scaling settings of replicas of the table that will
-- be modified.
updateTableReplicaAutoScaling_replicaUpdates :: Lens.Lens' UpdateTableReplicaAutoScaling (Prelude.Maybe (Prelude.NonEmpty ReplicaAutoScalingUpdate))
updateTableReplicaAutoScaling_replicaUpdates = Lens.lens (\UpdateTableReplicaAutoScaling' {replicaUpdates} -> replicaUpdates) (\s@UpdateTableReplicaAutoScaling' {} a -> s {replicaUpdates = a} :: UpdateTableReplicaAutoScaling) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTableReplicaAutoScalingResponse'
            Prelude.<$> (x Data..?> "TableAutoScalingDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateTableReplicaAutoScaling
  where
  hashWithSalt _salt UpdateTableReplicaAutoScaling' {..} =
    _salt
      `Prelude.hashWithSalt` globalSecondaryIndexUpdates
      `Prelude.hashWithSalt` provisionedWriteCapacityAutoScalingUpdate
      `Prelude.hashWithSalt` replicaUpdates
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData UpdateTableReplicaAutoScaling where
  rnf UpdateTableReplicaAutoScaling' {..} =
    Prelude.rnf globalSecondaryIndexUpdates `Prelude.seq`
      Prelude.rnf provisionedWriteCapacityAutoScalingUpdate `Prelude.seq`
        Prelude.rnf replicaUpdates `Prelude.seq`
          Prelude.rnf tableName

instance Data.ToHeaders UpdateTableReplicaAutoScaling where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.UpdateTableReplicaAutoScaling" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTableReplicaAutoScaling where
  toJSON UpdateTableReplicaAutoScaling' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GlobalSecondaryIndexUpdates" Data..=)
              Prelude.<$> globalSecondaryIndexUpdates,
            ("ProvisionedWriteCapacityAutoScalingUpdate" Data..=)
              Prelude.<$> provisionedWriteCapacityAutoScalingUpdate,
            ("ReplicaUpdates" Data..=)
              Prelude.<$> replicaUpdates,
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath UpdateTableReplicaAutoScaling where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTableReplicaAutoScaling where
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
  where
  rnf UpdateTableReplicaAutoScalingResponse' {..} =
    Prelude.rnf tableAutoScalingDescription `Prelude.seq`
      Prelude.rnf httpStatus
