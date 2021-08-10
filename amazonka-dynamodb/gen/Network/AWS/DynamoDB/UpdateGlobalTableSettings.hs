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
-- Module      : Network.AWS.DynamoDB.UpdateGlobalTableSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a global table.
module Network.AWS.DynamoDB.UpdateGlobalTableSettings
  ( -- * Creating a Request
    UpdateGlobalTableSettings (..),
    newUpdateGlobalTableSettings,

    -- * Request Lenses
    updateGlobalTableSettings_replicaSettingsUpdate,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
    updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate,
    updateGlobalTableSettings_globalTableBillingMode,
    updateGlobalTableSettings_globalTableName,

    -- * Destructuring the Response
    UpdateGlobalTableSettingsResponse (..),
    newUpdateGlobalTableSettingsResponse,

    -- * Response Lenses
    updateGlobalTableSettingsResponse_replicaSettings,
    updateGlobalTableSettingsResponse_globalTableName,
    updateGlobalTableSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGlobalTableSettings' smart constructor.
data UpdateGlobalTableSettings = UpdateGlobalTableSettings'
  { -- | Represents the settings for a global table in a Region that will be
    -- modified.
    replicaSettingsUpdate :: Prelude.Maybe (Prelude.NonEmpty ReplicaSettingsUpdate),
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException.@
    globalTableProvisionedWriteCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | Auto scaling settings for managing provisioned write capacity for the
    -- global table.
    globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | Represents the settings of a global secondary index for a global table
    -- that will be modified.
    globalTableGlobalSecondaryIndexSettingsUpdate :: Prelude.Maybe (Prelude.NonEmpty GlobalTableGlobalSecondaryIndexSettingsUpdate),
    -- | The billing mode of the global table. If @GlobalTableBillingMode@ is not
    -- specified, the global table defaults to @PROVISIONED@ capacity billing
    -- mode.
    --
    -- -   @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable
    --     workloads. @PROVISIONED@ sets the billing mode to
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode>.
    --
    -- -   @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for
    --     unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to
    --     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode>.
    globalTableBillingMode :: Prelude.Maybe BillingMode,
    -- | The name of the global table
    globalTableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalTableSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaSettingsUpdate', 'updateGlobalTableSettings_replicaSettingsUpdate' - Represents the settings for a global table in a Region that will be
-- modified.
--
-- 'globalTableProvisionedWriteCapacityUnits', 'updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException.@
--
-- 'globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate', 'updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing provisioned write capacity for the
-- global table.
--
-- 'globalTableGlobalSecondaryIndexSettingsUpdate', 'updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate' - Represents the settings of a global secondary index for a global table
-- that will be modified.
--
-- 'globalTableBillingMode', 'updateGlobalTableSettings_globalTableBillingMode' - The billing mode of the global table. If @GlobalTableBillingMode@ is not
-- specified, the global table defaults to @PROVISIONED@ capacity billing
-- mode.
--
-- -   @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable
--     workloads. @PROVISIONED@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode>.
--
-- -   @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode>.
--
-- 'globalTableName', 'updateGlobalTableSettings_globalTableName' - The name of the global table
newUpdateGlobalTableSettings ::
  -- | 'globalTableName'
  Prelude.Text ->
  UpdateGlobalTableSettings
newUpdateGlobalTableSettings pGlobalTableName_ =
  UpdateGlobalTableSettings'
    { replicaSettingsUpdate =
        Prelude.Nothing,
      globalTableProvisionedWriteCapacityUnits =
        Prelude.Nothing,
      globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate =
        Prelude.Nothing,
      globalTableGlobalSecondaryIndexSettingsUpdate =
        Prelude.Nothing,
      globalTableBillingMode = Prelude.Nothing,
      globalTableName = pGlobalTableName_
    }

-- | Represents the settings for a global table in a Region that will be
-- modified.
updateGlobalTableSettings_replicaSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Prelude.Maybe (Prelude.NonEmpty ReplicaSettingsUpdate))
updateGlobalTableSettings_replicaSettingsUpdate = Lens.lens (\UpdateGlobalTableSettings' {replicaSettingsUpdate} -> replicaSettingsUpdate) (\s@UpdateGlobalTableSettings' {} a -> s {replicaSettingsUpdate = a} :: UpdateGlobalTableSettings) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException.@
updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits :: Lens.Lens' UpdateGlobalTableSettings (Prelude.Maybe Prelude.Natural)
updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits = Lens.lens (\UpdateGlobalTableSettings' {globalTableProvisionedWriteCapacityUnits} -> globalTableProvisionedWriteCapacityUnits) (\s@UpdateGlobalTableSettings' {} a -> s {globalTableProvisionedWriteCapacityUnits = a} :: UpdateGlobalTableSettings)

-- | Auto scaling settings for managing provisioned write capacity for the
-- global table.
updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Prelude.Maybe AutoScalingSettingsUpdate)
updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate = Lens.lens (\UpdateGlobalTableSettings' {globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate} -> globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate) (\s@UpdateGlobalTableSettings' {} a -> s {globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate = a} :: UpdateGlobalTableSettings)

-- | Represents the settings of a global secondary index for a global table
-- that will be modified.
updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Prelude.Maybe (Prelude.NonEmpty GlobalTableGlobalSecondaryIndexSettingsUpdate))
updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate = Lens.lens (\UpdateGlobalTableSettings' {globalTableGlobalSecondaryIndexSettingsUpdate} -> globalTableGlobalSecondaryIndexSettingsUpdate) (\s@UpdateGlobalTableSettings' {} a -> s {globalTableGlobalSecondaryIndexSettingsUpdate = a} :: UpdateGlobalTableSettings) Prelude.. Lens.mapping Lens._Coerce

-- | The billing mode of the global table. If @GlobalTableBillingMode@ is not
-- specified, the global table defaults to @PROVISIONED@ capacity billing
-- mode.
--
-- -   @PROVISIONED@ - We recommend using @PROVISIONED@ for predictable
--     workloads. @PROVISIONED@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.ProvisionedThroughput.Manual Provisioned Mode>.
--
-- -   @PAY_PER_REQUEST@ - We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads. @PAY_PER_REQUEST@ sets the billing mode to
--     <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html#HowItWorks.OnDemand On-Demand Mode>.
updateGlobalTableSettings_globalTableBillingMode :: Lens.Lens' UpdateGlobalTableSettings (Prelude.Maybe BillingMode)
updateGlobalTableSettings_globalTableBillingMode = Lens.lens (\UpdateGlobalTableSettings' {globalTableBillingMode} -> globalTableBillingMode) (\s@UpdateGlobalTableSettings' {} a -> s {globalTableBillingMode = a} :: UpdateGlobalTableSettings)

-- | The name of the global table
updateGlobalTableSettings_globalTableName :: Lens.Lens' UpdateGlobalTableSettings Prelude.Text
updateGlobalTableSettings_globalTableName = Lens.lens (\UpdateGlobalTableSettings' {globalTableName} -> globalTableName) (\s@UpdateGlobalTableSettings' {} a -> s {globalTableName = a} :: UpdateGlobalTableSettings)

instance Core.AWSRequest UpdateGlobalTableSettings where
  type
    AWSResponse UpdateGlobalTableSettings =
      UpdateGlobalTableSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGlobalTableSettingsResponse'
            Prelude.<$> ( x Core..?> "ReplicaSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "GlobalTableName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGlobalTableSettings

instance Prelude.NFData UpdateGlobalTableSettings

instance Core.ToHeaders UpdateGlobalTableSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.UpdateGlobalTableSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateGlobalTableSettings where
  toJSON UpdateGlobalTableSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ReplicaSettingsUpdate" Core..=)
              Prelude.<$> replicaSettingsUpdate,
            ("GlobalTableProvisionedWriteCapacityUnits" Core..=)
              Prelude.<$> globalTableProvisionedWriteCapacityUnits,
            ( "GlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate"
                Core..=
            )
              Prelude.<$> globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
            ( "GlobalTableGlobalSecondaryIndexSettingsUpdate"
                Core..=
            )
              Prelude.<$> globalTableGlobalSecondaryIndexSettingsUpdate,
            ("GlobalTableBillingMode" Core..=)
              Prelude.<$> globalTableBillingMode,
            Prelude.Just
              ("GlobalTableName" Core..= globalTableName)
          ]
      )

instance Core.ToPath UpdateGlobalTableSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateGlobalTableSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGlobalTableSettingsResponse' smart constructor.
data UpdateGlobalTableSettingsResponse = UpdateGlobalTableSettingsResponse'
  { -- | The Region-specific settings for the global table.
    replicaSettings :: Prelude.Maybe [ReplicaSettingsDescription],
    -- | The name of the global table.
    globalTableName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalTableSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaSettings', 'updateGlobalTableSettingsResponse_replicaSettings' - The Region-specific settings for the global table.
--
-- 'globalTableName', 'updateGlobalTableSettingsResponse_globalTableName' - The name of the global table.
--
-- 'httpStatus', 'updateGlobalTableSettingsResponse_httpStatus' - The response's http status code.
newUpdateGlobalTableSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGlobalTableSettingsResponse
newUpdateGlobalTableSettingsResponse pHttpStatus_ =
  UpdateGlobalTableSettingsResponse'
    { replicaSettings =
        Prelude.Nothing,
      globalTableName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Region-specific settings for the global table.
updateGlobalTableSettingsResponse_replicaSettings :: Lens.Lens' UpdateGlobalTableSettingsResponse (Prelude.Maybe [ReplicaSettingsDescription])
updateGlobalTableSettingsResponse_replicaSettings = Lens.lens (\UpdateGlobalTableSettingsResponse' {replicaSettings} -> replicaSettings) (\s@UpdateGlobalTableSettingsResponse' {} a -> s {replicaSettings = a} :: UpdateGlobalTableSettingsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the global table.
updateGlobalTableSettingsResponse_globalTableName :: Lens.Lens' UpdateGlobalTableSettingsResponse (Prelude.Maybe Prelude.Text)
updateGlobalTableSettingsResponse_globalTableName = Lens.lens (\UpdateGlobalTableSettingsResponse' {globalTableName} -> globalTableName) (\s@UpdateGlobalTableSettingsResponse' {} a -> s {globalTableName = a} :: UpdateGlobalTableSettingsResponse)

-- | The response's http status code.
updateGlobalTableSettingsResponse_httpStatus :: Lens.Lens' UpdateGlobalTableSettingsResponse Prelude.Int
updateGlobalTableSettingsResponse_httpStatus = Lens.lens (\UpdateGlobalTableSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateGlobalTableSettingsResponse' {} a -> s {httpStatus = a} :: UpdateGlobalTableSettingsResponse)

instance
  Prelude.NFData
    UpdateGlobalTableSettingsResponse
