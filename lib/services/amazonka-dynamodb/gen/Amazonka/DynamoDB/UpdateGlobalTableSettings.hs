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
-- Module      : Amazonka.DynamoDB.UpdateGlobalTableSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a global table.
module Amazonka.DynamoDB.UpdateGlobalTableSettings
  ( -- * Creating a Request
    UpdateGlobalTableSettings (..),
    newUpdateGlobalTableSettings,

    -- * Request Lenses
    updateGlobalTableSettings_globalTableBillingMode,
    updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
    updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits,
    updateGlobalTableSettings_replicaSettingsUpdate,
    updateGlobalTableSettings_globalTableName,

    -- * Destructuring the Response
    UpdateGlobalTableSettingsResponse (..),
    newUpdateGlobalTableSettingsResponse,

    -- * Response Lenses
    updateGlobalTableSettingsResponse_globalTableName,
    updateGlobalTableSettingsResponse_replicaSettings,
    updateGlobalTableSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGlobalTableSettings' smart constructor.
data UpdateGlobalTableSettings = UpdateGlobalTableSettings'
  { -- | The billing mode of the global table. If @GlobalTableBillingMode@ is not
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
    -- | Represents the settings of a global secondary index for a global table
    -- that will be modified.
    globalTableGlobalSecondaryIndexSettingsUpdate :: Prelude.Maybe (Prelude.NonEmpty GlobalTableGlobalSecondaryIndexSettingsUpdate),
    -- | Auto scaling settings for managing provisioned write capacity for the
    -- global table.
    globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException.@
    globalTableProvisionedWriteCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | Represents the settings for a global table in a Region that will be
    -- modified.
    replicaSettingsUpdate :: Prelude.Maybe (Prelude.NonEmpty ReplicaSettingsUpdate),
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
-- 'globalTableGlobalSecondaryIndexSettingsUpdate', 'updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate' - Represents the settings of a global secondary index for a global table
-- that will be modified.
--
-- 'globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate', 'updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing provisioned write capacity for the
-- global table.
--
-- 'globalTableProvisionedWriteCapacityUnits', 'updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException.@
--
-- 'replicaSettingsUpdate', 'updateGlobalTableSettings_replicaSettingsUpdate' - Represents the settings for a global table in a Region that will be
-- modified.
--
-- 'globalTableName', 'updateGlobalTableSettings_globalTableName' - The name of the global table
newUpdateGlobalTableSettings ::
  -- | 'globalTableName'
  Prelude.Text ->
  UpdateGlobalTableSettings
newUpdateGlobalTableSettings pGlobalTableName_ =
  UpdateGlobalTableSettings'
    { globalTableBillingMode =
        Prelude.Nothing,
      globalTableGlobalSecondaryIndexSettingsUpdate =
        Prelude.Nothing,
      globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate =
        Prelude.Nothing,
      globalTableProvisionedWriteCapacityUnits =
        Prelude.Nothing,
      replicaSettingsUpdate = Prelude.Nothing,
      globalTableName = pGlobalTableName_
    }

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

-- | Represents the settings of a global secondary index for a global table
-- that will be modified.
updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Prelude.Maybe (Prelude.NonEmpty GlobalTableGlobalSecondaryIndexSettingsUpdate))
updateGlobalTableSettings_globalTableGlobalSecondaryIndexSettingsUpdate = Lens.lens (\UpdateGlobalTableSettings' {globalTableGlobalSecondaryIndexSettingsUpdate} -> globalTableGlobalSecondaryIndexSettingsUpdate) (\s@UpdateGlobalTableSettings' {} a -> s {globalTableGlobalSecondaryIndexSettingsUpdate = a} :: UpdateGlobalTableSettings) Prelude.. Lens.mapping Lens.coerced

-- | Auto scaling settings for managing provisioned write capacity for the
-- global table.
updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Prelude.Maybe AutoScalingSettingsUpdate)
updateGlobalTableSettings_globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate = Lens.lens (\UpdateGlobalTableSettings' {globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate} -> globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate) (\s@UpdateGlobalTableSettings' {} a -> s {globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate = a} :: UpdateGlobalTableSettings)

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException.@
updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits :: Lens.Lens' UpdateGlobalTableSettings (Prelude.Maybe Prelude.Natural)
updateGlobalTableSettings_globalTableProvisionedWriteCapacityUnits = Lens.lens (\UpdateGlobalTableSettings' {globalTableProvisionedWriteCapacityUnits} -> globalTableProvisionedWriteCapacityUnits) (\s@UpdateGlobalTableSettings' {} a -> s {globalTableProvisionedWriteCapacityUnits = a} :: UpdateGlobalTableSettings)

-- | Represents the settings for a global table in a Region that will be
-- modified.
updateGlobalTableSettings_replicaSettingsUpdate :: Lens.Lens' UpdateGlobalTableSettings (Prelude.Maybe (Prelude.NonEmpty ReplicaSettingsUpdate))
updateGlobalTableSettings_replicaSettingsUpdate = Lens.lens (\UpdateGlobalTableSettings' {replicaSettingsUpdate} -> replicaSettingsUpdate) (\s@UpdateGlobalTableSettings' {} a -> s {replicaSettingsUpdate = a} :: UpdateGlobalTableSettings) Prelude.. Lens.mapping Lens.coerced

-- | The name of the global table
updateGlobalTableSettings_globalTableName :: Lens.Lens' UpdateGlobalTableSettings Prelude.Text
updateGlobalTableSettings_globalTableName = Lens.lens (\UpdateGlobalTableSettings' {globalTableName} -> globalTableName) (\s@UpdateGlobalTableSettings' {} a -> s {globalTableName = a} :: UpdateGlobalTableSettings)

instance Core.AWSRequest UpdateGlobalTableSettings where
  type
    AWSResponse UpdateGlobalTableSettings =
      UpdateGlobalTableSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGlobalTableSettingsResponse'
            Prelude.<$> (x Data..?> "GlobalTableName")
            Prelude.<*> ( x
                            Data..?> "ReplicaSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGlobalTableSettings where
  hashWithSalt _salt UpdateGlobalTableSettings' {..} =
    _salt
      `Prelude.hashWithSalt` globalTableBillingMode
      `Prelude.hashWithSalt` globalTableGlobalSecondaryIndexSettingsUpdate
      `Prelude.hashWithSalt` globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate
      `Prelude.hashWithSalt` globalTableProvisionedWriteCapacityUnits
      `Prelude.hashWithSalt` replicaSettingsUpdate
      `Prelude.hashWithSalt` globalTableName

instance Prelude.NFData UpdateGlobalTableSettings where
  rnf UpdateGlobalTableSettings' {..} =
    Prelude.rnf globalTableBillingMode
      `Prelude.seq` Prelude.rnf
        globalTableGlobalSecondaryIndexSettingsUpdate
      `Prelude.seq` Prelude.rnf
        globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate
      `Prelude.seq` Prelude.rnf globalTableProvisionedWriteCapacityUnits
      `Prelude.seq` Prelude.rnf replicaSettingsUpdate
      `Prelude.seq` Prelude.rnf globalTableName

instance Data.ToHeaders UpdateGlobalTableSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.UpdateGlobalTableSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGlobalTableSettings where
  toJSON UpdateGlobalTableSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GlobalTableBillingMode" Data..=)
              Prelude.<$> globalTableBillingMode,
            ( "GlobalTableGlobalSecondaryIndexSettingsUpdate"
                Data..=
            )
              Prelude.<$> globalTableGlobalSecondaryIndexSettingsUpdate,
            ( "GlobalTableProvisionedWriteCapacityAutoScalingSettingsUpdate"
                Data..=
            )
              Prelude.<$> globalTableProvisionedWriteCapacityAutoScalingSettingsUpdate,
            ("GlobalTableProvisionedWriteCapacityUnits" Data..=)
              Prelude.<$> globalTableProvisionedWriteCapacityUnits,
            ("ReplicaSettingsUpdate" Data..=)
              Prelude.<$> replicaSettingsUpdate,
            Prelude.Just
              ("GlobalTableName" Data..= globalTableName)
          ]
      )

instance Data.ToPath UpdateGlobalTableSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateGlobalTableSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGlobalTableSettingsResponse' smart constructor.
data UpdateGlobalTableSettingsResponse = UpdateGlobalTableSettingsResponse'
  { -- | The name of the global table.
    globalTableName :: Prelude.Maybe Prelude.Text,
    -- | The Region-specific settings for the global table.
    replicaSettings :: Prelude.Maybe [ReplicaSettingsDescription],
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
-- 'globalTableName', 'updateGlobalTableSettingsResponse_globalTableName' - The name of the global table.
--
-- 'replicaSettings', 'updateGlobalTableSettingsResponse_replicaSettings' - The Region-specific settings for the global table.
--
-- 'httpStatus', 'updateGlobalTableSettingsResponse_httpStatus' - The response's http status code.
newUpdateGlobalTableSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGlobalTableSettingsResponse
newUpdateGlobalTableSettingsResponse pHttpStatus_ =
  UpdateGlobalTableSettingsResponse'
    { globalTableName =
        Prelude.Nothing,
      replicaSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the global table.
updateGlobalTableSettingsResponse_globalTableName :: Lens.Lens' UpdateGlobalTableSettingsResponse (Prelude.Maybe Prelude.Text)
updateGlobalTableSettingsResponse_globalTableName = Lens.lens (\UpdateGlobalTableSettingsResponse' {globalTableName} -> globalTableName) (\s@UpdateGlobalTableSettingsResponse' {} a -> s {globalTableName = a} :: UpdateGlobalTableSettingsResponse)

-- | The Region-specific settings for the global table.
updateGlobalTableSettingsResponse_replicaSettings :: Lens.Lens' UpdateGlobalTableSettingsResponse (Prelude.Maybe [ReplicaSettingsDescription])
updateGlobalTableSettingsResponse_replicaSettings = Lens.lens (\UpdateGlobalTableSettingsResponse' {replicaSettings} -> replicaSettings) (\s@UpdateGlobalTableSettingsResponse' {} a -> s {replicaSettings = a} :: UpdateGlobalTableSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateGlobalTableSettingsResponse_httpStatus :: Lens.Lens' UpdateGlobalTableSettingsResponse Prelude.Int
updateGlobalTableSettingsResponse_httpStatus = Lens.lens (\UpdateGlobalTableSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateGlobalTableSettingsResponse' {} a -> s {httpStatus = a} :: UpdateGlobalTableSettingsResponse)

instance
  Prelude.NFData
    UpdateGlobalTableSettingsResponse
  where
  rnf UpdateGlobalTableSettingsResponse' {..} =
    Prelude.rnf globalTableName
      `Prelude.seq` Prelude.rnf replicaSettings
      `Prelude.seq` Prelude.rnf httpStatus
