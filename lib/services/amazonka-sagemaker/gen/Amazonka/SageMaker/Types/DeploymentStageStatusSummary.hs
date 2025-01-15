{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.DeploymentStageStatusSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeploymentStageStatusSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DeviceSelectionConfig
import Amazonka.SageMaker.Types.EdgeDeploymentConfig
import Amazonka.SageMaker.Types.EdgeDeploymentStatus

-- | Contains information summarizing the deployment stage results.
--
-- /See:/ 'newDeploymentStageStatusSummary' smart constructor.
data DeploymentStageStatusSummary = DeploymentStageStatusSummary'
  { -- | The name of the stage.
    stageName :: Prelude.Text,
    -- | Configuration of the devices in the stage.
    deviceSelectionConfig :: DeviceSelectionConfig,
    -- | Configuration of the deployment details.
    deploymentConfig :: EdgeDeploymentConfig,
    -- | General status of the current state.
    deploymentStatus :: EdgeDeploymentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentStageStatusSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'deploymentStageStatusSummary_stageName' - The name of the stage.
--
-- 'deviceSelectionConfig', 'deploymentStageStatusSummary_deviceSelectionConfig' - Configuration of the devices in the stage.
--
-- 'deploymentConfig', 'deploymentStageStatusSummary_deploymentConfig' - Configuration of the deployment details.
--
-- 'deploymentStatus', 'deploymentStageStatusSummary_deploymentStatus' - General status of the current state.
newDeploymentStageStatusSummary ::
  -- | 'stageName'
  Prelude.Text ->
  -- | 'deviceSelectionConfig'
  DeviceSelectionConfig ->
  -- | 'deploymentConfig'
  EdgeDeploymentConfig ->
  -- | 'deploymentStatus'
  EdgeDeploymentStatus ->
  DeploymentStageStatusSummary
newDeploymentStageStatusSummary
  pStageName_
  pDeviceSelectionConfig_
  pDeploymentConfig_
  pDeploymentStatus_ =
    DeploymentStageStatusSummary'
      { stageName =
          pStageName_,
        deviceSelectionConfig =
          pDeviceSelectionConfig_,
        deploymentConfig = pDeploymentConfig_,
        deploymentStatus = pDeploymentStatus_
      }

-- | The name of the stage.
deploymentStageStatusSummary_stageName :: Lens.Lens' DeploymentStageStatusSummary Prelude.Text
deploymentStageStatusSummary_stageName = Lens.lens (\DeploymentStageStatusSummary' {stageName} -> stageName) (\s@DeploymentStageStatusSummary' {} a -> s {stageName = a} :: DeploymentStageStatusSummary)

-- | Configuration of the devices in the stage.
deploymentStageStatusSummary_deviceSelectionConfig :: Lens.Lens' DeploymentStageStatusSummary DeviceSelectionConfig
deploymentStageStatusSummary_deviceSelectionConfig = Lens.lens (\DeploymentStageStatusSummary' {deviceSelectionConfig} -> deviceSelectionConfig) (\s@DeploymentStageStatusSummary' {} a -> s {deviceSelectionConfig = a} :: DeploymentStageStatusSummary)

-- | Configuration of the deployment details.
deploymentStageStatusSummary_deploymentConfig :: Lens.Lens' DeploymentStageStatusSummary EdgeDeploymentConfig
deploymentStageStatusSummary_deploymentConfig = Lens.lens (\DeploymentStageStatusSummary' {deploymentConfig} -> deploymentConfig) (\s@DeploymentStageStatusSummary' {} a -> s {deploymentConfig = a} :: DeploymentStageStatusSummary)

-- | General status of the current state.
deploymentStageStatusSummary_deploymentStatus :: Lens.Lens' DeploymentStageStatusSummary EdgeDeploymentStatus
deploymentStageStatusSummary_deploymentStatus = Lens.lens (\DeploymentStageStatusSummary' {deploymentStatus} -> deploymentStatus) (\s@DeploymentStageStatusSummary' {} a -> s {deploymentStatus = a} :: DeploymentStageStatusSummary)

instance Data.FromJSON DeploymentStageStatusSummary where
  parseJSON =
    Data.withObject
      "DeploymentStageStatusSummary"
      ( \x ->
          DeploymentStageStatusSummary'
            Prelude.<$> (x Data..: "StageName")
            Prelude.<*> (x Data..: "DeviceSelectionConfig")
            Prelude.<*> (x Data..: "DeploymentConfig")
            Prelude.<*> (x Data..: "DeploymentStatus")
      )

instance
  Prelude.Hashable
    DeploymentStageStatusSummary
  where
  hashWithSalt _salt DeploymentStageStatusSummary' {..} =
    _salt
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` deviceSelectionConfig
      `Prelude.hashWithSalt` deploymentConfig
      `Prelude.hashWithSalt` deploymentStatus

instance Prelude.NFData DeploymentStageStatusSummary where
  rnf DeploymentStageStatusSummary' {..} =
    Prelude.rnf stageName `Prelude.seq`
      Prelude.rnf deviceSelectionConfig `Prelude.seq`
        Prelude.rnf deploymentConfig `Prelude.seq`
          Prelude.rnf deploymentStatus
