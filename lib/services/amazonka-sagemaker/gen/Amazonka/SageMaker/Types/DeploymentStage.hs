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
-- Module      : Amazonka.SageMaker.Types.DeploymentStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeploymentStage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DeviceSelectionConfig
import Amazonka.SageMaker.Types.EdgeDeploymentConfig

-- | Contains information about a stage in an edge deployment plan.
--
-- /See:/ 'newDeploymentStage' smart constructor.
data DeploymentStage = DeploymentStage'
  { -- | Configuration of the deployment details.
    deploymentConfig :: Prelude.Maybe EdgeDeploymentConfig,
    -- | The name of the stage.
    stageName :: Prelude.Text,
    -- | Configuration of the devices in the stage.
    deviceSelectionConfig :: DeviceSelectionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfig', 'deploymentStage_deploymentConfig' - Configuration of the deployment details.
--
-- 'stageName', 'deploymentStage_stageName' - The name of the stage.
--
-- 'deviceSelectionConfig', 'deploymentStage_deviceSelectionConfig' - Configuration of the devices in the stage.
newDeploymentStage ::
  -- | 'stageName'
  Prelude.Text ->
  -- | 'deviceSelectionConfig'
  DeviceSelectionConfig ->
  DeploymentStage
newDeploymentStage
  pStageName_
  pDeviceSelectionConfig_ =
    DeploymentStage'
      { deploymentConfig =
          Prelude.Nothing,
        stageName = pStageName_,
        deviceSelectionConfig = pDeviceSelectionConfig_
      }

-- | Configuration of the deployment details.
deploymentStage_deploymentConfig :: Lens.Lens' DeploymentStage (Prelude.Maybe EdgeDeploymentConfig)
deploymentStage_deploymentConfig = Lens.lens (\DeploymentStage' {deploymentConfig} -> deploymentConfig) (\s@DeploymentStage' {} a -> s {deploymentConfig = a} :: DeploymentStage)

-- | The name of the stage.
deploymentStage_stageName :: Lens.Lens' DeploymentStage Prelude.Text
deploymentStage_stageName = Lens.lens (\DeploymentStage' {stageName} -> stageName) (\s@DeploymentStage' {} a -> s {stageName = a} :: DeploymentStage)

-- | Configuration of the devices in the stage.
deploymentStage_deviceSelectionConfig :: Lens.Lens' DeploymentStage DeviceSelectionConfig
deploymentStage_deviceSelectionConfig = Lens.lens (\DeploymentStage' {deviceSelectionConfig} -> deviceSelectionConfig) (\s@DeploymentStage' {} a -> s {deviceSelectionConfig = a} :: DeploymentStage)

instance Prelude.Hashable DeploymentStage where
  hashWithSalt _salt DeploymentStage' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentConfig
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` deviceSelectionConfig

instance Prelude.NFData DeploymentStage where
  rnf DeploymentStage' {..} =
    Prelude.rnf deploymentConfig
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf deviceSelectionConfig

instance Data.ToJSON DeploymentStage where
  toJSON DeploymentStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeploymentConfig" Data..=)
              Prelude.<$> deploymentConfig,
            Prelude.Just ("StageName" Data..= stageName),
            Prelude.Just
              ( "DeviceSelectionConfig"
                  Data..= deviceSelectionConfig
              )
          ]
      )
