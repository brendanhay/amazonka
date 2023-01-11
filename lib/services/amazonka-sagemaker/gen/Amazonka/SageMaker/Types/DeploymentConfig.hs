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
-- Module      : Amazonka.SageMaker.Types.DeploymentConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeploymentConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoRollbackConfig
import Amazonka.SageMaker.Types.BlueGreenUpdatePolicy

-- | The deployment configuration for an endpoint, which contains the desired
-- deployment strategy and rollback configurations.
--
-- /See:/ 'newDeploymentConfig' smart constructor.
data DeploymentConfig = DeploymentConfig'
  { -- | Automatic rollback configuration for handling endpoint deployment
    -- failures and recovery.
    autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfig,
    -- | Update policy for a blue\/green deployment. If this update policy is
    -- specified, SageMaker creates a new fleet during the deployment while
    -- maintaining the old fleet. SageMaker flips traffic to the new fleet
    -- according to the specified traffic routing configuration. Only one
    -- update policy should be used in the deployment configuration. If no
    -- update policy is specified, SageMaker uses a blue\/green deployment
    -- strategy with all at once traffic shifting by default.
    blueGreenUpdatePolicy :: BlueGreenUpdatePolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRollbackConfiguration', 'deploymentConfig_autoRollbackConfiguration' - Automatic rollback configuration for handling endpoint deployment
-- failures and recovery.
--
-- 'blueGreenUpdatePolicy', 'deploymentConfig_blueGreenUpdatePolicy' - Update policy for a blue\/green deployment. If this update policy is
-- specified, SageMaker creates a new fleet during the deployment while
-- maintaining the old fleet. SageMaker flips traffic to the new fleet
-- according to the specified traffic routing configuration. Only one
-- update policy should be used in the deployment configuration. If no
-- update policy is specified, SageMaker uses a blue\/green deployment
-- strategy with all at once traffic shifting by default.
newDeploymentConfig ::
  -- | 'blueGreenUpdatePolicy'
  BlueGreenUpdatePolicy ->
  DeploymentConfig
newDeploymentConfig pBlueGreenUpdatePolicy_ =
  DeploymentConfig'
    { autoRollbackConfiguration =
        Prelude.Nothing,
      blueGreenUpdatePolicy = pBlueGreenUpdatePolicy_
    }

-- | Automatic rollback configuration for handling endpoint deployment
-- failures and recovery.
deploymentConfig_autoRollbackConfiguration :: Lens.Lens' DeploymentConfig (Prelude.Maybe AutoRollbackConfig)
deploymentConfig_autoRollbackConfiguration = Lens.lens (\DeploymentConfig' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@DeploymentConfig' {} a -> s {autoRollbackConfiguration = a} :: DeploymentConfig)

-- | Update policy for a blue\/green deployment. If this update policy is
-- specified, SageMaker creates a new fleet during the deployment while
-- maintaining the old fleet. SageMaker flips traffic to the new fleet
-- according to the specified traffic routing configuration. Only one
-- update policy should be used in the deployment configuration. If no
-- update policy is specified, SageMaker uses a blue\/green deployment
-- strategy with all at once traffic shifting by default.
deploymentConfig_blueGreenUpdatePolicy :: Lens.Lens' DeploymentConfig BlueGreenUpdatePolicy
deploymentConfig_blueGreenUpdatePolicy = Lens.lens (\DeploymentConfig' {blueGreenUpdatePolicy} -> blueGreenUpdatePolicy) (\s@DeploymentConfig' {} a -> s {blueGreenUpdatePolicy = a} :: DeploymentConfig)

instance Data.FromJSON DeploymentConfig where
  parseJSON =
    Data.withObject
      "DeploymentConfig"
      ( \x ->
          DeploymentConfig'
            Prelude.<$> (x Data..:? "AutoRollbackConfiguration")
            Prelude.<*> (x Data..: "BlueGreenUpdatePolicy")
      )

instance Prelude.Hashable DeploymentConfig where
  hashWithSalt _salt DeploymentConfig' {..} =
    _salt
      `Prelude.hashWithSalt` autoRollbackConfiguration
      `Prelude.hashWithSalt` blueGreenUpdatePolicy

instance Prelude.NFData DeploymentConfig where
  rnf DeploymentConfig' {..} =
    Prelude.rnf autoRollbackConfiguration
      `Prelude.seq` Prelude.rnf blueGreenUpdatePolicy

instance Data.ToJSON DeploymentConfig where
  toJSON DeploymentConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoRollbackConfiguration" Data..=)
              Prelude.<$> autoRollbackConfiguration,
            Prelude.Just
              ( "BlueGreenUpdatePolicy"
                  Data..= blueGreenUpdatePolicy
              )
          ]
      )
