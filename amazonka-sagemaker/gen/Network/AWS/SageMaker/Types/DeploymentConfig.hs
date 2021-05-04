{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.DeploymentConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DeploymentConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AutoRollbackConfig
import Network.AWS.SageMaker.Types.BlueGreenUpdatePolicy

-- | Currently, the @DeploymentConfig@ API is not supported.
--
-- /See:/ 'newDeploymentConfig' smart constructor.
data DeploymentConfig = DeploymentConfig'
  { autoRollbackConfiguration :: Prelude.Maybe AutoRollbackConfig,
    blueGreenUpdatePolicy :: BlueGreenUpdatePolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeploymentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRollbackConfiguration', 'deploymentConfig_autoRollbackConfiguration' -
--
-- 'blueGreenUpdatePolicy', 'deploymentConfig_blueGreenUpdatePolicy' -
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

-- |
deploymentConfig_autoRollbackConfiguration :: Lens.Lens' DeploymentConfig (Prelude.Maybe AutoRollbackConfig)
deploymentConfig_autoRollbackConfiguration = Lens.lens (\DeploymentConfig' {autoRollbackConfiguration} -> autoRollbackConfiguration) (\s@DeploymentConfig' {} a -> s {autoRollbackConfiguration = a} :: DeploymentConfig)

-- |
deploymentConfig_blueGreenUpdatePolicy :: Lens.Lens' DeploymentConfig BlueGreenUpdatePolicy
deploymentConfig_blueGreenUpdatePolicy = Lens.lens (\DeploymentConfig' {blueGreenUpdatePolicy} -> blueGreenUpdatePolicy) (\s@DeploymentConfig' {} a -> s {blueGreenUpdatePolicy = a} :: DeploymentConfig)

instance Prelude.FromJSON DeploymentConfig where
  parseJSON =
    Prelude.withObject
      "DeploymentConfig"
      ( \x ->
          DeploymentConfig'
            Prelude.<$> (x Prelude..:? "AutoRollbackConfiguration")
            Prelude.<*> (x Prelude..: "BlueGreenUpdatePolicy")
      )

instance Prelude.Hashable DeploymentConfig

instance Prelude.NFData DeploymentConfig

instance Prelude.ToJSON DeploymentConfig where
  toJSON DeploymentConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AutoRollbackConfiguration" Prelude..=)
              Prelude.<$> autoRollbackConfiguration,
            Prelude.Just
              ( "BlueGreenUpdatePolicy"
                  Prelude..= blueGreenUpdatePolicy
              )
          ]
      )
