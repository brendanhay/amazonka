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
-- Module      : Network.AWS.APIGateway.Types.DeploymentCanarySettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DeploymentCanarySettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The input configuration for a canary deployment.
--
-- /See:/ 'newDeploymentCanarySettings' smart constructor.
data DeploymentCanarySettings = DeploymentCanarySettings'
  { -- | The percentage (0.0-100.0) of traffic routed to the canary deployment.
    percentTraffic :: Core.Maybe Core.Double,
    -- | A Boolean flag to indicate whether the canary release deployment uses
    -- the stage cache or not.
    useStageCache :: Core.Maybe Core.Bool,
    -- | A stage variable overrides used for the canary release deployment. They
    -- can override existing stage variables or add new stage variables for the
    -- canary release deployment. These stage variables are represented as a
    -- string-to-string map between stage variable names and their values.
    stageVariableOverrides :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeploymentCanarySettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentTraffic', 'deploymentCanarySettings_percentTraffic' - The percentage (0.0-100.0) of traffic routed to the canary deployment.
--
-- 'useStageCache', 'deploymentCanarySettings_useStageCache' - A Boolean flag to indicate whether the canary release deployment uses
-- the stage cache or not.
--
-- 'stageVariableOverrides', 'deploymentCanarySettings_stageVariableOverrides' - A stage variable overrides used for the canary release deployment. They
-- can override existing stage variables or add new stage variables for the
-- canary release deployment. These stage variables are represented as a
-- string-to-string map between stage variable names and their values.
newDeploymentCanarySettings ::
  DeploymentCanarySettings
newDeploymentCanarySettings =
  DeploymentCanarySettings'
    { percentTraffic =
        Core.Nothing,
      useStageCache = Core.Nothing,
      stageVariableOverrides = Core.Nothing
    }

-- | The percentage (0.0-100.0) of traffic routed to the canary deployment.
deploymentCanarySettings_percentTraffic :: Lens.Lens' DeploymentCanarySettings (Core.Maybe Core.Double)
deploymentCanarySettings_percentTraffic = Lens.lens (\DeploymentCanarySettings' {percentTraffic} -> percentTraffic) (\s@DeploymentCanarySettings' {} a -> s {percentTraffic = a} :: DeploymentCanarySettings)

-- | A Boolean flag to indicate whether the canary release deployment uses
-- the stage cache or not.
deploymentCanarySettings_useStageCache :: Lens.Lens' DeploymentCanarySettings (Core.Maybe Core.Bool)
deploymentCanarySettings_useStageCache = Lens.lens (\DeploymentCanarySettings' {useStageCache} -> useStageCache) (\s@DeploymentCanarySettings' {} a -> s {useStageCache = a} :: DeploymentCanarySettings)

-- | A stage variable overrides used for the canary release deployment. They
-- can override existing stage variables or add new stage variables for the
-- canary release deployment. These stage variables are represented as a
-- string-to-string map between stage variable names and their values.
deploymentCanarySettings_stageVariableOverrides :: Lens.Lens' DeploymentCanarySettings (Core.Maybe (Core.HashMap Core.Text Core.Text))
deploymentCanarySettings_stageVariableOverrides = Lens.lens (\DeploymentCanarySettings' {stageVariableOverrides} -> stageVariableOverrides) (\s@DeploymentCanarySettings' {} a -> s {stageVariableOverrides = a} :: DeploymentCanarySettings) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable DeploymentCanarySettings

instance Core.NFData DeploymentCanarySettings

instance Core.ToJSON DeploymentCanarySettings where
  toJSON DeploymentCanarySettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("percentTraffic" Core..=) Core.<$> percentTraffic,
            ("useStageCache" Core..=) Core.<$> useStageCache,
            ("stageVariableOverrides" Core..=)
              Core.<$> stageVariableOverrides
          ]
      )
