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
-- Module      : Amazonka.APIGateway.Types.DeploymentCanarySettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.DeploymentCanarySettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input configuration for a canary deployment.
--
-- /See:/ 'newDeploymentCanarySettings' smart constructor.
data DeploymentCanarySettings = DeploymentCanarySettings'
  { -- | A Boolean flag to indicate whether the canary release deployment uses
    -- the stage cache or not.
    useStageCache :: Prelude.Maybe Prelude.Bool,
    -- | A stage variable overrides used for the canary release deployment. They
    -- can override existing stage variables or add new stage variables for the
    -- canary release deployment. These stage variables are represented as a
    -- string-to-string map between stage variable names and their values.
    stageVariableOverrides :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The percentage (0.0-100.0) of traffic routed to the canary deployment.
    percentTraffic :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentCanarySettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useStageCache', 'deploymentCanarySettings_useStageCache' - A Boolean flag to indicate whether the canary release deployment uses
-- the stage cache or not.
--
-- 'stageVariableOverrides', 'deploymentCanarySettings_stageVariableOverrides' - A stage variable overrides used for the canary release deployment. They
-- can override existing stage variables or add new stage variables for the
-- canary release deployment. These stage variables are represented as a
-- string-to-string map between stage variable names and their values.
--
-- 'percentTraffic', 'deploymentCanarySettings_percentTraffic' - The percentage (0.0-100.0) of traffic routed to the canary deployment.
newDeploymentCanarySettings ::
  DeploymentCanarySettings
newDeploymentCanarySettings =
  DeploymentCanarySettings'
    { useStageCache =
        Prelude.Nothing,
      stageVariableOverrides = Prelude.Nothing,
      percentTraffic = Prelude.Nothing
    }

-- | A Boolean flag to indicate whether the canary release deployment uses
-- the stage cache or not.
deploymentCanarySettings_useStageCache :: Lens.Lens' DeploymentCanarySettings (Prelude.Maybe Prelude.Bool)
deploymentCanarySettings_useStageCache = Lens.lens (\DeploymentCanarySettings' {useStageCache} -> useStageCache) (\s@DeploymentCanarySettings' {} a -> s {useStageCache = a} :: DeploymentCanarySettings)

-- | A stage variable overrides used for the canary release deployment. They
-- can override existing stage variables or add new stage variables for the
-- canary release deployment. These stage variables are represented as a
-- string-to-string map between stage variable names and their values.
deploymentCanarySettings_stageVariableOverrides :: Lens.Lens' DeploymentCanarySettings (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deploymentCanarySettings_stageVariableOverrides = Lens.lens (\DeploymentCanarySettings' {stageVariableOverrides} -> stageVariableOverrides) (\s@DeploymentCanarySettings' {} a -> s {stageVariableOverrides = a} :: DeploymentCanarySettings) Prelude.. Lens.mapping Lens.coerced

-- | The percentage (0.0-100.0) of traffic routed to the canary deployment.
deploymentCanarySettings_percentTraffic :: Lens.Lens' DeploymentCanarySettings (Prelude.Maybe Prelude.Double)
deploymentCanarySettings_percentTraffic = Lens.lens (\DeploymentCanarySettings' {percentTraffic} -> percentTraffic) (\s@DeploymentCanarySettings' {} a -> s {percentTraffic = a} :: DeploymentCanarySettings)

instance Prelude.Hashable DeploymentCanarySettings where
  hashWithSalt _salt DeploymentCanarySettings' {..} =
    _salt `Prelude.hashWithSalt` useStageCache
      `Prelude.hashWithSalt` stageVariableOverrides
      `Prelude.hashWithSalt` percentTraffic

instance Prelude.NFData DeploymentCanarySettings where
  rnf DeploymentCanarySettings' {..} =
    Prelude.rnf useStageCache
      `Prelude.seq` Prelude.rnf stageVariableOverrides
      `Prelude.seq` Prelude.rnf percentTraffic

instance Data.ToJSON DeploymentCanarySettings where
  toJSON DeploymentCanarySettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("useStageCache" Data..=) Prelude.<$> useStageCache,
            ("stageVariableOverrides" Data..=)
              Prelude.<$> stageVariableOverrides,
            ("percentTraffic" Data..=)
              Prelude.<$> percentTraffic
          ]
      )
