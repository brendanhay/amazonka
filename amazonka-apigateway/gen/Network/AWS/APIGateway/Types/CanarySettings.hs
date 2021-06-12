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
-- Module      : Network.AWS.APIGateway.Types.CanarySettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.CanarySettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration settings of a canary deployment.
--
-- /See:/ 'newCanarySettings' smart constructor.
data CanarySettings = CanarySettings'
  { -- | The ID of the canary deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The percent (0-100) of traffic diverted to a canary deployment.
    percentTraffic :: Core.Maybe Core.Double,
    -- | A Boolean flag to indicate whether the canary deployment uses the stage
    -- cache or not.
    useStageCache :: Core.Maybe Core.Bool,
    -- | Stage variables overridden for a canary release deployment, including
    -- new stage variables introduced in the canary. These stage variables are
    -- represented as a string-to-string map between stage variable names and
    -- their values.
    stageVariableOverrides :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CanarySettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'canarySettings_deploymentId' - The ID of the canary deployment.
--
-- 'percentTraffic', 'canarySettings_percentTraffic' - The percent (0-100) of traffic diverted to a canary deployment.
--
-- 'useStageCache', 'canarySettings_useStageCache' - A Boolean flag to indicate whether the canary deployment uses the stage
-- cache or not.
--
-- 'stageVariableOverrides', 'canarySettings_stageVariableOverrides' - Stage variables overridden for a canary release deployment, including
-- new stage variables introduced in the canary. These stage variables are
-- represented as a string-to-string map between stage variable names and
-- their values.
newCanarySettings ::
  CanarySettings
newCanarySettings =
  CanarySettings'
    { deploymentId = Core.Nothing,
      percentTraffic = Core.Nothing,
      useStageCache = Core.Nothing,
      stageVariableOverrides = Core.Nothing
    }

-- | The ID of the canary deployment.
canarySettings_deploymentId :: Lens.Lens' CanarySettings (Core.Maybe Core.Text)
canarySettings_deploymentId = Lens.lens (\CanarySettings' {deploymentId} -> deploymentId) (\s@CanarySettings' {} a -> s {deploymentId = a} :: CanarySettings)

-- | The percent (0-100) of traffic diverted to a canary deployment.
canarySettings_percentTraffic :: Lens.Lens' CanarySettings (Core.Maybe Core.Double)
canarySettings_percentTraffic = Lens.lens (\CanarySettings' {percentTraffic} -> percentTraffic) (\s@CanarySettings' {} a -> s {percentTraffic = a} :: CanarySettings)

-- | A Boolean flag to indicate whether the canary deployment uses the stage
-- cache or not.
canarySettings_useStageCache :: Lens.Lens' CanarySettings (Core.Maybe Core.Bool)
canarySettings_useStageCache = Lens.lens (\CanarySettings' {useStageCache} -> useStageCache) (\s@CanarySettings' {} a -> s {useStageCache = a} :: CanarySettings)

-- | Stage variables overridden for a canary release deployment, including
-- new stage variables introduced in the canary. These stage variables are
-- represented as a string-to-string map between stage variable names and
-- their values.
canarySettings_stageVariableOverrides :: Lens.Lens' CanarySettings (Core.Maybe (Core.HashMap Core.Text Core.Text))
canarySettings_stageVariableOverrides = Lens.lens (\CanarySettings' {stageVariableOverrides} -> stageVariableOverrides) (\s@CanarySettings' {} a -> s {stageVariableOverrides = a} :: CanarySettings) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON CanarySettings where
  parseJSON =
    Core.withObject
      "CanarySettings"
      ( \x ->
          CanarySettings'
            Core.<$> (x Core..:? "deploymentId")
            Core.<*> (x Core..:? "percentTraffic")
            Core.<*> (x Core..:? "useStageCache")
            Core.<*> ( x Core..:? "stageVariableOverrides"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable CanarySettings

instance Core.NFData CanarySettings

instance Core.ToJSON CanarySettings where
  toJSON CanarySettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deploymentId" Core..=) Core.<$> deploymentId,
            ("percentTraffic" Core..=) Core.<$> percentTraffic,
            ("useStageCache" Core..=) Core.<$> useStageCache,
            ("stageVariableOverrides" Core..=)
              Core.<$> stageVariableOverrides
          ]
      )
