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
-- Module      : Network.AWS.SageMaker.Types.ModelQualityBaselineConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelQualityBaselineConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource

-- | Configuration for monitoring constraints and monitoring statistics.
-- These baseline resources are compared against the results of the current
-- job from the series of jobs scheduled to collect data periodically.
--
-- /See:/ 'newModelQualityBaselineConfig' smart constructor.
data ModelQualityBaselineConfig = ModelQualityBaselineConfig'
  { constraintsResource :: Core.Maybe MonitoringConstraintsResource,
    -- | The name of the job that performs baselining for the monitoring job.
    baseliningJobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelQualityBaselineConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintsResource', 'modelQualityBaselineConfig_constraintsResource' - Undocumented member.
--
-- 'baseliningJobName', 'modelQualityBaselineConfig_baseliningJobName' - The name of the job that performs baselining for the monitoring job.
newModelQualityBaselineConfig ::
  ModelQualityBaselineConfig
newModelQualityBaselineConfig =
  ModelQualityBaselineConfig'
    { constraintsResource =
        Core.Nothing,
      baseliningJobName = Core.Nothing
    }

-- | Undocumented member.
modelQualityBaselineConfig_constraintsResource :: Lens.Lens' ModelQualityBaselineConfig (Core.Maybe MonitoringConstraintsResource)
modelQualityBaselineConfig_constraintsResource = Lens.lens (\ModelQualityBaselineConfig' {constraintsResource} -> constraintsResource) (\s@ModelQualityBaselineConfig' {} a -> s {constraintsResource = a} :: ModelQualityBaselineConfig)

-- | The name of the job that performs baselining for the monitoring job.
modelQualityBaselineConfig_baseliningJobName :: Lens.Lens' ModelQualityBaselineConfig (Core.Maybe Core.Text)
modelQualityBaselineConfig_baseliningJobName = Lens.lens (\ModelQualityBaselineConfig' {baseliningJobName} -> baseliningJobName) (\s@ModelQualityBaselineConfig' {} a -> s {baseliningJobName = a} :: ModelQualityBaselineConfig)

instance Core.FromJSON ModelQualityBaselineConfig where
  parseJSON =
    Core.withObject
      "ModelQualityBaselineConfig"
      ( \x ->
          ModelQualityBaselineConfig'
            Core.<$> (x Core..:? "ConstraintsResource")
            Core.<*> (x Core..:? "BaseliningJobName")
      )

instance Core.Hashable ModelQualityBaselineConfig

instance Core.NFData ModelQualityBaselineConfig

instance Core.ToJSON ModelQualityBaselineConfig where
  toJSON ModelQualityBaselineConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConstraintsResource" Core..=)
              Core.<$> constraintsResource,
            ("BaseliningJobName" Core..=)
              Core.<$> baseliningJobName
          ]
      )
