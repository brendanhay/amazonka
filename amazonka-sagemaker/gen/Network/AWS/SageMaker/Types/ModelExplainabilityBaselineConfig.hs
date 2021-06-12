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
-- Module      : Network.AWS.SageMaker.Types.ModelExplainabilityBaselineConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelExplainabilityBaselineConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource

-- | The configuration for a baseline model explainability job.
--
-- /See:/ 'newModelExplainabilityBaselineConfig' smart constructor.
data ModelExplainabilityBaselineConfig = ModelExplainabilityBaselineConfig'
  { constraintsResource :: Core.Maybe MonitoringConstraintsResource,
    -- | The name of the baseline model explainability job.
    baseliningJobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelExplainabilityBaselineConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintsResource', 'modelExplainabilityBaselineConfig_constraintsResource' - Undocumented member.
--
-- 'baseliningJobName', 'modelExplainabilityBaselineConfig_baseliningJobName' - The name of the baseline model explainability job.
newModelExplainabilityBaselineConfig ::
  ModelExplainabilityBaselineConfig
newModelExplainabilityBaselineConfig =
  ModelExplainabilityBaselineConfig'
    { constraintsResource =
        Core.Nothing,
      baseliningJobName = Core.Nothing
    }

-- | Undocumented member.
modelExplainabilityBaselineConfig_constraintsResource :: Lens.Lens' ModelExplainabilityBaselineConfig (Core.Maybe MonitoringConstraintsResource)
modelExplainabilityBaselineConfig_constraintsResource = Lens.lens (\ModelExplainabilityBaselineConfig' {constraintsResource} -> constraintsResource) (\s@ModelExplainabilityBaselineConfig' {} a -> s {constraintsResource = a} :: ModelExplainabilityBaselineConfig)

-- | The name of the baseline model explainability job.
modelExplainabilityBaselineConfig_baseliningJobName :: Lens.Lens' ModelExplainabilityBaselineConfig (Core.Maybe Core.Text)
modelExplainabilityBaselineConfig_baseliningJobName = Lens.lens (\ModelExplainabilityBaselineConfig' {baseliningJobName} -> baseliningJobName) (\s@ModelExplainabilityBaselineConfig' {} a -> s {baseliningJobName = a} :: ModelExplainabilityBaselineConfig)

instance
  Core.FromJSON
    ModelExplainabilityBaselineConfig
  where
  parseJSON =
    Core.withObject
      "ModelExplainabilityBaselineConfig"
      ( \x ->
          ModelExplainabilityBaselineConfig'
            Core.<$> (x Core..:? "ConstraintsResource")
            Core.<*> (x Core..:? "BaseliningJobName")
      )

instance
  Core.Hashable
    ModelExplainabilityBaselineConfig

instance
  Core.NFData
    ModelExplainabilityBaselineConfig

instance
  Core.ToJSON
    ModelExplainabilityBaselineConfig
  where
  toJSON ModelExplainabilityBaselineConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConstraintsResource" Core..=)
              Core.<$> constraintsResource,
            ("BaseliningJobName" Core..=)
              Core.<$> baseliningJobName
          ]
      )
