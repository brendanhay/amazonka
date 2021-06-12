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
-- Module      : Network.AWS.SageMaker.Types.ModelBiasBaselineConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelBiasBaselineConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource

-- | The configuration for a baseline model bias job.
--
-- /See:/ 'newModelBiasBaselineConfig' smart constructor.
data ModelBiasBaselineConfig = ModelBiasBaselineConfig'
  { constraintsResource :: Core.Maybe MonitoringConstraintsResource,
    -- | The name of the baseline model bias job.
    baseliningJobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelBiasBaselineConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintsResource', 'modelBiasBaselineConfig_constraintsResource' - Undocumented member.
--
-- 'baseliningJobName', 'modelBiasBaselineConfig_baseliningJobName' - The name of the baseline model bias job.
newModelBiasBaselineConfig ::
  ModelBiasBaselineConfig
newModelBiasBaselineConfig =
  ModelBiasBaselineConfig'
    { constraintsResource =
        Core.Nothing,
      baseliningJobName = Core.Nothing
    }

-- | Undocumented member.
modelBiasBaselineConfig_constraintsResource :: Lens.Lens' ModelBiasBaselineConfig (Core.Maybe MonitoringConstraintsResource)
modelBiasBaselineConfig_constraintsResource = Lens.lens (\ModelBiasBaselineConfig' {constraintsResource} -> constraintsResource) (\s@ModelBiasBaselineConfig' {} a -> s {constraintsResource = a} :: ModelBiasBaselineConfig)

-- | The name of the baseline model bias job.
modelBiasBaselineConfig_baseliningJobName :: Lens.Lens' ModelBiasBaselineConfig (Core.Maybe Core.Text)
modelBiasBaselineConfig_baseliningJobName = Lens.lens (\ModelBiasBaselineConfig' {baseliningJobName} -> baseliningJobName) (\s@ModelBiasBaselineConfig' {} a -> s {baseliningJobName = a} :: ModelBiasBaselineConfig)

instance Core.FromJSON ModelBiasBaselineConfig where
  parseJSON =
    Core.withObject
      "ModelBiasBaselineConfig"
      ( \x ->
          ModelBiasBaselineConfig'
            Core.<$> (x Core..:? "ConstraintsResource")
            Core.<*> (x Core..:? "BaseliningJobName")
      )

instance Core.Hashable ModelBiasBaselineConfig

instance Core.NFData ModelBiasBaselineConfig

instance Core.ToJSON ModelBiasBaselineConfig where
  toJSON ModelBiasBaselineConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConstraintsResource" Core..=)
              Core.<$> constraintsResource,
            ("BaseliningJobName" Core..=)
              Core.<$> baseliningJobName
          ]
      )
