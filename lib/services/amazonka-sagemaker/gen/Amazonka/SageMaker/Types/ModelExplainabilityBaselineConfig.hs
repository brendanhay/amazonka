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
-- Module      : Amazonka.SageMaker.Types.ModelExplainabilityBaselineConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelExplainabilityBaselineConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringConstraintsResource

-- | The configuration for a baseline model explainability job.
--
-- /See:/ 'newModelExplainabilityBaselineConfig' smart constructor.
data ModelExplainabilityBaselineConfig = ModelExplainabilityBaselineConfig'
  { constraintsResource :: Prelude.Maybe MonitoringConstraintsResource,
    -- | The name of the baseline model explainability job.
    baseliningJobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      baseliningJobName = Prelude.Nothing
    }

-- | Undocumented member.
modelExplainabilityBaselineConfig_constraintsResource :: Lens.Lens' ModelExplainabilityBaselineConfig (Prelude.Maybe MonitoringConstraintsResource)
modelExplainabilityBaselineConfig_constraintsResource = Lens.lens (\ModelExplainabilityBaselineConfig' {constraintsResource} -> constraintsResource) (\s@ModelExplainabilityBaselineConfig' {} a -> s {constraintsResource = a} :: ModelExplainabilityBaselineConfig)

-- | The name of the baseline model explainability job.
modelExplainabilityBaselineConfig_baseliningJobName :: Lens.Lens' ModelExplainabilityBaselineConfig (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> (x Core..:? "ConstraintsResource")
            Prelude.<*> (x Core..:? "BaseliningJobName")
      )

instance
  Prelude.Hashable
    ModelExplainabilityBaselineConfig

instance
  Prelude.NFData
    ModelExplainabilityBaselineConfig

instance
  Core.ToJSON
    ModelExplainabilityBaselineConfig
  where
  toJSON ModelExplainabilityBaselineConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConstraintsResource" Core..=)
              Prelude.<$> constraintsResource,
            ("BaseliningJobName" Core..=)
              Prelude.<$> baseliningJobName
          ]
      )
