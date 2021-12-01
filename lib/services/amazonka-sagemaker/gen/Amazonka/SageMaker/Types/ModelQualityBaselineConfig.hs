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
-- Module      : Amazonka.SageMaker.Types.ModelQualityBaselineConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelQualityBaselineConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringConstraintsResource

-- | Configuration for monitoring constraints and monitoring statistics.
-- These baseline resources are compared against the results of the current
-- job from the series of jobs scheduled to collect data periodically.
--
-- /See:/ 'newModelQualityBaselineConfig' smart constructor.
data ModelQualityBaselineConfig = ModelQualityBaselineConfig'
  { constraintsResource :: Prelude.Maybe MonitoringConstraintsResource,
    -- | The name of the job that performs baselining for the monitoring job.
    baseliningJobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      baseliningJobName = Prelude.Nothing
    }

-- | Undocumented member.
modelQualityBaselineConfig_constraintsResource :: Lens.Lens' ModelQualityBaselineConfig (Prelude.Maybe MonitoringConstraintsResource)
modelQualityBaselineConfig_constraintsResource = Lens.lens (\ModelQualityBaselineConfig' {constraintsResource} -> constraintsResource) (\s@ModelQualityBaselineConfig' {} a -> s {constraintsResource = a} :: ModelQualityBaselineConfig)

-- | The name of the job that performs baselining for the monitoring job.
modelQualityBaselineConfig_baseliningJobName :: Lens.Lens' ModelQualityBaselineConfig (Prelude.Maybe Prelude.Text)
modelQualityBaselineConfig_baseliningJobName = Lens.lens (\ModelQualityBaselineConfig' {baseliningJobName} -> baseliningJobName) (\s@ModelQualityBaselineConfig' {} a -> s {baseliningJobName = a} :: ModelQualityBaselineConfig)

instance Core.FromJSON ModelQualityBaselineConfig where
  parseJSON =
    Core.withObject
      "ModelQualityBaselineConfig"
      ( \x ->
          ModelQualityBaselineConfig'
            Prelude.<$> (x Core..:? "ConstraintsResource")
            Prelude.<*> (x Core..:? "BaseliningJobName")
      )

instance Prelude.Hashable ModelQualityBaselineConfig where
  hashWithSalt salt' ModelQualityBaselineConfig' {..} =
    salt' `Prelude.hashWithSalt` baseliningJobName
      `Prelude.hashWithSalt` constraintsResource

instance Prelude.NFData ModelQualityBaselineConfig where
  rnf ModelQualityBaselineConfig' {..} =
    Prelude.rnf constraintsResource
      `Prelude.seq` Prelude.rnf baseliningJobName

instance Core.ToJSON ModelQualityBaselineConfig where
  toJSON ModelQualityBaselineConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConstraintsResource" Core..=)
              Prelude.<$> constraintsResource,
            ("BaseliningJobName" Core..=)
              Prelude.<$> baseliningJobName
          ]
      )
