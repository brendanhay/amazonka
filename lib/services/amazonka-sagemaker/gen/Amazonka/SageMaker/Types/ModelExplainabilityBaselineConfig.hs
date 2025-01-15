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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelExplainabilityBaselineConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringConstraintsResource

-- | The configuration for a baseline model explainability job.
--
-- /See:/ 'newModelExplainabilityBaselineConfig' smart constructor.
data ModelExplainabilityBaselineConfig = ModelExplainabilityBaselineConfig'
  { -- | The name of the baseline model explainability job.
    baseliningJobName :: Prelude.Maybe Prelude.Text,
    constraintsResource :: Prelude.Maybe MonitoringConstraintsResource
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
-- 'baseliningJobName', 'modelExplainabilityBaselineConfig_baseliningJobName' - The name of the baseline model explainability job.
--
-- 'constraintsResource', 'modelExplainabilityBaselineConfig_constraintsResource' - Undocumented member.
newModelExplainabilityBaselineConfig ::
  ModelExplainabilityBaselineConfig
newModelExplainabilityBaselineConfig =
  ModelExplainabilityBaselineConfig'
    { baseliningJobName =
        Prelude.Nothing,
      constraintsResource = Prelude.Nothing
    }

-- | The name of the baseline model explainability job.
modelExplainabilityBaselineConfig_baseliningJobName :: Lens.Lens' ModelExplainabilityBaselineConfig (Prelude.Maybe Prelude.Text)
modelExplainabilityBaselineConfig_baseliningJobName = Lens.lens (\ModelExplainabilityBaselineConfig' {baseliningJobName} -> baseliningJobName) (\s@ModelExplainabilityBaselineConfig' {} a -> s {baseliningJobName = a} :: ModelExplainabilityBaselineConfig)

-- | Undocumented member.
modelExplainabilityBaselineConfig_constraintsResource :: Lens.Lens' ModelExplainabilityBaselineConfig (Prelude.Maybe MonitoringConstraintsResource)
modelExplainabilityBaselineConfig_constraintsResource = Lens.lens (\ModelExplainabilityBaselineConfig' {constraintsResource} -> constraintsResource) (\s@ModelExplainabilityBaselineConfig' {} a -> s {constraintsResource = a} :: ModelExplainabilityBaselineConfig)

instance
  Data.FromJSON
    ModelExplainabilityBaselineConfig
  where
  parseJSON =
    Data.withObject
      "ModelExplainabilityBaselineConfig"
      ( \x ->
          ModelExplainabilityBaselineConfig'
            Prelude.<$> (x Data..:? "BaseliningJobName")
            Prelude.<*> (x Data..:? "ConstraintsResource")
      )

instance
  Prelude.Hashable
    ModelExplainabilityBaselineConfig
  where
  hashWithSalt
    _salt
    ModelExplainabilityBaselineConfig' {..} =
      _salt
        `Prelude.hashWithSalt` baseliningJobName
        `Prelude.hashWithSalt` constraintsResource

instance
  Prelude.NFData
    ModelExplainabilityBaselineConfig
  where
  rnf ModelExplainabilityBaselineConfig' {..} =
    Prelude.rnf baseliningJobName `Prelude.seq`
      Prelude.rnf constraintsResource

instance
  Data.ToJSON
    ModelExplainabilityBaselineConfig
  where
  toJSON ModelExplainabilityBaselineConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaseliningJobName" Data..=)
              Prelude.<$> baseliningJobName,
            ("ConstraintsResource" Data..=)
              Prelude.<$> constraintsResource
          ]
      )
