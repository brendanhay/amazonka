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
-- Module      : Amazonka.SageMaker.Types.ModelBiasBaselineConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelBiasBaselineConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringConstraintsResource

-- | The configuration for a baseline model bias job.
--
-- /See:/ 'newModelBiasBaselineConfig' smart constructor.
data ModelBiasBaselineConfig = ModelBiasBaselineConfig'
  { -- | The name of the baseline model bias job.
    baseliningJobName :: Prelude.Maybe Prelude.Text,
    constraintsResource :: Prelude.Maybe MonitoringConstraintsResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelBiasBaselineConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseliningJobName', 'modelBiasBaselineConfig_baseliningJobName' - The name of the baseline model bias job.
--
-- 'constraintsResource', 'modelBiasBaselineConfig_constraintsResource' - Undocumented member.
newModelBiasBaselineConfig ::
  ModelBiasBaselineConfig
newModelBiasBaselineConfig =
  ModelBiasBaselineConfig'
    { baseliningJobName =
        Prelude.Nothing,
      constraintsResource = Prelude.Nothing
    }

-- | The name of the baseline model bias job.
modelBiasBaselineConfig_baseliningJobName :: Lens.Lens' ModelBiasBaselineConfig (Prelude.Maybe Prelude.Text)
modelBiasBaselineConfig_baseliningJobName = Lens.lens (\ModelBiasBaselineConfig' {baseliningJobName} -> baseliningJobName) (\s@ModelBiasBaselineConfig' {} a -> s {baseliningJobName = a} :: ModelBiasBaselineConfig)

-- | Undocumented member.
modelBiasBaselineConfig_constraintsResource :: Lens.Lens' ModelBiasBaselineConfig (Prelude.Maybe MonitoringConstraintsResource)
modelBiasBaselineConfig_constraintsResource = Lens.lens (\ModelBiasBaselineConfig' {constraintsResource} -> constraintsResource) (\s@ModelBiasBaselineConfig' {} a -> s {constraintsResource = a} :: ModelBiasBaselineConfig)

instance Data.FromJSON ModelBiasBaselineConfig where
  parseJSON =
    Data.withObject
      "ModelBiasBaselineConfig"
      ( \x ->
          ModelBiasBaselineConfig'
            Prelude.<$> (x Data..:? "BaseliningJobName")
            Prelude.<*> (x Data..:? "ConstraintsResource")
      )

instance Prelude.Hashable ModelBiasBaselineConfig where
  hashWithSalt _salt ModelBiasBaselineConfig' {..} =
    _salt
      `Prelude.hashWithSalt` baseliningJobName
      `Prelude.hashWithSalt` constraintsResource

instance Prelude.NFData ModelBiasBaselineConfig where
  rnf ModelBiasBaselineConfig' {..} =
    Prelude.rnf baseliningJobName
      `Prelude.seq` Prelude.rnf constraintsResource

instance Data.ToJSON ModelBiasBaselineConfig where
  toJSON ModelBiasBaselineConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaseliningJobName" Data..=)
              Prelude.<$> baseliningJobName,
            ("ConstraintsResource" Data..=)
              Prelude.<$> constraintsResource
          ]
      )
