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
-- Module      : Amazonka.SageMaker.Types.DriftCheckBaselines
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DriftCheckBaselines where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DriftCheckBias
import Amazonka.SageMaker.Types.DriftCheckExplainability
import Amazonka.SageMaker.Types.DriftCheckModelDataQuality
import Amazonka.SageMaker.Types.DriftCheckModelQuality

-- | Represents the drift check baselines that can be used when the model
-- monitor is set using the model package.
--
-- /See:/ 'newDriftCheckBaselines' smart constructor.
data DriftCheckBaselines = DriftCheckBaselines'
  { -- | Represents the drift check model data quality baselines that can be used
    -- when the model monitor is set using the model package.
    modelDataQuality :: Prelude.Maybe DriftCheckModelDataQuality,
    -- | Represents the drift check model quality baselines that can be used when
    -- the model monitor is set using the model package.
    modelQuality :: Prelude.Maybe DriftCheckModelQuality,
    -- | Represents the drift check bias baselines that can be used when the
    -- model monitor is set using the model package.
    bias :: Prelude.Maybe DriftCheckBias,
    -- | Represents the drift check explainability baselines that can be used
    -- when the model monitor is set using the model package.
    explainability :: Prelude.Maybe DriftCheckExplainability
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DriftCheckBaselines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelDataQuality', 'driftCheckBaselines_modelDataQuality' - Represents the drift check model data quality baselines that can be used
-- when the model monitor is set using the model package.
--
-- 'modelQuality', 'driftCheckBaselines_modelQuality' - Represents the drift check model quality baselines that can be used when
-- the model monitor is set using the model package.
--
-- 'bias', 'driftCheckBaselines_bias' - Represents the drift check bias baselines that can be used when the
-- model monitor is set using the model package.
--
-- 'explainability', 'driftCheckBaselines_explainability' - Represents the drift check explainability baselines that can be used
-- when the model monitor is set using the model package.
newDriftCheckBaselines ::
  DriftCheckBaselines
newDriftCheckBaselines =
  DriftCheckBaselines'
    { modelDataQuality =
        Prelude.Nothing,
      modelQuality = Prelude.Nothing,
      bias = Prelude.Nothing,
      explainability = Prelude.Nothing
    }

-- | Represents the drift check model data quality baselines that can be used
-- when the model monitor is set using the model package.
driftCheckBaselines_modelDataQuality :: Lens.Lens' DriftCheckBaselines (Prelude.Maybe DriftCheckModelDataQuality)
driftCheckBaselines_modelDataQuality = Lens.lens (\DriftCheckBaselines' {modelDataQuality} -> modelDataQuality) (\s@DriftCheckBaselines' {} a -> s {modelDataQuality = a} :: DriftCheckBaselines)

-- | Represents the drift check model quality baselines that can be used when
-- the model monitor is set using the model package.
driftCheckBaselines_modelQuality :: Lens.Lens' DriftCheckBaselines (Prelude.Maybe DriftCheckModelQuality)
driftCheckBaselines_modelQuality = Lens.lens (\DriftCheckBaselines' {modelQuality} -> modelQuality) (\s@DriftCheckBaselines' {} a -> s {modelQuality = a} :: DriftCheckBaselines)

-- | Represents the drift check bias baselines that can be used when the
-- model monitor is set using the model package.
driftCheckBaselines_bias :: Lens.Lens' DriftCheckBaselines (Prelude.Maybe DriftCheckBias)
driftCheckBaselines_bias = Lens.lens (\DriftCheckBaselines' {bias} -> bias) (\s@DriftCheckBaselines' {} a -> s {bias = a} :: DriftCheckBaselines)

-- | Represents the drift check explainability baselines that can be used
-- when the model monitor is set using the model package.
driftCheckBaselines_explainability :: Lens.Lens' DriftCheckBaselines (Prelude.Maybe DriftCheckExplainability)
driftCheckBaselines_explainability = Lens.lens (\DriftCheckBaselines' {explainability} -> explainability) (\s@DriftCheckBaselines' {} a -> s {explainability = a} :: DriftCheckBaselines)

instance Data.FromJSON DriftCheckBaselines where
  parseJSON =
    Data.withObject
      "DriftCheckBaselines"
      ( \x ->
          DriftCheckBaselines'
            Prelude.<$> (x Data..:? "ModelDataQuality")
            Prelude.<*> (x Data..:? "ModelQuality")
            Prelude.<*> (x Data..:? "Bias")
            Prelude.<*> (x Data..:? "Explainability")
      )

instance Prelude.Hashable DriftCheckBaselines where
  hashWithSalt _salt DriftCheckBaselines' {..} =
    _salt `Prelude.hashWithSalt` modelDataQuality
      `Prelude.hashWithSalt` modelQuality
      `Prelude.hashWithSalt` bias
      `Prelude.hashWithSalt` explainability

instance Prelude.NFData DriftCheckBaselines where
  rnf DriftCheckBaselines' {..} =
    Prelude.rnf modelDataQuality
      `Prelude.seq` Prelude.rnf modelQuality
      `Prelude.seq` Prelude.rnf bias
      `Prelude.seq` Prelude.rnf explainability

instance Data.ToJSON DriftCheckBaselines where
  toJSON DriftCheckBaselines' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ModelDataQuality" Data..=)
              Prelude.<$> modelDataQuality,
            ("ModelQuality" Data..=) Prelude.<$> modelQuality,
            ("Bias" Data..=) Prelude.<$> bias,
            ("Explainability" Data..=)
              Prelude.<$> explainability
          ]
      )
