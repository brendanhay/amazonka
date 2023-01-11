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
-- Module      : Amazonka.FraudDetector.Types.ModelScores
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ModelScores where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ModelVersion
import qualified Amazonka.Prelude as Prelude

-- | The fraud prediction scores.
--
-- /See:/ 'newModelScores' smart constructor.
data ModelScores = ModelScores'
  { -- | The model version.
    modelVersion :: Prelude.Maybe ModelVersion,
    -- | The model\'s fraud prediction scores.
    scores :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelScores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelVersion', 'modelScores_modelVersion' - The model version.
--
-- 'scores', 'modelScores_scores' - The model\'s fraud prediction scores.
newModelScores ::
  ModelScores
newModelScores =
  ModelScores'
    { modelVersion = Prelude.Nothing,
      scores = Prelude.Nothing
    }

-- | The model version.
modelScores_modelVersion :: Lens.Lens' ModelScores (Prelude.Maybe ModelVersion)
modelScores_modelVersion = Lens.lens (\ModelScores' {modelVersion} -> modelVersion) (\s@ModelScores' {} a -> s {modelVersion = a} :: ModelScores)

-- | The model\'s fraud prediction scores.
modelScores_scores :: Lens.Lens' ModelScores (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
modelScores_scores = Lens.lens (\ModelScores' {scores} -> scores) (\s@ModelScores' {} a -> s {scores = a} :: ModelScores) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ModelScores where
  parseJSON =
    Data.withObject
      "ModelScores"
      ( \x ->
          ModelScores'
            Prelude.<$> (x Data..:? "modelVersion")
            Prelude.<*> (x Data..:? "scores" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ModelScores where
  hashWithSalt _salt ModelScores' {..} =
    _salt `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` scores

instance Prelude.NFData ModelScores where
  rnf ModelScores' {..} =
    Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf scores
