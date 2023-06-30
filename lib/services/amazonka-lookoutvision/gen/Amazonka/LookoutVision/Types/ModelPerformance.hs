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
-- Module      : Amazonka.LookoutVision.Types.ModelPerformance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ModelPerformance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the evaluation performance of a trained model.
--
-- /See:/ 'newModelPerformance' smart constructor.
data ModelPerformance = ModelPerformance'
  { -- | The overall F1 score metric for the trained model.
    f1Score :: Prelude.Maybe Prelude.Double,
    -- | The overall precision metric value for the trained model.
    precision :: Prelude.Maybe Prelude.Double,
    -- | The overall recall metric value for the trained model.
    recall :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelPerformance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'f1Score', 'modelPerformance_f1Score' - The overall F1 score metric for the trained model.
--
-- 'precision', 'modelPerformance_precision' - The overall precision metric value for the trained model.
--
-- 'recall', 'modelPerformance_recall' - The overall recall metric value for the trained model.
newModelPerformance ::
  ModelPerformance
newModelPerformance =
  ModelPerformance'
    { f1Score = Prelude.Nothing,
      precision = Prelude.Nothing,
      recall = Prelude.Nothing
    }

-- | The overall F1 score metric for the trained model.
modelPerformance_f1Score :: Lens.Lens' ModelPerformance (Prelude.Maybe Prelude.Double)
modelPerformance_f1Score = Lens.lens (\ModelPerformance' {f1Score} -> f1Score) (\s@ModelPerformance' {} a -> s {f1Score = a} :: ModelPerformance)

-- | The overall precision metric value for the trained model.
modelPerformance_precision :: Lens.Lens' ModelPerformance (Prelude.Maybe Prelude.Double)
modelPerformance_precision = Lens.lens (\ModelPerformance' {precision} -> precision) (\s@ModelPerformance' {} a -> s {precision = a} :: ModelPerformance)

-- | The overall recall metric value for the trained model.
modelPerformance_recall :: Lens.Lens' ModelPerformance (Prelude.Maybe Prelude.Double)
modelPerformance_recall = Lens.lens (\ModelPerformance' {recall} -> recall) (\s@ModelPerformance' {} a -> s {recall = a} :: ModelPerformance)

instance Data.FromJSON ModelPerformance where
  parseJSON =
    Data.withObject
      "ModelPerformance"
      ( \x ->
          ModelPerformance'
            Prelude.<$> (x Data..:? "F1Score")
            Prelude.<*> (x Data..:? "Precision")
            Prelude.<*> (x Data..:? "Recall")
      )

instance Prelude.Hashable ModelPerformance where
  hashWithSalt _salt ModelPerformance' {..} =
    _salt
      `Prelude.hashWithSalt` f1Score
      `Prelude.hashWithSalt` precision
      `Prelude.hashWithSalt` recall

instance Prelude.NFData ModelPerformance where
  rnf ModelPerformance' {..} =
    Prelude.rnf f1Score
      `Prelude.seq` Prelude.rnf precision
      `Prelude.seq` Prelude.rnf recall
