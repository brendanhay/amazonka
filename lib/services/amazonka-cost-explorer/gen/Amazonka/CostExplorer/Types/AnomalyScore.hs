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
-- Module      : Amazonka.CostExplorer.Types.AnomalyScore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.AnomalyScore where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Quantifies the anomaly. The higher score means that it\'s more
-- anomalous.
--
-- /See:/ 'newAnomalyScore' smart constructor.
data AnomalyScore = AnomalyScore'
  { -- | The maximum score that\'s observed during the @AnomalyDateInterval@.
    maxScore :: Prelude.Double,
    -- | The last observed score.
    currentScore :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyScore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxScore', 'anomalyScore_maxScore' - The maximum score that\'s observed during the @AnomalyDateInterval@.
--
-- 'currentScore', 'anomalyScore_currentScore' - The last observed score.
newAnomalyScore ::
  -- | 'maxScore'
  Prelude.Double ->
  -- | 'currentScore'
  Prelude.Double ->
  AnomalyScore
newAnomalyScore pMaxScore_ pCurrentScore_ =
  AnomalyScore'
    { maxScore = pMaxScore_,
      currentScore = pCurrentScore_
    }

-- | The maximum score that\'s observed during the @AnomalyDateInterval@.
anomalyScore_maxScore :: Lens.Lens' AnomalyScore Prelude.Double
anomalyScore_maxScore = Lens.lens (\AnomalyScore' {maxScore} -> maxScore) (\s@AnomalyScore' {} a -> s {maxScore = a} :: AnomalyScore)

-- | The last observed score.
anomalyScore_currentScore :: Lens.Lens' AnomalyScore Prelude.Double
anomalyScore_currentScore = Lens.lens (\AnomalyScore' {currentScore} -> currentScore) (\s@AnomalyScore' {} a -> s {currentScore = a} :: AnomalyScore)

instance Data.FromJSON AnomalyScore where
  parseJSON =
    Data.withObject
      "AnomalyScore"
      ( \x ->
          AnomalyScore'
            Prelude.<$> (x Data..: "MaxScore")
            Prelude.<*> (x Data..: "CurrentScore")
      )

instance Prelude.Hashable AnomalyScore where
  hashWithSalt _salt AnomalyScore' {..} =
    _salt
      `Prelude.hashWithSalt` maxScore
      `Prelude.hashWithSalt` currentScore

instance Prelude.NFData AnomalyScore where
  rnf AnomalyScore' {..} =
    Prelude.rnf maxScore
      `Prelude.seq` Prelude.rnf currentScore
