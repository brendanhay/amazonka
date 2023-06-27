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
-- Module      : Amazonka.SageMaker.Types.BestObjectiveNotImproving
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.BestObjectiveNotImproving where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that keeps track of which training jobs launched by your
-- hyperparameter tuning job are not improving model performance as
-- evaluated against an objective function.
--
-- /See:/ 'newBestObjectiveNotImproving' smart constructor.
data BestObjectiveNotImproving = BestObjectiveNotImproving'
  { -- | The number of training jobs that have failed to improve model
    -- performance by 1% or greater over prior training jobs as evaluated
    -- against an objective function.
    maxNumberOfTrainingJobsNotImproving :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BestObjectiveNotImproving' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxNumberOfTrainingJobsNotImproving', 'bestObjectiveNotImproving_maxNumberOfTrainingJobsNotImproving' - The number of training jobs that have failed to improve model
-- performance by 1% or greater over prior training jobs as evaluated
-- against an objective function.
newBestObjectiveNotImproving ::
  BestObjectiveNotImproving
newBestObjectiveNotImproving =
  BestObjectiveNotImproving'
    { maxNumberOfTrainingJobsNotImproving =
        Prelude.Nothing
    }

-- | The number of training jobs that have failed to improve model
-- performance by 1% or greater over prior training jobs as evaluated
-- against an objective function.
bestObjectiveNotImproving_maxNumberOfTrainingJobsNotImproving :: Lens.Lens' BestObjectiveNotImproving (Prelude.Maybe Prelude.Natural)
bestObjectiveNotImproving_maxNumberOfTrainingJobsNotImproving = Lens.lens (\BestObjectiveNotImproving' {maxNumberOfTrainingJobsNotImproving} -> maxNumberOfTrainingJobsNotImproving) (\s@BestObjectiveNotImproving' {} a -> s {maxNumberOfTrainingJobsNotImproving = a} :: BestObjectiveNotImproving)

instance Data.FromJSON BestObjectiveNotImproving where
  parseJSON =
    Data.withObject
      "BestObjectiveNotImproving"
      ( \x ->
          BestObjectiveNotImproving'
            Prelude.<$> (x Data..:? "MaxNumberOfTrainingJobsNotImproving")
      )

instance Prelude.Hashable BestObjectiveNotImproving where
  hashWithSalt _salt BestObjectiveNotImproving' {..} =
    _salt
      `Prelude.hashWithSalt` maxNumberOfTrainingJobsNotImproving

instance Prelude.NFData BestObjectiveNotImproving where
  rnf BestObjectiveNotImproving' {..} =
    Prelude.rnf maxNumberOfTrainingJobsNotImproving

instance Data.ToJSON BestObjectiveNotImproving where
  toJSON BestObjectiveNotImproving' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxNumberOfTrainingJobsNotImproving" Data..=)
              Prelude.<$> maxNumberOfTrainingJobsNotImproving
          ]
      )
