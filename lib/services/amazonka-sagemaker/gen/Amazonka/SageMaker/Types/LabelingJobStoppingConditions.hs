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
-- Module      : Amazonka.SageMaker.Types.LabelingJobStoppingConditions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobStoppingConditions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of conditions for stopping a labeling job. If any of the
-- conditions are met, the job is automatically stopped. You can use these
-- conditions to control the cost of data labeling.
--
-- Labeling jobs fail after 30 days with an appropriate client error
-- message.
--
-- /See:/ 'newLabelingJobStoppingConditions' smart constructor.
data LabelingJobStoppingConditions = LabelingJobStoppingConditions'
  { -- | The maximum number of objects that can be labeled by human workers.
    maxHumanLabeledObjectCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of input data objects that should be labeled.
    maxPercentageOfInputDatasetLabeled :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobStoppingConditions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxHumanLabeledObjectCount', 'labelingJobStoppingConditions_maxHumanLabeledObjectCount' - The maximum number of objects that can be labeled by human workers.
--
-- 'maxPercentageOfInputDatasetLabeled', 'labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled' - The maximum number of input data objects that should be labeled.
newLabelingJobStoppingConditions ::
  LabelingJobStoppingConditions
newLabelingJobStoppingConditions =
  LabelingJobStoppingConditions'
    { maxHumanLabeledObjectCount =
        Prelude.Nothing,
      maxPercentageOfInputDatasetLabeled =
        Prelude.Nothing
    }

-- | The maximum number of objects that can be labeled by human workers.
labelingJobStoppingConditions_maxHumanLabeledObjectCount :: Lens.Lens' LabelingJobStoppingConditions (Prelude.Maybe Prelude.Natural)
labelingJobStoppingConditions_maxHumanLabeledObjectCount = Lens.lens (\LabelingJobStoppingConditions' {maxHumanLabeledObjectCount} -> maxHumanLabeledObjectCount) (\s@LabelingJobStoppingConditions' {} a -> s {maxHumanLabeledObjectCount = a} :: LabelingJobStoppingConditions)

-- | The maximum number of input data objects that should be labeled.
labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled :: Lens.Lens' LabelingJobStoppingConditions (Prelude.Maybe Prelude.Natural)
labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled = Lens.lens (\LabelingJobStoppingConditions' {maxPercentageOfInputDatasetLabeled} -> maxPercentageOfInputDatasetLabeled) (\s@LabelingJobStoppingConditions' {} a -> s {maxPercentageOfInputDatasetLabeled = a} :: LabelingJobStoppingConditions)

instance Data.FromJSON LabelingJobStoppingConditions where
  parseJSON =
    Data.withObject
      "LabelingJobStoppingConditions"
      ( \x ->
          LabelingJobStoppingConditions'
            Prelude.<$> (x Data..:? "MaxHumanLabeledObjectCount")
            Prelude.<*> (x Data..:? "MaxPercentageOfInputDatasetLabeled")
      )

instance
  Prelude.Hashable
    LabelingJobStoppingConditions
  where
  hashWithSalt _salt LabelingJobStoppingConditions' {..} =
    _salt
      `Prelude.hashWithSalt` maxHumanLabeledObjectCount
      `Prelude.hashWithSalt` maxPercentageOfInputDatasetLabeled

instance Prelude.NFData LabelingJobStoppingConditions where
  rnf LabelingJobStoppingConditions' {..} =
    Prelude.rnf maxHumanLabeledObjectCount
      `Prelude.seq` Prelude.rnf maxPercentageOfInputDatasetLabeled

instance Data.ToJSON LabelingJobStoppingConditions where
  toJSON LabelingJobStoppingConditions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxHumanLabeledObjectCount" Data..=)
              Prelude.<$> maxHumanLabeledObjectCount,
            ("MaxPercentageOfInputDatasetLabeled" Data..=)
              Prelude.<$> maxPercentageOfInputDatasetLabeled
          ]
      )
