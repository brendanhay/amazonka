{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobStoppingConditions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobStoppingConditions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A set of conditions for stopping a labeling job. If any of the
-- conditions are met, the job is automatically stopped. You can use these
-- conditions to control the cost of data labeling.
--
-- Labeling jobs fail after 30 days with an appropriate client error
-- message.
--
-- /See:/ 'newLabelingJobStoppingConditions' smart constructor.
data LabelingJobStoppingConditions = LabelingJobStoppingConditions'
  { -- | The maximum number of input data objects that should be labeled.
    maxPercentageOfInputDatasetLabeled :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of objects that can be labeled by human workers.
    maxHumanLabeledObjectCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobStoppingConditions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxPercentageOfInputDatasetLabeled', 'labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled' - The maximum number of input data objects that should be labeled.
--
-- 'maxHumanLabeledObjectCount', 'labelingJobStoppingConditions_maxHumanLabeledObjectCount' - The maximum number of objects that can be labeled by human workers.
newLabelingJobStoppingConditions ::
  LabelingJobStoppingConditions
newLabelingJobStoppingConditions =
  LabelingJobStoppingConditions'
    { maxPercentageOfInputDatasetLabeled =
        Prelude.Nothing,
      maxHumanLabeledObjectCount = Prelude.Nothing
    }

-- | The maximum number of input data objects that should be labeled.
labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled :: Lens.Lens' LabelingJobStoppingConditions (Prelude.Maybe Prelude.Natural)
labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled = Lens.lens (\LabelingJobStoppingConditions' {maxPercentageOfInputDatasetLabeled} -> maxPercentageOfInputDatasetLabeled) (\s@LabelingJobStoppingConditions' {} a -> s {maxPercentageOfInputDatasetLabeled = a} :: LabelingJobStoppingConditions)

-- | The maximum number of objects that can be labeled by human workers.
labelingJobStoppingConditions_maxHumanLabeledObjectCount :: Lens.Lens' LabelingJobStoppingConditions (Prelude.Maybe Prelude.Natural)
labelingJobStoppingConditions_maxHumanLabeledObjectCount = Lens.lens (\LabelingJobStoppingConditions' {maxHumanLabeledObjectCount} -> maxHumanLabeledObjectCount) (\s@LabelingJobStoppingConditions' {} a -> s {maxHumanLabeledObjectCount = a} :: LabelingJobStoppingConditions)

instance
  Prelude.FromJSON
    LabelingJobStoppingConditions
  where
  parseJSON =
    Prelude.withObject
      "LabelingJobStoppingConditions"
      ( \x ->
          LabelingJobStoppingConditions'
            Prelude.<$> (x Prelude..:? "MaxPercentageOfInputDatasetLabeled")
            Prelude.<*> (x Prelude..:? "MaxHumanLabeledObjectCount")
      )

instance
  Prelude.Hashable
    LabelingJobStoppingConditions

instance Prelude.NFData LabelingJobStoppingConditions

instance Prelude.ToJSON LabelingJobStoppingConditions where
  toJSON LabelingJobStoppingConditions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxPercentageOfInputDatasetLabeled" Prelude..=)
              Prelude.<$> maxPercentageOfInputDatasetLabeled,
            ("MaxHumanLabeledObjectCount" Prelude..=)
              Prelude.<$> maxHumanLabeledObjectCount
          ]
      )
