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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    maxPercentageOfInputDatasetLabeled :: Core.Maybe Core.Natural,
    -- | The maximum number of objects that can be labeled by human workers.
    maxHumanLabeledObjectCount :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      maxHumanLabeledObjectCount = Core.Nothing
    }

-- | The maximum number of input data objects that should be labeled.
labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled :: Lens.Lens' LabelingJobStoppingConditions (Core.Maybe Core.Natural)
labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled = Lens.lens (\LabelingJobStoppingConditions' {maxPercentageOfInputDatasetLabeled} -> maxPercentageOfInputDatasetLabeled) (\s@LabelingJobStoppingConditions' {} a -> s {maxPercentageOfInputDatasetLabeled = a} :: LabelingJobStoppingConditions)

-- | The maximum number of objects that can be labeled by human workers.
labelingJobStoppingConditions_maxHumanLabeledObjectCount :: Lens.Lens' LabelingJobStoppingConditions (Core.Maybe Core.Natural)
labelingJobStoppingConditions_maxHumanLabeledObjectCount = Lens.lens (\LabelingJobStoppingConditions' {maxHumanLabeledObjectCount} -> maxHumanLabeledObjectCount) (\s@LabelingJobStoppingConditions' {} a -> s {maxHumanLabeledObjectCount = a} :: LabelingJobStoppingConditions)

instance Core.FromJSON LabelingJobStoppingConditions where
  parseJSON =
    Core.withObject
      "LabelingJobStoppingConditions"
      ( \x ->
          LabelingJobStoppingConditions'
            Core.<$> (x Core..:? "MaxPercentageOfInputDatasetLabeled")
            Core.<*> (x Core..:? "MaxHumanLabeledObjectCount")
      )

instance Core.Hashable LabelingJobStoppingConditions

instance Core.NFData LabelingJobStoppingConditions

instance Core.ToJSON LabelingJobStoppingConditions where
  toJSON LabelingJobStoppingConditions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxPercentageOfInputDatasetLabeled" Core..=)
              Core.<$> maxPercentageOfInputDatasetLabeled,
            ("MaxHumanLabeledObjectCount" Core..=)
              Core.<$> maxHumanLabeledObjectCount
          ]
      )
