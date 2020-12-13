{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobStoppingConditions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobStoppingConditions
  ( LabelingJobStoppingConditions (..),

    -- * Smart constructor
    mkLabelingJobStoppingConditions,

    -- * Lenses
    ljscMaxHumanLabeledObjectCount,
    ljscMaxPercentageOfInputDatasetLabeled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped. You can use these conditions to control the cost of data labeling.
--
-- /See:/ 'mkLabelingJobStoppingConditions' smart constructor.
data LabelingJobStoppingConditions = LabelingJobStoppingConditions'
  { -- | The maximum number of objects that can be labeled by human workers.
    maxHumanLabeledObjectCount :: Lude.Maybe Lude.Natural,
    -- | The maximum number of input data objects that should be labeled.
    maxPercentageOfInputDatasetLabeled :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobStoppingConditions' with the minimum fields required to make a request.
--
-- * 'maxHumanLabeledObjectCount' - The maximum number of objects that can be labeled by human workers.
-- * 'maxPercentageOfInputDatasetLabeled' - The maximum number of input data objects that should be labeled.
mkLabelingJobStoppingConditions ::
  LabelingJobStoppingConditions
mkLabelingJobStoppingConditions =
  LabelingJobStoppingConditions'
    { maxHumanLabeledObjectCount =
        Lude.Nothing,
      maxPercentageOfInputDatasetLabeled = Lude.Nothing
    }

-- | The maximum number of objects that can be labeled by human workers.
--
-- /Note:/ Consider using 'maxHumanLabeledObjectCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljscMaxHumanLabeledObjectCount :: Lens.Lens' LabelingJobStoppingConditions (Lude.Maybe Lude.Natural)
ljscMaxHumanLabeledObjectCount = Lens.lens (maxHumanLabeledObjectCount :: LabelingJobStoppingConditions -> Lude.Maybe Lude.Natural) (\s a -> s {maxHumanLabeledObjectCount = a} :: LabelingJobStoppingConditions)
{-# DEPRECATED ljscMaxHumanLabeledObjectCount "Use generic-lens or generic-optics with 'maxHumanLabeledObjectCount' instead." #-}

-- | The maximum number of input data objects that should be labeled.
--
-- /Note:/ Consider using 'maxPercentageOfInputDatasetLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljscMaxPercentageOfInputDatasetLabeled :: Lens.Lens' LabelingJobStoppingConditions (Lude.Maybe Lude.Natural)
ljscMaxPercentageOfInputDatasetLabeled = Lens.lens (maxPercentageOfInputDatasetLabeled :: LabelingJobStoppingConditions -> Lude.Maybe Lude.Natural) (\s a -> s {maxPercentageOfInputDatasetLabeled = a} :: LabelingJobStoppingConditions)
{-# DEPRECATED ljscMaxPercentageOfInputDatasetLabeled "Use generic-lens or generic-optics with 'maxPercentageOfInputDatasetLabeled' instead." #-}

instance Lude.FromJSON LabelingJobStoppingConditions where
  parseJSON =
    Lude.withObject
      "LabelingJobStoppingConditions"
      ( \x ->
          LabelingJobStoppingConditions'
            Lude.<$> (x Lude..:? "MaxHumanLabeledObjectCount")
            Lude.<*> (x Lude..:? "MaxPercentageOfInputDatasetLabeled")
      )

instance Lude.ToJSON LabelingJobStoppingConditions where
  toJSON LabelingJobStoppingConditions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxHumanLabeledObjectCount" Lude..=)
              Lude.<$> maxHumanLabeledObjectCount,
            ("MaxPercentageOfInputDatasetLabeled" Lude..=)
              Lude.<$> maxPercentageOfInputDatasetLabeled
          ]
      )
