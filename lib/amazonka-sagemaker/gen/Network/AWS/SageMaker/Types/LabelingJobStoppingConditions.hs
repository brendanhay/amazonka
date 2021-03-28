{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobStoppingConditions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.LabelingJobStoppingConditions
  ( LabelingJobStoppingConditions (..)
  -- * Smart constructor
  , mkLabelingJobStoppingConditions
  -- * Lenses
  , ljscMaxHumanLabeledObjectCount
  , ljscMaxPercentageOfInputDatasetLabeled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A set of conditions for stopping a labeling job. If any of the conditions are met, the job is automatically stopped. You can use these conditions to control the cost of data labeling.
--
-- /See:/ 'mkLabelingJobStoppingConditions' smart constructor.
data LabelingJobStoppingConditions = LabelingJobStoppingConditions'
  { maxHumanLabeledObjectCount :: Core.Maybe Core.Natural
    -- ^ The maximum number of objects that can be labeled by human workers.
  , maxPercentageOfInputDatasetLabeled :: Core.Maybe Core.Natural
    -- ^ The maximum number of input data objects that should be labeled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobStoppingConditions' value with any optional fields omitted.
mkLabelingJobStoppingConditions
    :: LabelingJobStoppingConditions
mkLabelingJobStoppingConditions
  = LabelingJobStoppingConditions'{maxHumanLabeledObjectCount =
                                     Core.Nothing,
                                   maxPercentageOfInputDatasetLabeled = Core.Nothing}

-- | The maximum number of objects that can be labeled by human workers.
--
-- /Note:/ Consider using 'maxHumanLabeledObjectCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljscMaxHumanLabeledObjectCount :: Lens.Lens' LabelingJobStoppingConditions (Core.Maybe Core.Natural)
ljscMaxHumanLabeledObjectCount = Lens.field @"maxHumanLabeledObjectCount"
{-# INLINEABLE ljscMaxHumanLabeledObjectCount #-}
{-# DEPRECATED maxHumanLabeledObjectCount "Use generic-lens or generic-optics with 'maxHumanLabeledObjectCount' instead"  #-}

-- | The maximum number of input data objects that should be labeled.
--
-- /Note:/ Consider using 'maxPercentageOfInputDatasetLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljscMaxPercentageOfInputDatasetLabeled :: Lens.Lens' LabelingJobStoppingConditions (Core.Maybe Core.Natural)
ljscMaxPercentageOfInputDatasetLabeled = Lens.field @"maxPercentageOfInputDatasetLabeled"
{-# INLINEABLE ljscMaxPercentageOfInputDatasetLabeled #-}
{-# DEPRECATED maxPercentageOfInputDatasetLabeled "Use generic-lens or generic-optics with 'maxPercentageOfInputDatasetLabeled' instead"  #-}

instance Core.FromJSON LabelingJobStoppingConditions where
        toJSON LabelingJobStoppingConditions{..}
          = Core.object
              (Core.catMaybes
                 [("MaxHumanLabeledObjectCount" Core..=) Core.<$>
                    maxHumanLabeledObjectCount,
                  ("MaxPercentageOfInputDatasetLabeled" Core..=) Core.<$>
                    maxPercentageOfInputDatasetLabeled])

instance Core.FromJSON LabelingJobStoppingConditions where
        parseJSON
          = Core.withObject "LabelingJobStoppingConditions" Core.$
              \ x ->
                LabelingJobStoppingConditions' Core.<$>
                  (x Core..:? "MaxHumanLabeledObjectCount") Core.<*>
                    x Core..:? "MaxPercentageOfInputDatasetLabeled"
