{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.StoppingCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.StoppingCondition
  ( StoppingCondition (..)
  -- * Smart constructor
  , mkStoppingCondition
  -- * Lenses
  , scMaxRuntimeInSeconds
  , scMaxWaitTimeInSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a limit to how long a model training or compilation job can run. It also specifies how long you are willing to wait for a managed spot training job to complete. When the job reaches the time limit, Amazon SageMaker ends the training or compilation job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost. 
-- The training algorithms provided by Amazon SageMaker automatically save the intermediate results of a model training job when possible. This attempt to save artifacts is only a best effort case as model might not be in a state from which it can be saved. For example, if training has just started, the model might not be ready to save. When saved, this intermediate data is a valid model artifact. You can use it to create a model with @CreateModel@ .
--
-- /See:/ 'mkStoppingCondition' smart constructor.
data StoppingCondition = StoppingCondition'
  { maxRuntimeInSeconds :: Core.Maybe Core.Natural
    -- ^ The maximum length of time, in seconds, that the training or compilation job can run. If job does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. The maximum value is 28 days.
  , maxWaitTimeInSeconds :: Core.Maybe Core.Natural
    -- ^ The maximum length of time, in seconds, how long you are willing to wait for a managed spot training job to complete. It is the amount of time spent waiting for Spot capacity plus the amount of time the training job runs. It must be equal to or greater than @MaxRuntimeInSeconds@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StoppingCondition' value with any optional fields omitted.
mkStoppingCondition
    :: StoppingCondition
mkStoppingCondition
  = StoppingCondition'{maxRuntimeInSeconds = Core.Nothing,
                       maxWaitTimeInSeconds = Core.Nothing}

-- | The maximum length of time, in seconds, that the training or compilation job can run. If job does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. The maximum value is 28 days.
--
-- /Note:/ Consider using 'maxRuntimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxRuntimeInSeconds :: Lens.Lens' StoppingCondition (Core.Maybe Core.Natural)
scMaxRuntimeInSeconds = Lens.field @"maxRuntimeInSeconds"
{-# INLINEABLE scMaxRuntimeInSeconds #-}
{-# DEPRECATED maxRuntimeInSeconds "Use generic-lens or generic-optics with 'maxRuntimeInSeconds' instead"  #-}

-- | The maximum length of time, in seconds, how long you are willing to wait for a managed spot training job to complete. It is the amount of time spent waiting for Spot capacity plus the amount of time the training job runs. It must be equal to or greater than @MaxRuntimeInSeconds@ . 
--
-- /Note:/ Consider using 'maxWaitTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxWaitTimeInSeconds :: Lens.Lens' StoppingCondition (Core.Maybe Core.Natural)
scMaxWaitTimeInSeconds = Lens.field @"maxWaitTimeInSeconds"
{-# INLINEABLE scMaxWaitTimeInSeconds #-}
{-# DEPRECATED maxWaitTimeInSeconds "Use generic-lens or generic-optics with 'maxWaitTimeInSeconds' instead"  #-}

instance Core.FromJSON StoppingCondition where
        toJSON StoppingCondition{..}
          = Core.object
              (Core.catMaybes
                 [("MaxRuntimeInSeconds" Core..=) Core.<$> maxRuntimeInSeconds,
                  ("MaxWaitTimeInSeconds" Core..=) Core.<$> maxWaitTimeInSeconds])

instance Core.FromJSON StoppingCondition where
        parseJSON
          = Core.withObject "StoppingCondition" Core.$
              \ x ->
                StoppingCondition' Core.<$>
                  (x Core..:? "MaxRuntimeInSeconds") Core.<*>
                    x Core..:? "MaxWaitTimeInSeconds"
