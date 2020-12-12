{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.StoppingCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.StoppingCondition
  ( StoppingCondition (..),

    -- * Smart constructor
    mkStoppingCondition,

    -- * Lenses
    scMaxWaitTimeInSeconds,
    scMaxRuntimeInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a limit to how long a model training or compilation job can run. It also specifies how long you are willing to wait for a managed spot training job to complete. When the job reaches the time limit, Amazon SageMaker ends the training or compilation job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
-- The training algorithms provided by Amazon SageMaker automatically save the intermediate results of a model training job when possible. This attempt to save artifacts is only a best effort case as model might not be in a state from which it can be saved. For example, if training has just started, the model might not be ready to save. When saved, this intermediate data is a valid model artifact. You can use it to create a model with @CreateModel@ .
--
-- /See:/ 'mkStoppingCondition' smart constructor.
data StoppingCondition = StoppingCondition'
  { maxWaitTimeInSeconds ::
      Lude.Maybe Lude.Natural,
    maxRuntimeInSeconds :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StoppingCondition' with the minimum fields required to make a request.
--
-- * 'maxRuntimeInSeconds' - The maximum length of time, in seconds, that the training or compilation job can run. If job does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. The maximum value is 28 days.
-- * 'maxWaitTimeInSeconds' - The maximum length of time, in seconds, how long you are willing to wait for a managed spot training job to complete. It is the amount of time spent waiting for Spot capacity plus the amount of time the training job runs. It must be equal to or greater than @MaxRuntimeInSeconds@ .
mkStoppingCondition ::
  StoppingCondition
mkStoppingCondition =
  StoppingCondition'
    { maxWaitTimeInSeconds = Lude.Nothing,
      maxRuntimeInSeconds = Lude.Nothing
    }

-- | The maximum length of time, in seconds, how long you are willing to wait for a managed spot training job to complete. It is the amount of time spent waiting for Spot capacity plus the amount of time the training job runs. It must be equal to or greater than @MaxRuntimeInSeconds@ .
--
-- /Note:/ Consider using 'maxWaitTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxWaitTimeInSeconds :: Lens.Lens' StoppingCondition (Lude.Maybe Lude.Natural)
scMaxWaitTimeInSeconds = Lens.lens (maxWaitTimeInSeconds :: StoppingCondition -> Lude.Maybe Lude.Natural) (\s a -> s {maxWaitTimeInSeconds = a} :: StoppingCondition)
{-# DEPRECATED scMaxWaitTimeInSeconds "Use generic-lens or generic-optics with 'maxWaitTimeInSeconds' instead." #-}

-- | The maximum length of time, in seconds, that the training or compilation job can run. If job does not complete during this time, Amazon SageMaker ends the job. If value is not specified, default value is 1 day. The maximum value is 28 days.
--
-- /Note:/ Consider using 'maxRuntimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxRuntimeInSeconds :: Lens.Lens' StoppingCondition (Lude.Maybe Lude.Natural)
scMaxRuntimeInSeconds = Lens.lens (maxRuntimeInSeconds :: StoppingCondition -> Lude.Maybe Lude.Natural) (\s a -> s {maxRuntimeInSeconds = a} :: StoppingCondition)
{-# DEPRECATED scMaxRuntimeInSeconds "Use generic-lens or generic-optics with 'maxRuntimeInSeconds' instead." #-}

instance Lude.FromJSON StoppingCondition where
  parseJSON =
    Lude.withObject
      "StoppingCondition"
      ( \x ->
          StoppingCondition'
            Lude.<$> (x Lude..:? "MaxWaitTimeInSeconds")
            Lude.<*> (x Lude..:? "MaxRuntimeInSeconds")
      )

instance Lude.ToJSON StoppingCondition where
  toJSON StoppingCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxWaitTimeInSeconds" Lude..=) Lude.<$> maxWaitTimeInSeconds,
            ("MaxRuntimeInSeconds" Lude..=) Lude.<$> maxRuntimeInSeconds
          ]
      )
