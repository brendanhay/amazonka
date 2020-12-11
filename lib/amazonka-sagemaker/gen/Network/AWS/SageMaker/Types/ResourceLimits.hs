-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceLimits
  ( ResourceLimits (..),

    -- * Smart constructor
    mkResourceLimits,

    -- * Lenses
    rlMaxNumberOfTrainingJobs,
    rlMaxParallelTrainingJobs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the maximum number of training jobs and parallel training jobs that a hyperparameter tuning job can launch.
--
-- /See:/ 'mkResourceLimits' smart constructor.
data ResourceLimits = ResourceLimits'
  { maxNumberOfTrainingJobs ::
      Lude.Natural,
    maxParallelTrainingJobs :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceLimits' with the minimum fields required to make a request.
--
-- * 'maxNumberOfTrainingJobs' - The maximum number of training jobs that a hyperparameter tuning job can launch.
-- * 'maxParallelTrainingJobs' - The maximum number of concurrent training jobs that a hyperparameter tuning job can launch.
mkResourceLimits ::
  -- | 'maxNumberOfTrainingJobs'
  Lude.Natural ->
  -- | 'maxParallelTrainingJobs'
  Lude.Natural ->
  ResourceLimits
mkResourceLimits
  pMaxNumberOfTrainingJobs_
  pMaxParallelTrainingJobs_ =
    ResourceLimits'
      { maxNumberOfTrainingJobs =
          pMaxNumberOfTrainingJobs_,
        maxParallelTrainingJobs = pMaxParallelTrainingJobs_
      }

-- | The maximum number of training jobs that a hyperparameter tuning job can launch.
--
-- /Note:/ Consider using 'maxNumberOfTrainingJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlMaxNumberOfTrainingJobs :: Lens.Lens' ResourceLimits Lude.Natural
rlMaxNumberOfTrainingJobs = Lens.lens (maxNumberOfTrainingJobs :: ResourceLimits -> Lude.Natural) (\s a -> s {maxNumberOfTrainingJobs = a} :: ResourceLimits)
{-# DEPRECATED rlMaxNumberOfTrainingJobs "Use generic-lens or generic-optics with 'maxNumberOfTrainingJobs' instead." #-}

-- | The maximum number of concurrent training jobs that a hyperparameter tuning job can launch.
--
-- /Note:/ Consider using 'maxParallelTrainingJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlMaxParallelTrainingJobs :: Lens.Lens' ResourceLimits Lude.Natural
rlMaxParallelTrainingJobs = Lens.lens (maxParallelTrainingJobs :: ResourceLimits -> Lude.Natural) (\s a -> s {maxParallelTrainingJobs = a} :: ResourceLimits)
{-# DEPRECATED rlMaxParallelTrainingJobs "Use generic-lens or generic-optics with 'maxParallelTrainingJobs' instead." #-}

instance Lude.FromJSON ResourceLimits where
  parseJSON =
    Lude.withObject
      "ResourceLimits"
      ( \x ->
          ResourceLimits'
            Lude.<$> (x Lude..: "MaxNumberOfTrainingJobs")
            Lude.<*> (x Lude..: "MaxParallelTrainingJobs")
      )

instance Lude.ToJSON ResourceLimits where
  toJSON ResourceLimits' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("MaxNumberOfTrainingJobs" Lude..= maxNumberOfTrainingJobs),
            Lude.Just
              ("MaxParallelTrainingJobs" Lude..= maxParallelTrainingJobs)
          ]
      )
