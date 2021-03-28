{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ResourceLimits
  ( ResourceLimits (..)
  -- * Smart constructor
  , mkResourceLimits
  -- * Lenses
  , rlMaxNumberOfTrainingJobs
  , rlMaxParallelTrainingJobs
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the maximum number of training jobs and parallel training jobs that a hyperparameter tuning job can launch.
--
-- /See:/ 'mkResourceLimits' smart constructor.
data ResourceLimits = ResourceLimits'
  { maxNumberOfTrainingJobs :: Core.Natural
    -- ^ The maximum number of training jobs that a hyperparameter tuning job can launch.
  , maxParallelTrainingJobs :: Core.Natural
    -- ^ The maximum number of concurrent training jobs that a hyperparameter tuning job can launch.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceLimits' value with any optional fields omitted.
mkResourceLimits
    :: Core.Natural -- ^ 'maxNumberOfTrainingJobs'
    -> Core.Natural -- ^ 'maxParallelTrainingJobs'
    -> ResourceLimits
mkResourceLimits maxNumberOfTrainingJobs maxParallelTrainingJobs
  = ResourceLimits'{maxNumberOfTrainingJobs, maxParallelTrainingJobs}

-- | The maximum number of training jobs that a hyperparameter tuning job can launch.
--
-- /Note:/ Consider using 'maxNumberOfTrainingJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlMaxNumberOfTrainingJobs :: Lens.Lens' ResourceLimits Core.Natural
rlMaxNumberOfTrainingJobs = Lens.field @"maxNumberOfTrainingJobs"
{-# INLINEABLE rlMaxNumberOfTrainingJobs #-}
{-# DEPRECATED maxNumberOfTrainingJobs "Use generic-lens or generic-optics with 'maxNumberOfTrainingJobs' instead"  #-}

-- | The maximum number of concurrent training jobs that a hyperparameter tuning job can launch.
--
-- /Note:/ Consider using 'maxParallelTrainingJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlMaxParallelTrainingJobs :: Lens.Lens' ResourceLimits Core.Natural
rlMaxParallelTrainingJobs = Lens.field @"maxParallelTrainingJobs"
{-# INLINEABLE rlMaxParallelTrainingJobs #-}
{-# DEPRECATED maxParallelTrainingJobs "Use generic-lens or generic-optics with 'maxParallelTrainingJobs' instead"  #-}

instance Core.FromJSON ResourceLimits where
        toJSON ResourceLimits{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("MaxNumberOfTrainingJobs" Core..= maxNumberOfTrainingJobs),
                  Core.Just
                    ("MaxParallelTrainingJobs" Core..= maxParallelTrainingJobs)])

instance Core.FromJSON ResourceLimits where
        parseJSON
          = Core.withObject "ResourceLimits" Core.$
              \ x ->
                ResourceLimits' Core.<$>
                  (x Core..: "MaxNumberOfTrainingJobs") Core.<*>
                    x Core..: "MaxParallelTrainingJobs"
