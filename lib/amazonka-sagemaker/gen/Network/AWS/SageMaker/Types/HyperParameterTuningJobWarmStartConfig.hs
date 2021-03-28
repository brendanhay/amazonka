{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
  ( HyperParameterTuningJobWarmStartConfig (..)
  -- * Smart constructor
  , mkHyperParameterTuningJobWarmStartConfig
  -- * Lenses
  , hptjwscParentHyperParameterTuningJobs
  , hptjwscWarmStartType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType as Types
import qualified Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob as Types

-- | Specifies the configuration for a hyperparameter tuning job that uses one or more previous hyperparameter tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
-- All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric, and the training job that performs the best is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
--
-- /See:/ 'mkHyperParameterTuningJobWarmStartConfig' smart constructor.
data HyperParameterTuningJobWarmStartConfig = HyperParameterTuningJobWarmStartConfig'
  { parentHyperParameterTuningJobs :: Core.NonEmpty Types.ParentHyperParameterTuningJob
    -- ^ An array of hyperparameter tuning jobs that are used as the starting point for the new hyperparameter tuning job. For more information about warm starting a hyperparameter tuning job, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point> .
--
-- Hyperparameter tuning jobs created before October 1, 2018 cannot be used as parent jobs for warm start tuning jobs.
  , warmStartType :: Types.HyperParameterTuningJobWarmStartType
    -- ^ Specifies one of the following:
--
--
--     * IDENTICAL_DATA_AND_ALGORITHM
--
--     * The new hyperparameter tuning job uses the same input data and training image as the parent tuning jobs. You can change the hyperparameter ranges to search and the maximum number of training jobs that the hyperparameter tuning job launches. You cannot use a new version of the training algorithm, unless the changes in the new version do not affect the algorithm itself. For example, changes that improve logging or adding support for a different data format are allowed. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.
--
--
--     * TRANSFER_LEARNING
--
--     * The new hyperparameter tuning job can include input data, hyperparameter ranges, maximum number of concurrent training jobs, and maximum number of training jobs that are different than those of its parent hyperparameter tuning jobs. The training image can also be a different version from the version used in the parent hyperparameter tuning job. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HyperParameterTuningJobWarmStartConfig' value with any optional fields omitted.
mkHyperParameterTuningJobWarmStartConfig
    :: Core.NonEmpty Types.ParentHyperParameterTuningJob -- ^ 'parentHyperParameterTuningJobs'
    -> Types.HyperParameterTuningJobWarmStartType -- ^ 'warmStartType'
    -> HyperParameterTuningJobWarmStartConfig
mkHyperParameterTuningJobWarmStartConfig
  parentHyperParameterTuningJobs warmStartType
  = HyperParameterTuningJobWarmStartConfig'{parentHyperParameterTuningJobs,
                                            warmStartType}

-- | An array of hyperparameter tuning jobs that are used as the starting point for the new hyperparameter tuning job. For more information about warm starting a hyperparameter tuning job, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point> .
--
-- Hyperparameter tuning jobs created before October 1, 2018 cannot be used as parent jobs for warm start tuning jobs.
--
-- /Note:/ Consider using 'parentHyperParameterTuningJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjwscParentHyperParameterTuningJobs :: Lens.Lens' HyperParameterTuningJobWarmStartConfig (Core.NonEmpty Types.ParentHyperParameterTuningJob)
hptjwscParentHyperParameterTuningJobs = Lens.field @"parentHyperParameterTuningJobs"
{-# INLINEABLE hptjwscParentHyperParameterTuningJobs #-}
{-# DEPRECATED parentHyperParameterTuningJobs "Use generic-lens or generic-optics with 'parentHyperParameterTuningJobs' instead"  #-}

-- | Specifies one of the following:
--
--
--     * IDENTICAL_DATA_AND_ALGORITHM
--
--     * The new hyperparameter tuning job uses the same input data and training image as the parent tuning jobs. You can change the hyperparameter ranges to search and the maximum number of training jobs that the hyperparameter tuning job launches. You cannot use a new version of the training algorithm, unless the changes in the new version do not affect the algorithm itself. For example, changes that improve logging or adding support for a different data format are allowed. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.
--
--
--     * TRANSFER_LEARNING
--
--     * The new hyperparameter tuning job can include input data, hyperparameter ranges, maximum number of concurrent training jobs, and maximum number of training jobs that are different than those of its parent hyperparameter tuning jobs. The training image can also be a different version from the version used in the parent hyperparameter tuning job. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.
--
--
--
-- /Note:/ Consider using 'warmStartType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjwscWarmStartType :: Lens.Lens' HyperParameterTuningJobWarmStartConfig Types.HyperParameterTuningJobWarmStartType
hptjwscWarmStartType = Lens.field @"warmStartType"
{-# INLINEABLE hptjwscWarmStartType #-}
{-# DEPRECATED warmStartType "Use generic-lens or generic-optics with 'warmStartType' instead"  #-}

instance Core.FromJSON HyperParameterTuningJobWarmStartConfig where
        toJSON HyperParameterTuningJobWarmStartConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ParentHyperParameterTuningJobs" Core..=
                       parentHyperParameterTuningJobs),
                  Core.Just ("WarmStartType" Core..= warmStartType)])

instance Core.FromJSON HyperParameterTuningJobWarmStartConfig where
        parseJSON
          = Core.withObject "HyperParameterTuningJobWarmStartConfig" Core.$
              \ x ->
                HyperParameterTuningJobWarmStartConfig' Core.<$>
                  (x Core..: "ParentHyperParameterTuningJobs") Core.<*>
                    x Core..: "WarmStartType"
