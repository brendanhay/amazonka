{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
  ( HyperParameterTuningJobWarmStartConfig (..),

    -- * Smart constructor
    mkHyperParameterTuningJobWarmStartConfig,

    -- * Lenses
    hptjwscWarmStartType,
    hptjwscParentHyperParameterTuningJobs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType
import Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob

-- | Specifies the configuration for a hyperparameter tuning job that uses one or more previous hyperparameter tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
-- All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric, and the training job that performs the best is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
--
-- /See:/ 'mkHyperParameterTuningJobWarmStartConfig' smart constructor.
data HyperParameterTuningJobWarmStartConfig = HyperParameterTuningJobWarmStartConfig'
  { -- | Specifies one of the following:
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
    warmStartType :: HyperParameterTuningJobWarmStartType,
    -- | An array of hyperparameter tuning jobs that are used as the starting point for the new hyperparameter tuning job. For more information about warm starting a hyperparameter tuning job, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point> .
    --
    -- Hyperparameter tuning jobs created before October 1, 2018 cannot be used as parent jobs for warm start tuning jobs.
    parentHyperParameterTuningJobs :: Lude.NonEmpty ParentHyperParameterTuningJob
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HyperParameterTuningJobWarmStartConfig' with the minimum fields required to make a request.
--
-- * 'warmStartType' - Specifies one of the following:
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
-- * 'parentHyperParameterTuningJobs' - An array of hyperparameter tuning jobs that are used as the starting point for the new hyperparameter tuning job. For more information about warm starting a hyperparameter tuning job, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point> .
--
-- Hyperparameter tuning jobs created before October 1, 2018 cannot be used as parent jobs for warm start tuning jobs.
mkHyperParameterTuningJobWarmStartConfig ::
  -- | 'warmStartType'
  HyperParameterTuningJobWarmStartType ->
  -- | 'parentHyperParameterTuningJobs'
  Lude.NonEmpty ParentHyperParameterTuningJob ->
  HyperParameterTuningJobWarmStartConfig
mkHyperParameterTuningJobWarmStartConfig
  pWarmStartType_
  pParentHyperParameterTuningJobs_ =
    HyperParameterTuningJobWarmStartConfig'
      { warmStartType =
          pWarmStartType_,
        parentHyperParameterTuningJobs =
          pParentHyperParameterTuningJobs_
      }

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
hptjwscWarmStartType :: Lens.Lens' HyperParameterTuningJobWarmStartConfig HyperParameterTuningJobWarmStartType
hptjwscWarmStartType = Lens.lens (warmStartType :: HyperParameterTuningJobWarmStartConfig -> HyperParameterTuningJobWarmStartType) (\s a -> s {warmStartType = a} :: HyperParameterTuningJobWarmStartConfig)
{-# DEPRECATED hptjwscWarmStartType "Use generic-lens or generic-optics with 'warmStartType' instead." #-}

-- | An array of hyperparameter tuning jobs that are used as the starting point for the new hyperparameter tuning job. For more information about warm starting a hyperparameter tuning job, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point> .
--
-- Hyperparameter tuning jobs created before October 1, 2018 cannot be used as parent jobs for warm start tuning jobs.
--
-- /Note:/ Consider using 'parentHyperParameterTuningJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjwscParentHyperParameterTuningJobs :: Lens.Lens' HyperParameterTuningJobWarmStartConfig (Lude.NonEmpty ParentHyperParameterTuningJob)
hptjwscParentHyperParameterTuningJobs = Lens.lens (parentHyperParameterTuningJobs :: HyperParameterTuningJobWarmStartConfig -> Lude.NonEmpty ParentHyperParameterTuningJob) (\s a -> s {parentHyperParameterTuningJobs = a} :: HyperParameterTuningJobWarmStartConfig)
{-# DEPRECATED hptjwscParentHyperParameterTuningJobs "Use generic-lens or generic-optics with 'parentHyperParameterTuningJobs' instead." #-}

instance Lude.FromJSON HyperParameterTuningJobWarmStartConfig where
  parseJSON =
    Lude.withObject
      "HyperParameterTuningJobWarmStartConfig"
      ( \x ->
          HyperParameterTuningJobWarmStartConfig'
            Lude.<$> (x Lude..: "WarmStartType")
            Lude.<*> (x Lude..: "ParentHyperParameterTuningJobs")
      )

instance Lude.ToJSON HyperParameterTuningJobWarmStartConfig where
  toJSON HyperParameterTuningJobWarmStartConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WarmStartType" Lude..= warmStartType),
            Lude.Just
              ( "ParentHyperParameterTuningJobs"
                  Lude..= parentHyperParameterTuningJobs
              )
          ]
      )
