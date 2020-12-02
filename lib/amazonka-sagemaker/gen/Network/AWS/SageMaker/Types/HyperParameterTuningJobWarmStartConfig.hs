{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType
import Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob

-- | Specifies the configuration for a hyperparameter tuning job that uses one or more previous hyperparameter tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
--
-- All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric, and the training job that performs the best is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
--
--
-- /See:/ 'hyperParameterTuningJobWarmStartConfig' smart constructor.
data HyperParameterTuningJobWarmStartConfig = HyperParameterTuningJobWarmStartConfig'
  { _hptjwscParentHyperParameterTuningJobs ::
      !( List1
           ParentHyperParameterTuningJob
       ),
    _hptjwscWarmStartType ::
      !HyperParameterTuningJobWarmStartType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HyperParameterTuningJobWarmStartConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjwscParentHyperParameterTuningJobs' - An array of hyperparameter tuning jobs that are used as the starting point for the new hyperparameter tuning job. For more information about warm starting a hyperparameter tuning job, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point> . Hyperparameter tuning jobs created before October 1, 2018 cannot be used as parent jobs for warm start tuning jobs.
--
-- * 'hptjwscWarmStartType' - Specifies one of the following:     * IDENTICAL_DATA_AND_ALGORITHM    * The new hyperparameter tuning job uses the same input data and training image as the parent tuning jobs. You can change the hyperparameter ranges to search and the maximum number of training jobs that the hyperparameter tuning job launches. You cannot use a new version of the training algorithm, unless the changes in the new version do not affect the algorithm itself. For example, changes that improve logging or adding support for a different data format are allowed. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.     * TRANSFER_LEARNING    * The new hyperparameter tuning job can include input data, hyperparameter ranges, maximum number of concurrent training jobs, and maximum number of training jobs that are different than those of its parent hyperparameter tuning jobs. The training image can also be a different version from the version used in the parent hyperparameter tuning job. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.
hyperParameterTuningJobWarmStartConfig ::
  -- | 'hptjwscParentHyperParameterTuningJobs'
  NonEmpty ParentHyperParameterTuningJob ->
  -- | 'hptjwscWarmStartType'
  HyperParameterTuningJobWarmStartType ->
  HyperParameterTuningJobWarmStartConfig
hyperParameterTuningJobWarmStartConfig
  pParentHyperParameterTuningJobs_
  pWarmStartType_ =
    HyperParameterTuningJobWarmStartConfig'
      { _hptjwscParentHyperParameterTuningJobs =
          _List1 # pParentHyperParameterTuningJobs_,
        _hptjwscWarmStartType = pWarmStartType_
      }

-- | An array of hyperparameter tuning jobs that are used as the starting point for the new hyperparameter tuning job. For more information about warm starting a hyperparameter tuning job, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point> . Hyperparameter tuning jobs created before October 1, 2018 cannot be used as parent jobs for warm start tuning jobs.
hptjwscParentHyperParameterTuningJobs :: Lens' HyperParameterTuningJobWarmStartConfig (NonEmpty ParentHyperParameterTuningJob)
hptjwscParentHyperParameterTuningJobs = lens _hptjwscParentHyperParameterTuningJobs (\s a -> s {_hptjwscParentHyperParameterTuningJobs = a}) . _List1

-- | Specifies one of the following:     * IDENTICAL_DATA_AND_ALGORITHM    * The new hyperparameter tuning job uses the same input data and training image as the parent tuning jobs. You can change the hyperparameter ranges to search and the maximum number of training jobs that the hyperparameter tuning job launches. You cannot use a new version of the training algorithm, unless the changes in the new version do not affect the algorithm itself. For example, changes that improve logging or adding support for a different data format are allowed. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.     * TRANSFER_LEARNING    * The new hyperparameter tuning job can include input data, hyperparameter ranges, maximum number of concurrent training jobs, and maximum number of training jobs that are different than those of its parent hyperparameter tuning jobs. The training image can also be a different version from the version used in the parent hyperparameter tuning job. You can also change hyperparameters from tunable to static, and from static to tunable, but the total number of static plus tunable hyperparameters must remain the same as it is in all parent jobs. The objective metric for the new tuning job must be the same as for all parent jobs.
hptjwscWarmStartType :: Lens' HyperParameterTuningJobWarmStartConfig HyperParameterTuningJobWarmStartType
hptjwscWarmStartType = lens _hptjwscWarmStartType (\s a -> s {_hptjwscWarmStartType = a})

instance FromJSON HyperParameterTuningJobWarmStartConfig where
  parseJSON =
    withObject
      "HyperParameterTuningJobWarmStartConfig"
      ( \x ->
          HyperParameterTuningJobWarmStartConfig'
            <$> (x .: "ParentHyperParameterTuningJobs") <*> (x .: "WarmStartType")
      )

instance Hashable HyperParameterTuningJobWarmStartConfig

instance NFData HyperParameterTuningJobWarmStartConfig

instance ToJSON HyperParameterTuningJobWarmStartConfig where
  toJSON HyperParameterTuningJobWarmStartConfig' {..} =
    object
      ( catMaybes
          [ Just
              ( "ParentHyperParameterTuningJobs"
                  .= _hptjwscParentHyperParameterTuningJobs
              ),
            Just ("WarmStartType" .= _hptjwscWarmStartType)
          ]
      )
