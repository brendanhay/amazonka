{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
import Network.AWS.SageMaker.Types.ParameterRanges
import Network.AWS.SageMaker.Types.ResourceLimits
import Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType
import Network.AWS.SageMaker.Types.TuningJobCompletionCriteria

-- | Configures a hyperparameter tuning job.
--
--
--
-- /See:/ 'hyperParameterTuningJobConfig' smart constructor.
data HyperParameterTuningJobConfig = HyperParameterTuningJobConfig'
  { _hptjcTuningJobCompletionCriteria ::
      !( Maybe
           TuningJobCompletionCriteria
       ),
    _hptjcParameterRanges ::
      !(Maybe ParameterRanges),
    _hptjcHyperParameterTuningJobObjective ::
      !( Maybe
           HyperParameterTuningJobObjective
       ),
    _hptjcTrainingJobEarlyStoppingType ::
      !( Maybe
           TrainingJobEarlyStoppingType
       ),
    _hptjcStrategy ::
      !HyperParameterTuningJobStrategyType,
    _hptjcResourceLimits ::
      !ResourceLimits
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HyperParameterTuningJobConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjcTuningJobCompletionCriteria' - The tuning job's completion criteria.
--
-- * 'hptjcParameterRanges' - The 'ParameterRanges' object that specifies the ranges of hyperparameters that this tuning job searches.
--
-- * 'hptjcHyperParameterTuningJobObjective' - The 'HyperParameterTuningJobObjective' object that specifies the objective metric for this tuning job.
--
-- * 'hptjcTrainingJobEarlyStoppingType' - Specifies whether to use early stopping for training jobs launched by the hyperparameter tuning job. This can be one of the following values (the default value is @OFF@ ):     * OFF    * Training jobs launched by the hyperparameter tuning job do not use early stopping.     * AUTO    * Amazon SageMaker stops training jobs launched by the hyperparameter tuning job when they are unlikely to perform better than previously completed training jobs. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early> .
--
-- * 'hptjcStrategy' - Specifies how hyperparameter tuning chooses the combinations of hyperparameter values to use for the training job it launches. To use the Bayesian search strategy, set this to @Bayesian@ . To randomly search, set it to @Random@ . For information about search strategies, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
--
-- * 'hptjcResourceLimits' - The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs for this tuning job.
hyperParameterTuningJobConfig ::
  -- | 'hptjcStrategy'
  HyperParameterTuningJobStrategyType ->
  -- | 'hptjcResourceLimits'
  ResourceLimits ->
  HyperParameterTuningJobConfig
hyperParameterTuningJobConfig pStrategy_ pResourceLimits_ =
  HyperParameterTuningJobConfig'
    { _hptjcTuningJobCompletionCriteria =
        Nothing,
      _hptjcParameterRanges = Nothing,
      _hptjcHyperParameterTuningJobObjective = Nothing,
      _hptjcTrainingJobEarlyStoppingType = Nothing,
      _hptjcStrategy = pStrategy_,
      _hptjcResourceLimits = pResourceLimits_
    }

-- | The tuning job's completion criteria.
hptjcTuningJobCompletionCriteria :: Lens' HyperParameterTuningJobConfig (Maybe TuningJobCompletionCriteria)
hptjcTuningJobCompletionCriteria = lens _hptjcTuningJobCompletionCriteria (\s a -> s {_hptjcTuningJobCompletionCriteria = a})

-- | The 'ParameterRanges' object that specifies the ranges of hyperparameters that this tuning job searches.
hptjcParameterRanges :: Lens' HyperParameterTuningJobConfig (Maybe ParameterRanges)
hptjcParameterRanges = lens _hptjcParameterRanges (\s a -> s {_hptjcParameterRanges = a})

-- | The 'HyperParameterTuningJobObjective' object that specifies the objective metric for this tuning job.
hptjcHyperParameterTuningJobObjective :: Lens' HyperParameterTuningJobConfig (Maybe HyperParameterTuningJobObjective)
hptjcHyperParameterTuningJobObjective = lens _hptjcHyperParameterTuningJobObjective (\s a -> s {_hptjcHyperParameterTuningJobObjective = a})

-- | Specifies whether to use early stopping for training jobs launched by the hyperparameter tuning job. This can be one of the following values (the default value is @OFF@ ):     * OFF    * Training jobs launched by the hyperparameter tuning job do not use early stopping.     * AUTO    * Amazon SageMaker stops training jobs launched by the hyperparameter tuning job when they are unlikely to perform better than previously completed training jobs. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early> .
hptjcTrainingJobEarlyStoppingType :: Lens' HyperParameterTuningJobConfig (Maybe TrainingJobEarlyStoppingType)
hptjcTrainingJobEarlyStoppingType = lens _hptjcTrainingJobEarlyStoppingType (\s a -> s {_hptjcTrainingJobEarlyStoppingType = a})

-- | Specifies how hyperparameter tuning chooses the combinations of hyperparameter values to use for the training job it launches. To use the Bayesian search strategy, set this to @Bayesian@ . To randomly search, set it to @Random@ . For information about search strategies, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
hptjcStrategy :: Lens' HyperParameterTuningJobConfig HyperParameterTuningJobStrategyType
hptjcStrategy = lens _hptjcStrategy (\s a -> s {_hptjcStrategy = a})

-- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs for this tuning job.
hptjcResourceLimits :: Lens' HyperParameterTuningJobConfig ResourceLimits
hptjcResourceLimits = lens _hptjcResourceLimits (\s a -> s {_hptjcResourceLimits = a})

instance FromJSON HyperParameterTuningJobConfig where
  parseJSON =
    withObject
      "HyperParameterTuningJobConfig"
      ( \x ->
          HyperParameterTuningJobConfig'
            <$> (x .:? "TuningJobCompletionCriteria")
            <*> (x .:? "ParameterRanges")
            <*> (x .:? "HyperParameterTuningJobObjective")
            <*> (x .:? "TrainingJobEarlyStoppingType")
            <*> (x .: "Strategy")
            <*> (x .: "ResourceLimits")
      )

instance Hashable HyperParameterTuningJobConfig

instance NFData HyperParameterTuningJobConfig

instance ToJSON HyperParameterTuningJobConfig where
  toJSON HyperParameterTuningJobConfig' {..} =
    object
      ( catMaybes
          [ ("TuningJobCompletionCriteria" .=)
              <$> _hptjcTuningJobCompletionCriteria,
            ("ParameterRanges" .=) <$> _hptjcParameterRanges,
            ("HyperParameterTuningJobObjective" .=)
              <$> _hptjcHyperParameterTuningJobObjective,
            ("TrainingJobEarlyStoppingType" .=)
              <$> _hptjcTrainingJobEarlyStoppingType,
            Just ("Strategy" .= _hptjcStrategy),
            Just ("ResourceLimits" .= _hptjcResourceLimits)
          ]
      )
