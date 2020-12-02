{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTrainingJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTrainingJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
import Network.AWS.SageMaker.Types.ObjectiveStatus
import Network.AWS.SageMaker.Types.TrainingJobStatus

-- | Specifies summary information about a training job.
--
--
--
-- /See:/ 'hyperParameterTrainingJobSummary' smart constructor.
data HyperParameterTrainingJobSummary = HyperParameterTrainingJobSummary'
  { _hptjsFailureReason ::
      !(Maybe Text),
    _hptjsTuningJobName ::
      !(Maybe Text),
    _hptjsTrainingEndTime ::
      !(Maybe POSIX),
    _hptjsObjectiveStatus ::
      !(Maybe ObjectiveStatus),
    _hptjsTrainingJobDefinitionName ::
      !(Maybe Text),
    _hptjsTrainingStartTime ::
      !(Maybe POSIX),
    _hptjsFinalHyperParameterTuningJobObjectiveMetric ::
      !( Maybe
           FinalHyperParameterTuningJobObjectiveMetric
       ),
    _hptjsTrainingJobName ::
      !Text,
    _hptjsTrainingJobARN ::
      !Text,
    _hptjsCreationTime ::
      !POSIX,
    _hptjsTrainingJobStatus ::
      !TrainingJobStatus,
    _hptjsTunedHyperParameters ::
      !(Map Text (Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HyperParameterTrainingJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjsFailureReason' - The reason that the training job failed.
--
-- * 'hptjsTuningJobName' - The HyperParameter tuning job that launched the training job.
--
-- * 'hptjsTrainingEndTime' - Specifies the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- * 'hptjsObjectiveStatus' - The status of the objective metric for the training job:     * Succeeded: The final objective metric for the training job was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.     * Pending: The training job is in progress and evaluation of its final objective metric is pending.     * Failed: The final objective metric for the training job was not evaluated, and was not used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
--
-- * 'hptjsTrainingJobDefinitionName' - The training job definition name.
--
-- * 'hptjsTrainingStartTime' - The date and time that the training job started.
--
-- * 'hptjsFinalHyperParameterTuningJobObjectiveMetric' - The 'FinalHyperParameterTuningJobObjectiveMetric' object that specifies the value of the objective metric of the tuning job that launched this training job.
--
-- * 'hptjsTrainingJobName' - The name of the training job.
--
-- * 'hptjsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'hptjsCreationTime' - The date and time that the training job was created.
--
-- * 'hptjsTrainingJobStatus' - The status of the training job.
--
-- * 'hptjsTunedHyperParameters' - A list of the hyperparameters for which you specified ranges to search.
hyperParameterTrainingJobSummary ::
  -- | 'hptjsTrainingJobName'
  Text ->
  -- | 'hptjsTrainingJobARN'
  Text ->
  -- | 'hptjsCreationTime'
  UTCTime ->
  -- | 'hptjsTrainingJobStatus'
  TrainingJobStatus ->
  HyperParameterTrainingJobSummary
hyperParameterTrainingJobSummary
  pTrainingJobName_
  pTrainingJobARN_
  pCreationTime_
  pTrainingJobStatus_ =
    HyperParameterTrainingJobSummary'
      { _hptjsFailureReason = Nothing,
        _hptjsTuningJobName = Nothing,
        _hptjsTrainingEndTime = Nothing,
        _hptjsObjectiveStatus = Nothing,
        _hptjsTrainingJobDefinitionName = Nothing,
        _hptjsTrainingStartTime = Nothing,
        _hptjsFinalHyperParameterTuningJobObjectiveMetric = Nothing,
        _hptjsTrainingJobName = pTrainingJobName_,
        _hptjsTrainingJobARN = pTrainingJobARN_,
        _hptjsCreationTime = _Time # pCreationTime_,
        _hptjsTrainingJobStatus = pTrainingJobStatus_,
        _hptjsTunedHyperParameters = mempty
      }

-- | The reason that the training job failed.
hptjsFailureReason :: Lens' HyperParameterTrainingJobSummary (Maybe Text)
hptjsFailureReason = lens _hptjsFailureReason (\s a -> s {_hptjsFailureReason = a})

-- | The HyperParameter tuning job that launched the training job.
hptjsTuningJobName :: Lens' HyperParameterTrainingJobSummary (Maybe Text)
hptjsTuningJobName = lens _hptjsTuningJobName (\s a -> s {_hptjsTuningJobName = a})

-- | Specifies the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
hptjsTrainingEndTime :: Lens' HyperParameterTrainingJobSummary (Maybe UTCTime)
hptjsTrainingEndTime = lens _hptjsTrainingEndTime (\s a -> s {_hptjsTrainingEndTime = a}) . mapping _Time

-- | The status of the objective metric for the training job:     * Succeeded: The final objective metric for the training job was evaluated by the hyperparameter tuning job and used in the hyperparameter tuning process.     * Pending: The training job is in progress and evaluation of its final objective metric is pending.     * Failed: The final objective metric for the training job was not evaluated, and was not used in the hyperparameter tuning process. This typically occurs when the training job failed or did not emit an objective metric.
hptjsObjectiveStatus :: Lens' HyperParameterTrainingJobSummary (Maybe ObjectiveStatus)
hptjsObjectiveStatus = lens _hptjsObjectiveStatus (\s a -> s {_hptjsObjectiveStatus = a})

-- | The training job definition name.
hptjsTrainingJobDefinitionName :: Lens' HyperParameterTrainingJobSummary (Maybe Text)
hptjsTrainingJobDefinitionName = lens _hptjsTrainingJobDefinitionName (\s a -> s {_hptjsTrainingJobDefinitionName = a})

-- | The date and time that the training job started.
hptjsTrainingStartTime :: Lens' HyperParameterTrainingJobSummary (Maybe UTCTime)
hptjsTrainingStartTime = lens _hptjsTrainingStartTime (\s a -> s {_hptjsTrainingStartTime = a}) . mapping _Time

-- | The 'FinalHyperParameterTuningJobObjectiveMetric' object that specifies the value of the objective metric of the tuning job that launched this training job.
hptjsFinalHyperParameterTuningJobObjectiveMetric :: Lens' HyperParameterTrainingJobSummary (Maybe FinalHyperParameterTuningJobObjectiveMetric)
hptjsFinalHyperParameterTuningJobObjectiveMetric = lens _hptjsFinalHyperParameterTuningJobObjectiveMetric (\s a -> s {_hptjsFinalHyperParameterTuningJobObjectiveMetric = a})

-- | The name of the training job.
hptjsTrainingJobName :: Lens' HyperParameterTrainingJobSummary Text
hptjsTrainingJobName = lens _hptjsTrainingJobName (\s a -> s {_hptjsTrainingJobName = a})

-- | The Amazon Resource Name (ARN) of the training job.
hptjsTrainingJobARN :: Lens' HyperParameterTrainingJobSummary Text
hptjsTrainingJobARN = lens _hptjsTrainingJobARN (\s a -> s {_hptjsTrainingJobARN = a})

-- | The date and time that the training job was created.
hptjsCreationTime :: Lens' HyperParameterTrainingJobSummary UTCTime
hptjsCreationTime = lens _hptjsCreationTime (\s a -> s {_hptjsCreationTime = a}) . _Time

-- | The status of the training job.
hptjsTrainingJobStatus :: Lens' HyperParameterTrainingJobSummary TrainingJobStatus
hptjsTrainingJobStatus = lens _hptjsTrainingJobStatus (\s a -> s {_hptjsTrainingJobStatus = a})

-- | A list of the hyperparameters for which you specified ranges to search.
hptjsTunedHyperParameters :: Lens' HyperParameterTrainingJobSummary (HashMap Text (Text))
hptjsTunedHyperParameters = lens _hptjsTunedHyperParameters (\s a -> s {_hptjsTunedHyperParameters = a}) . _Map

instance FromJSON HyperParameterTrainingJobSummary where
  parseJSON =
    withObject
      "HyperParameterTrainingJobSummary"
      ( \x ->
          HyperParameterTrainingJobSummary'
            <$> (x .:? "FailureReason")
            <*> (x .:? "TuningJobName")
            <*> (x .:? "TrainingEndTime")
            <*> (x .:? "ObjectiveStatus")
            <*> (x .:? "TrainingJobDefinitionName")
            <*> (x .:? "TrainingStartTime")
            <*> (x .:? "FinalHyperParameterTuningJobObjectiveMetric")
            <*> (x .: "TrainingJobName")
            <*> (x .: "TrainingJobArn")
            <*> (x .: "CreationTime")
            <*> (x .: "TrainingJobStatus")
            <*> (x .:? "TunedHyperParameters" .!= mempty)
      )

instance Hashable HyperParameterTrainingJobSummary

instance NFData HyperParameterTrainingJobSummary
