{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
import Network.AWS.SageMaker.Types.ObjectiveStatusCounters
import Network.AWS.SageMaker.Types.ResourceLimits
import Network.AWS.SageMaker.Types.TrainingJobStatusCounters

-- | Provides summary information about a hyperparameter tuning job.
--
--
--
-- /See:/ 'hyperParameterTuningJobSummary' smart constructor.
data HyperParameterTuningJobSummary = HyperParameterTuningJobSummary'
  { _hResourceLimits ::
      !(Maybe ResourceLimits),
    _hLastModifiedTime ::
      !(Maybe POSIX),
    _hHyperParameterTuningEndTime ::
      !(Maybe POSIX),
    _hHyperParameterTuningJobName ::
      !Text,
    _hHyperParameterTuningJobARN ::
      !Text,
    _hHyperParameterTuningJobStatus ::
      !HyperParameterTuningJobStatus,
    _hStrategy ::
      !HyperParameterTuningJobStrategyType,
    _hCreationTime :: !POSIX,
    _hTrainingJobStatusCounters ::
      !TrainingJobStatusCounters,
    _hObjectiveStatusCounters ::
      !ObjectiveStatusCounters
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HyperParameterTuningJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hResourceLimits' - The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
--
-- * 'hLastModifiedTime' - The date and time that the tuning job was modified.
--
-- * 'hHyperParameterTuningEndTime' - The date and time that the tuning job ended.
--
-- * 'hHyperParameterTuningJobName' - The name of the tuning job.
--
-- * 'hHyperParameterTuningJobARN' - The Amazon Resource Name (ARN) of the tuning job.
--
-- * 'hHyperParameterTuningJobStatus' - The status of the tuning job.
--
-- * 'hStrategy' - Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
--
-- * 'hCreationTime' - The date and time that the tuning job was created.
--
-- * 'hTrainingJobStatusCounters' - The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
--
-- * 'hObjectiveStatusCounters' - The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
hyperParameterTuningJobSummary ::
  -- | 'hHyperParameterTuningJobName'
  Text ->
  -- | 'hHyperParameterTuningJobARN'
  Text ->
  -- | 'hHyperParameterTuningJobStatus'
  HyperParameterTuningJobStatus ->
  -- | 'hStrategy'
  HyperParameterTuningJobStrategyType ->
  -- | 'hCreationTime'
  UTCTime ->
  -- | 'hTrainingJobStatusCounters'
  TrainingJobStatusCounters ->
  -- | 'hObjectiveStatusCounters'
  ObjectiveStatusCounters ->
  HyperParameterTuningJobSummary
hyperParameterTuningJobSummary
  pHyperParameterTuningJobName_
  pHyperParameterTuningJobARN_
  pHyperParameterTuningJobStatus_
  pStrategy_
  pCreationTime_
  pTrainingJobStatusCounters_
  pObjectiveStatusCounters_ =
    HyperParameterTuningJobSummary'
      { _hResourceLimits = Nothing,
        _hLastModifiedTime = Nothing,
        _hHyperParameterTuningEndTime = Nothing,
        _hHyperParameterTuningJobName = pHyperParameterTuningJobName_,
        _hHyperParameterTuningJobARN = pHyperParameterTuningJobARN_,
        _hHyperParameterTuningJobStatus =
          pHyperParameterTuningJobStatus_,
        _hStrategy = pStrategy_,
        _hCreationTime = _Time # pCreationTime_,
        _hTrainingJobStatusCounters = pTrainingJobStatusCounters_,
        _hObjectiveStatusCounters = pObjectiveStatusCounters_
      }

-- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs allowed for this tuning job.
hResourceLimits :: Lens' HyperParameterTuningJobSummary (Maybe ResourceLimits)
hResourceLimits = lens _hResourceLimits (\s a -> s {_hResourceLimits = a})

-- | The date and time that the tuning job was modified.
hLastModifiedTime :: Lens' HyperParameterTuningJobSummary (Maybe UTCTime)
hLastModifiedTime = lens _hLastModifiedTime (\s a -> s {_hLastModifiedTime = a}) . mapping _Time

-- | The date and time that the tuning job ended.
hHyperParameterTuningEndTime :: Lens' HyperParameterTuningJobSummary (Maybe UTCTime)
hHyperParameterTuningEndTime = lens _hHyperParameterTuningEndTime (\s a -> s {_hHyperParameterTuningEndTime = a}) . mapping _Time

-- | The name of the tuning job.
hHyperParameterTuningJobName :: Lens' HyperParameterTuningJobSummary Text
hHyperParameterTuningJobName = lens _hHyperParameterTuningJobName (\s a -> s {_hHyperParameterTuningJobName = a})

-- | The Amazon Resource Name (ARN) of the tuning job.
hHyperParameterTuningJobARN :: Lens' HyperParameterTuningJobSummary Text
hHyperParameterTuningJobARN = lens _hHyperParameterTuningJobARN (\s a -> s {_hHyperParameterTuningJobARN = a})

-- | The status of the tuning job.
hHyperParameterTuningJobStatus :: Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStatus
hHyperParameterTuningJobStatus = lens _hHyperParameterTuningJobStatus (\s a -> s {_hHyperParameterTuningJobStatus = a})

-- | Specifies the search strategy hyperparameter tuning uses to choose which hyperparameters to use for each iteration. Currently, the only valid value is Bayesian.
hStrategy :: Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStrategyType
hStrategy = lens _hStrategy (\s a -> s {_hStrategy = a})

-- | The date and time that the tuning job was created.
hCreationTime :: Lens' HyperParameterTuningJobSummary UTCTime
hCreationTime = lens _hCreationTime (\s a -> s {_hCreationTime = a}) . _Time

-- | The 'TrainingJobStatusCounters' object that specifies the numbers of training jobs, categorized by status, that this tuning job launched.
hTrainingJobStatusCounters :: Lens' HyperParameterTuningJobSummary TrainingJobStatusCounters
hTrainingJobStatusCounters = lens _hTrainingJobStatusCounters (\s a -> s {_hTrainingJobStatusCounters = a})

-- | The 'ObjectiveStatusCounters' object that specifies the numbers of training jobs, categorized by objective metric status, that this tuning job launched.
hObjectiveStatusCounters :: Lens' HyperParameterTuningJobSummary ObjectiveStatusCounters
hObjectiveStatusCounters = lens _hObjectiveStatusCounters (\s a -> s {_hObjectiveStatusCounters = a})

instance FromJSON HyperParameterTuningJobSummary where
  parseJSON =
    withObject
      "HyperParameterTuningJobSummary"
      ( \x ->
          HyperParameterTuningJobSummary'
            <$> (x .:? "ResourceLimits")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "HyperParameterTuningEndTime")
            <*> (x .: "HyperParameterTuningJobName")
            <*> (x .: "HyperParameterTuningJobArn")
            <*> (x .: "HyperParameterTuningJobStatus")
            <*> (x .: "Strategy")
            <*> (x .: "CreationTime")
            <*> (x .: "TrainingJobStatusCounters")
            <*> (x .: "ObjectiveStatusCounters")
      )

instance Hashable HyperParameterTuningJobSummary

instance NFData HyperParameterTuningJobSummary
