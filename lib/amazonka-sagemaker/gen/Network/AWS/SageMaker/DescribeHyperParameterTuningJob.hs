{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description of a hyperparameter tuning job.
module Network.AWS.SageMaker.DescribeHyperParameterTuningJob
  ( -- * Creating a Request
    describeHyperParameterTuningJob,
    DescribeHyperParameterTuningJob,

    -- * Request Lenses
    dhptjHyperParameterTuningJobName,

    -- * Destructuring the Response
    describeHyperParameterTuningJobResponse,
    DescribeHyperParameterTuningJobResponse,

    -- * Response Lenses
    dhptjrsFailureReason,
    dhptjrsTrainingJobDefinition,
    dhptjrsLastModifiedTime,
    dhptjrsBestTrainingJob,
    dhptjrsHyperParameterTuningEndTime,
    dhptjrsOverallBestTrainingJob,
    dhptjrsWarmStartConfig,
    dhptjrsTrainingJobDefinitions,
    dhptjrsResponseStatus,
    dhptjrsHyperParameterTuningJobName,
    dhptjrsHyperParameterTuningJobARN,
    dhptjrsHyperParameterTuningJobConfig,
    dhptjrsHyperParameterTuningJobStatus,
    dhptjrsCreationTime,
    dhptjrsTrainingJobStatusCounters,
    dhptjrsObjectiveStatusCounters,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeHyperParameterTuningJob' smart constructor.
newtype DescribeHyperParameterTuningJob = DescribeHyperParameterTuningJob'
  { _dhptjHyperParameterTuningJobName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhptjHyperParameterTuningJobName' - The name of the tuning job.
describeHyperParameterTuningJob ::
  -- | 'dhptjHyperParameterTuningJobName'
  Text ->
  DescribeHyperParameterTuningJob
describeHyperParameterTuningJob pHyperParameterTuningJobName_ =
  DescribeHyperParameterTuningJob'
    { _dhptjHyperParameterTuningJobName =
        pHyperParameterTuningJobName_
    }

-- | The name of the tuning job.
dhptjHyperParameterTuningJobName :: Lens' DescribeHyperParameterTuningJob Text
dhptjHyperParameterTuningJobName = lens _dhptjHyperParameterTuningJobName (\s a -> s {_dhptjHyperParameterTuningJobName = a})

instance AWSRequest DescribeHyperParameterTuningJob where
  type
    Rs DescribeHyperParameterTuningJob =
      DescribeHyperParameterTuningJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeHyperParameterTuningJobResponse'
            <$> (x .?> "FailureReason")
            <*> (x .?> "TrainingJobDefinition")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "BestTrainingJob")
            <*> (x .?> "HyperParameterTuningEndTime")
            <*> (x .?> "OverallBestTrainingJob")
            <*> (x .?> "WarmStartConfig")
            <*> (x .?> "TrainingJobDefinitions")
            <*> (pure (fromEnum s))
            <*> (x .:> "HyperParameterTuningJobName")
            <*> (x .:> "HyperParameterTuningJobArn")
            <*> (x .:> "HyperParameterTuningJobConfig")
            <*> (x .:> "HyperParameterTuningJobStatus")
            <*> (x .:> "CreationTime")
            <*> (x .:> "TrainingJobStatusCounters")
            <*> (x .:> "ObjectiveStatusCounters")
      )

instance Hashable DescribeHyperParameterTuningJob

instance NFData DescribeHyperParameterTuningJob

instance ToHeaders DescribeHyperParameterTuningJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DescribeHyperParameterTuningJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeHyperParameterTuningJob where
  toJSON DescribeHyperParameterTuningJob' {..} =
    object
      ( catMaybes
          [ Just
              ( "HyperParameterTuningJobName"
                  .= _dhptjHyperParameterTuningJobName
              )
          ]
      )

instance ToPath DescribeHyperParameterTuningJob where
  toPath = const "/"

instance ToQuery DescribeHyperParameterTuningJob where
  toQuery = const mempty

-- | /See:/ 'describeHyperParameterTuningJobResponse' smart constructor.
data DescribeHyperParameterTuningJobResponse = DescribeHyperParameterTuningJobResponse'
  { _dhptjrsFailureReason ::
      !( Maybe
           Text
       ),
    _dhptjrsTrainingJobDefinition ::
      !( Maybe
           HyperParameterTrainingJobDefinition
       ),
    _dhptjrsLastModifiedTime ::
      !( Maybe
           POSIX
       ),
    _dhptjrsBestTrainingJob ::
      !( Maybe
           HyperParameterTrainingJobSummary
       ),
    _dhptjrsHyperParameterTuningEndTime ::
      !( Maybe
           POSIX
       ),
    _dhptjrsOverallBestTrainingJob ::
      !( Maybe
           HyperParameterTrainingJobSummary
       ),
    _dhptjrsWarmStartConfig ::
      !( Maybe
           HyperParameterTuningJobWarmStartConfig
       ),
    _dhptjrsTrainingJobDefinitions ::
      !( Maybe
           ( List1
               HyperParameterTrainingJobDefinition
           )
       ),
    _dhptjrsResponseStatus ::
      !Int,
    _dhptjrsHyperParameterTuningJobName ::
      !Text,
    _dhptjrsHyperParameterTuningJobARN ::
      !Text,
    _dhptjrsHyperParameterTuningJobConfig ::
      !HyperParameterTuningJobConfig,
    _dhptjrsHyperParameterTuningJobStatus ::
      !HyperParameterTuningJobStatus,
    _dhptjrsCreationTime ::
      !POSIX,
    _dhptjrsTrainingJobStatusCounters ::
      !TrainingJobStatusCounters,
    _dhptjrsObjectiveStatusCounters ::
      !ObjectiveStatusCounters
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeHyperParameterTuningJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhptjrsFailureReason' - If the tuning job failed, the reason it failed.
--
-- * 'dhptjrsTrainingJobDefinition' - The 'HyperParameterTrainingJobDefinition' object that specifies the definition of the training jobs that this tuning job launches.
--
-- * 'dhptjrsLastModifiedTime' - The date and time that the status of the tuning job was modified.
--
-- * 'dhptjrsBestTrainingJob' - A 'TrainingJobSummary' object that describes the training job that completed with the best current 'HyperParameterTuningJobObjective' .
--
-- * 'dhptjrsHyperParameterTuningEndTime' - The date and time that the tuning job ended.
--
-- * 'dhptjrsOverallBestTrainingJob' - If the hyperparameter tuning job is an warm start tuning job with a @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@ , this is the 'TrainingJobSummary' for the training job with the best objective metric value of all training jobs launched by this tuning job and all parent jobs specified for the warm start tuning job.
--
-- * 'dhptjrsWarmStartConfig' - The configuration for starting the hyperparameter parameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
--
-- * 'dhptjrsTrainingJobDefinitions' - A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
--
-- * 'dhptjrsResponseStatus' - -- | The response status code.
--
-- * 'dhptjrsHyperParameterTuningJobName' - The name of the tuning job.
--
-- * 'dhptjrsHyperParameterTuningJobARN' - The Amazon Resource Name (ARN) of the tuning job.
--
-- * 'dhptjrsHyperParameterTuningJobConfig' - The 'HyperParameterTuningJobConfig' object that specifies the configuration of the tuning job.
--
-- * 'dhptjrsHyperParameterTuningJobStatus' - The status of the tuning job: InProgress, Completed, Failed, Stopping, or Stopped.
--
-- * 'dhptjrsCreationTime' - The date and time that the tuning job started.
--
-- * 'dhptjrsTrainingJobStatusCounters' - The 'TrainingJobStatusCounters' object that specifies the number of training jobs, categorized by status, that this tuning job launched.
--
-- * 'dhptjrsObjectiveStatusCounters' - The 'ObjectiveStatusCounters' object that specifies the number of training jobs, categorized by the status of their final objective metric, that this tuning job launched.
describeHyperParameterTuningJobResponse ::
  -- | 'dhptjrsResponseStatus'
  Int ->
  -- | 'dhptjrsHyperParameterTuningJobName'
  Text ->
  -- | 'dhptjrsHyperParameterTuningJobARN'
  Text ->
  -- | 'dhptjrsHyperParameterTuningJobConfig'
  HyperParameterTuningJobConfig ->
  -- | 'dhptjrsHyperParameterTuningJobStatus'
  HyperParameterTuningJobStatus ->
  -- | 'dhptjrsCreationTime'
  UTCTime ->
  -- | 'dhptjrsTrainingJobStatusCounters'
  TrainingJobStatusCounters ->
  -- | 'dhptjrsObjectiveStatusCounters'
  ObjectiveStatusCounters ->
  DescribeHyperParameterTuningJobResponse
describeHyperParameterTuningJobResponse
  pResponseStatus_
  pHyperParameterTuningJobName_
  pHyperParameterTuningJobARN_
  pHyperParameterTuningJobConfig_
  pHyperParameterTuningJobStatus_
  pCreationTime_
  pTrainingJobStatusCounters_
  pObjectiveStatusCounters_ =
    DescribeHyperParameterTuningJobResponse'
      { _dhptjrsFailureReason =
          Nothing,
        _dhptjrsTrainingJobDefinition = Nothing,
        _dhptjrsLastModifiedTime = Nothing,
        _dhptjrsBestTrainingJob = Nothing,
        _dhptjrsHyperParameterTuningEndTime = Nothing,
        _dhptjrsOverallBestTrainingJob = Nothing,
        _dhptjrsWarmStartConfig = Nothing,
        _dhptjrsTrainingJobDefinitions = Nothing,
        _dhptjrsResponseStatus = pResponseStatus_,
        _dhptjrsHyperParameterTuningJobName =
          pHyperParameterTuningJobName_,
        _dhptjrsHyperParameterTuningJobARN =
          pHyperParameterTuningJobARN_,
        _dhptjrsHyperParameterTuningJobConfig =
          pHyperParameterTuningJobConfig_,
        _dhptjrsHyperParameterTuningJobStatus =
          pHyperParameterTuningJobStatus_,
        _dhptjrsCreationTime = _Time # pCreationTime_,
        _dhptjrsTrainingJobStatusCounters =
          pTrainingJobStatusCounters_,
        _dhptjrsObjectiveStatusCounters =
          pObjectiveStatusCounters_
      }

-- | If the tuning job failed, the reason it failed.
dhptjrsFailureReason :: Lens' DescribeHyperParameterTuningJobResponse (Maybe Text)
dhptjrsFailureReason = lens _dhptjrsFailureReason (\s a -> s {_dhptjrsFailureReason = a})

-- | The 'HyperParameterTrainingJobDefinition' object that specifies the definition of the training jobs that this tuning job launches.
dhptjrsTrainingJobDefinition :: Lens' DescribeHyperParameterTuningJobResponse (Maybe HyperParameterTrainingJobDefinition)
dhptjrsTrainingJobDefinition = lens _dhptjrsTrainingJobDefinition (\s a -> s {_dhptjrsTrainingJobDefinition = a})

-- | The date and time that the status of the tuning job was modified.
dhptjrsLastModifiedTime :: Lens' DescribeHyperParameterTuningJobResponse (Maybe UTCTime)
dhptjrsLastModifiedTime = lens _dhptjrsLastModifiedTime (\s a -> s {_dhptjrsLastModifiedTime = a}) . mapping _Time

-- | A 'TrainingJobSummary' object that describes the training job that completed with the best current 'HyperParameterTuningJobObjective' .
dhptjrsBestTrainingJob :: Lens' DescribeHyperParameterTuningJobResponse (Maybe HyperParameterTrainingJobSummary)
dhptjrsBestTrainingJob = lens _dhptjrsBestTrainingJob (\s a -> s {_dhptjrsBestTrainingJob = a})

-- | The date and time that the tuning job ended.
dhptjrsHyperParameterTuningEndTime :: Lens' DescribeHyperParameterTuningJobResponse (Maybe UTCTime)
dhptjrsHyperParameterTuningEndTime = lens _dhptjrsHyperParameterTuningEndTime (\s a -> s {_dhptjrsHyperParameterTuningEndTime = a}) . mapping _Time

-- | If the hyperparameter tuning job is an warm start tuning job with a @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@ , this is the 'TrainingJobSummary' for the training job with the best objective metric value of all training jobs launched by this tuning job and all parent jobs specified for the warm start tuning job.
dhptjrsOverallBestTrainingJob :: Lens' DescribeHyperParameterTuningJobResponse (Maybe HyperParameterTrainingJobSummary)
dhptjrsOverallBestTrainingJob = lens _dhptjrsOverallBestTrainingJob (\s a -> s {_dhptjrsOverallBestTrainingJob = a})

-- | The configuration for starting the hyperparameter parameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job.
dhptjrsWarmStartConfig :: Lens' DescribeHyperParameterTuningJobResponse (Maybe HyperParameterTuningJobWarmStartConfig)
dhptjrsWarmStartConfig = lens _dhptjrsWarmStartConfig (\s a -> s {_dhptjrsWarmStartConfig = a})

-- | A list of the 'HyperParameterTrainingJobDefinition' objects launched for this tuning job.
dhptjrsTrainingJobDefinitions :: Lens' DescribeHyperParameterTuningJobResponse (Maybe (NonEmpty HyperParameterTrainingJobDefinition))
dhptjrsTrainingJobDefinitions = lens _dhptjrsTrainingJobDefinitions (\s a -> s {_dhptjrsTrainingJobDefinitions = a}) . mapping _List1

-- | -- | The response status code.
dhptjrsResponseStatus :: Lens' DescribeHyperParameterTuningJobResponse Int
dhptjrsResponseStatus = lens _dhptjrsResponseStatus (\s a -> s {_dhptjrsResponseStatus = a})

-- | The name of the tuning job.
dhptjrsHyperParameterTuningJobName :: Lens' DescribeHyperParameterTuningJobResponse Text
dhptjrsHyperParameterTuningJobName = lens _dhptjrsHyperParameterTuningJobName (\s a -> s {_dhptjrsHyperParameterTuningJobName = a})

-- | The Amazon Resource Name (ARN) of the tuning job.
dhptjrsHyperParameterTuningJobARN :: Lens' DescribeHyperParameterTuningJobResponse Text
dhptjrsHyperParameterTuningJobARN = lens _dhptjrsHyperParameterTuningJobARN (\s a -> s {_dhptjrsHyperParameterTuningJobARN = a})

-- | The 'HyperParameterTuningJobConfig' object that specifies the configuration of the tuning job.
dhptjrsHyperParameterTuningJobConfig :: Lens' DescribeHyperParameterTuningJobResponse HyperParameterTuningJobConfig
dhptjrsHyperParameterTuningJobConfig = lens _dhptjrsHyperParameterTuningJobConfig (\s a -> s {_dhptjrsHyperParameterTuningJobConfig = a})

-- | The status of the tuning job: InProgress, Completed, Failed, Stopping, or Stopped.
dhptjrsHyperParameterTuningJobStatus :: Lens' DescribeHyperParameterTuningJobResponse HyperParameterTuningJobStatus
dhptjrsHyperParameterTuningJobStatus = lens _dhptjrsHyperParameterTuningJobStatus (\s a -> s {_dhptjrsHyperParameterTuningJobStatus = a})

-- | The date and time that the tuning job started.
dhptjrsCreationTime :: Lens' DescribeHyperParameterTuningJobResponse UTCTime
dhptjrsCreationTime = lens _dhptjrsCreationTime (\s a -> s {_dhptjrsCreationTime = a}) . _Time

-- | The 'TrainingJobStatusCounters' object that specifies the number of training jobs, categorized by status, that this tuning job launched.
dhptjrsTrainingJobStatusCounters :: Lens' DescribeHyperParameterTuningJobResponse TrainingJobStatusCounters
dhptjrsTrainingJobStatusCounters = lens _dhptjrsTrainingJobStatusCounters (\s a -> s {_dhptjrsTrainingJobStatusCounters = a})

-- | The 'ObjectiveStatusCounters' object that specifies the number of training jobs, categorized by the status of their final objective metric, that this tuning job launched.
dhptjrsObjectiveStatusCounters :: Lens' DescribeHyperParameterTuningJobResponse ObjectiveStatusCounters
dhptjrsObjectiveStatusCounters = lens _dhptjrsObjectiveStatusCounters (\s a -> s {_dhptjrsObjectiveStatusCounters = a})

instance NFData DescribeHyperParameterTuningJobResponse
