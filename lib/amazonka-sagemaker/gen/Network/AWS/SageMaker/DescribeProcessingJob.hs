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
-- Module      : Network.AWS.SageMaker.DescribeProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a processing job.
module Network.AWS.SageMaker.DescribeProcessingJob
  ( -- * Creating a Request
    describeProcessingJob,
    DescribeProcessingJob,

    -- * Request Lenses
    dpjProcessingJobName,

    -- * Destructuring the Response
    describeProcessingJobResponse,
    DescribeProcessingJobResponse,

    -- * Response Lenses
    dpjrsFailureReason,
    dpjrsMonitoringScheduleARN,
    dpjrsEnvironment,
    dpjrsStoppingCondition,
    dpjrsExperimentConfig,
    dpjrsLastModifiedTime,
    dpjrsProcessingInputs,
    dpjrsNetworkConfig,
    dpjrsAutoMLJobARN,
    dpjrsTrainingJobARN,
    dpjrsExitMessage,
    dpjrsProcessingOutputConfig,
    dpjrsProcessingStartTime,
    dpjrsProcessingEndTime,
    dpjrsRoleARN,
    dpjrsResponseStatus,
    dpjrsProcessingJobName,
    dpjrsProcessingResources,
    dpjrsAppSpecification,
    dpjrsProcessingJobARN,
    dpjrsProcessingJobStatus,
    dpjrsCreationTime,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeProcessingJob' smart constructor.
newtype DescribeProcessingJob = DescribeProcessingJob'
  { _dpjProcessingJobName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProcessingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpjProcessingJobName' - The name of the processing job. The name must be unique within an AWS Region in the AWS account.
describeProcessingJob ::
  -- | 'dpjProcessingJobName'
  Text ->
  DescribeProcessingJob
describeProcessingJob pProcessingJobName_ =
  DescribeProcessingJob'
    { _dpjProcessingJobName =
        pProcessingJobName_
    }

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
dpjProcessingJobName :: Lens' DescribeProcessingJob Text
dpjProcessingJobName = lens _dpjProcessingJobName (\s a -> s {_dpjProcessingJobName = a})

instance AWSRequest DescribeProcessingJob where
  type Rs DescribeProcessingJob = DescribeProcessingJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeProcessingJobResponse'
            <$> (x .?> "FailureReason")
            <*> (x .?> "MonitoringScheduleArn")
            <*> (x .?> "Environment" .!@ mempty)
            <*> (x .?> "StoppingCondition")
            <*> (x .?> "ExperimentConfig")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "ProcessingInputs" .!@ mempty)
            <*> (x .?> "NetworkConfig")
            <*> (x .?> "AutoMLJobArn")
            <*> (x .?> "TrainingJobArn")
            <*> (x .?> "ExitMessage")
            <*> (x .?> "ProcessingOutputConfig")
            <*> (x .?> "ProcessingStartTime")
            <*> (x .?> "ProcessingEndTime")
            <*> (x .?> "RoleArn")
            <*> (pure (fromEnum s))
            <*> (x .:> "ProcessingJobName")
            <*> (x .:> "ProcessingResources")
            <*> (x .:> "AppSpecification")
            <*> (x .:> "ProcessingJobArn")
            <*> (x .:> "ProcessingJobStatus")
            <*> (x .:> "CreationTime")
      )

instance Hashable DescribeProcessingJob

instance NFData DescribeProcessingJob

instance ToHeaders DescribeProcessingJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DescribeProcessingJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeProcessingJob where
  toJSON DescribeProcessingJob' {..} =
    object
      (catMaybes [Just ("ProcessingJobName" .= _dpjProcessingJobName)])

instance ToPath DescribeProcessingJob where
  toPath = const "/"

instance ToQuery DescribeProcessingJob where
  toQuery = const mempty

-- | /See:/ 'describeProcessingJobResponse' smart constructor.
data DescribeProcessingJobResponse = DescribeProcessingJobResponse'
  { _dpjrsFailureReason ::
      !(Maybe Text),
    _dpjrsMonitoringScheduleARN ::
      !(Maybe Text),
    _dpjrsEnvironment ::
      !(Maybe (Map Text (Text))),
    _dpjrsStoppingCondition ::
      !( Maybe
           ProcessingStoppingCondition
       ),
    _dpjrsExperimentConfig ::
      !(Maybe ExperimentConfig),
    _dpjrsLastModifiedTime ::
      !(Maybe POSIX),
    _dpjrsProcessingInputs ::
      !(Maybe [ProcessingInput]),
    _dpjrsNetworkConfig ::
      !(Maybe NetworkConfig),
    _dpjrsAutoMLJobARN ::
      !(Maybe Text),
    _dpjrsTrainingJobARN ::
      !(Maybe Text),
    _dpjrsExitMessage ::
      !(Maybe Text),
    _dpjrsProcessingOutputConfig ::
      !(Maybe ProcessingOutputConfig),
    _dpjrsProcessingStartTime ::
      !(Maybe POSIX),
    _dpjrsProcessingEndTime ::
      !(Maybe POSIX),
    _dpjrsRoleARN :: !(Maybe Text),
    _dpjrsResponseStatus :: !Int,
    _dpjrsProcessingJobName ::
      !Text,
    _dpjrsProcessingResources ::
      !ProcessingResources,
    _dpjrsAppSpecification ::
      !AppSpecification,
    _dpjrsProcessingJobARN :: !Text,
    _dpjrsProcessingJobStatus ::
      !ProcessingJobStatus,
    _dpjrsCreationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProcessingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpjrsFailureReason' - A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
--
-- * 'dpjrsMonitoringScheduleARN' - The ARN of a monitoring schedule for an endpoint associated with this processing job.
--
-- * 'dpjrsEnvironment' - The environment variables set in the Docker container.
--
-- * 'dpjrsStoppingCondition' - The time limit for how long the processing job is allowed to run.
--
-- * 'dpjrsExperimentConfig' - The configuration information used to create an experiment.
--
-- * 'dpjrsLastModifiedTime' - The time at which the processing job was last modified.
--
-- * 'dpjrsProcessingInputs' - The inputs for a processing job.
--
-- * 'dpjrsNetworkConfig' - Networking options for a processing job.
--
-- * 'dpjrsAutoMLJobARN' - The ARN of an AutoML job associated with this processing job.
--
-- * 'dpjrsTrainingJobARN' - The ARN of a training job associated with this processing job.
--
-- * 'dpjrsExitMessage' - An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
--
-- * 'dpjrsProcessingOutputConfig' - Output configuration for the processing job.
--
-- * 'dpjrsProcessingStartTime' - The time at which the processing job started.
--
-- * 'dpjrsProcessingEndTime' - The time at which the processing job completed.
--
-- * 'dpjrsRoleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- * 'dpjrsResponseStatus' - -- | The response status code.
--
-- * 'dpjrsProcessingJobName' - The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- * 'dpjrsProcessingResources' - Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- * 'dpjrsAppSpecification' - Configures the processing job to run a specified container image.
--
-- * 'dpjrsProcessingJobARN' - The Amazon Resource Name (ARN) of the processing job.
--
-- * 'dpjrsProcessingJobStatus' - Provides the status of a processing job.
--
-- * 'dpjrsCreationTime' - The time at which the processing job was created.
describeProcessingJobResponse ::
  -- | 'dpjrsResponseStatus'
  Int ->
  -- | 'dpjrsProcessingJobName'
  Text ->
  -- | 'dpjrsProcessingResources'
  ProcessingResources ->
  -- | 'dpjrsAppSpecification'
  AppSpecification ->
  -- | 'dpjrsProcessingJobARN'
  Text ->
  -- | 'dpjrsProcessingJobStatus'
  ProcessingJobStatus ->
  -- | 'dpjrsCreationTime'
  UTCTime ->
  DescribeProcessingJobResponse
describeProcessingJobResponse
  pResponseStatus_
  pProcessingJobName_
  pProcessingResources_
  pAppSpecification_
  pProcessingJobARN_
  pProcessingJobStatus_
  pCreationTime_ =
    DescribeProcessingJobResponse'
      { _dpjrsFailureReason = Nothing,
        _dpjrsMonitoringScheduleARN = Nothing,
        _dpjrsEnvironment = Nothing,
        _dpjrsStoppingCondition = Nothing,
        _dpjrsExperimentConfig = Nothing,
        _dpjrsLastModifiedTime = Nothing,
        _dpjrsProcessingInputs = Nothing,
        _dpjrsNetworkConfig = Nothing,
        _dpjrsAutoMLJobARN = Nothing,
        _dpjrsTrainingJobARN = Nothing,
        _dpjrsExitMessage = Nothing,
        _dpjrsProcessingOutputConfig = Nothing,
        _dpjrsProcessingStartTime = Nothing,
        _dpjrsProcessingEndTime = Nothing,
        _dpjrsRoleARN = Nothing,
        _dpjrsResponseStatus = pResponseStatus_,
        _dpjrsProcessingJobName = pProcessingJobName_,
        _dpjrsProcessingResources = pProcessingResources_,
        _dpjrsAppSpecification = pAppSpecification_,
        _dpjrsProcessingJobARN = pProcessingJobARN_,
        _dpjrsProcessingJobStatus = pProcessingJobStatus_,
        _dpjrsCreationTime = _Time # pCreationTime_
      }

-- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
dpjrsFailureReason :: Lens' DescribeProcessingJobResponse (Maybe Text)
dpjrsFailureReason = lens _dpjrsFailureReason (\s a -> s {_dpjrsFailureReason = a})

-- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
dpjrsMonitoringScheduleARN :: Lens' DescribeProcessingJobResponse (Maybe Text)
dpjrsMonitoringScheduleARN = lens _dpjrsMonitoringScheduleARN (\s a -> s {_dpjrsMonitoringScheduleARN = a})

-- | The environment variables set in the Docker container.
dpjrsEnvironment :: Lens' DescribeProcessingJobResponse (HashMap Text (Text))
dpjrsEnvironment = lens _dpjrsEnvironment (\s a -> s {_dpjrsEnvironment = a}) . _Default . _Map

-- | The time limit for how long the processing job is allowed to run.
dpjrsStoppingCondition :: Lens' DescribeProcessingJobResponse (Maybe ProcessingStoppingCondition)
dpjrsStoppingCondition = lens _dpjrsStoppingCondition (\s a -> s {_dpjrsStoppingCondition = a})

-- | The configuration information used to create an experiment.
dpjrsExperimentConfig :: Lens' DescribeProcessingJobResponse (Maybe ExperimentConfig)
dpjrsExperimentConfig = lens _dpjrsExperimentConfig (\s a -> s {_dpjrsExperimentConfig = a})

-- | The time at which the processing job was last modified.
dpjrsLastModifiedTime :: Lens' DescribeProcessingJobResponse (Maybe UTCTime)
dpjrsLastModifiedTime = lens _dpjrsLastModifiedTime (\s a -> s {_dpjrsLastModifiedTime = a}) . mapping _Time

-- | The inputs for a processing job.
dpjrsProcessingInputs :: Lens' DescribeProcessingJobResponse [ProcessingInput]
dpjrsProcessingInputs = lens _dpjrsProcessingInputs (\s a -> s {_dpjrsProcessingInputs = a}) . _Default . _Coerce

-- | Networking options for a processing job.
dpjrsNetworkConfig :: Lens' DescribeProcessingJobResponse (Maybe NetworkConfig)
dpjrsNetworkConfig = lens _dpjrsNetworkConfig (\s a -> s {_dpjrsNetworkConfig = a})

-- | The ARN of an AutoML job associated with this processing job.
dpjrsAutoMLJobARN :: Lens' DescribeProcessingJobResponse (Maybe Text)
dpjrsAutoMLJobARN = lens _dpjrsAutoMLJobARN (\s a -> s {_dpjrsAutoMLJobARN = a})

-- | The ARN of a training job associated with this processing job.
dpjrsTrainingJobARN :: Lens' DescribeProcessingJobResponse (Maybe Text)
dpjrsTrainingJobARN = lens _dpjrsTrainingJobARN (\s a -> s {_dpjrsTrainingJobARN = a})

-- | An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
dpjrsExitMessage :: Lens' DescribeProcessingJobResponse (Maybe Text)
dpjrsExitMessage = lens _dpjrsExitMessage (\s a -> s {_dpjrsExitMessage = a})

-- | Output configuration for the processing job.
dpjrsProcessingOutputConfig :: Lens' DescribeProcessingJobResponse (Maybe ProcessingOutputConfig)
dpjrsProcessingOutputConfig = lens _dpjrsProcessingOutputConfig (\s a -> s {_dpjrsProcessingOutputConfig = a})

-- | The time at which the processing job started.
dpjrsProcessingStartTime :: Lens' DescribeProcessingJobResponse (Maybe UTCTime)
dpjrsProcessingStartTime = lens _dpjrsProcessingStartTime (\s a -> s {_dpjrsProcessingStartTime = a}) . mapping _Time

-- | The time at which the processing job completed.
dpjrsProcessingEndTime :: Lens' DescribeProcessingJobResponse (Maybe UTCTime)
dpjrsProcessingEndTime = lens _dpjrsProcessingEndTime (\s a -> s {_dpjrsProcessingEndTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
dpjrsRoleARN :: Lens' DescribeProcessingJobResponse (Maybe Text)
dpjrsRoleARN = lens _dpjrsRoleARN (\s a -> s {_dpjrsRoleARN = a})

-- | -- | The response status code.
dpjrsResponseStatus :: Lens' DescribeProcessingJobResponse Int
dpjrsResponseStatus = lens _dpjrsResponseStatus (\s a -> s {_dpjrsResponseStatus = a})

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
dpjrsProcessingJobName :: Lens' DescribeProcessingJobResponse Text
dpjrsProcessingJobName = lens _dpjrsProcessingJobName (\s a -> s {_dpjrsProcessingJobName = a})

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
dpjrsProcessingResources :: Lens' DescribeProcessingJobResponse ProcessingResources
dpjrsProcessingResources = lens _dpjrsProcessingResources (\s a -> s {_dpjrsProcessingResources = a})

-- | Configures the processing job to run a specified container image.
dpjrsAppSpecification :: Lens' DescribeProcessingJobResponse AppSpecification
dpjrsAppSpecification = lens _dpjrsAppSpecification (\s a -> s {_dpjrsAppSpecification = a})

-- | The Amazon Resource Name (ARN) of the processing job.
dpjrsProcessingJobARN :: Lens' DescribeProcessingJobResponse Text
dpjrsProcessingJobARN = lens _dpjrsProcessingJobARN (\s a -> s {_dpjrsProcessingJobARN = a})

-- | Provides the status of a processing job.
dpjrsProcessingJobStatus :: Lens' DescribeProcessingJobResponse ProcessingJobStatus
dpjrsProcessingJobStatus = lens _dpjrsProcessingJobStatus (\s a -> s {_dpjrsProcessingJobStatus = a})

-- | The time at which the processing job was created.
dpjrsCreationTime :: Lens' DescribeProcessingJobResponse UTCTime
dpjrsCreationTime = lens _dpjrsCreationTime (\s a -> s {_dpjrsCreationTime = a}) . _Time

instance NFData DescribeProcessingJobResponse
