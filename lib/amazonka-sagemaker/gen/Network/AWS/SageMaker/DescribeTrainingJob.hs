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
-- Module      : Network.AWS.SageMaker.DescribeTrainingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a training job.
module Network.AWS.SageMaker.DescribeTrainingJob
  ( -- * Creating a Request
    describeTrainingJob,
    DescribeTrainingJob,

    -- * Request Lenses
    dtjTrainingJobName,

    -- * Destructuring the Response
    describeTrainingJobResponse,
    DescribeTrainingJobResponse,

    -- * Response Lenses
    dtjtrsLabelingJobARN,
    dtjtrsFailureReason,
    dtjtrsSecondaryStatusTransitions,
    dtjtrsTrainingEndTime,
    dtjtrsBillableTimeInSeconds,
    dtjtrsDebugHookConfig,
    dtjtrsCheckpointConfig,
    dtjtrsDebugRuleEvaluationStatuses,
    dtjtrsEnableNetworkIsolation,
    dtjtrsExperimentConfig,
    dtjtrsLastModifiedTime,
    dtjtrsDebugRuleConfigurations,
    dtjtrsEnableManagedSpotTraining,
    dtjtrsAutoMLJobARN,
    dtjtrsHyperParameters,
    dtjtrsInputDataConfig,
    dtjtrsVPCConfig,
    dtjtrsFinalMetricDataList,
    dtjtrsOutputDataConfig,
    dtjtrsTrainingStartTime,
    dtjtrsTuningJobARN,
    dtjtrsEnableInterContainerTrafficEncryption,
    dtjtrsTensorBoardOutputConfig,
    dtjtrsTrainingTimeInSeconds,
    dtjtrsRoleARN,
    dtjtrsResponseStatus,
    dtjtrsTrainingJobName,
    dtjtrsTrainingJobARN,
    dtjtrsModelArtifacts,
    dtjtrsTrainingJobStatus,
    dtjtrsSecondaryStatus,
    dtjtrsAlgorithmSpecification,
    dtjtrsResourceConfig,
    dtjtrsStoppingCondition,
    dtjtrsCreationTime,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeTrainingJob' smart constructor.
newtype DescribeTrainingJob = DescribeTrainingJob'
  { _dtjTrainingJobName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrainingJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtjTrainingJobName' - The name of the training job.
describeTrainingJob ::
  -- | 'dtjTrainingJobName'
  Text ->
  DescribeTrainingJob
describeTrainingJob pTrainingJobName_ =
  DescribeTrainingJob' {_dtjTrainingJobName = pTrainingJobName_}

-- | The name of the training job.
dtjTrainingJobName :: Lens' DescribeTrainingJob Text
dtjTrainingJobName = lens _dtjTrainingJobName (\s a -> s {_dtjTrainingJobName = a})

instance AWSRequest DescribeTrainingJob where
  type Rs DescribeTrainingJob = DescribeTrainingJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeTrainingJobResponse'
            <$> (x .?> "LabelingJobArn")
            <*> (x .?> "FailureReason")
            <*> (x .?> "SecondaryStatusTransitions" .!@ mempty)
            <*> (x .?> "TrainingEndTime")
            <*> (x .?> "BillableTimeInSeconds")
            <*> (x .?> "DebugHookConfig")
            <*> (x .?> "CheckpointConfig")
            <*> (x .?> "DebugRuleEvaluationStatuses" .!@ mempty)
            <*> (x .?> "EnableNetworkIsolation")
            <*> (x .?> "ExperimentConfig")
            <*> (x .?> "LastModifiedTime")
            <*> (x .?> "DebugRuleConfigurations" .!@ mempty)
            <*> (x .?> "EnableManagedSpotTraining")
            <*> (x .?> "AutoMLJobArn")
            <*> (x .?> "HyperParameters" .!@ mempty)
            <*> (x .?> "InputDataConfig")
            <*> (x .?> "VpcConfig")
            <*> (x .?> "FinalMetricDataList" .!@ mempty)
            <*> (x .?> "OutputDataConfig")
            <*> (x .?> "TrainingStartTime")
            <*> (x .?> "TuningJobArn")
            <*> (x .?> "EnableInterContainerTrafficEncryption")
            <*> (x .?> "TensorBoardOutputConfig")
            <*> (x .?> "TrainingTimeInSeconds")
            <*> (x .?> "RoleArn")
            <*> (pure (fromEnum s))
            <*> (x .:> "TrainingJobName")
            <*> (x .:> "TrainingJobArn")
            <*> (x .:> "ModelArtifacts")
            <*> (x .:> "TrainingJobStatus")
            <*> (x .:> "SecondaryStatus")
            <*> (x .:> "AlgorithmSpecification")
            <*> (x .:> "ResourceConfig")
            <*> (x .:> "StoppingCondition")
            <*> (x .:> "CreationTime")
      )

instance Hashable DescribeTrainingJob

instance NFData DescribeTrainingJob

instance ToHeaders DescribeTrainingJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeTrainingJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeTrainingJob where
  toJSON DescribeTrainingJob' {..} =
    object
      (catMaybes [Just ("TrainingJobName" .= _dtjTrainingJobName)])

instance ToPath DescribeTrainingJob where
  toPath = const "/"

instance ToQuery DescribeTrainingJob where
  toQuery = const mempty

-- | /See:/ 'describeTrainingJobResponse' smart constructor.
data DescribeTrainingJobResponse = DescribeTrainingJobResponse'
  { _dtjtrsLabelingJobARN ::
      !(Maybe Text),
    _dtjtrsFailureReason ::
      !(Maybe Text),
    _dtjtrsSecondaryStatusTransitions ::
      !( Maybe
           [SecondaryStatusTransition]
       ),
    _dtjtrsTrainingEndTime ::
      !(Maybe POSIX),
    _dtjtrsBillableTimeInSeconds ::
      !(Maybe Nat),
    _dtjtrsDebugHookConfig ::
      !(Maybe DebugHookConfig),
    _dtjtrsCheckpointConfig ::
      !(Maybe CheckpointConfig),
    _dtjtrsDebugRuleEvaluationStatuses ::
      !( Maybe
           [DebugRuleEvaluationStatus]
       ),
    _dtjtrsEnableNetworkIsolation ::
      !(Maybe Bool),
    _dtjtrsExperimentConfig ::
      !(Maybe ExperimentConfig),
    _dtjtrsLastModifiedTime ::
      !(Maybe POSIX),
    _dtjtrsDebugRuleConfigurations ::
      !(Maybe [DebugRuleConfiguration]),
    _dtjtrsEnableManagedSpotTraining ::
      !(Maybe Bool),
    _dtjtrsAutoMLJobARN ::
      !(Maybe Text),
    _dtjtrsHyperParameters ::
      !(Maybe (Map Text (Text))),
    _dtjtrsInputDataConfig ::
      !(Maybe (List1 Channel)),
    _dtjtrsVPCConfig ::
      !(Maybe VPCConfig),
    _dtjtrsFinalMetricDataList ::
      !(Maybe [MetricData]),
    _dtjtrsOutputDataConfig ::
      !(Maybe OutputDataConfig),
    _dtjtrsTrainingStartTime ::
      !(Maybe POSIX),
    _dtjtrsTuningJobARN ::
      !(Maybe Text),
    _dtjtrsEnableInterContainerTrafficEncryption ::
      !(Maybe Bool),
    _dtjtrsTensorBoardOutputConfig ::
      !(Maybe TensorBoardOutputConfig),
    _dtjtrsTrainingTimeInSeconds ::
      !(Maybe Nat),
    _dtjtrsRoleARN :: !(Maybe Text),
    _dtjtrsResponseStatus :: !Int,
    _dtjtrsTrainingJobName :: !Text,
    _dtjtrsTrainingJobARN :: !Text,
    _dtjtrsModelArtifacts ::
      !ModelArtifacts,
    _dtjtrsTrainingJobStatus ::
      !TrainingJobStatus,
    _dtjtrsSecondaryStatus ::
      !SecondaryStatus,
    _dtjtrsAlgorithmSpecification ::
      !AlgorithmSpecification,
    _dtjtrsResourceConfig ::
      !ResourceConfig,
    _dtjtrsStoppingCondition ::
      !StoppingCondition,
    _dtjtrsCreationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrainingJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtjtrsLabelingJobARN' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
--
-- * 'dtjtrsFailureReason' - If the training job failed, the reason it failed.
--
-- * 'dtjtrsSecondaryStatusTransitions' - A history of all of the secondary statuses that the training job has transitioned through.
--
-- * 'dtjtrsTrainingEndTime' - Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- * 'dtjtrsBillableTimeInSeconds' - The billable time in seconds. You can calculate the savings from using managed spot training using the formula @(1 - BillableTimeInSeconds / TrainingTimeInSeconds) * 100@ . For example, if @BillableTimeInSeconds@ is 100 and @TrainingTimeInSeconds@ is 500, the savings is 80%.
--
-- * 'dtjtrsDebugHookConfig' - Undocumented member.
--
-- * 'dtjtrsCheckpointConfig' - Undocumented member.
--
-- * 'dtjtrsDebugRuleEvaluationStatuses' - Status about the debug rule evaluation.
--
-- * 'dtjtrsEnableNetworkIsolation' - If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- * 'dtjtrsExperimentConfig' - Undocumented member.
--
-- * 'dtjtrsLastModifiedTime' - A timestamp that indicates when the status of the training job was last modified.
--
-- * 'dtjtrsDebugRuleConfigurations' - Configuration information for debugging rules.
--
-- * 'dtjtrsEnableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
--
-- * 'dtjtrsAutoMLJobARN' - The Amazon Resource Name (ARN) of an AutoML job.
--
-- * 'dtjtrsHyperParameters' - Algorithm-specific parameters.
--
-- * 'dtjtrsInputDataConfig' - An array of @Channel@ objects that describes each data input channel.
--
-- * 'dtjtrsVPCConfig' - A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- * 'dtjtrsFinalMetricDataList' - A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
--
-- * 'dtjtrsOutputDataConfig' - The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- * 'dtjtrsTrainingStartTime' - Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
--
-- * 'dtjtrsTuningJobARN' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
--
-- * 'dtjtrsEnableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithms in distributed training.
--
-- * 'dtjtrsTensorBoardOutputConfig' - Undocumented member.
--
-- * 'dtjtrsTrainingTimeInSeconds' - The training time in seconds.
--
-- * 'dtjtrsRoleARN' - The AWS Identity and Access Management (IAM) role configured for the training job.
--
-- * 'dtjtrsResponseStatus' - -- | The response status code.
--
-- * 'dtjtrsTrainingJobName' - Name of the model training job.
--
-- * 'dtjtrsTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'dtjtrsModelArtifacts' - Information about the Amazon S3 location that is configured for storing model artifacts.
--
-- * 'dtjtrsTrainingJobStatus' - The status of the training job. Amazon SageMaker provides the following training job statuses:     * @InProgress@ - The training is in progress.     * @Completed@ - The training job has completed.     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.     * @Stopping@ - The training job is stopping.     * @Stopped@ - The training job has stopped. For more detailed information, see @SecondaryStatus@ .
--
-- * 'dtjtrsSecondaryStatus' - Provides detailed information about the state of the training job. For detailed information on the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' . Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Interrupted@ - The job stopped because the managed spot training instances were interrupted.      * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @MaxWaitTimeExceeded@ - The job stopped because it exceeded the maximum allowed wait time.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. /Important:/ Valid values for @SecondaryStatus@ are subject to change.  We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
--
-- * 'dtjtrsAlgorithmSpecification' - Information about the algorithm used for training, and algorithm metadata.
--
-- * 'dtjtrsResourceConfig' - Resources, including ML compute instances and ML storage volumes, that are configured for model training.
--
-- * 'dtjtrsStoppingCondition' - Specifies a limit to how long a model training job can run. It also specifies the maximum time to wait for a spot instance. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
--
-- * 'dtjtrsCreationTime' - A timestamp that indicates when the training job was created.
describeTrainingJobResponse ::
  -- | 'dtjtrsResponseStatus'
  Int ->
  -- | 'dtjtrsTrainingJobName'
  Text ->
  -- | 'dtjtrsTrainingJobARN'
  Text ->
  -- | 'dtjtrsModelArtifacts'
  ModelArtifacts ->
  -- | 'dtjtrsTrainingJobStatus'
  TrainingJobStatus ->
  -- | 'dtjtrsSecondaryStatus'
  SecondaryStatus ->
  -- | 'dtjtrsAlgorithmSpecification'
  AlgorithmSpecification ->
  -- | 'dtjtrsResourceConfig'
  ResourceConfig ->
  -- | 'dtjtrsStoppingCondition'
  StoppingCondition ->
  -- | 'dtjtrsCreationTime'
  UTCTime ->
  DescribeTrainingJobResponse
describeTrainingJobResponse
  pResponseStatus_
  pTrainingJobName_
  pTrainingJobARN_
  pModelArtifacts_
  pTrainingJobStatus_
  pSecondaryStatus_
  pAlgorithmSpecification_
  pResourceConfig_
  pStoppingCondition_
  pCreationTime_ =
    DescribeTrainingJobResponse'
      { _dtjtrsLabelingJobARN = Nothing,
        _dtjtrsFailureReason = Nothing,
        _dtjtrsSecondaryStatusTransitions = Nothing,
        _dtjtrsTrainingEndTime = Nothing,
        _dtjtrsBillableTimeInSeconds = Nothing,
        _dtjtrsDebugHookConfig = Nothing,
        _dtjtrsCheckpointConfig = Nothing,
        _dtjtrsDebugRuleEvaluationStatuses = Nothing,
        _dtjtrsEnableNetworkIsolation = Nothing,
        _dtjtrsExperimentConfig = Nothing,
        _dtjtrsLastModifiedTime = Nothing,
        _dtjtrsDebugRuleConfigurations = Nothing,
        _dtjtrsEnableManagedSpotTraining = Nothing,
        _dtjtrsAutoMLJobARN = Nothing,
        _dtjtrsHyperParameters = Nothing,
        _dtjtrsInputDataConfig = Nothing,
        _dtjtrsVPCConfig = Nothing,
        _dtjtrsFinalMetricDataList = Nothing,
        _dtjtrsOutputDataConfig = Nothing,
        _dtjtrsTrainingStartTime = Nothing,
        _dtjtrsTuningJobARN = Nothing,
        _dtjtrsEnableInterContainerTrafficEncryption = Nothing,
        _dtjtrsTensorBoardOutputConfig = Nothing,
        _dtjtrsTrainingTimeInSeconds = Nothing,
        _dtjtrsRoleARN = Nothing,
        _dtjtrsResponseStatus = pResponseStatus_,
        _dtjtrsTrainingJobName = pTrainingJobName_,
        _dtjtrsTrainingJobARN = pTrainingJobARN_,
        _dtjtrsModelArtifacts = pModelArtifacts_,
        _dtjtrsTrainingJobStatus = pTrainingJobStatus_,
        _dtjtrsSecondaryStatus = pSecondaryStatus_,
        _dtjtrsAlgorithmSpecification = pAlgorithmSpecification_,
        _dtjtrsResourceConfig = pResourceConfig_,
        _dtjtrsStoppingCondition = pStoppingCondition_,
        _dtjtrsCreationTime = _Time # pCreationTime_
      }

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
dtjtrsLabelingJobARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
dtjtrsLabelingJobARN = lens _dtjtrsLabelingJobARN (\s a -> s {_dtjtrsLabelingJobARN = a})

-- | If the training job failed, the reason it failed.
dtjtrsFailureReason :: Lens' DescribeTrainingJobResponse (Maybe Text)
dtjtrsFailureReason = lens _dtjtrsFailureReason (\s a -> s {_dtjtrsFailureReason = a})

-- | A history of all of the secondary statuses that the training job has transitioned through.
dtjtrsSecondaryStatusTransitions :: Lens' DescribeTrainingJobResponse [SecondaryStatusTransition]
dtjtrsSecondaryStatusTransitions = lens _dtjtrsSecondaryStatusTransitions (\s a -> s {_dtjtrsSecondaryStatusTransitions = a}) . _Default . _Coerce

-- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
dtjtrsTrainingEndTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
dtjtrsTrainingEndTime = lens _dtjtrsTrainingEndTime (\s a -> s {_dtjtrsTrainingEndTime = a}) . mapping _Time

-- | The billable time in seconds. You can calculate the savings from using managed spot training using the formula @(1 - BillableTimeInSeconds / TrainingTimeInSeconds) * 100@ . For example, if @BillableTimeInSeconds@ is 100 and @TrainingTimeInSeconds@ is 500, the savings is 80%.
dtjtrsBillableTimeInSeconds :: Lens' DescribeTrainingJobResponse (Maybe Natural)
dtjtrsBillableTimeInSeconds = lens _dtjtrsBillableTimeInSeconds (\s a -> s {_dtjtrsBillableTimeInSeconds = a}) . mapping _Nat

-- | Undocumented member.
dtjtrsDebugHookConfig :: Lens' DescribeTrainingJobResponse (Maybe DebugHookConfig)
dtjtrsDebugHookConfig = lens _dtjtrsDebugHookConfig (\s a -> s {_dtjtrsDebugHookConfig = a})

-- | Undocumented member.
dtjtrsCheckpointConfig :: Lens' DescribeTrainingJobResponse (Maybe CheckpointConfig)
dtjtrsCheckpointConfig = lens _dtjtrsCheckpointConfig (\s a -> s {_dtjtrsCheckpointConfig = a})

-- | Status about the debug rule evaluation.
dtjtrsDebugRuleEvaluationStatuses :: Lens' DescribeTrainingJobResponse [DebugRuleEvaluationStatus]
dtjtrsDebugRuleEvaluationStatuses = lens _dtjtrsDebugRuleEvaluationStatuses (\s a -> s {_dtjtrsDebugRuleEvaluationStatuses = a}) . _Default . _Coerce

-- | If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
dtjtrsEnableNetworkIsolation :: Lens' DescribeTrainingJobResponse (Maybe Bool)
dtjtrsEnableNetworkIsolation = lens _dtjtrsEnableNetworkIsolation (\s a -> s {_dtjtrsEnableNetworkIsolation = a})

-- | Undocumented member.
dtjtrsExperimentConfig :: Lens' DescribeTrainingJobResponse (Maybe ExperimentConfig)
dtjtrsExperimentConfig = lens _dtjtrsExperimentConfig (\s a -> s {_dtjtrsExperimentConfig = a})

-- | A timestamp that indicates when the status of the training job was last modified.
dtjtrsLastModifiedTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
dtjtrsLastModifiedTime = lens _dtjtrsLastModifiedTime (\s a -> s {_dtjtrsLastModifiedTime = a}) . mapping _Time

-- | Configuration information for debugging rules.
dtjtrsDebugRuleConfigurations :: Lens' DescribeTrainingJobResponse [DebugRuleConfiguration]
dtjtrsDebugRuleConfigurations = lens _dtjtrsDebugRuleConfigurations (\s a -> s {_dtjtrsDebugRuleConfigurations = a}) . _Default . _Coerce

-- | A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
dtjtrsEnableManagedSpotTraining :: Lens' DescribeTrainingJobResponse (Maybe Bool)
dtjtrsEnableManagedSpotTraining = lens _dtjtrsEnableManagedSpotTraining (\s a -> s {_dtjtrsEnableManagedSpotTraining = a})

-- | The Amazon Resource Name (ARN) of an AutoML job.
dtjtrsAutoMLJobARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
dtjtrsAutoMLJobARN = lens _dtjtrsAutoMLJobARN (\s a -> s {_dtjtrsAutoMLJobARN = a})

-- | Algorithm-specific parameters.
dtjtrsHyperParameters :: Lens' DescribeTrainingJobResponse (HashMap Text (Text))
dtjtrsHyperParameters = lens _dtjtrsHyperParameters (\s a -> s {_dtjtrsHyperParameters = a}) . _Default . _Map

-- | An array of @Channel@ objects that describes each data input channel.
dtjtrsInputDataConfig :: Lens' DescribeTrainingJobResponse (Maybe (NonEmpty Channel))
dtjtrsInputDataConfig = lens _dtjtrsInputDataConfig (\s a -> s {_dtjtrsInputDataConfig = a}) . mapping _List1

-- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
dtjtrsVPCConfig :: Lens' DescribeTrainingJobResponse (Maybe VPCConfig)
dtjtrsVPCConfig = lens _dtjtrsVPCConfig (\s a -> s {_dtjtrsVPCConfig = a})

-- | A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
dtjtrsFinalMetricDataList :: Lens' DescribeTrainingJobResponse [MetricData]
dtjtrsFinalMetricDataList = lens _dtjtrsFinalMetricDataList (\s a -> s {_dtjtrsFinalMetricDataList = a}) . _Default . _Coerce

-- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
dtjtrsOutputDataConfig :: Lens' DescribeTrainingJobResponse (Maybe OutputDataConfig)
dtjtrsOutputDataConfig = lens _dtjtrsOutputDataConfig (\s a -> s {_dtjtrsOutputDataConfig = a})

-- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
dtjtrsTrainingStartTime :: Lens' DescribeTrainingJobResponse (Maybe UTCTime)
dtjtrsTrainingStartTime = lens _dtjtrsTrainingStartTime (\s a -> s {_dtjtrsTrainingStartTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
dtjtrsTuningJobARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
dtjtrsTuningJobARN = lens _dtjtrsTuningJobARN (\s a -> s {_dtjtrsTuningJobARN = a})

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithms in distributed training.
dtjtrsEnableInterContainerTrafficEncryption :: Lens' DescribeTrainingJobResponse (Maybe Bool)
dtjtrsEnableInterContainerTrafficEncryption = lens _dtjtrsEnableInterContainerTrafficEncryption (\s a -> s {_dtjtrsEnableInterContainerTrafficEncryption = a})

-- | Undocumented member.
dtjtrsTensorBoardOutputConfig :: Lens' DescribeTrainingJobResponse (Maybe TensorBoardOutputConfig)
dtjtrsTensorBoardOutputConfig = lens _dtjtrsTensorBoardOutputConfig (\s a -> s {_dtjtrsTensorBoardOutputConfig = a})

-- | The training time in seconds.
dtjtrsTrainingTimeInSeconds :: Lens' DescribeTrainingJobResponse (Maybe Natural)
dtjtrsTrainingTimeInSeconds = lens _dtjtrsTrainingTimeInSeconds (\s a -> s {_dtjtrsTrainingTimeInSeconds = a}) . mapping _Nat

-- | The AWS Identity and Access Management (IAM) role configured for the training job.
dtjtrsRoleARN :: Lens' DescribeTrainingJobResponse (Maybe Text)
dtjtrsRoleARN = lens _dtjtrsRoleARN (\s a -> s {_dtjtrsRoleARN = a})

-- | -- | The response status code.
dtjtrsResponseStatus :: Lens' DescribeTrainingJobResponse Int
dtjtrsResponseStatus = lens _dtjtrsResponseStatus (\s a -> s {_dtjtrsResponseStatus = a})

-- | Name of the model training job.
dtjtrsTrainingJobName :: Lens' DescribeTrainingJobResponse Text
dtjtrsTrainingJobName = lens _dtjtrsTrainingJobName (\s a -> s {_dtjtrsTrainingJobName = a})

-- | The Amazon Resource Name (ARN) of the training job.
dtjtrsTrainingJobARN :: Lens' DescribeTrainingJobResponse Text
dtjtrsTrainingJobARN = lens _dtjtrsTrainingJobARN (\s a -> s {_dtjtrsTrainingJobARN = a})

-- | Information about the Amazon S3 location that is configured for storing model artifacts.
dtjtrsModelArtifacts :: Lens' DescribeTrainingJobResponse ModelArtifacts
dtjtrsModelArtifacts = lens _dtjtrsModelArtifacts (\s a -> s {_dtjtrsModelArtifacts = a})

-- | The status of the training job. Amazon SageMaker provides the following training job statuses:     * @InProgress@ - The training is in progress.     * @Completed@ - The training job has completed.     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.     * @Stopping@ - The training job is stopping.     * @Stopped@ - The training job has stopped. For more detailed information, see @SecondaryStatus@ .
dtjtrsTrainingJobStatus :: Lens' DescribeTrainingJobResponse TrainingJobStatus
dtjtrsTrainingJobStatus = lens _dtjtrsTrainingJobStatus (\s a -> s {_dtjtrsTrainingJobStatus = a})

-- | Provides detailed information about the state of the training job. For detailed information on the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' . Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:     * InProgress    *     * @Starting@ - Starting the training job.     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.     * @Training@ - Training is in progress.     * @Interrupted@ - The job stopped because the managed spot training instances were interrupted.      * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.     * Completed    *     * @Completed@ - The training job has completed.     * Failed    *     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .     * Stopped    *     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.     * @MaxWaitTimeExceeded@ - The job stopped because it exceeded the maximum allowed wait time.     * @Stopped@ - The training job has stopped.     * Stopping    *     * @Stopping@ - Stopping the training job. /Important:/ Valid values for @SecondaryStatus@ are subject to change.  We no longer support the following secondary statuses:     * @LaunchingMLInstances@      * @PreparingTrainingStack@      * @DownloadingTrainingImage@
dtjtrsSecondaryStatus :: Lens' DescribeTrainingJobResponse SecondaryStatus
dtjtrsSecondaryStatus = lens _dtjtrsSecondaryStatus (\s a -> s {_dtjtrsSecondaryStatus = a})

-- | Information about the algorithm used for training, and algorithm metadata.
dtjtrsAlgorithmSpecification :: Lens' DescribeTrainingJobResponse AlgorithmSpecification
dtjtrsAlgorithmSpecification = lens _dtjtrsAlgorithmSpecification (\s a -> s {_dtjtrsAlgorithmSpecification = a})

-- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
dtjtrsResourceConfig :: Lens' DescribeTrainingJobResponse ResourceConfig
dtjtrsResourceConfig = lens _dtjtrsResourceConfig (\s a -> s {_dtjtrsResourceConfig = a})

-- | Specifies a limit to how long a model training job can run. It also specifies the maximum time to wait for a spot instance. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
dtjtrsStoppingCondition :: Lens' DescribeTrainingJobResponse StoppingCondition
dtjtrsStoppingCondition = lens _dtjtrsStoppingCondition (\s a -> s {_dtjtrsStoppingCondition = a})

-- | A timestamp that indicates when the training job was created.
dtjtrsCreationTime :: Lens' DescribeTrainingJobResponse UTCTime
dtjtrsCreationTime = lens _dtjtrsCreationTime (\s a -> s {_dtjtrsCreationTime = a}) . _Time

instance NFData DescribeTrainingJobResponse
