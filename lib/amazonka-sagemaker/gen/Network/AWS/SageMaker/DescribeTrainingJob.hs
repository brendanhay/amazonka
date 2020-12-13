{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    DescribeTrainingJob (..),
    mkDescribeTrainingJob,

    -- ** Request lenses
    dtjTrainingJobName,

    -- * Destructuring the response
    DescribeTrainingJobResponse (..),
    mkDescribeTrainingJobResponse,

    -- ** Response lenses
    dtjfrsCreationTime,
    dtjfrsLabelingJobARN,
    dtjfrsFailureReason,
    dtjfrsSecondaryStatusTransitions,
    dtjfrsModelArtifacts,
    dtjfrsTrainingEndTime,
    dtjfrsBillableTimeInSeconds,
    dtjfrsDebugHookConfig,
    dtjfrsCheckpointConfig,
    dtjfrsStoppingCondition,
    dtjfrsDebugRuleEvaluationStatuses,
    dtjfrsTrainingJobStatus,
    dtjfrsEnableNetworkIsolation,
    dtjfrsExperimentConfig,
    dtjfrsLastModifiedTime,
    dtjfrsDebugRuleConfigurations,
    dtjfrsEnableManagedSpotTraining,
    dtjfrsAutoMLJobARN,
    dtjfrsHyperParameters,
    dtjfrsInputDataConfig,
    dtjfrsVPCConfig,
    dtjfrsTrainingJobARN,
    dtjfrsAlgorithmSpecification,
    dtjfrsFinalMetricDataList,
    dtjfrsOutputDataConfig,
    dtjfrsTrainingStartTime,
    dtjfrsTuningJobARN,
    dtjfrsTrainingJobName,
    dtjfrsResourceConfig,
    dtjfrsEnableInterContainerTrafficEncryption,
    dtjfrsTensorBoardOutputConfig,
    dtjfrsSecondaryStatus,
    dtjfrsTrainingTimeInSeconds,
    dtjfrsRoleARN,
    dtjfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeTrainingJob' smart constructor.
newtype DescribeTrainingJob = DescribeTrainingJob'
  { -- | The name of the training job.
    trainingJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrainingJob' with the minimum fields required to make a request.
--
-- * 'trainingJobName' - The name of the training job.
mkDescribeTrainingJob ::
  -- | 'trainingJobName'
  Lude.Text ->
  DescribeTrainingJob
mkDescribeTrainingJob pTrainingJobName_ =
  DescribeTrainingJob' {trainingJobName = pTrainingJobName_}

-- | The name of the training job.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjTrainingJobName :: Lens.Lens' DescribeTrainingJob Lude.Text
dtjTrainingJobName = Lens.lens (trainingJobName :: DescribeTrainingJob -> Lude.Text) (\s a -> s {trainingJobName = a} :: DescribeTrainingJob)
{-# DEPRECATED dtjTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

instance Lude.AWSRequest DescribeTrainingJob where
  type Rs DescribeTrainingJob = DescribeTrainingJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTrainingJobResponse'
            Lude.<$> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..?> "LabelingJobArn")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "SecondaryStatusTransitions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "ModelArtifacts")
            Lude.<*> (x Lude..?> "TrainingEndTime")
            Lude.<*> (x Lude..?> "BillableTimeInSeconds")
            Lude.<*> (x Lude..?> "DebugHookConfig")
            Lude.<*> (x Lude..?> "CheckpointConfig")
            Lude.<*> (x Lude..:> "StoppingCondition")
            Lude.<*> (x Lude..?> "DebugRuleEvaluationStatuses" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "TrainingJobStatus")
            Lude.<*> (x Lude..?> "EnableNetworkIsolation")
            Lude.<*> (x Lude..?> "ExperimentConfig")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "DebugRuleConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "EnableManagedSpotTraining")
            Lude.<*> (x Lude..?> "AutoMLJobArn")
            Lude.<*> (x Lude..?> "HyperParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "InputDataConfig")
            Lude.<*> (x Lude..?> "VpcConfig")
            Lude.<*> (x Lude..:> "TrainingJobArn")
            Lude.<*> (x Lude..:> "AlgorithmSpecification")
            Lude.<*> (x Lude..?> "FinalMetricDataList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "OutputDataConfig")
            Lude.<*> (x Lude..?> "TrainingStartTime")
            Lude.<*> (x Lude..?> "TuningJobArn")
            Lude.<*> (x Lude..:> "TrainingJobName")
            Lude.<*> (x Lude..:> "ResourceConfig")
            Lude.<*> (x Lude..?> "EnableInterContainerTrafficEncryption")
            Lude.<*> (x Lude..?> "TensorBoardOutputConfig")
            Lude.<*> (x Lude..:> "SecondaryStatus")
            Lude.<*> (x Lude..?> "TrainingTimeInSeconds")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrainingJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeTrainingJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTrainingJob where
  toJSON DescribeTrainingJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("TrainingJobName" Lude..= trainingJobName)]
      )

instance Lude.ToPath DescribeTrainingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrainingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTrainingJobResponse' smart constructor.
data DescribeTrainingJobResponse = DescribeTrainingJobResponse'
  { -- | A timestamp that indicates when the training job was created.
    creationTime :: Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
    labelingJobARN :: Lude.Maybe Lude.Text,
    -- | If the training job failed, the reason it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | A history of all of the secondary statuses that the training job has transitioned through.
    secondaryStatusTransitions :: Lude.Maybe [SecondaryStatusTransition],
    -- | Information about the Amazon S3 location that is configured for storing model artifacts.
    modelArtifacts :: ModelArtifacts,
    -- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
    trainingEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The billable time in seconds.
    --
    -- You can calculate the savings from using managed spot training using the formula @(1 - BillableTimeInSeconds / TrainingTimeInSeconds) * 100@ . For example, if @BillableTimeInSeconds@ is 100 and @TrainingTimeInSeconds@ is 500, the savings is 80%.
    billableTimeInSeconds :: Lude.Maybe Lude.Natural,
    debugHookConfig :: Lude.Maybe DebugHookConfig,
    checkpointConfig :: Lude.Maybe CheckpointConfig,
    -- | Specifies a limit to how long a model training job can run. It also specifies the maximum time to wait for a spot instance. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
    stoppingCondition :: StoppingCondition,
    -- | Status about the debug rule evaluation.
    debugRuleEvaluationStatuses :: Lude.Maybe [DebugRuleEvaluationStatus],
    -- | The status of the training job.
    --
    -- Amazon SageMaker provides the following training job statuses:
    --
    --     * @InProgress@ - The training is in progress.
    --
    --
    --     * @Completed@ - The training job has completed.
    --
    --
    --     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.
    --
    --
    --     * @Stopping@ - The training job is stopping.
    --
    --
    --     * @Stopped@ - The training job has stopped.
    --
    --
    -- For more detailed information, see @SecondaryStatus@ .
    trainingJobStatus :: TrainingJobStatus,
    -- | If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
    enableNetworkIsolation :: Lude.Maybe Lude.Bool,
    experimentConfig :: Lude.Maybe ExperimentConfig,
    -- | A timestamp that indicates when the status of the training job was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | Configuration information for debugging rules.
    debugRuleConfigurations :: Lude.Maybe [DebugRuleConfiguration],
    -- | A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
    enableManagedSpotTraining :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) of an AutoML job.
    autoMLJobARN :: Lude.Maybe Lude.Text,
    -- | Algorithm-specific parameters.
    hyperParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | An array of @Channel@ objects that describes each data input channel.
    inputDataConfig :: Lude.Maybe (Lude.NonEmpty Channel),
    -- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobARN :: Lude.Text,
    -- | Information about the algorithm used for training, and algorithm metadata.
    algorithmSpecification :: AlgorithmSpecification,
    -- | A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
    finalMetricDataList :: Lude.Maybe [MetricData],
    -- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
    outputDataConfig :: Lude.Maybe OutputDataConfig,
    -- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
    trainingStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
    tuningJobARN :: Lude.Maybe Lude.Text,
    -- | Name of the model training job.
    trainingJobName :: Lude.Text,
    -- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
    resourceConfig :: ResourceConfig,
    -- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithms in distributed training.
    enableInterContainerTrafficEncryption :: Lude.Maybe Lude.Bool,
    tensorBoardOutputConfig :: Lude.Maybe TensorBoardOutputConfig,
    -- | Provides detailed information about the state of the training job. For detailed information on the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' .
    --
    -- Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:
    --
    --     * InProgress
    --
    --     *
    --     * @Starting@ - Starting the training job.
    --
    --
    --     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.
    --
    --
    --     * @Training@ - Training is in progress.
    --
    --
    --     * @Interrupted@ - The job stopped because the managed spot training instances were interrupted.
    --
    --
    --     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.
    --
    --
    --
    --
    --     * Completed
    --
    --     *
    --     * @Completed@ - The training job has completed.
    --
    --
    --
    --
    --     * Failed
    --
    --     *
    --     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .
    --
    --
    --
    --
    --     * Stopped
    --
    --     *
    --     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.
    --
    --
    --     * @MaxWaitTimeExceeded@ - The job stopped because it exceeded the maximum allowed wait time.
    --
    --
    --     * @Stopped@ - The training job has stopped.
    --
    --
    --
    --
    --     * Stopping
    --
    --     *
    --     * @Stopping@ - Stopping the training job.
    --
    --
    --
    --
    -- /Important:/ Valid values for @SecondaryStatus@ are subject to change.
    -- We no longer support the following secondary statuses:
    --
    --     * @LaunchingMLInstances@
    --
    --
    --     * @PreparingTrainingStack@
    --
    --
    --     * @DownloadingTrainingImage@
    secondaryStatus :: SecondaryStatus,
    -- | The training time in seconds.
    trainingTimeInSeconds :: Lude.Maybe Lude.Natural,
    -- | The AWS Identity and Access Management (IAM) role configured for the training job.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrainingJobResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that indicates when the training job was created.
-- * 'labelingJobARN' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
-- * 'failureReason' - If the training job failed, the reason it failed.
-- * 'secondaryStatusTransitions' - A history of all of the secondary statuses that the training job has transitioned through.
-- * 'modelArtifacts' - Information about the Amazon S3 location that is configured for storing model artifacts.
-- * 'trainingEndTime' - Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
-- * 'billableTimeInSeconds' - The billable time in seconds.
--
-- You can calculate the savings from using managed spot training using the formula @(1 - BillableTimeInSeconds / TrainingTimeInSeconds) * 100@ . For example, if @BillableTimeInSeconds@ is 100 and @TrainingTimeInSeconds@ is 500, the savings is 80%.
-- * 'debugHookConfig' -
-- * 'checkpointConfig' -
-- * 'stoppingCondition' - Specifies a limit to how long a model training job can run. It also specifies the maximum time to wait for a spot instance. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
-- * 'debugRuleEvaluationStatuses' - Status about the debug rule evaluation.
-- * 'trainingJobStatus' - The status of the training job.
--
-- Amazon SageMaker provides the following training job statuses:
--
--     * @InProgress@ - The training is in progress.
--
--
--     * @Completed@ - The training job has completed.
--
--
--     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.
--
--
--     * @Stopping@ - The training job is stopping.
--
--
--     * @Stopped@ - The training job has stopped.
--
--
-- For more detailed information, see @SecondaryStatus@ .
-- * 'enableNetworkIsolation' - If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
-- * 'experimentConfig' -
-- * 'lastModifiedTime' - A timestamp that indicates when the status of the training job was last modified.
-- * 'debugRuleConfigurations' - Configuration information for debugging rules.
-- * 'enableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
-- * 'autoMLJobARN' - The Amazon Resource Name (ARN) of an AutoML job.
-- * 'hyperParameters' - Algorithm-specific parameters.
-- * 'inputDataConfig' - An array of @Channel@ objects that describes each data input channel.
-- * 'vpcConfig' - A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
-- * 'trainingJobARN' - The Amazon Resource Name (ARN) of the training job.
-- * 'algorithmSpecification' - Information about the algorithm used for training, and algorithm metadata.
-- * 'finalMetricDataList' - A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
-- * 'outputDataConfig' - The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
-- * 'trainingStartTime' - Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
-- * 'tuningJobARN' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
-- * 'trainingJobName' - Name of the model training job.
-- * 'resourceConfig' - Resources, including ML compute instances and ML storage volumes, that are configured for model training.
-- * 'enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithms in distributed training.
-- * 'tensorBoardOutputConfig' -
-- * 'secondaryStatus' - Provides detailed information about the state of the training job. For detailed information on the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' .
--
-- Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:
--
--     * InProgress
--
--     *
--     * @Starting@ - Starting the training job.
--
--
--     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.
--
--
--     * @Training@ - Training is in progress.
--
--
--     * @Interrupted@ - The job stopped because the managed spot training instances were interrupted.
--
--
--     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.
--
--
--
--
--     * Completed
--
--     *
--     * @Completed@ - The training job has completed.
--
--
--
--
--     * Failed
--
--     *
--     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .
--
--
--
--
--     * Stopped
--
--     *
--     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.
--
--
--     * @MaxWaitTimeExceeded@ - The job stopped because it exceeded the maximum allowed wait time.
--
--
--     * @Stopped@ - The training job has stopped.
--
--
--
--
--     * Stopping
--
--     *
--     * @Stopping@ - Stopping the training job.
--
--
--
--
-- /Important:/ Valid values for @SecondaryStatus@ are subject to change.
-- We no longer support the following secondary statuses:
--
--     * @LaunchingMLInstances@
--
--
--     * @PreparingTrainingStack@
--
--
--     * @DownloadingTrainingImage@
--
--
-- * 'trainingTimeInSeconds' - The training time in seconds.
-- * 'roleARN' - The AWS Identity and Access Management (IAM) role configured for the training job.
-- * 'responseStatus' - The response status code.
mkDescribeTrainingJobResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'modelArtifacts'
  ModelArtifacts ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  -- | 'trainingJobStatus'
  TrainingJobStatus ->
  -- | 'trainingJobARN'
  Lude.Text ->
  -- | 'algorithmSpecification'
  AlgorithmSpecification ->
  -- | 'trainingJobName'
  Lude.Text ->
  -- | 'resourceConfig'
  ResourceConfig ->
  -- | 'secondaryStatus'
  SecondaryStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrainingJobResponse
mkDescribeTrainingJobResponse
  pCreationTime_
  pModelArtifacts_
  pStoppingCondition_
  pTrainingJobStatus_
  pTrainingJobARN_
  pAlgorithmSpecification_
  pTrainingJobName_
  pResourceConfig_
  pSecondaryStatus_
  pResponseStatus_ =
    DescribeTrainingJobResponse'
      { creationTime = pCreationTime_,
        labelingJobARN = Lude.Nothing,
        failureReason = Lude.Nothing,
        secondaryStatusTransitions = Lude.Nothing,
        modelArtifacts = pModelArtifacts_,
        trainingEndTime = Lude.Nothing,
        billableTimeInSeconds = Lude.Nothing,
        debugHookConfig = Lude.Nothing,
        checkpointConfig = Lude.Nothing,
        stoppingCondition = pStoppingCondition_,
        debugRuleEvaluationStatuses = Lude.Nothing,
        trainingJobStatus = pTrainingJobStatus_,
        enableNetworkIsolation = Lude.Nothing,
        experimentConfig = Lude.Nothing,
        lastModifiedTime = Lude.Nothing,
        debugRuleConfigurations = Lude.Nothing,
        enableManagedSpotTraining = Lude.Nothing,
        autoMLJobARN = Lude.Nothing,
        hyperParameters = Lude.Nothing,
        inputDataConfig = Lude.Nothing,
        vpcConfig = Lude.Nothing,
        trainingJobARN = pTrainingJobARN_,
        algorithmSpecification = pAlgorithmSpecification_,
        finalMetricDataList = Lude.Nothing,
        outputDataConfig = Lude.Nothing,
        trainingStartTime = Lude.Nothing,
        tuningJobARN = Lude.Nothing,
        trainingJobName = pTrainingJobName_,
        resourceConfig = pResourceConfig_,
        enableInterContainerTrafficEncryption = Lude.Nothing,
        tensorBoardOutputConfig = Lude.Nothing,
        secondaryStatus = pSecondaryStatus_,
        trainingTimeInSeconds = Lude.Nothing,
        roleARN = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | A timestamp that indicates when the training job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsCreationTime :: Lens.Lens' DescribeTrainingJobResponse Lude.Timestamp
dtjfrsCreationTime = Lens.lens (creationTime :: DescribeTrainingJobResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
--
-- /Note:/ Consider using 'labelingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsLabelingJobARN :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Text)
dtjfrsLabelingJobARN = Lens.lens (labelingJobARN :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {labelingJobARN = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsLabelingJobARN "Use generic-lens or generic-optics with 'labelingJobARN' instead." #-}

-- | If the training job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsFailureReason :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Text)
dtjfrsFailureReason = Lens.lens (failureReason :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | A history of all of the secondary statuses that the training job has transitioned through.
--
-- /Note:/ Consider using 'secondaryStatusTransitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsSecondaryStatusTransitions :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe [SecondaryStatusTransition])
dtjfrsSecondaryStatusTransitions = Lens.lens (secondaryStatusTransitions :: DescribeTrainingJobResponse -> Lude.Maybe [SecondaryStatusTransition]) (\s a -> s {secondaryStatusTransitions = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsSecondaryStatusTransitions "Use generic-lens or generic-optics with 'secondaryStatusTransitions' instead." #-}

-- | Information about the Amazon S3 location that is configured for storing model artifacts.
--
-- /Note:/ Consider using 'modelArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsModelArtifacts :: Lens.Lens' DescribeTrainingJobResponse ModelArtifacts
dtjfrsModelArtifacts = Lens.lens (modelArtifacts :: DescribeTrainingJobResponse -> ModelArtifacts) (\s a -> s {modelArtifacts = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsModelArtifacts "Use generic-lens or generic-optics with 'modelArtifacts' instead." #-}

-- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsTrainingEndTime :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Timestamp)
dtjfrsTrainingEndTime = Lens.lens (trainingEndTime :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingEndTime = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsTrainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead." #-}

-- | The billable time in seconds.
--
-- You can calculate the savings from using managed spot training using the formula @(1 - BillableTimeInSeconds / TrainingTimeInSeconds) * 100@ . For example, if @BillableTimeInSeconds@ is 100 and @TrainingTimeInSeconds@ is 500, the savings is 80%.
--
-- /Note:/ Consider using 'billableTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsBillableTimeInSeconds :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Natural)
dtjfrsBillableTimeInSeconds = Lens.lens (billableTimeInSeconds :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Natural) (\s a -> s {billableTimeInSeconds = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsBillableTimeInSeconds "Use generic-lens or generic-optics with 'billableTimeInSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'debugHookConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsDebugHookConfig :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe DebugHookConfig)
dtjfrsDebugHookConfig = Lens.lens (debugHookConfig :: DescribeTrainingJobResponse -> Lude.Maybe DebugHookConfig) (\s a -> s {debugHookConfig = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsDebugHookConfig "Use generic-lens or generic-optics with 'debugHookConfig' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsCheckpointConfig :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe CheckpointConfig)
dtjfrsCheckpointConfig = Lens.lens (checkpointConfig :: DescribeTrainingJobResponse -> Lude.Maybe CheckpointConfig) (\s a -> s {checkpointConfig = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsCheckpointConfig "Use generic-lens or generic-optics with 'checkpointConfig' instead." #-}

-- | Specifies a limit to how long a model training job can run. It also specifies the maximum time to wait for a spot instance. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsStoppingCondition :: Lens.Lens' DescribeTrainingJobResponse StoppingCondition
dtjfrsStoppingCondition = Lens.lens (stoppingCondition :: DescribeTrainingJobResponse -> StoppingCondition) (\s a -> s {stoppingCondition = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | Status about the debug rule evaluation.
--
-- /Note:/ Consider using 'debugRuleEvaluationStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsDebugRuleEvaluationStatuses :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe [DebugRuleEvaluationStatus])
dtjfrsDebugRuleEvaluationStatuses = Lens.lens (debugRuleEvaluationStatuses :: DescribeTrainingJobResponse -> Lude.Maybe [DebugRuleEvaluationStatus]) (\s a -> s {debugRuleEvaluationStatuses = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsDebugRuleEvaluationStatuses "Use generic-lens or generic-optics with 'debugRuleEvaluationStatuses' instead." #-}

-- | The status of the training job.
--
-- Amazon SageMaker provides the following training job statuses:
--
--     * @InProgress@ - The training is in progress.
--
--
--     * @Completed@ - The training job has completed.
--
--
--     * @Failed@ - The training job has failed. To see the reason for the failure, see the @FailureReason@ field in the response to a @DescribeTrainingJobResponse@ call.
--
--
--     * @Stopping@ - The training job is stopping.
--
--
--     * @Stopped@ - The training job has stopped.
--
--
-- For more detailed information, see @SecondaryStatus@ .
--
-- /Note:/ Consider using 'trainingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsTrainingJobStatus :: Lens.Lens' DescribeTrainingJobResponse TrainingJobStatus
dtjfrsTrainingJobStatus = Lens.lens (trainingJobStatus :: DescribeTrainingJobResponse -> TrainingJobStatus) (\s a -> s {trainingJobStatus = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsTrainingJobStatus "Use generic-lens or generic-optics with 'trainingJobStatus' instead." #-}

-- | If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsEnableNetworkIsolation :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Bool)
dtjfrsEnableNetworkIsolation = Lens.lens (enableNetworkIsolation :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enableNetworkIsolation = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsExperimentConfig :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe ExperimentConfig)
dtjfrsExperimentConfig = Lens.lens (experimentConfig :: DescribeTrainingJobResponse -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | A timestamp that indicates when the status of the training job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsLastModifiedTime :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Timestamp)
dtjfrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Configuration information for debugging rules.
--
-- /Note:/ Consider using 'debugRuleConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsDebugRuleConfigurations :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe [DebugRuleConfiguration])
dtjfrsDebugRuleConfigurations = Lens.lens (debugRuleConfigurations :: DescribeTrainingJobResponse -> Lude.Maybe [DebugRuleConfiguration]) (\s a -> s {debugRuleConfigurations = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsDebugRuleConfigurations "Use generic-lens or generic-optics with 'debugRuleConfigurations' instead." #-}

-- | A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
--
-- /Note:/ Consider using 'enableManagedSpotTraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsEnableManagedSpotTraining :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Bool)
dtjfrsEnableManagedSpotTraining = Lens.lens (enableManagedSpotTraining :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enableManagedSpotTraining = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsEnableManagedSpotTraining "Use generic-lens or generic-optics with 'enableManagedSpotTraining' instead." #-}

-- | The Amazon Resource Name (ARN) of an AutoML job.
--
-- /Note:/ Consider using 'autoMLJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsAutoMLJobARN :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Text)
dtjfrsAutoMLJobARN = Lens.lens (autoMLJobARN :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {autoMLJobARN = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsAutoMLJobARN "Use generic-lens or generic-optics with 'autoMLJobARN' instead." #-}

-- | Algorithm-specific parameters.
--
-- /Note:/ Consider using 'hyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsHyperParameters :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dtjfrsHyperParameters = Lens.lens (hyperParameters :: DescribeTrainingJobResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {hyperParameters = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsHyperParameters "Use generic-lens or generic-optics with 'hyperParameters' instead." #-}

-- | An array of @Channel@ objects that describes each data input channel.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsInputDataConfig :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe (Lude.NonEmpty Channel))
dtjfrsInputDataConfig = Lens.lens (inputDataConfig :: DescribeTrainingJobResponse -> Lude.Maybe (Lude.NonEmpty Channel)) (\s a -> s {inputDataConfig = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsVPCConfig :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe VPCConfig)
dtjfrsVPCConfig = Lens.lens (vpcConfig :: DescribeTrainingJobResponse -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsTrainingJobARN :: Lens.Lens' DescribeTrainingJobResponse Lude.Text
dtjfrsTrainingJobARN = Lens.lens (trainingJobARN :: DescribeTrainingJobResponse -> Lude.Text) (\s a -> s {trainingJobARN = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsTrainingJobARN "Use generic-lens or generic-optics with 'trainingJobARN' instead." #-}

-- | Information about the algorithm used for training, and algorithm metadata.
--
-- /Note:/ Consider using 'algorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsAlgorithmSpecification :: Lens.Lens' DescribeTrainingJobResponse AlgorithmSpecification
dtjfrsAlgorithmSpecification = Lens.lens (algorithmSpecification :: DescribeTrainingJobResponse -> AlgorithmSpecification) (\s a -> s {algorithmSpecification = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsAlgorithmSpecification "Use generic-lens or generic-optics with 'algorithmSpecification' instead." #-}

-- | A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
--
-- /Note:/ Consider using 'finalMetricDataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsFinalMetricDataList :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe [MetricData])
dtjfrsFinalMetricDataList = Lens.lens (finalMetricDataList :: DescribeTrainingJobResponse -> Lude.Maybe [MetricData]) (\s a -> s {finalMetricDataList = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsFinalMetricDataList "Use generic-lens or generic-optics with 'finalMetricDataList' instead." #-}

-- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsOutputDataConfig :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe OutputDataConfig)
dtjfrsOutputDataConfig = Lens.lens (outputDataConfig :: DescribeTrainingJobResponse -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
--
-- /Note:/ Consider using 'trainingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsTrainingStartTime :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Timestamp)
dtjfrsTrainingStartTime = Lens.lens (trainingStartTime :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingStartTime = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsTrainingStartTime "Use generic-lens or generic-optics with 'trainingStartTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
--
-- /Note:/ Consider using 'tuningJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsTuningJobARN :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Text)
dtjfrsTuningJobARN = Lens.lens (tuningJobARN :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {tuningJobARN = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsTuningJobARN "Use generic-lens or generic-optics with 'tuningJobARN' instead." #-}

-- | Name of the model training job.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsTrainingJobName :: Lens.Lens' DescribeTrainingJobResponse Lude.Text
dtjfrsTrainingJobName = Lens.lens (trainingJobName :: DescribeTrainingJobResponse -> Lude.Text) (\s a -> s {trainingJobName = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

-- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsResourceConfig :: Lens.Lens' DescribeTrainingJobResponse ResourceConfig
dtjfrsResourceConfig = Lens.lens (resourceConfig :: DescribeTrainingJobResponse -> ResourceConfig) (\s a -> s {resourceConfig = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsResourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead." #-}

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithms in distributed training.
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsEnableInterContainerTrafficEncryption :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Bool)
dtjfrsEnableInterContainerTrafficEncryption = Lens.lens (enableInterContainerTrafficEncryption :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enableInterContainerTrafficEncryption = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tensorBoardOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsTensorBoardOutputConfig :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe TensorBoardOutputConfig)
dtjfrsTensorBoardOutputConfig = Lens.lens (tensorBoardOutputConfig :: DescribeTrainingJobResponse -> Lude.Maybe TensorBoardOutputConfig) (\s a -> s {tensorBoardOutputConfig = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsTensorBoardOutputConfig "Use generic-lens or generic-optics with 'tensorBoardOutputConfig' instead." #-}

-- | Provides detailed information about the state of the training job. For detailed information on the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' .
--
-- Amazon SageMaker provides primary statuses and secondary statuses that apply to each of them:
--
--     * InProgress
--
--     *
--     * @Starting@ - Starting the training job.
--
--
--     * @Downloading@ - An optional stage for algorithms that support @File@ training input mode. It indicates that data is being downloaded to the ML storage volumes.
--
--
--     * @Training@ - Training is in progress.
--
--
--     * @Interrupted@ - The job stopped because the managed spot training instances were interrupted.
--
--
--     * @Uploading@ - Training is complete and the model artifacts are being uploaded to the S3 location.
--
--
--
--
--     * Completed
--
--     *
--     * @Completed@ - The training job has completed.
--
--
--
--
--     * Failed
--
--     *
--     * @Failed@ - The training job has failed. The reason for the failure is returned in the @FailureReason@ field of @DescribeTrainingJobResponse@ .
--
--
--
--
--     * Stopped
--
--     *
--     * @MaxRuntimeExceeded@ - The job stopped because it exceeded the maximum allowed runtime.
--
--
--     * @MaxWaitTimeExceeded@ - The job stopped because it exceeded the maximum allowed wait time.
--
--
--     * @Stopped@ - The training job has stopped.
--
--
--
--
--     * Stopping
--
--     *
--     * @Stopping@ - Stopping the training job.
--
--
--
--
-- /Important:/ Valid values for @SecondaryStatus@ are subject to change.
-- We no longer support the following secondary statuses:
--
--     * @LaunchingMLInstances@
--
--
--     * @PreparingTrainingStack@
--
--
--     * @DownloadingTrainingImage@
--
--
--
-- /Note:/ Consider using 'secondaryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsSecondaryStatus :: Lens.Lens' DescribeTrainingJobResponse SecondaryStatus
dtjfrsSecondaryStatus = Lens.lens (secondaryStatus :: DescribeTrainingJobResponse -> SecondaryStatus) (\s a -> s {secondaryStatus = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsSecondaryStatus "Use generic-lens or generic-optics with 'secondaryStatus' instead." #-}

-- | The training time in seconds.
--
-- /Note:/ Consider using 'trainingTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsTrainingTimeInSeconds :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Natural)
dtjfrsTrainingTimeInSeconds = Lens.lens (trainingTimeInSeconds :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Natural) (\s a -> s {trainingTimeInSeconds = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsTrainingTimeInSeconds "Use generic-lens or generic-optics with 'trainingTimeInSeconds' instead." #-}

-- | The AWS Identity and Access Management (IAM) role configured for the training job.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsRoleARN :: Lens.Lens' DescribeTrainingJobResponse (Lude.Maybe Lude.Text)
dtjfrsRoleARN = Lens.lens (roleARN :: DescribeTrainingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjfrsResponseStatus :: Lens.Lens' DescribeTrainingJobResponse Lude.Int
dtjfrsResponseStatus = Lens.lens (responseStatus :: DescribeTrainingJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrainingJobResponse)
{-# DEPRECATED dtjfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
