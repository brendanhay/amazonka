{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJob
  ( TrainingJob (..),

    -- * Smart constructor
    mkTrainingJob,

    -- * Lenses
    tjCreationTime,
    tjLabelingJobARN,
    tjFailureReason,
    tjSecondaryStatusTransitions,
    tjModelArtifacts,
    tjTrainingEndTime,
    tjBillableTimeInSeconds,
    tjDebugHookConfig,
    tjCheckpointConfig,
    tjStoppingCondition,
    tjDebugRuleEvaluationStatuses,
    tjTrainingJobStatus,
    tjEnableNetworkIsolation,
    tjExperimentConfig,
    tjLastModifiedTime,
    tjDebugRuleConfigurations,
    tjEnableManagedSpotTraining,
    tjAutoMLJobARN,
    tjHyperParameters,
    tjInputDataConfig,
    tjVPCConfig,
    tjTrainingJobARN,
    tjAlgorithmSpecification,
    tjFinalMetricDataList,
    tjOutputDataConfig,
    tjTrainingStartTime,
    tjTuningJobARN,
    tjTrainingJobName,
    tjResourceConfig,
    tjEnableInterContainerTrafficEncryption,
    tjTensorBoardOutputConfig,
    tjSecondaryStatus,
    tjTags,
    tjTrainingTimeInSeconds,
    tjRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AlgorithmSpecification
import Network.AWS.SageMaker.Types.Channel
import Network.AWS.SageMaker.Types.CheckpointConfig
import Network.AWS.SageMaker.Types.DebugHookConfig
import Network.AWS.SageMaker.Types.DebugRuleConfiguration
import Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus
import Network.AWS.SageMaker.Types.ExperimentConfig
import Network.AWS.SageMaker.Types.MetricData
import Network.AWS.SageMaker.Types.ModelArtifacts
import Network.AWS.SageMaker.Types.OutputDataConfig
import Network.AWS.SageMaker.Types.ResourceConfig
import Network.AWS.SageMaker.Types.SecondaryStatus
import Network.AWS.SageMaker.Types.SecondaryStatusTransition
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TensorBoardOutputConfig
import Network.AWS.SageMaker.Types.TrainingJobStatus
import Network.AWS.SageMaker.Types.VPCConfig

-- | Contains information about a training job.
--
-- /See:/ 'mkTrainingJob' smart constructor.
data TrainingJob = TrainingJob'
  { -- | A timestamp that indicates when the training job was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the labeling job.
    labelingJobARN :: Lude.Maybe Lude.Text,
    -- | If the training job failed, the reason it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | A history of all of the secondary statuses that the training job has transitioned through.
    secondaryStatusTransitions :: Lude.Maybe [SecondaryStatusTransition],
    -- | Information about the Amazon S3 location that is configured for storing model artifacts.
    modelArtifacts :: Lude.Maybe ModelArtifacts,
    -- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
    trainingEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The billable time in seconds.
    billableTimeInSeconds :: Lude.Maybe Lude.Natural,
    debugHookConfig :: Lude.Maybe DebugHookConfig,
    checkpointConfig :: Lude.Maybe CheckpointConfig,
    -- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
    stoppingCondition :: Lude.Maybe StoppingCondition,
    -- | Information about the evaluation status of the rules for the training job.
    debugRuleEvaluationStatuses :: Lude.Maybe [DebugRuleEvaluationStatus],
    -- | The status of the training job.
    --
    -- Training job statuses are:
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
    trainingJobStatus :: Lude.Maybe TrainingJobStatus,
    -- | If the @TrainingJob@ was created with network isolation, the value is set to @true@ . If network isolation is enabled, nodes can't communicate beyond the VPC they run in.
    enableNetworkIsolation :: Lude.Maybe Lude.Bool,
    experimentConfig :: Lude.Maybe ExperimentConfig,
    -- | A timestamp that indicates when the status of the training job was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | Information about the debug rule configuration.
    debugRuleConfigurations :: Lude.Maybe [DebugRuleConfiguration],
    -- | When true, enables managed spot training using Amazon EC2 Spot instances to run training jobs instead of on-demand instances. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training> .
    enableManagedSpotTraining :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) of the job.
    autoMLJobARN :: Lude.Maybe Lude.Text,
    -- | Algorithm-specific parameters.
    hyperParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | An array of @Channel@ objects that describes each data input channel.
    inputDataConfig :: Lude.Maybe (Lude.NonEmpty Channel),
    -- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobARN :: Lude.Maybe Lude.Text,
    -- | Information about the algorithm used for training, and algorithm metadata.
    algorithmSpecification :: Lude.Maybe AlgorithmSpecification,
    -- | A list of final metric values that are set when the training job completes. Used only if the training job was configured to use metrics.
    finalMetricDataList :: Lude.Maybe [MetricData],
    -- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
    outputDataConfig :: Lude.Maybe OutputDataConfig,
    -- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
    trainingStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
    tuningJobARN :: Lude.Maybe Lude.Text,
    -- | The name of the training job.
    trainingJobName :: Lude.Maybe Lude.Text,
    -- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
    resourceConfig :: Lude.Maybe ResourceConfig,
    -- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
    enableInterContainerTrafficEncryption :: Lude.Maybe Lude.Bool,
    tensorBoardOutputConfig :: Lude.Maybe TensorBoardOutputConfig,
    -- | Provides detailed information about the state of the training job. For detailed information about the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' .
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
    secondaryStatus :: Lude.Maybe SecondaryStatus,
    -- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Lude.Maybe [Tag],
    -- | The training time in seconds.
    trainingTimeInSeconds :: Lude.Maybe Lude.Natural,
    -- | The AWS Identity and Access Management (IAM) role configured for the training job.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrainingJob' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that indicates when the training job was created.
-- * 'labelingJobARN' - The Amazon Resource Name (ARN) of the labeling job.
-- * 'failureReason' - If the training job failed, the reason it failed.
-- * 'secondaryStatusTransitions' - A history of all of the secondary statuses that the training job has transitioned through.
-- * 'modelArtifacts' - Information about the Amazon S3 location that is configured for storing model artifacts.
-- * 'trainingEndTime' - Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
-- * 'billableTimeInSeconds' - The billable time in seconds.
-- * 'debugHookConfig' -
-- * 'checkpointConfig' -
-- * 'stoppingCondition' - Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
-- * 'debugRuleEvaluationStatuses' - Information about the evaluation status of the rules for the training job.
-- * 'trainingJobStatus' - The status of the training job.
--
-- Training job statuses are:
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
-- * 'enableNetworkIsolation' - If the @TrainingJob@ was created with network isolation, the value is set to @true@ . If network isolation is enabled, nodes can't communicate beyond the VPC they run in.
-- * 'experimentConfig' -
-- * 'lastModifiedTime' - A timestamp that indicates when the status of the training job was last modified.
-- * 'debugRuleConfigurations' - Information about the debug rule configuration.
-- * 'enableManagedSpotTraining' - When true, enables managed spot training using Amazon EC2 Spot instances to run training jobs instead of on-demand instances. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training> .
-- * 'autoMLJobARN' - The Amazon Resource Name (ARN) of the job.
-- * 'hyperParameters' - Algorithm-specific parameters.
-- * 'inputDataConfig' - An array of @Channel@ objects that describes each data input channel.
-- * 'vpcConfig' - A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
-- * 'trainingJobARN' - The Amazon Resource Name (ARN) of the training job.
-- * 'algorithmSpecification' - Information about the algorithm used for training, and algorithm metadata.
-- * 'finalMetricDataList' - A list of final metric values that are set when the training job completes. Used only if the training job was configured to use metrics.
-- * 'outputDataConfig' - The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
-- * 'trainingStartTime' - Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
-- * 'tuningJobARN' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
-- * 'trainingJobName' - The name of the training job.
-- * 'resourceConfig' - Resources, including ML compute instances and ML storage volumes, that are configured for model training.
-- * 'enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
-- * 'tensorBoardOutputConfig' -
-- * 'secondaryStatus' - Provides detailed information about the state of the training job. For detailed information about the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' .
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
-- * 'tags' - An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
-- * 'trainingTimeInSeconds' - The training time in seconds.
-- * 'roleARN' - The AWS Identity and Access Management (IAM) role configured for the training job.
mkTrainingJob ::
  TrainingJob
mkTrainingJob =
  TrainingJob'
    { creationTime = Lude.Nothing,
      labelingJobARN = Lude.Nothing,
      failureReason = Lude.Nothing,
      secondaryStatusTransitions = Lude.Nothing,
      modelArtifacts = Lude.Nothing,
      trainingEndTime = Lude.Nothing,
      billableTimeInSeconds = Lude.Nothing,
      debugHookConfig = Lude.Nothing,
      checkpointConfig = Lude.Nothing,
      stoppingCondition = Lude.Nothing,
      debugRuleEvaluationStatuses = Lude.Nothing,
      trainingJobStatus = Lude.Nothing,
      enableNetworkIsolation = Lude.Nothing,
      experimentConfig = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      debugRuleConfigurations = Lude.Nothing,
      enableManagedSpotTraining = Lude.Nothing,
      autoMLJobARN = Lude.Nothing,
      hyperParameters = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      trainingJobARN = Lude.Nothing,
      algorithmSpecification = Lude.Nothing,
      finalMetricDataList = Lude.Nothing,
      outputDataConfig = Lude.Nothing,
      trainingStartTime = Lude.Nothing,
      tuningJobARN = Lude.Nothing,
      trainingJobName = Lude.Nothing,
      resourceConfig = Lude.Nothing,
      enableInterContainerTrafficEncryption = Lude.Nothing,
      tensorBoardOutputConfig = Lude.Nothing,
      secondaryStatus = Lude.Nothing,
      tags = Lude.Nothing,
      trainingTimeInSeconds = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | A timestamp that indicates when the training job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjCreationTime :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Timestamp)
tjCreationTime = Lens.lens (creationTime :: TrainingJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: TrainingJob)
{-# DEPRECATED tjCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the labeling job.
--
-- /Note:/ Consider using 'labelingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjLabelingJobARN :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Text)
tjLabelingJobARN = Lens.lens (labelingJobARN :: TrainingJob -> Lude.Maybe Lude.Text) (\s a -> s {labelingJobARN = a} :: TrainingJob)
{-# DEPRECATED tjLabelingJobARN "Use generic-lens or generic-optics with 'labelingJobARN' instead." #-}

-- | If the training job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjFailureReason :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Text)
tjFailureReason = Lens.lens (failureReason :: TrainingJob -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: TrainingJob)
{-# DEPRECATED tjFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | A history of all of the secondary statuses that the training job has transitioned through.
--
-- /Note:/ Consider using 'secondaryStatusTransitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjSecondaryStatusTransitions :: Lens.Lens' TrainingJob (Lude.Maybe [SecondaryStatusTransition])
tjSecondaryStatusTransitions = Lens.lens (secondaryStatusTransitions :: TrainingJob -> Lude.Maybe [SecondaryStatusTransition]) (\s a -> s {secondaryStatusTransitions = a} :: TrainingJob)
{-# DEPRECATED tjSecondaryStatusTransitions "Use generic-lens or generic-optics with 'secondaryStatusTransitions' instead." #-}

-- | Information about the Amazon S3 location that is configured for storing model artifacts.
--
-- /Note:/ Consider using 'modelArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjModelArtifacts :: Lens.Lens' TrainingJob (Lude.Maybe ModelArtifacts)
tjModelArtifacts = Lens.lens (modelArtifacts :: TrainingJob -> Lude.Maybe ModelArtifacts) (\s a -> s {modelArtifacts = a} :: TrainingJob)
{-# DEPRECATED tjModelArtifacts "Use generic-lens or generic-optics with 'modelArtifacts' instead." #-}

-- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTrainingEndTime :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Timestamp)
tjTrainingEndTime = Lens.lens (trainingEndTime :: TrainingJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingEndTime = a} :: TrainingJob)
{-# DEPRECATED tjTrainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead." #-}

-- | The billable time in seconds.
--
-- /Note:/ Consider using 'billableTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjBillableTimeInSeconds :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Natural)
tjBillableTimeInSeconds = Lens.lens (billableTimeInSeconds :: TrainingJob -> Lude.Maybe Lude.Natural) (\s a -> s {billableTimeInSeconds = a} :: TrainingJob)
{-# DEPRECATED tjBillableTimeInSeconds "Use generic-lens or generic-optics with 'billableTimeInSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'debugHookConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjDebugHookConfig :: Lens.Lens' TrainingJob (Lude.Maybe DebugHookConfig)
tjDebugHookConfig = Lens.lens (debugHookConfig :: TrainingJob -> Lude.Maybe DebugHookConfig) (\s a -> s {debugHookConfig = a} :: TrainingJob)
{-# DEPRECATED tjDebugHookConfig "Use generic-lens or generic-optics with 'debugHookConfig' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjCheckpointConfig :: Lens.Lens' TrainingJob (Lude.Maybe CheckpointConfig)
tjCheckpointConfig = Lens.lens (checkpointConfig :: TrainingJob -> Lude.Maybe CheckpointConfig) (\s a -> s {checkpointConfig = a} :: TrainingJob)
{-# DEPRECATED tjCheckpointConfig "Use generic-lens or generic-optics with 'checkpointConfig' instead." #-}

-- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjStoppingCondition :: Lens.Lens' TrainingJob (Lude.Maybe StoppingCondition)
tjStoppingCondition = Lens.lens (stoppingCondition :: TrainingJob -> Lude.Maybe StoppingCondition) (\s a -> s {stoppingCondition = a} :: TrainingJob)
{-# DEPRECATED tjStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | Information about the evaluation status of the rules for the training job.
--
-- /Note:/ Consider using 'debugRuleEvaluationStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjDebugRuleEvaluationStatuses :: Lens.Lens' TrainingJob (Lude.Maybe [DebugRuleEvaluationStatus])
tjDebugRuleEvaluationStatuses = Lens.lens (debugRuleEvaluationStatuses :: TrainingJob -> Lude.Maybe [DebugRuleEvaluationStatus]) (\s a -> s {debugRuleEvaluationStatuses = a} :: TrainingJob)
{-# DEPRECATED tjDebugRuleEvaluationStatuses "Use generic-lens or generic-optics with 'debugRuleEvaluationStatuses' instead." #-}

-- | The status of the training job.
--
-- Training job statuses are:
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
tjTrainingJobStatus :: Lens.Lens' TrainingJob (Lude.Maybe TrainingJobStatus)
tjTrainingJobStatus = Lens.lens (trainingJobStatus :: TrainingJob -> Lude.Maybe TrainingJobStatus) (\s a -> s {trainingJobStatus = a} :: TrainingJob)
{-# DEPRECATED tjTrainingJobStatus "Use generic-lens or generic-optics with 'trainingJobStatus' instead." #-}

-- | If the @TrainingJob@ was created with network isolation, the value is set to @true@ . If network isolation is enabled, nodes can't communicate beyond the VPC they run in.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjEnableNetworkIsolation :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Bool)
tjEnableNetworkIsolation = Lens.lens (enableNetworkIsolation :: TrainingJob -> Lude.Maybe Lude.Bool) (\s a -> s {enableNetworkIsolation = a} :: TrainingJob)
{-# DEPRECATED tjEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjExperimentConfig :: Lens.Lens' TrainingJob (Lude.Maybe ExperimentConfig)
tjExperimentConfig = Lens.lens (experimentConfig :: TrainingJob -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: TrainingJob)
{-# DEPRECATED tjExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | A timestamp that indicates when the status of the training job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjLastModifiedTime :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Timestamp)
tjLastModifiedTime = Lens.lens (lastModifiedTime :: TrainingJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: TrainingJob)
{-# DEPRECATED tjLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Information about the debug rule configuration.
--
-- /Note:/ Consider using 'debugRuleConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjDebugRuleConfigurations :: Lens.Lens' TrainingJob (Lude.Maybe [DebugRuleConfiguration])
tjDebugRuleConfigurations = Lens.lens (debugRuleConfigurations :: TrainingJob -> Lude.Maybe [DebugRuleConfiguration]) (\s a -> s {debugRuleConfigurations = a} :: TrainingJob)
{-# DEPRECATED tjDebugRuleConfigurations "Use generic-lens or generic-optics with 'debugRuleConfigurations' instead." #-}

-- | When true, enables managed spot training using Amazon EC2 Spot instances to run training jobs instead of on-demand instances. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training> .
--
-- /Note:/ Consider using 'enableManagedSpotTraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjEnableManagedSpotTraining :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Bool)
tjEnableManagedSpotTraining = Lens.lens (enableManagedSpotTraining :: TrainingJob -> Lude.Maybe Lude.Bool) (\s a -> s {enableManagedSpotTraining = a} :: TrainingJob)
{-# DEPRECATED tjEnableManagedSpotTraining "Use generic-lens or generic-optics with 'enableManagedSpotTraining' instead." #-}

-- | The Amazon Resource Name (ARN) of the job.
--
-- /Note:/ Consider using 'autoMLJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjAutoMLJobARN :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Text)
tjAutoMLJobARN = Lens.lens (autoMLJobARN :: TrainingJob -> Lude.Maybe Lude.Text) (\s a -> s {autoMLJobARN = a} :: TrainingJob)
{-# DEPRECATED tjAutoMLJobARN "Use generic-lens or generic-optics with 'autoMLJobARN' instead." #-}

-- | Algorithm-specific parameters.
--
-- /Note:/ Consider using 'hyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjHyperParameters :: Lens.Lens' TrainingJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tjHyperParameters = Lens.lens (hyperParameters :: TrainingJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {hyperParameters = a} :: TrainingJob)
{-# DEPRECATED tjHyperParameters "Use generic-lens or generic-optics with 'hyperParameters' instead." #-}

-- | An array of @Channel@ objects that describes each data input channel.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjInputDataConfig :: Lens.Lens' TrainingJob (Lude.Maybe (Lude.NonEmpty Channel))
tjInputDataConfig = Lens.lens (inputDataConfig :: TrainingJob -> Lude.Maybe (Lude.NonEmpty Channel)) (\s a -> s {inputDataConfig = a} :: TrainingJob)
{-# DEPRECATED tjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjVPCConfig :: Lens.Lens' TrainingJob (Lude.Maybe VPCConfig)
tjVPCConfig = Lens.lens (vpcConfig :: TrainingJob -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: TrainingJob)
{-# DEPRECATED tjVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTrainingJobARN :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Text)
tjTrainingJobARN = Lens.lens (trainingJobARN :: TrainingJob -> Lude.Maybe Lude.Text) (\s a -> s {trainingJobARN = a} :: TrainingJob)
{-# DEPRECATED tjTrainingJobARN "Use generic-lens or generic-optics with 'trainingJobARN' instead." #-}

-- | Information about the algorithm used for training, and algorithm metadata.
--
-- /Note:/ Consider using 'algorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjAlgorithmSpecification :: Lens.Lens' TrainingJob (Lude.Maybe AlgorithmSpecification)
tjAlgorithmSpecification = Lens.lens (algorithmSpecification :: TrainingJob -> Lude.Maybe AlgorithmSpecification) (\s a -> s {algorithmSpecification = a} :: TrainingJob)
{-# DEPRECATED tjAlgorithmSpecification "Use generic-lens or generic-optics with 'algorithmSpecification' instead." #-}

-- | A list of final metric values that are set when the training job completes. Used only if the training job was configured to use metrics.
--
-- /Note:/ Consider using 'finalMetricDataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjFinalMetricDataList :: Lens.Lens' TrainingJob (Lude.Maybe [MetricData])
tjFinalMetricDataList = Lens.lens (finalMetricDataList :: TrainingJob -> Lude.Maybe [MetricData]) (\s a -> s {finalMetricDataList = a} :: TrainingJob)
{-# DEPRECATED tjFinalMetricDataList "Use generic-lens or generic-optics with 'finalMetricDataList' instead." #-}

-- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjOutputDataConfig :: Lens.Lens' TrainingJob (Lude.Maybe OutputDataConfig)
tjOutputDataConfig = Lens.lens (outputDataConfig :: TrainingJob -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: TrainingJob)
{-# DEPRECATED tjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
--
-- /Note:/ Consider using 'trainingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTrainingStartTime :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Timestamp)
tjTrainingStartTime = Lens.lens (trainingStartTime :: TrainingJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingStartTime = a} :: TrainingJob)
{-# DEPRECATED tjTrainingStartTime "Use generic-lens or generic-optics with 'trainingStartTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
--
-- /Note:/ Consider using 'tuningJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTuningJobARN :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Text)
tjTuningJobARN = Lens.lens (tuningJobARN :: TrainingJob -> Lude.Maybe Lude.Text) (\s a -> s {tuningJobARN = a} :: TrainingJob)
{-# DEPRECATED tjTuningJobARN "Use generic-lens or generic-optics with 'tuningJobARN' instead." #-}

-- | The name of the training job.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTrainingJobName :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Text)
tjTrainingJobName = Lens.lens (trainingJobName :: TrainingJob -> Lude.Maybe Lude.Text) (\s a -> s {trainingJobName = a} :: TrainingJob)
{-# DEPRECATED tjTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

-- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjResourceConfig :: Lens.Lens' TrainingJob (Lude.Maybe ResourceConfig)
tjResourceConfig = Lens.lens (resourceConfig :: TrainingJob -> Lude.Maybe ResourceConfig) (\s a -> s {resourceConfig = a} :: TrainingJob)
{-# DEPRECATED tjResourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead." #-}

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjEnableInterContainerTrafficEncryption :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Bool)
tjEnableInterContainerTrafficEncryption = Lens.lens (enableInterContainerTrafficEncryption :: TrainingJob -> Lude.Maybe Lude.Bool) (\s a -> s {enableInterContainerTrafficEncryption = a} :: TrainingJob)
{-# DEPRECATED tjEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tensorBoardOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTensorBoardOutputConfig :: Lens.Lens' TrainingJob (Lude.Maybe TensorBoardOutputConfig)
tjTensorBoardOutputConfig = Lens.lens (tensorBoardOutputConfig :: TrainingJob -> Lude.Maybe TensorBoardOutputConfig) (\s a -> s {tensorBoardOutputConfig = a} :: TrainingJob)
{-# DEPRECATED tjTensorBoardOutputConfig "Use generic-lens or generic-optics with 'tensorBoardOutputConfig' instead." #-}

-- | Provides detailed information about the state of the training job. For detailed information about the secondary status of the training job, see @StatusMessage@ under 'SecondaryStatusTransition' .
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
tjSecondaryStatus :: Lens.Lens' TrainingJob (Lude.Maybe SecondaryStatus)
tjSecondaryStatus = Lens.lens (secondaryStatus :: TrainingJob -> Lude.Maybe SecondaryStatus) (\s a -> s {secondaryStatus = a} :: TrainingJob)
{-# DEPRECATED tjSecondaryStatus "Use generic-lens or generic-optics with 'secondaryStatus' instead." #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTags :: Lens.Lens' TrainingJob (Lude.Maybe [Tag])
tjTags = Lens.lens (tags :: TrainingJob -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TrainingJob)
{-# DEPRECATED tjTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The training time in seconds.
--
-- /Note:/ Consider using 'trainingTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjTrainingTimeInSeconds :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Natural)
tjTrainingTimeInSeconds = Lens.lens (trainingTimeInSeconds :: TrainingJob -> Lude.Maybe Lude.Natural) (\s a -> s {trainingTimeInSeconds = a} :: TrainingJob)
{-# DEPRECATED tjTrainingTimeInSeconds "Use generic-lens or generic-optics with 'trainingTimeInSeconds' instead." #-}

-- | The AWS Identity and Access Management (IAM) role configured for the training job.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjRoleARN :: Lens.Lens' TrainingJob (Lude.Maybe Lude.Text)
tjRoleARN = Lens.lens (roleARN :: TrainingJob -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: TrainingJob)
{-# DEPRECATED tjRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON TrainingJob where
  parseJSON =
    Lude.withObject
      "TrainingJob"
      ( \x ->
          TrainingJob'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LabelingJobArn")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "SecondaryStatusTransitions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ModelArtifacts")
            Lude.<*> (x Lude..:? "TrainingEndTime")
            Lude.<*> (x Lude..:? "BillableTimeInSeconds")
            Lude.<*> (x Lude..:? "DebugHookConfig")
            Lude.<*> (x Lude..:? "CheckpointConfig")
            Lude.<*> (x Lude..:? "StoppingCondition")
            Lude.<*> (x Lude..:? "DebugRuleEvaluationStatuses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TrainingJobStatus")
            Lude.<*> (x Lude..:? "EnableNetworkIsolation")
            Lude.<*> (x Lude..:? "ExperimentConfig")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "DebugRuleConfigurations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EnableManagedSpotTraining")
            Lude.<*> (x Lude..:? "AutoMLJobArn")
            Lude.<*> (x Lude..:? "HyperParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "TrainingJobArn")
            Lude.<*> (x Lude..:? "AlgorithmSpecification")
            Lude.<*> (x Lude..:? "FinalMetricDataList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "OutputDataConfig")
            Lude.<*> (x Lude..:? "TrainingStartTime")
            Lude.<*> (x Lude..:? "TuningJobArn")
            Lude.<*> (x Lude..:? "TrainingJobName")
            Lude.<*> (x Lude..:? "ResourceConfig")
            Lude.<*> (x Lude..:? "EnableInterContainerTrafficEncryption")
            Lude.<*> (x Lude..:? "TensorBoardOutputConfig")
            Lude.<*> (x Lude..:? "SecondaryStatus")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TrainingTimeInSeconds")
            Lude.<*> (x Lude..:? "RoleArn")
      )
