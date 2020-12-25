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
    dtjrfrsTrainingJobName,
    dtjrfrsTrainingJobArn,
    dtjrfrsModelArtifacts,
    dtjrfrsTrainingJobStatus,
    dtjrfrsSecondaryStatus,
    dtjrfrsAlgorithmSpecification,
    dtjrfrsResourceConfig,
    dtjrfrsStoppingCondition,
    dtjrfrsCreationTime,
    dtjrfrsAutoMLJobArn,
    dtjrfrsBillableTimeInSeconds,
    dtjrfrsCheckpointConfig,
    dtjrfrsDebugHookConfig,
    dtjrfrsDebugRuleConfigurations,
    dtjrfrsDebugRuleEvaluationStatuses,
    dtjrfrsEnableInterContainerTrafficEncryption,
    dtjrfrsEnableManagedSpotTraining,
    dtjrfrsEnableNetworkIsolation,
    dtjrfrsExperimentConfig,
    dtjrfrsFailureReason,
    dtjrfrsFinalMetricDataList,
    dtjrfrsHyperParameters,
    dtjrfrsInputDataConfig,
    dtjrfrsLabelingJobArn,
    dtjrfrsLastModifiedTime,
    dtjrfrsOutputDataConfig,
    dtjrfrsRoleArn,
    dtjrfrsSecondaryStatusTransitions,
    dtjrfrsTensorBoardOutputConfig,
    dtjrfrsTrainingEndTime,
    dtjrfrsTrainingStartTime,
    dtjrfrsTrainingTimeInSeconds,
    dtjrfrsTuningJobArn,
    dtjrfrsVpcConfig,
    dtjrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeTrainingJob' smart constructor.
newtype DescribeTrainingJob = DescribeTrainingJob'
  { -- | The name of the training job.
    trainingJobName :: Types.TrainingJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrainingJob' value with any optional fields omitted.
mkDescribeTrainingJob ::
  -- | 'trainingJobName'
  Types.TrainingJobName ->
  DescribeTrainingJob
mkDescribeTrainingJob trainingJobName =
  DescribeTrainingJob' {trainingJobName}

-- | The name of the training job.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjTrainingJobName :: Lens.Lens' DescribeTrainingJob Types.TrainingJobName
dtjTrainingJobName = Lens.field @"trainingJobName"
{-# DEPRECATED dtjTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

instance Core.FromJSON DescribeTrainingJob where
  toJSON DescribeTrainingJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TrainingJobName" Core..= trainingJobName)]
      )

instance Core.AWSRequest DescribeTrainingJob where
  type Rs DescribeTrainingJob = DescribeTrainingJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeTrainingJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrainingJobResponse'
            Core.<$> (x Core..: "TrainingJobName")
            Core.<*> (x Core..: "TrainingJobArn")
            Core.<*> (x Core..: "ModelArtifacts")
            Core.<*> (x Core..: "TrainingJobStatus")
            Core.<*> (x Core..: "SecondaryStatus")
            Core.<*> (x Core..: "AlgorithmSpecification")
            Core.<*> (x Core..: "ResourceConfig")
            Core.<*> (x Core..: "StoppingCondition")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..:? "AutoMLJobArn")
            Core.<*> (x Core..:? "BillableTimeInSeconds")
            Core.<*> (x Core..:? "CheckpointConfig")
            Core.<*> (x Core..:? "DebugHookConfig")
            Core.<*> (x Core..:? "DebugRuleConfigurations")
            Core.<*> (x Core..:? "DebugRuleEvaluationStatuses")
            Core.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
            Core.<*> (x Core..:? "EnableManagedSpotTraining")
            Core.<*> (x Core..:? "EnableNetworkIsolation")
            Core.<*> (x Core..:? "ExperimentConfig")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "FinalMetricDataList")
            Core.<*> (x Core..:? "HyperParameters")
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "LabelingJobArn")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "OutputDataConfig")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "SecondaryStatusTransitions")
            Core.<*> (x Core..:? "TensorBoardOutputConfig")
            Core.<*> (x Core..:? "TrainingEndTime")
            Core.<*> (x Core..:? "TrainingStartTime")
            Core.<*> (x Core..:? "TrainingTimeInSeconds")
            Core.<*> (x Core..:? "TuningJobArn")
            Core.<*> (x Core..:? "VpcConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTrainingJobResponse' smart constructor.
data DescribeTrainingJobResponse = DescribeTrainingJobResponse'
  { -- | Name of the model training job.
    trainingJobName :: Types.TrainingJobName,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Types.TrainingJobArn,
    -- | Information about the Amazon S3 location that is configured for storing model artifacts.
    modelArtifacts :: Types.ModelArtifacts,
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
    trainingJobStatus :: Types.TrainingJobStatus,
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
    secondaryStatus :: Types.SecondaryStatus,
    -- | Information about the algorithm used for training, and algorithm metadata.
    algorithmSpecification :: Types.AlgorithmSpecification,
    -- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
    resourceConfig :: Types.ResourceConfig,
    -- | Specifies a limit to how long a model training job can run. It also specifies the maximum time to wait for a spot instance. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
    stoppingCondition :: Types.StoppingCondition,
    -- | A timestamp that indicates when the training job was created.
    creationTime :: Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of an AutoML job.
    autoMLJobArn :: Core.Maybe Types.AutoMLJobArn,
    -- | The billable time in seconds.
    --
    -- You can calculate the savings from using managed spot training using the formula @(1 - BillableTimeInSeconds / TrainingTimeInSeconds) * 100@ . For example, if @BillableTimeInSeconds@ is 100 and @TrainingTimeInSeconds@ is 500, the savings is 80%.
    billableTimeInSeconds :: Core.Maybe Core.Natural,
    checkpointConfig :: Core.Maybe Types.CheckpointConfig,
    debugHookConfig :: Core.Maybe Types.DebugHookConfig,
    -- | Configuration information for debugging rules.
    debugRuleConfigurations :: Core.Maybe [Types.DebugRuleConfiguration],
    -- | Status about the debug rule evaluation.
    debugRuleEvaluationStatuses :: Core.Maybe [Types.DebugRuleEvaluationStatus],
    -- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithms in distributed training.
    enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool,
    -- | A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
    enableManagedSpotTraining :: Core.Maybe Core.Bool,
    -- | If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    experimentConfig :: Core.Maybe Types.ExperimentConfig,
    -- | If the training job failed, the reason it failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
    finalMetricDataList :: Core.Maybe [Types.MetricData],
    -- | Algorithm-specific parameters.
    hyperParameters :: Core.Maybe (Core.HashMap Types.HyperParameterKey Types.HyperParameterValue),
    -- | An array of @Channel@ objects that describes each data input channel.
    inputDataConfig :: Core.Maybe (Core.NonEmpty Types.Channel),
    -- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
    labelingJobArn :: Core.Maybe Types.LabelingJobArn,
    -- | A timestamp that indicates when the status of the training job was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
    outputDataConfig :: Core.Maybe Types.OutputDataConfig,
    -- | The AWS Identity and Access Management (IAM) role configured for the training job.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | A history of all of the secondary statuses that the training job has transitioned through.
    secondaryStatusTransitions :: Core.Maybe [Types.SecondaryStatusTransition],
    tensorBoardOutputConfig :: Core.Maybe Types.TensorBoardOutputConfig,
    -- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
    trainingEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
    trainingStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The training time in seconds.
    trainingTimeInSeconds :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
    tuningJobArn :: Core.Maybe Types.HyperParameterTuningJobArn,
    -- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
    vpcConfig :: Core.Maybe Types.VpcConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTrainingJobResponse' value with any optional fields omitted.
mkDescribeTrainingJobResponse ::
  -- | 'trainingJobName'
  Types.TrainingJobName ->
  -- | 'trainingJobArn'
  Types.TrainingJobArn ->
  -- | 'modelArtifacts'
  Types.ModelArtifacts ->
  -- | 'trainingJobStatus'
  Types.TrainingJobStatus ->
  -- | 'secondaryStatus'
  Types.SecondaryStatus ->
  -- | 'algorithmSpecification'
  Types.AlgorithmSpecification ->
  -- | 'resourceConfig'
  Types.ResourceConfig ->
  -- | 'stoppingCondition'
  Types.StoppingCondition ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeTrainingJobResponse
mkDescribeTrainingJobResponse
  trainingJobName
  trainingJobArn
  modelArtifacts
  trainingJobStatus
  secondaryStatus
  algorithmSpecification
  resourceConfig
  stoppingCondition
  creationTime
  responseStatus =
    DescribeTrainingJobResponse'
      { trainingJobName,
        trainingJobArn,
        modelArtifacts,
        trainingJobStatus,
        secondaryStatus,
        algorithmSpecification,
        resourceConfig,
        stoppingCondition,
        creationTime,
        autoMLJobArn = Core.Nothing,
        billableTimeInSeconds = Core.Nothing,
        checkpointConfig = Core.Nothing,
        debugHookConfig = Core.Nothing,
        debugRuleConfigurations = Core.Nothing,
        debugRuleEvaluationStatuses = Core.Nothing,
        enableInterContainerTrafficEncryption = Core.Nothing,
        enableManagedSpotTraining = Core.Nothing,
        enableNetworkIsolation = Core.Nothing,
        experimentConfig = Core.Nothing,
        failureReason = Core.Nothing,
        finalMetricDataList = Core.Nothing,
        hyperParameters = Core.Nothing,
        inputDataConfig = Core.Nothing,
        labelingJobArn = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        outputDataConfig = Core.Nothing,
        roleArn = Core.Nothing,
        secondaryStatusTransitions = Core.Nothing,
        tensorBoardOutputConfig = Core.Nothing,
        trainingEndTime = Core.Nothing,
        trainingStartTime = Core.Nothing,
        trainingTimeInSeconds = Core.Nothing,
        tuningJobArn = Core.Nothing,
        vpcConfig = Core.Nothing,
        responseStatus
      }

-- | Name of the model training job.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsTrainingJobName :: Lens.Lens' DescribeTrainingJobResponse Types.TrainingJobName
dtjrfrsTrainingJobName = Lens.field @"trainingJobName"
{-# DEPRECATED dtjrfrsTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsTrainingJobArn :: Lens.Lens' DescribeTrainingJobResponse Types.TrainingJobArn
dtjrfrsTrainingJobArn = Lens.field @"trainingJobArn"
{-# DEPRECATED dtjrfrsTrainingJobArn "Use generic-lens or generic-optics with 'trainingJobArn' instead." #-}

-- | Information about the Amazon S3 location that is configured for storing model artifacts.
--
-- /Note:/ Consider using 'modelArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsModelArtifacts :: Lens.Lens' DescribeTrainingJobResponse Types.ModelArtifacts
dtjrfrsModelArtifacts = Lens.field @"modelArtifacts"
{-# DEPRECATED dtjrfrsModelArtifacts "Use generic-lens or generic-optics with 'modelArtifacts' instead." #-}

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
dtjrfrsTrainingJobStatus :: Lens.Lens' DescribeTrainingJobResponse Types.TrainingJobStatus
dtjrfrsTrainingJobStatus = Lens.field @"trainingJobStatus"
{-# DEPRECATED dtjrfrsTrainingJobStatus "Use generic-lens or generic-optics with 'trainingJobStatus' instead." #-}

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
dtjrfrsSecondaryStatus :: Lens.Lens' DescribeTrainingJobResponse Types.SecondaryStatus
dtjrfrsSecondaryStatus = Lens.field @"secondaryStatus"
{-# DEPRECATED dtjrfrsSecondaryStatus "Use generic-lens or generic-optics with 'secondaryStatus' instead." #-}

-- | Information about the algorithm used for training, and algorithm metadata.
--
-- /Note:/ Consider using 'algorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsAlgorithmSpecification :: Lens.Lens' DescribeTrainingJobResponse Types.AlgorithmSpecification
dtjrfrsAlgorithmSpecification = Lens.field @"algorithmSpecification"
{-# DEPRECATED dtjrfrsAlgorithmSpecification "Use generic-lens or generic-optics with 'algorithmSpecification' instead." #-}

-- | Resources, including ML compute instances and ML storage volumes, that are configured for model training.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsResourceConfig :: Lens.Lens' DescribeTrainingJobResponse Types.ResourceConfig
dtjrfrsResourceConfig = Lens.field @"resourceConfig"
{-# DEPRECATED dtjrfrsResourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead." #-}

-- | Specifies a limit to how long a model training job can run. It also specifies the maximum time to wait for a spot instance. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts, so the results of training are not lost.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsStoppingCondition :: Lens.Lens' DescribeTrainingJobResponse Types.StoppingCondition
dtjrfrsStoppingCondition = Lens.field @"stoppingCondition"
{-# DEPRECATED dtjrfrsStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | A timestamp that indicates when the training job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsCreationTime :: Lens.Lens' DescribeTrainingJobResponse Core.NominalDiffTime
dtjrfrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dtjrfrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of an AutoML job.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsAutoMLJobArn :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.AutoMLJobArn)
dtjrfrsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# DEPRECATED dtjrfrsAutoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead." #-}

-- | The billable time in seconds.
--
-- You can calculate the savings from using managed spot training using the formula @(1 - BillableTimeInSeconds / TrainingTimeInSeconds) * 100@ . For example, if @BillableTimeInSeconds@ is 100 and @TrainingTimeInSeconds@ is 500, the savings is 80%.
--
-- /Note:/ Consider using 'billableTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsBillableTimeInSeconds :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Natural)
dtjrfrsBillableTimeInSeconds = Lens.field @"billableTimeInSeconds"
{-# DEPRECATED dtjrfrsBillableTimeInSeconds "Use generic-lens or generic-optics with 'billableTimeInSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsCheckpointConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.CheckpointConfig)
dtjrfrsCheckpointConfig = Lens.field @"checkpointConfig"
{-# DEPRECATED dtjrfrsCheckpointConfig "Use generic-lens or generic-optics with 'checkpointConfig' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'debugHookConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsDebugHookConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.DebugHookConfig)
dtjrfrsDebugHookConfig = Lens.field @"debugHookConfig"
{-# DEPRECATED dtjrfrsDebugHookConfig "Use generic-lens or generic-optics with 'debugHookConfig' instead." #-}

-- | Configuration information for debugging rules.
--
-- /Note:/ Consider using 'debugRuleConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsDebugRuleConfigurations :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [Types.DebugRuleConfiguration])
dtjrfrsDebugRuleConfigurations = Lens.field @"debugRuleConfigurations"
{-# DEPRECATED dtjrfrsDebugRuleConfigurations "Use generic-lens or generic-optics with 'debugRuleConfigurations' instead." #-}

-- | Status about the debug rule evaluation.
--
-- /Note:/ Consider using 'debugRuleEvaluationStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsDebugRuleEvaluationStatuses :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [Types.DebugRuleEvaluationStatus])
dtjrfrsDebugRuleEvaluationStatuses = Lens.field @"debugRuleEvaluationStatuses"
{-# DEPRECATED dtjrfrsDebugRuleEvaluationStatuses "Use generic-lens or generic-optics with 'debugRuleEvaluationStatuses' instead." #-}

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithms in distributed training.
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsEnableInterContainerTrafficEncryption :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Bool)
dtjrfrsEnableInterContainerTrafficEncryption = Lens.field @"enableInterContainerTrafficEncryption"
{-# DEPRECATED dtjrfrsEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

-- | A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
--
-- /Note:/ Consider using 'enableManagedSpotTraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsEnableManagedSpotTraining :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Bool)
dtjrfrsEnableManagedSpotTraining = Lens.field @"enableManagedSpotTraining"
{-# DEPRECATED dtjrfrsEnableManagedSpotTraining "Use generic-lens or generic-optics with 'enableManagedSpotTraining' instead." #-}

-- | If you want to allow inbound or outbound network calls, except for calls between peers within a training cluster for distributed training, choose @True@ . If you enable network isolation for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsEnableNetworkIsolation :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Bool)
dtjrfrsEnableNetworkIsolation = Lens.field @"enableNetworkIsolation"
{-# DEPRECATED dtjrfrsEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsExperimentConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.ExperimentConfig)
dtjrfrsExperimentConfig = Lens.field @"experimentConfig"
{-# DEPRECATED dtjrfrsExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | If the training job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsFailureReason :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.FailureReason)
dtjrfrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED dtjrfrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | A collection of @MetricData@ objects that specify the names, values, and dates and times that the training algorithm emitted to Amazon CloudWatch.
--
-- /Note:/ Consider using 'finalMetricDataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsFinalMetricDataList :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [Types.MetricData])
dtjrfrsFinalMetricDataList = Lens.field @"finalMetricDataList"
{-# DEPRECATED dtjrfrsFinalMetricDataList "Use generic-lens or generic-optics with 'finalMetricDataList' instead." #-}

-- | Algorithm-specific parameters.
--
-- /Note:/ Consider using 'hyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsHyperParameters :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe (Core.HashMap Types.HyperParameterKey Types.HyperParameterValue))
dtjrfrsHyperParameters = Lens.field @"hyperParameters"
{-# DEPRECATED dtjrfrsHyperParameters "Use generic-lens or generic-optics with 'hyperParameters' instead." #-}

-- | An array of @Channel@ objects that describes each data input channel.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsInputDataConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe (Core.NonEmpty Types.Channel))
dtjrfrsInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED dtjrfrsInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth labeling job that created the transform or training job.
--
-- /Note:/ Consider using 'labelingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsLabelingJobArn :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.LabelingJobArn)
dtjrfrsLabelingJobArn = Lens.field @"labelingJobArn"
{-# DEPRECATED dtjrfrsLabelingJobArn "Use generic-lens or generic-optics with 'labelingJobArn' instead." #-}

-- | A timestamp that indicates when the status of the training job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsLastModifiedTime :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.NominalDiffTime)
dtjrfrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED dtjrfrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The S3 path where model artifacts that you configured when creating the job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsOutputDataConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.OutputDataConfig)
dtjrfrsOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED dtjrfrsOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The AWS Identity and Access Management (IAM) role configured for the training job.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsRoleArn :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.RoleArn)
dtjrfrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dtjrfrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | A history of all of the secondary statuses that the training job has transitioned through.
--
-- /Note:/ Consider using 'secondaryStatusTransitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsSecondaryStatusTransitions :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [Types.SecondaryStatusTransition])
dtjrfrsSecondaryStatusTransitions = Lens.field @"secondaryStatusTransitions"
{-# DEPRECATED dtjrfrsSecondaryStatusTransitions "Use generic-lens or generic-optics with 'secondaryStatusTransitions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tensorBoardOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsTensorBoardOutputConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.TensorBoardOutputConfig)
dtjrfrsTensorBoardOutputConfig = Lens.field @"tensorBoardOutputConfig"
{-# DEPRECATED dtjrfrsTensorBoardOutputConfig "Use generic-lens or generic-optics with 'tensorBoardOutputConfig' instead." #-}

-- | Indicates the time when the training job ends on training instances. You are billed for the time interval between the value of @TrainingStartTime@ and this time. For successful jobs and stopped jobs, this is the time after model artifacts are uploaded. For failed jobs, this is the time when Amazon SageMaker detects a job failure.
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsTrainingEndTime :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.NominalDiffTime)
dtjrfrsTrainingEndTime = Lens.field @"trainingEndTime"
{-# DEPRECATED dtjrfrsTrainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead." #-}

-- | Indicates the time when the training job starts on training instances. You are billed for the time interval between this time and the value of @TrainingEndTime@ . The start time in CloudWatch Logs might be later than this time. The difference is due to the time it takes to download the training data and to the size of the training container.
--
-- /Note:/ Consider using 'trainingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsTrainingStartTime :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.NominalDiffTime)
dtjrfrsTrainingStartTime = Lens.field @"trainingStartTime"
{-# DEPRECATED dtjrfrsTrainingStartTime "Use generic-lens or generic-optics with 'trainingStartTime' instead." #-}

-- | The training time in seconds.
--
-- /Note:/ Consider using 'trainingTimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsTrainingTimeInSeconds :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Natural)
dtjrfrsTrainingTimeInSeconds = Lens.field @"trainingTimeInSeconds"
{-# DEPRECATED dtjrfrsTrainingTimeInSeconds "Use generic-lens or generic-optics with 'trainingTimeInSeconds' instead." #-}

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning job if the training job was launched by a hyperparameter tuning job.
--
-- /Note:/ Consider using 'tuningJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsTuningJobArn :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.HyperParameterTuningJobArn)
dtjrfrsTuningJobArn = Lens.field @"tuningJobArn"
{-# DEPRECATED dtjrfrsTuningJobArn "Use generic-lens or generic-optics with 'tuningJobArn' instead." #-}

-- | A 'VpcConfig' object that specifies the VPC that this training job has access to. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsVpcConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Types.VpcConfig)
dtjrfrsVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED dtjrfrsVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjrfrsResponseStatus :: Lens.Lens' DescribeTrainingJobResponse Core.Int
dtjrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtjrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
