{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJob where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
import Network.AWS.SageMaker.Types.RetryStrategy
import Network.AWS.SageMaker.Types.SecondaryStatus
import Network.AWS.SageMaker.Types.SecondaryStatusTransition
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TensorBoardOutputConfig
import Network.AWS.SageMaker.Types.TrainingJobStatus
import Network.AWS.SageMaker.Types.VpcConfig

-- | Contains information about a training job.
--
-- /See:/ 'newTrainingJob' smart constructor.
data TrainingJob = TrainingJob'
  { -- | A timestamp that indicates when the training job was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the labeling job.
    labelingJobArn :: Prelude.Maybe Prelude.Text,
    -- | If the training job failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A history of all of the secondary statuses that the training job has
    -- transitioned through.
    secondaryStatusTransitions :: Prelude.Maybe [SecondaryStatusTransition],
    -- | Information about the Amazon S3 location that is configured for storing
    -- model artifacts.
    modelArtifacts :: Prelude.Maybe ModelArtifacts,
    -- | Indicates the time when the training job ends on training instances. You
    -- are billed for the time interval between the value of
    -- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
    -- this is the time after model artifacts are uploaded. For failed jobs,
    -- this is the time when Amazon SageMaker detects a job failure.
    trainingEndTime :: Prelude.Maybe Core.POSIX,
    -- | The environment variables to set in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The billable time in seconds.
    billableTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    debugHookConfig :: Prelude.Maybe DebugHookConfig,
    checkpointConfig :: Prelude.Maybe CheckpointConfig,
    -- | The number of times to retry the job when the job fails due to an
    -- @InternalServerError@.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | Specifies a limit to how long a model training job can run. It also
    -- specifies how long a managed Spot training job has to complete. When the
    -- job reaches the time limit, Amazon SageMaker ends the training job. Use
    -- this API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
    -- signal, which delays job termination for 120 seconds. Algorithms can use
    -- this 120-second window to save the model artifacts, so the results of
    -- training are not lost.
    stoppingCondition :: Prelude.Maybe StoppingCondition,
    -- | Information about the evaluation status of the rules for the training
    -- job.
    debugRuleEvaluationStatuses :: Prelude.Maybe [DebugRuleEvaluationStatus],
    -- | The status of the training job.
    --
    -- Training job statuses are:
    --
    -- -   @InProgress@ - The training is in progress.
    --
    -- -   @Completed@ - The training job has completed.
    --
    -- -   @Failed@ - The training job has failed. To see the reason for the
    --     failure, see the @FailureReason@ field in the response to a
    --     @DescribeTrainingJobResponse@ call.
    --
    -- -   @Stopping@ - The training job is stopping.
    --
    -- -   @Stopped@ - The training job has stopped.
    --
    -- For more detailed information, see @SecondaryStatus@.
    trainingJobStatus :: Prelude.Maybe TrainingJobStatus,
    -- | If the @TrainingJob@ was created with network isolation, the value is
    -- set to @true@. If network isolation is enabled, nodes can\'t communicate
    -- beyond the VPC they run in.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | A timestamp that indicates when the status of the training job was last
    -- modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | Information about the debug rule configuration.
    debugRuleConfigurations :: Prelude.Maybe [DebugRuleConfiguration],
    -- | When true, enables managed spot training using Amazon EC2 Spot instances
    -- to run training jobs instead of on-demand instances. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
    enableManagedSpotTraining :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | Algorithm-specific parameters.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An array of @Channel@ objects that describes each data input channel.
    inputDataConfig :: Prelude.Maybe (Prelude.NonEmpty Channel),
    -- | A VpcConfig object that specifies the VPC that this training job has
    -- access to. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the algorithm used for training, and algorithm
    -- metadata.
    algorithmSpecification :: Prelude.Maybe AlgorithmSpecification,
    -- | A list of final metric values that are set when the training job
    -- completes. Used only if the training job was configured to use metrics.
    finalMetricDataList :: Prelude.Maybe [MetricData],
    -- | The S3 path where model artifacts that you configured when creating the
    -- job are stored. Amazon SageMaker creates subfolders for model artifacts.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | Indicates the time when the training job starts on training instances.
    -- You are billed for the time interval between this time and the value of
    -- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
    -- this time. The difference is due to the time it takes to download the
    -- training data and to the size of the training container.
    trainingStartTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
    -- job if the training job was launched by a hyperparameter tuning job.
    tuningJobArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the training job.
    trainingJobName :: Prelude.Maybe Prelude.Text,
    -- | Resources, including ML compute instances and ML storage volumes, that
    -- are configured for model training.
    resourceConfig :: Prelude.Maybe ResourceConfig,
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    tensorBoardOutputConfig :: Prelude.Maybe TensorBoardOutputConfig,
    -- | Provides detailed information about the state of the training job. For
    -- detailed information about the secondary status of the training job, see
    -- @StatusMessage@ under SecondaryStatusTransition.
    --
    -- Amazon SageMaker provides primary statuses and secondary statuses that
    -- apply to each of them:
    --
    -- [InProgress]
    --     -   @Starting@ - Starting the training job.
    --
    --     -   @Downloading@ - An optional stage for algorithms that support
    --         @File@ training input mode. It indicates that data is being
    --         downloaded to the ML storage volumes.
    --
    --     -   @Training@ - Training is in progress.
    --
    --     -   @Uploading@ - Training is complete and the model artifacts are
    --         being uploaded to the S3 location.
    --
    -- [Completed]
    --     -   @Completed@ - The training job has completed.
    --
    -- [Failed]
    --     -   @Failed@ - The training job has failed. The reason for the
    --         failure is returned in the @FailureReason@ field of
    --         @DescribeTrainingJobResponse@.
    --
    -- [Stopped]
    --     -   @MaxRuntimeExceeded@ - The job stopped because it exceeded the
    --         maximum allowed runtime.
    --
    --     -   @Stopped@ - The training job has stopped.
    --
    -- [Stopping]
    --     -   @Stopping@ - Stopping the training job.
    --
    -- Valid values for @SecondaryStatus@ are subject to change.
    --
    -- We no longer support the following secondary statuses:
    --
    -- -   @LaunchingMLInstances@
    --
    -- -   @PreparingTrainingStack@
    --
    -- -   @DownloadingTrainingImage@
    secondaryStatus :: Prelude.Maybe SecondaryStatus,
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The training time in seconds.
    trainingTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services Identity and Access Management (IAM) role
    -- configured for the training job.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'trainingJob_creationTime' - A timestamp that indicates when the training job was created.
--
-- 'labelingJobArn', 'trainingJob_labelingJobArn' - The Amazon Resource Name (ARN) of the labeling job.
--
-- 'failureReason', 'trainingJob_failureReason' - If the training job failed, the reason it failed.
--
-- 'secondaryStatusTransitions', 'trainingJob_secondaryStatusTransitions' - A history of all of the secondary statuses that the training job has
-- transitioned through.
--
-- 'modelArtifacts', 'trainingJob_modelArtifacts' - Information about the Amazon S3 location that is configured for storing
-- model artifacts.
--
-- 'trainingEndTime', 'trainingJob_trainingEndTime' - Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
--
-- 'environment', 'trainingJob_environment' - The environment variables to set in the Docker container.
--
-- 'billableTimeInSeconds', 'trainingJob_billableTimeInSeconds' - The billable time in seconds.
--
-- 'debugHookConfig', 'trainingJob_debugHookConfig' - Undocumented member.
--
-- 'checkpointConfig', 'trainingJob_checkpointConfig' - Undocumented member.
--
-- 'retryStrategy', 'trainingJob_retryStrategy' - The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
--
-- 'stoppingCondition', 'trainingJob_stoppingCondition' - Specifies a limit to how long a model training job can run. It also
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, Amazon SageMaker ends the training job. Use
-- this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
--
-- 'debugRuleEvaluationStatuses', 'trainingJob_debugRuleEvaluationStatuses' - Information about the evaluation status of the rules for the training
-- job.
--
-- 'trainingJobStatus', 'trainingJob_trainingJobStatus' - The status of the training job.
--
-- Training job statuses are:
--
-- -   @InProgress@ - The training is in progress.
--
-- -   @Completed@ - The training job has completed.
--
-- -   @Failed@ - The training job has failed. To see the reason for the
--     failure, see the @FailureReason@ field in the response to a
--     @DescribeTrainingJobResponse@ call.
--
-- -   @Stopping@ - The training job is stopping.
--
-- -   @Stopped@ - The training job has stopped.
--
-- For more detailed information, see @SecondaryStatus@.
--
-- 'enableNetworkIsolation', 'trainingJob_enableNetworkIsolation' - If the @TrainingJob@ was created with network isolation, the value is
-- set to @true@. If network isolation is enabled, nodes can\'t communicate
-- beyond the VPC they run in.
--
-- 'experimentConfig', 'trainingJob_experimentConfig' - Undocumented member.
--
-- 'lastModifiedTime', 'trainingJob_lastModifiedTime' - A timestamp that indicates when the status of the training job was last
-- modified.
--
-- 'debugRuleConfigurations', 'trainingJob_debugRuleConfigurations' - Information about the debug rule configuration.
--
-- 'enableManagedSpotTraining', 'trainingJob_enableManagedSpotTraining' - When true, enables managed spot training using Amazon EC2 Spot instances
-- to run training jobs instead of on-demand instances. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
--
-- 'autoMLJobArn', 'trainingJob_autoMLJobArn' - The Amazon Resource Name (ARN) of the job.
--
-- 'hyperParameters', 'trainingJob_hyperParameters' - Algorithm-specific parameters.
--
-- 'inputDataConfig', 'trainingJob_inputDataConfig' - An array of @Channel@ objects that describes each data input channel.
--
-- 'vpcConfig', 'trainingJob_vpcConfig' - A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'trainingJobArn', 'trainingJob_trainingJobArn' - The Amazon Resource Name (ARN) of the training job.
--
-- 'algorithmSpecification', 'trainingJob_algorithmSpecification' - Information about the algorithm used for training, and algorithm
-- metadata.
--
-- 'finalMetricDataList', 'trainingJob_finalMetricDataList' - A list of final metric values that are set when the training job
-- completes. Used only if the training job was configured to use metrics.
--
-- 'outputDataConfig', 'trainingJob_outputDataConfig' - The S3 path where model artifacts that you configured when creating the
-- job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- 'trainingStartTime', 'trainingJob_trainingStartTime' - Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
--
-- 'tuningJobArn', 'trainingJob_tuningJobArn' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
--
-- 'trainingJobName', 'trainingJob_trainingJobName' - The name of the training job.
--
-- 'resourceConfig', 'trainingJob_resourceConfig' - Resources, including ML compute instances and ML storage volumes, that
-- are configured for model training.
--
-- 'enableInterContainerTrafficEncryption', 'trainingJob_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
--
-- 'tensorBoardOutputConfig', 'trainingJob_tensorBoardOutputConfig' - Undocumented member.
--
-- 'secondaryStatus', 'trainingJob_secondaryStatus' - Provides detailed information about the state of the training job. For
-- detailed information about the secondary status of the training job, see
-- @StatusMessage@ under SecondaryStatusTransition.
--
-- Amazon SageMaker provides primary statuses and secondary statuses that
-- apply to each of them:
--
-- [InProgress]
--     -   @Starting@ - Starting the training job.
--
--     -   @Downloading@ - An optional stage for algorithms that support
--         @File@ training input mode. It indicates that data is being
--         downloaded to the ML storage volumes.
--
--     -   @Training@ - Training is in progress.
--
--     -   @Uploading@ - Training is complete and the model artifacts are
--         being uploaded to the S3 location.
--
-- [Completed]
--     -   @Completed@ - The training job has completed.
--
-- [Failed]
--     -   @Failed@ - The training job has failed. The reason for the
--         failure is returned in the @FailureReason@ field of
--         @DescribeTrainingJobResponse@.
--
-- [Stopped]
--     -   @MaxRuntimeExceeded@ - The job stopped because it exceeded the
--         maximum allowed runtime.
--
--     -   @Stopped@ - The training job has stopped.
--
-- [Stopping]
--     -   @Stopping@ - Stopping the training job.
--
-- Valid values for @SecondaryStatus@ are subject to change.
--
-- We no longer support the following secondary statuses:
--
-- -   @LaunchingMLInstances@
--
-- -   @PreparingTrainingStack@
--
-- -   @DownloadingTrainingImage@
--
-- 'tags', 'trainingJob_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
--
-- 'trainingTimeInSeconds', 'trainingJob_trainingTimeInSeconds' - The training time in seconds.
--
-- 'roleArn', 'trainingJob_roleArn' - The Amazon Web Services Identity and Access Management (IAM) role
-- configured for the training job.
newTrainingJob ::
  TrainingJob
newTrainingJob =
  TrainingJob'
    { creationTime = Prelude.Nothing,
      labelingJobArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      secondaryStatusTransitions = Prelude.Nothing,
      modelArtifacts = Prelude.Nothing,
      trainingEndTime = Prelude.Nothing,
      environment = Prelude.Nothing,
      billableTimeInSeconds = Prelude.Nothing,
      debugHookConfig = Prelude.Nothing,
      checkpointConfig = Prelude.Nothing,
      retryStrategy = Prelude.Nothing,
      stoppingCondition = Prelude.Nothing,
      debugRuleEvaluationStatuses = Prelude.Nothing,
      trainingJobStatus = Prelude.Nothing,
      enableNetworkIsolation = Prelude.Nothing,
      experimentConfig = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      debugRuleConfigurations = Prelude.Nothing,
      enableManagedSpotTraining = Prelude.Nothing,
      autoMLJobArn = Prelude.Nothing,
      hyperParameters = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      trainingJobArn = Prelude.Nothing,
      algorithmSpecification = Prelude.Nothing,
      finalMetricDataList = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      trainingStartTime = Prelude.Nothing,
      tuningJobArn = Prelude.Nothing,
      trainingJobName = Prelude.Nothing,
      resourceConfig = Prelude.Nothing,
      enableInterContainerTrafficEncryption =
        Prelude.Nothing,
      tensorBoardOutputConfig = Prelude.Nothing,
      secondaryStatus = Prelude.Nothing,
      tags = Prelude.Nothing,
      trainingTimeInSeconds = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | A timestamp that indicates when the training job was created.
trainingJob_creationTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_creationTime = Lens.lens (\TrainingJob' {creationTime} -> creationTime) (\s@TrainingJob' {} a -> s {creationTime = a} :: TrainingJob) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the labeling job.
trainingJob_labelingJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_labelingJobArn = Lens.lens (\TrainingJob' {labelingJobArn} -> labelingJobArn) (\s@TrainingJob' {} a -> s {labelingJobArn = a} :: TrainingJob)

-- | If the training job failed, the reason it failed.
trainingJob_failureReason :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_failureReason = Lens.lens (\TrainingJob' {failureReason} -> failureReason) (\s@TrainingJob' {} a -> s {failureReason = a} :: TrainingJob)

-- | A history of all of the secondary statuses that the training job has
-- transitioned through.
trainingJob_secondaryStatusTransitions :: Lens.Lens' TrainingJob (Prelude.Maybe [SecondaryStatusTransition])
trainingJob_secondaryStatusTransitions = Lens.lens (\TrainingJob' {secondaryStatusTransitions} -> secondaryStatusTransitions) (\s@TrainingJob' {} a -> s {secondaryStatusTransitions = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | Information about the Amazon S3 location that is configured for storing
-- model artifacts.
trainingJob_modelArtifacts :: Lens.Lens' TrainingJob (Prelude.Maybe ModelArtifacts)
trainingJob_modelArtifacts = Lens.lens (\TrainingJob' {modelArtifacts} -> modelArtifacts) (\s@TrainingJob' {} a -> s {modelArtifacts = a} :: TrainingJob)

-- | Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
trainingJob_trainingEndTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_trainingEndTime = Lens.lens (\TrainingJob' {trainingEndTime} -> trainingEndTime) (\s@TrainingJob' {} a -> s {trainingEndTime = a} :: TrainingJob) Prelude.. Lens.mapping Core._Time

-- | The environment variables to set in the Docker container.
trainingJob_environment :: Lens.Lens' TrainingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
trainingJob_environment = Lens.lens (\TrainingJob' {environment} -> environment) (\s@TrainingJob' {} a -> s {environment = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The billable time in seconds.
trainingJob_billableTimeInSeconds :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Natural)
trainingJob_billableTimeInSeconds = Lens.lens (\TrainingJob' {billableTimeInSeconds} -> billableTimeInSeconds) (\s@TrainingJob' {} a -> s {billableTimeInSeconds = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_debugHookConfig :: Lens.Lens' TrainingJob (Prelude.Maybe DebugHookConfig)
trainingJob_debugHookConfig = Lens.lens (\TrainingJob' {debugHookConfig} -> debugHookConfig) (\s@TrainingJob' {} a -> s {debugHookConfig = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_checkpointConfig :: Lens.Lens' TrainingJob (Prelude.Maybe CheckpointConfig)
trainingJob_checkpointConfig = Lens.lens (\TrainingJob' {checkpointConfig} -> checkpointConfig) (\s@TrainingJob' {} a -> s {checkpointConfig = a} :: TrainingJob)

-- | The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
trainingJob_retryStrategy :: Lens.Lens' TrainingJob (Prelude.Maybe RetryStrategy)
trainingJob_retryStrategy = Lens.lens (\TrainingJob' {retryStrategy} -> retryStrategy) (\s@TrainingJob' {} a -> s {retryStrategy = a} :: TrainingJob)

-- | Specifies a limit to how long a model training job can run. It also
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, Amazon SageMaker ends the training job. Use
-- this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
trainingJob_stoppingCondition :: Lens.Lens' TrainingJob (Prelude.Maybe StoppingCondition)
trainingJob_stoppingCondition = Lens.lens (\TrainingJob' {stoppingCondition} -> stoppingCondition) (\s@TrainingJob' {} a -> s {stoppingCondition = a} :: TrainingJob)

-- | Information about the evaluation status of the rules for the training
-- job.
trainingJob_debugRuleEvaluationStatuses :: Lens.Lens' TrainingJob (Prelude.Maybe [DebugRuleEvaluationStatus])
trainingJob_debugRuleEvaluationStatuses = Lens.lens (\TrainingJob' {debugRuleEvaluationStatuses} -> debugRuleEvaluationStatuses) (\s@TrainingJob' {} a -> s {debugRuleEvaluationStatuses = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The status of the training job.
--
-- Training job statuses are:
--
-- -   @InProgress@ - The training is in progress.
--
-- -   @Completed@ - The training job has completed.
--
-- -   @Failed@ - The training job has failed. To see the reason for the
--     failure, see the @FailureReason@ field in the response to a
--     @DescribeTrainingJobResponse@ call.
--
-- -   @Stopping@ - The training job is stopping.
--
-- -   @Stopped@ - The training job has stopped.
--
-- For more detailed information, see @SecondaryStatus@.
trainingJob_trainingJobStatus :: Lens.Lens' TrainingJob (Prelude.Maybe TrainingJobStatus)
trainingJob_trainingJobStatus = Lens.lens (\TrainingJob' {trainingJobStatus} -> trainingJobStatus) (\s@TrainingJob' {} a -> s {trainingJobStatus = a} :: TrainingJob)

-- | If the @TrainingJob@ was created with network isolation, the value is
-- set to @true@. If network isolation is enabled, nodes can\'t communicate
-- beyond the VPC they run in.
trainingJob_enableNetworkIsolation :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Bool)
trainingJob_enableNetworkIsolation = Lens.lens (\TrainingJob' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@TrainingJob' {} a -> s {enableNetworkIsolation = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_experimentConfig :: Lens.Lens' TrainingJob (Prelude.Maybe ExperimentConfig)
trainingJob_experimentConfig = Lens.lens (\TrainingJob' {experimentConfig} -> experimentConfig) (\s@TrainingJob' {} a -> s {experimentConfig = a} :: TrainingJob)

-- | A timestamp that indicates when the status of the training job was last
-- modified.
trainingJob_lastModifiedTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_lastModifiedTime = Lens.lens (\TrainingJob' {lastModifiedTime} -> lastModifiedTime) (\s@TrainingJob' {} a -> s {lastModifiedTime = a} :: TrainingJob) Prelude.. Lens.mapping Core._Time

-- | Information about the debug rule configuration.
trainingJob_debugRuleConfigurations :: Lens.Lens' TrainingJob (Prelude.Maybe [DebugRuleConfiguration])
trainingJob_debugRuleConfigurations = Lens.lens (\TrainingJob' {debugRuleConfigurations} -> debugRuleConfigurations) (\s@TrainingJob' {} a -> s {debugRuleConfigurations = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | When true, enables managed spot training using Amazon EC2 Spot instances
-- to run training jobs instead of on-demand instances. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
trainingJob_enableManagedSpotTraining :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Bool)
trainingJob_enableManagedSpotTraining = Lens.lens (\TrainingJob' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@TrainingJob' {} a -> s {enableManagedSpotTraining = a} :: TrainingJob)

-- | The Amazon Resource Name (ARN) of the job.
trainingJob_autoMLJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_autoMLJobArn = Lens.lens (\TrainingJob' {autoMLJobArn} -> autoMLJobArn) (\s@TrainingJob' {} a -> s {autoMLJobArn = a} :: TrainingJob)

-- | Algorithm-specific parameters.
trainingJob_hyperParameters :: Lens.Lens' TrainingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
trainingJob_hyperParameters = Lens.lens (\TrainingJob' {hyperParameters} -> hyperParameters) (\s@TrainingJob' {} a -> s {hyperParameters = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | An array of @Channel@ objects that describes each data input channel.
trainingJob_inputDataConfig :: Lens.Lens' TrainingJob (Prelude.Maybe (Prelude.NonEmpty Channel))
trainingJob_inputDataConfig = Lens.lens (\TrainingJob' {inputDataConfig} -> inputDataConfig) (\s@TrainingJob' {} a -> s {inputDataConfig = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
trainingJob_vpcConfig :: Lens.Lens' TrainingJob (Prelude.Maybe VpcConfig)
trainingJob_vpcConfig = Lens.lens (\TrainingJob' {vpcConfig} -> vpcConfig) (\s@TrainingJob' {} a -> s {vpcConfig = a} :: TrainingJob)

-- | The Amazon Resource Name (ARN) of the training job.
trainingJob_trainingJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_trainingJobArn = Lens.lens (\TrainingJob' {trainingJobArn} -> trainingJobArn) (\s@TrainingJob' {} a -> s {trainingJobArn = a} :: TrainingJob)

-- | Information about the algorithm used for training, and algorithm
-- metadata.
trainingJob_algorithmSpecification :: Lens.Lens' TrainingJob (Prelude.Maybe AlgorithmSpecification)
trainingJob_algorithmSpecification = Lens.lens (\TrainingJob' {algorithmSpecification} -> algorithmSpecification) (\s@TrainingJob' {} a -> s {algorithmSpecification = a} :: TrainingJob)

-- | A list of final metric values that are set when the training job
-- completes. Used only if the training job was configured to use metrics.
trainingJob_finalMetricDataList :: Lens.Lens' TrainingJob (Prelude.Maybe [MetricData])
trainingJob_finalMetricDataList = Lens.lens (\TrainingJob' {finalMetricDataList} -> finalMetricDataList) (\s@TrainingJob' {} a -> s {finalMetricDataList = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The S3 path where model artifacts that you configured when creating the
-- job are stored. Amazon SageMaker creates subfolders for model artifacts.
trainingJob_outputDataConfig :: Lens.Lens' TrainingJob (Prelude.Maybe OutputDataConfig)
trainingJob_outputDataConfig = Lens.lens (\TrainingJob' {outputDataConfig} -> outputDataConfig) (\s@TrainingJob' {} a -> s {outputDataConfig = a} :: TrainingJob)

-- | Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
trainingJob_trainingStartTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_trainingStartTime = Lens.lens (\TrainingJob' {trainingStartTime} -> trainingStartTime) (\s@TrainingJob' {} a -> s {trainingStartTime = a} :: TrainingJob) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
trainingJob_tuningJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_tuningJobArn = Lens.lens (\TrainingJob' {tuningJobArn} -> tuningJobArn) (\s@TrainingJob' {} a -> s {tuningJobArn = a} :: TrainingJob)

-- | The name of the training job.
trainingJob_trainingJobName :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_trainingJobName = Lens.lens (\TrainingJob' {trainingJobName} -> trainingJobName) (\s@TrainingJob' {} a -> s {trainingJobName = a} :: TrainingJob)

-- | Resources, including ML compute instances and ML storage volumes, that
-- are configured for model training.
trainingJob_resourceConfig :: Lens.Lens' TrainingJob (Prelude.Maybe ResourceConfig)
trainingJob_resourceConfig = Lens.lens (\TrainingJob' {resourceConfig} -> resourceConfig) (\s@TrainingJob' {} a -> s {resourceConfig = a} :: TrainingJob)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
trainingJob_enableInterContainerTrafficEncryption :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Bool)
trainingJob_enableInterContainerTrafficEncryption = Lens.lens (\TrainingJob' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@TrainingJob' {} a -> s {enableInterContainerTrafficEncryption = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_tensorBoardOutputConfig :: Lens.Lens' TrainingJob (Prelude.Maybe TensorBoardOutputConfig)
trainingJob_tensorBoardOutputConfig = Lens.lens (\TrainingJob' {tensorBoardOutputConfig} -> tensorBoardOutputConfig) (\s@TrainingJob' {} a -> s {tensorBoardOutputConfig = a} :: TrainingJob)

-- | Provides detailed information about the state of the training job. For
-- detailed information about the secondary status of the training job, see
-- @StatusMessage@ under SecondaryStatusTransition.
--
-- Amazon SageMaker provides primary statuses and secondary statuses that
-- apply to each of them:
--
-- [InProgress]
--     -   @Starting@ - Starting the training job.
--
--     -   @Downloading@ - An optional stage for algorithms that support
--         @File@ training input mode. It indicates that data is being
--         downloaded to the ML storage volumes.
--
--     -   @Training@ - Training is in progress.
--
--     -   @Uploading@ - Training is complete and the model artifacts are
--         being uploaded to the S3 location.
--
-- [Completed]
--     -   @Completed@ - The training job has completed.
--
-- [Failed]
--     -   @Failed@ - The training job has failed. The reason for the
--         failure is returned in the @FailureReason@ field of
--         @DescribeTrainingJobResponse@.
--
-- [Stopped]
--     -   @MaxRuntimeExceeded@ - The job stopped because it exceeded the
--         maximum allowed runtime.
--
--     -   @Stopped@ - The training job has stopped.
--
-- [Stopping]
--     -   @Stopping@ - Stopping the training job.
--
-- Valid values for @SecondaryStatus@ are subject to change.
--
-- We no longer support the following secondary statuses:
--
-- -   @LaunchingMLInstances@
--
-- -   @PreparingTrainingStack@
--
-- -   @DownloadingTrainingImage@
trainingJob_secondaryStatus :: Lens.Lens' TrainingJob (Prelude.Maybe SecondaryStatus)
trainingJob_secondaryStatus = Lens.lens (\TrainingJob' {secondaryStatus} -> secondaryStatus) (\s@TrainingJob' {} a -> s {secondaryStatus = a} :: TrainingJob)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
trainingJob_tags :: Lens.Lens' TrainingJob (Prelude.Maybe [Tag])
trainingJob_tags = Lens.lens (\TrainingJob' {tags} -> tags) (\s@TrainingJob' {} a -> s {tags = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The training time in seconds.
trainingJob_trainingTimeInSeconds :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Natural)
trainingJob_trainingTimeInSeconds = Lens.lens (\TrainingJob' {trainingTimeInSeconds} -> trainingTimeInSeconds) (\s@TrainingJob' {} a -> s {trainingTimeInSeconds = a} :: TrainingJob)

-- | The Amazon Web Services Identity and Access Management (IAM) role
-- configured for the training job.
trainingJob_roleArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_roleArn = Lens.lens (\TrainingJob' {roleArn} -> roleArn) (\s@TrainingJob' {} a -> s {roleArn = a} :: TrainingJob)

instance Core.FromJSON TrainingJob where
  parseJSON =
    Core.withObject
      "TrainingJob"
      ( \x ->
          TrainingJob'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "LabelingJobArn")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> ( x Core..:? "SecondaryStatusTransitions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ModelArtifacts")
            Prelude.<*> (x Core..:? "TrainingEndTime")
            Prelude.<*> (x Core..:? "Environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "BillableTimeInSeconds")
            Prelude.<*> (x Core..:? "DebugHookConfig")
            Prelude.<*> (x Core..:? "CheckpointConfig")
            Prelude.<*> (x Core..:? "RetryStrategy")
            Prelude.<*> (x Core..:? "StoppingCondition")
            Prelude.<*> ( x Core..:? "DebugRuleEvaluationStatuses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TrainingJobStatus")
            Prelude.<*> (x Core..:? "EnableNetworkIsolation")
            Prelude.<*> (x Core..:? "ExperimentConfig")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> ( x Core..:? "DebugRuleConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EnableManagedSpotTraining")
            Prelude.<*> (x Core..:? "AutoMLJobArn")
            Prelude.<*> ( x Core..:? "HyperParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "TrainingJobArn")
            Prelude.<*> (x Core..:? "AlgorithmSpecification")
            Prelude.<*> ( x Core..:? "FinalMetricDataList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "TrainingStartTime")
            Prelude.<*> (x Core..:? "TuningJobArn")
            Prelude.<*> (x Core..:? "TrainingJobName")
            Prelude.<*> (x Core..:? "ResourceConfig")
            Prelude.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
            Prelude.<*> (x Core..:? "TensorBoardOutputConfig")
            Prelude.<*> (x Core..:? "SecondaryStatus")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TrainingTimeInSeconds")
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance Prelude.Hashable TrainingJob

instance Prelude.NFData TrainingJob
