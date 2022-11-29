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
-- Module      : Amazonka.SageMaker.Types.TrainingJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AlgorithmSpecification
import Amazonka.SageMaker.Types.Channel
import Amazonka.SageMaker.Types.CheckpointConfig
import Amazonka.SageMaker.Types.DebugHookConfig
import Amazonka.SageMaker.Types.DebugRuleConfiguration
import Amazonka.SageMaker.Types.DebugRuleEvaluationStatus
import Amazonka.SageMaker.Types.ExperimentConfig
import Amazonka.SageMaker.Types.MetricData
import Amazonka.SageMaker.Types.ModelArtifacts
import Amazonka.SageMaker.Types.OutputDataConfig
import Amazonka.SageMaker.Types.ResourceConfig
import Amazonka.SageMaker.Types.RetryStrategy
import Amazonka.SageMaker.Types.SecondaryStatus
import Amazonka.SageMaker.Types.SecondaryStatusTransition
import Amazonka.SageMaker.Types.StoppingCondition
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.TensorBoardOutputConfig
import Amazonka.SageMaker.Types.TrainingJobStatus
import Amazonka.SageMaker.Types.VpcConfig

-- | Contains information about a training job.
--
-- /See:/ 'newTrainingJob' smart constructor.
data TrainingJob = TrainingJob'
  { -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The S3 path where model artifacts that you configured when creating the
    -- job are stored. SageMaker creates subfolders for model artifacts.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | When true, enables managed spot training using Amazon EC2 Spot instances
    -- to run training jobs instead of on-demand instances. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
    enableManagedSpotTraining :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services Identity and Access Management (IAM) role
    -- configured for the training job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The environment variables to set in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The training time in seconds.
    trainingTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Information about the evaluation status of the rules for the training
    -- job.
    debugRuleEvaluationStatuses :: Prelude.Maybe [DebugRuleEvaluationStatus],
    -- | The number of times to retry the job when the job fails due to an
    -- @InternalServerError@.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | A VpcConfig object that specifies the VPC that this training job has
    -- access to. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | A history of all of the secondary statuses that the training job has
    -- transitioned through.
    secondaryStatusTransitions :: Prelude.Maybe [SecondaryStatusTransition],
    -- | If the @TrainingJob@ was created with network isolation, the value is
    -- set to @true@. If network isolation is enabled, nodes can\'t communicate
    -- beyond the VPC they run in.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    -- | Resources, including ML compute instances and ML storage volumes, that
    -- are configured for model training.
    resourceConfig :: Prelude.Maybe ResourceConfig,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
    -- job if the training job was launched by a hyperparameter tuning job.
    tuningJobArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the time when the training job starts on training instances.
    -- You are billed for the time interval between this time and the value of
    -- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
    -- this time. The difference is due to the time it takes to download the
    -- training data and to the size of the training container.
    trainingStartTime :: Prelude.Maybe Core.POSIX,
    checkpointConfig :: Prelude.Maybe CheckpointConfig,
    -- | The Amazon Resource Name (ARN) of the job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    debugHookConfig :: Prelude.Maybe DebugHookConfig,
    -- | A timestamp that indicates when the status of the training job was last
    -- modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | Provides detailed information about the state of the training job. For
    -- detailed information about the secondary status of the training job, see
    -- @StatusMessage@ under SecondaryStatusTransition.
    --
    -- SageMaker provides primary statuses and secondary statuses that apply to
    -- each of them:
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
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    -- | A list of final metric values that are set when the training job
    -- completes. Used only if the training job was configured to use metrics.
    finalMetricDataList :: Prelude.Maybe [MetricData],
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
    -- | Information about the Amazon S3 location that is configured for storing
    -- model artifacts.
    modelArtifacts :: Prelude.Maybe ModelArtifacts,
    -- | Specifies a limit to how long a model training job can run. It also
    -- specifies how long a managed Spot training job has to complete. When the
    -- job reaches the time limit, SageMaker ends the training job. Use this
    -- API to cap model training costs.
    --
    -- To stop a job, SageMaker sends the algorithm the @SIGTERM@ signal, which
    -- delays job termination for 120 seconds. Algorithms can use this
    -- 120-second window to save the model artifacts, so the results of
    -- training are not lost.
    stoppingCondition :: Prelude.Maybe StoppingCondition,
    -- | Information about the algorithm used for training, and algorithm
    -- metadata.
    algorithmSpecification :: Prelude.Maybe AlgorithmSpecification,
    -- | Information about the debug rule configuration.
    debugRuleConfigurations :: Prelude.Maybe [DebugRuleConfiguration],
    -- | The Amazon Resource Name (ARN) of the labeling job.
    labelingJobArn :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the training job was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the training job.
    trainingJobName :: Prelude.Maybe Prelude.Text,
    tensorBoardOutputConfig :: Prelude.Maybe TensorBoardOutputConfig,
    -- | The billable time in seconds.
    billableTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Maybe Prelude.Text,
    -- | An array of @Channel@ objects that describes each data input channel.
    inputDataConfig :: Prelude.Maybe (Prelude.NonEmpty Channel),
    -- | Algorithm-specific parameters.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | If the training job failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Indicates the time when the training job ends on training instances. You
    -- are billed for the time interval between the value of
    -- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
    -- this is the time after model artifacts are uploaded. For failed jobs,
    -- this is the time when SageMaker detects a job failure.
    trainingEndTime :: Prelude.Maybe Core.POSIX
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
-- 'tags', 'trainingJob_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
--
-- 'outputDataConfig', 'trainingJob_outputDataConfig' - The S3 path where model artifacts that you configured when creating the
-- job are stored. SageMaker creates subfolders for model artifacts.
--
-- 'enableManagedSpotTraining', 'trainingJob_enableManagedSpotTraining' - When true, enables managed spot training using Amazon EC2 Spot instances
-- to run training jobs instead of on-demand instances. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
--
-- 'roleArn', 'trainingJob_roleArn' - The Amazon Web Services Identity and Access Management (IAM) role
-- configured for the training job.
--
-- 'environment', 'trainingJob_environment' - The environment variables to set in the Docker container.
--
-- 'trainingTimeInSeconds', 'trainingJob_trainingTimeInSeconds' - The training time in seconds.
--
-- 'debugRuleEvaluationStatuses', 'trainingJob_debugRuleEvaluationStatuses' - Information about the evaluation status of the rules for the training
-- job.
--
-- 'retryStrategy', 'trainingJob_retryStrategy' - The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
--
-- 'vpcConfig', 'trainingJob_vpcConfig' - A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'secondaryStatusTransitions', 'trainingJob_secondaryStatusTransitions' - A history of all of the secondary statuses that the training job has
-- transitioned through.
--
-- 'enableNetworkIsolation', 'trainingJob_enableNetworkIsolation' - If the @TrainingJob@ was created with network isolation, the value is
-- set to @true@. If network isolation is enabled, nodes can\'t communicate
-- beyond the VPC they run in.
--
-- 'resourceConfig', 'trainingJob_resourceConfig' - Resources, including ML compute instances and ML storage volumes, that
-- are configured for model training.
--
-- 'experimentConfig', 'trainingJob_experimentConfig' - Undocumented member.
--
-- 'tuningJobArn', 'trainingJob_tuningJobArn' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
--
-- 'trainingStartTime', 'trainingJob_trainingStartTime' - Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
--
-- 'checkpointConfig', 'trainingJob_checkpointConfig' - Undocumented member.
--
-- 'autoMLJobArn', 'trainingJob_autoMLJobArn' - The Amazon Resource Name (ARN) of the job.
--
-- 'debugHookConfig', 'trainingJob_debugHookConfig' - Undocumented member.
--
-- 'lastModifiedTime', 'trainingJob_lastModifiedTime' - A timestamp that indicates when the status of the training job was last
-- modified.
--
-- 'secondaryStatus', 'trainingJob_secondaryStatus' - Provides detailed information about the state of the training job. For
-- detailed information about the secondary status of the training job, see
-- @StatusMessage@ under SecondaryStatusTransition.
--
-- SageMaker provides primary statuses and secondary statuses that apply to
-- each of them:
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
-- 'enableInterContainerTrafficEncryption', 'trainingJob_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
--
-- 'finalMetricDataList', 'trainingJob_finalMetricDataList' - A list of final metric values that are set when the training job
-- completes. Used only if the training job was configured to use metrics.
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
-- 'modelArtifacts', 'trainingJob_modelArtifacts' - Information about the Amazon S3 location that is configured for storing
-- model artifacts.
--
-- 'stoppingCondition', 'trainingJob_stoppingCondition' - Specifies a limit to how long a model training job can run. It also
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, SageMaker sends the algorithm the @SIGTERM@ signal, which
-- delays job termination for 120 seconds. Algorithms can use this
-- 120-second window to save the model artifacts, so the results of
-- training are not lost.
--
-- 'algorithmSpecification', 'trainingJob_algorithmSpecification' - Information about the algorithm used for training, and algorithm
-- metadata.
--
-- 'debugRuleConfigurations', 'trainingJob_debugRuleConfigurations' - Information about the debug rule configuration.
--
-- 'labelingJobArn', 'trainingJob_labelingJobArn' - The Amazon Resource Name (ARN) of the labeling job.
--
-- 'creationTime', 'trainingJob_creationTime' - A timestamp that indicates when the training job was created.
--
-- 'trainingJobName', 'trainingJob_trainingJobName' - The name of the training job.
--
-- 'tensorBoardOutputConfig', 'trainingJob_tensorBoardOutputConfig' - Undocumented member.
--
-- 'billableTimeInSeconds', 'trainingJob_billableTimeInSeconds' - The billable time in seconds.
--
-- 'trainingJobArn', 'trainingJob_trainingJobArn' - The Amazon Resource Name (ARN) of the training job.
--
-- 'inputDataConfig', 'trainingJob_inputDataConfig' - An array of @Channel@ objects that describes each data input channel.
--
-- 'hyperParameters', 'trainingJob_hyperParameters' - Algorithm-specific parameters.
--
-- 'failureReason', 'trainingJob_failureReason' - If the training job failed, the reason it failed.
--
-- 'trainingEndTime', 'trainingJob_trainingEndTime' - Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when SageMaker detects a job failure.
newTrainingJob ::
  TrainingJob
newTrainingJob =
  TrainingJob'
    { tags = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      enableManagedSpotTraining = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      environment = Prelude.Nothing,
      trainingTimeInSeconds = Prelude.Nothing,
      debugRuleEvaluationStatuses = Prelude.Nothing,
      retryStrategy = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      secondaryStatusTransitions = Prelude.Nothing,
      enableNetworkIsolation = Prelude.Nothing,
      resourceConfig = Prelude.Nothing,
      experimentConfig = Prelude.Nothing,
      tuningJobArn = Prelude.Nothing,
      trainingStartTime = Prelude.Nothing,
      checkpointConfig = Prelude.Nothing,
      autoMLJobArn = Prelude.Nothing,
      debugHookConfig = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      secondaryStatus = Prelude.Nothing,
      enableInterContainerTrafficEncryption =
        Prelude.Nothing,
      finalMetricDataList = Prelude.Nothing,
      trainingJobStatus = Prelude.Nothing,
      modelArtifacts = Prelude.Nothing,
      stoppingCondition = Prelude.Nothing,
      algorithmSpecification = Prelude.Nothing,
      debugRuleConfigurations = Prelude.Nothing,
      labelingJobArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      trainingJobName = Prelude.Nothing,
      tensorBoardOutputConfig = Prelude.Nothing,
      billableTimeInSeconds = Prelude.Nothing,
      trainingJobArn = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      hyperParameters = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      trainingEndTime = Prelude.Nothing
    }

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
trainingJob_tags :: Lens.Lens' TrainingJob (Prelude.Maybe [Tag])
trainingJob_tags = Lens.lens (\TrainingJob' {tags} -> tags) (\s@TrainingJob' {} a -> s {tags = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The S3 path where model artifacts that you configured when creating the
-- job are stored. SageMaker creates subfolders for model artifacts.
trainingJob_outputDataConfig :: Lens.Lens' TrainingJob (Prelude.Maybe OutputDataConfig)
trainingJob_outputDataConfig = Lens.lens (\TrainingJob' {outputDataConfig} -> outputDataConfig) (\s@TrainingJob' {} a -> s {outputDataConfig = a} :: TrainingJob)

-- | When true, enables managed spot training using Amazon EC2 Spot instances
-- to run training jobs instead of on-demand instances. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
trainingJob_enableManagedSpotTraining :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Bool)
trainingJob_enableManagedSpotTraining = Lens.lens (\TrainingJob' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@TrainingJob' {} a -> s {enableManagedSpotTraining = a} :: TrainingJob)

-- | The Amazon Web Services Identity and Access Management (IAM) role
-- configured for the training job.
trainingJob_roleArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_roleArn = Lens.lens (\TrainingJob' {roleArn} -> roleArn) (\s@TrainingJob' {} a -> s {roleArn = a} :: TrainingJob)

-- | The environment variables to set in the Docker container.
trainingJob_environment :: Lens.Lens' TrainingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
trainingJob_environment = Lens.lens (\TrainingJob' {environment} -> environment) (\s@TrainingJob' {} a -> s {environment = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The training time in seconds.
trainingJob_trainingTimeInSeconds :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Natural)
trainingJob_trainingTimeInSeconds = Lens.lens (\TrainingJob' {trainingTimeInSeconds} -> trainingTimeInSeconds) (\s@TrainingJob' {} a -> s {trainingTimeInSeconds = a} :: TrainingJob)

-- | Information about the evaluation status of the rules for the training
-- job.
trainingJob_debugRuleEvaluationStatuses :: Lens.Lens' TrainingJob (Prelude.Maybe [DebugRuleEvaluationStatus])
trainingJob_debugRuleEvaluationStatuses = Lens.lens (\TrainingJob' {debugRuleEvaluationStatuses} -> debugRuleEvaluationStatuses) (\s@TrainingJob' {} a -> s {debugRuleEvaluationStatuses = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
trainingJob_retryStrategy :: Lens.Lens' TrainingJob (Prelude.Maybe RetryStrategy)
trainingJob_retryStrategy = Lens.lens (\TrainingJob' {retryStrategy} -> retryStrategy) (\s@TrainingJob' {} a -> s {retryStrategy = a} :: TrainingJob)

-- | A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
trainingJob_vpcConfig :: Lens.Lens' TrainingJob (Prelude.Maybe VpcConfig)
trainingJob_vpcConfig = Lens.lens (\TrainingJob' {vpcConfig} -> vpcConfig) (\s@TrainingJob' {} a -> s {vpcConfig = a} :: TrainingJob)

-- | A history of all of the secondary statuses that the training job has
-- transitioned through.
trainingJob_secondaryStatusTransitions :: Lens.Lens' TrainingJob (Prelude.Maybe [SecondaryStatusTransition])
trainingJob_secondaryStatusTransitions = Lens.lens (\TrainingJob' {secondaryStatusTransitions} -> secondaryStatusTransitions) (\s@TrainingJob' {} a -> s {secondaryStatusTransitions = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | If the @TrainingJob@ was created with network isolation, the value is
-- set to @true@. If network isolation is enabled, nodes can\'t communicate
-- beyond the VPC they run in.
trainingJob_enableNetworkIsolation :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Bool)
trainingJob_enableNetworkIsolation = Lens.lens (\TrainingJob' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@TrainingJob' {} a -> s {enableNetworkIsolation = a} :: TrainingJob)

-- | Resources, including ML compute instances and ML storage volumes, that
-- are configured for model training.
trainingJob_resourceConfig :: Lens.Lens' TrainingJob (Prelude.Maybe ResourceConfig)
trainingJob_resourceConfig = Lens.lens (\TrainingJob' {resourceConfig} -> resourceConfig) (\s@TrainingJob' {} a -> s {resourceConfig = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_experimentConfig :: Lens.Lens' TrainingJob (Prelude.Maybe ExperimentConfig)
trainingJob_experimentConfig = Lens.lens (\TrainingJob' {experimentConfig} -> experimentConfig) (\s@TrainingJob' {} a -> s {experimentConfig = a} :: TrainingJob)

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
trainingJob_tuningJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_tuningJobArn = Lens.lens (\TrainingJob' {tuningJobArn} -> tuningJobArn) (\s@TrainingJob' {} a -> s {tuningJobArn = a} :: TrainingJob)

-- | Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
trainingJob_trainingStartTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_trainingStartTime = Lens.lens (\TrainingJob' {trainingStartTime} -> trainingStartTime) (\s@TrainingJob' {} a -> s {trainingStartTime = a} :: TrainingJob) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
trainingJob_checkpointConfig :: Lens.Lens' TrainingJob (Prelude.Maybe CheckpointConfig)
trainingJob_checkpointConfig = Lens.lens (\TrainingJob' {checkpointConfig} -> checkpointConfig) (\s@TrainingJob' {} a -> s {checkpointConfig = a} :: TrainingJob)

-- | The Amazon Resource Name (ARN) of the job.
trainingJob_autoMLJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_autoMLJobArn = Lens.lens (\TrainingJob' {autoMLJobArn} -> autoMLJobArn) (\s@TrainingJob' {} a -> s {autoMLJobArn = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_debugHookConfig :: Lens.Lens' TrainingJob (Prelude.Maybe DebugHookConfig)
trainingJob_debugHookConfig = Lens.lens (\TrainingJob' {debugHookConfig} -> debugHookConfig) (\s@TrainingJob' {} a -> s {debugHookConfig = a} :: TrainingJob)

-- | A timestamp that indicates when the status of the training job was last
-- modified.
trainingJob_lastModifiedTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_lastModifiedTime = Lens.lens (\TrainingJob' {lastModifiedTime} -> lastModifiedTime) (\s@TrainingJob' {} a -> s {lastModifiedTime = a} :: TrainingJob) Prelude.. Lens.mapping Core._Time

-- | Provides detailed information about the state of the training job. For
-- detailed information about the secondary status of the training job, see
-- @StatusMessage@ under SecondaryStatusTransition.
--
-- SageMaker provides primary statuses and secondary statuses that apply to
-- each of them:
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

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
trainingJob_enableInterContainerTrafficEncryption :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Bool)
trainingJob_enableInterContainerTrafficEncryption = Lens.lens (\TrainingJob' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@TrainingJob' {} a -> s {enableInterContainerTrafficEncryption = a} :: TrainingJob)

-- | A list of final metric values that are set when the training job
-- completes. Used only if the training job was configured to use metrics.
trainingJob_finalMetricDataList :: Lens.Lens' TrainingJob (Prelude.Maybe [MetricData])
trainingJob_finalMetricDataList = Lens.lens (\TrainingJob' {finalMetricDataList} -> finalMetricDataList) (\s@TrainingJob' {} a -> s {finalMetricDataList = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

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

-- | Information about the Amazon S3 location that is configured for storing
-- model artifacts.
trainingJob_modelArtifacts :: Lens.Lens' TrainingJob (Prelude.Maybe ModelArtifacts)
trainingJob_modelArtifacts = Lens.lens (\TrainingJob' {modelArtifacts} -> modelArtifacts) (\s@TrainingJob' {} a -> s {modelArtifacts = a} :: TrainingJob)

-- | Specifies a limit to how long a model training job can run. It also
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, SageMaker sends the algorithm the @SIGTERM@ signal, which
-- delays job termination for 120 seconds. Algorithms can use this
-- 120-second window to save the model artifacts, so the results of
-- training are not lost.
trainingJob_stoppingCondition :: Lens.Lens' TrainingJob (Prelude.Maybe StoppingCondition)
trainingJob_stoppingCondition = Lens.lens (\TrainingJob' {stoppingCondition} -> stoppingCondition) (\s@TrainingJob' {} a -> s {stoppingCondition = a} :: TrainingJob)

-- | Information about the algorithm used for training, and algorithm
-- metadata.
trainingJob_algorithmSpecification :: Lens.Lens' TrainingJob (Prelude.Maybe AlgorithmSpecification)
trainingJob_algorithmSpecification = Lens.lens (\TrainingJob' {algorithmSpecification} -> algorithmSpecification) (\s@TrainingJob' {} a -> s {algorithmSpecification = a} :: TrainingJob)

-- | Information about the debug rule configuration.
trainingJob_debugRuleConfigurations :: Lens.Lens' TrainingJob (Prelude.Maybe [DebugRuleConfiguration])
trainingJob_debugRuleConfigurations = Lens.lens (\TrainingJob' {debugRuleConfigurations} -> debugRuleConfigurations) (\s@TrainingJob' {} a -> s {debugRuleConfigurations = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the labeling job.
trainingJob_labelingJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_labelingJobArn = Lens.lens (\TrainingJob' {labelingJobArn} -> labelingJobArn) (\s@TrainingJob' {} a -> s {labelingJobArn = a} :: TrainingJob)

-- | A timestamp that indicates when the training job was created.
trainingJob_creationTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_creationTime = Lens.lens (\TrainingJob' {creationTime} -> creationTime) (\s@TrainingJob' {} a -> s {creationTime = a} :: TrainingJob) Prelude.. Lens.mapping Core._Time

-- | The name of the training job.
trainingJob_trainingJobName :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_trainingJobName = Lens.lens (\TrainingJob' {trainingJobName} -> trainingJobName) (\s@TrainingJob' {} a -> s {trainingJobName = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_tensorBoardOutputConfig :: Lens.Lens' TrainingJob (Prelude.Maybe TensorBoardOutputConfig)
trainingJob_tensorBoardOutputConfig = Lens.lens (\TrainingJob' {tensorBoardOutputConfig} -> tensorBoardOutputConfig) (\s@TrainingJob' {} a -> s {tensorBoardOutputConfig = a} :: TrainingJob)

-- | The billable time in seconds.
trainingJob_billableTimeInSeconds :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Natural)
trainingJob_billableTimeInSeconds = Lens.lens (\TrainingJob' {billableTimeInSeconds} -> billableTimeInSeconds) (\s@TrainingJob' {} a -> s {billableTimeInSeconds = a} :: TrainingJob)

-- | The Amazon Resource Name (ARN) of the training job.
trainingJob_trainingJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_trainingJobArn = Lens.lens (\TrainingJob' {trainingJobArn} -> trainingJobArn) (\s@TrainingJob' {} a -> s {trainingJobArn = a} :: TrainingJob)

-- | An array of @Channel@ objects that describes each data input channel.
trainingJob_inputDataConfig :: Lens.Lens' TrainingJob (Prelude.Maybe (Prelude.NonEmpty Channel))
trainingJob_inputDataConfig = Lens.lens (\TrainingJob' {inputDataConfig} -> inputDataConfig) (\s@TrainingJob' {} a -> s {inputDataConfig = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | Algorithm-specific parameters.
trainingJob_hyperParameters :: Lens.Lens' TrainingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
trainingJob_hyperParameters = Lens.lens (\TrainingJob' {hyperParameters} -> hyperParameters) (\s@TrainingJob' {} a -> s {hyperParameters = a} :: TrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | If the training job failed, the reason it failed.
trainingJob_failureReason :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_failureReason = Lens.lens (\TrainingJob' {failureReason} -> failureReason) (\s@TrainingJob' {} a -> s {failureReason = a} :: TrainingJob)

-- | Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when SageMaker detects a job failure.
trainingJob_trainingEndTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_trainingEndTime = Lens.lens (\TrainingJob' {trainingEndTime} -> trainingEndTime) (\s@TrainingJob' {} a -> s {trainingEndTime = a} :: TrainingJob) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON TrainingJob where
  parseJSON =
    Core.withObject
      "TrainingJob"
      ( \x ->
          TrainingJob'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "EnableManagedSpotTraining")
            Prelude.<*> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "Environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TrainingTimeInSeconds")
            Prelude.<*> ( x Core..:? "DebugRuleEvaluationStatuses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "RetryStrategy")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> ( x Core..:? "SecondaryStatusTransitions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EnableNetworkIsolation")
            Prelude.<*> (x Core..:? "ResourceConfig")
            Prelude.<*> (x Core..:? "ExperimentConfig")
            Prelude.<*> (x Core..:? "TuningJobArn")
            Prelude.<*> (x Core..:? "TrainingStartTime")
            Prelude.<*> (x Core..:? "CheckpointConfig")
            Prelude.<*> (x Core..:? "AutoMLJobArn")
            Prelude.<*> (x Core..:? "DebugHookConfig")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "SecondaryStatus")
            Prelude.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
            Prelude.<*> ( x Core..:? "FinalMetricDataList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TrainingJobStatus")
            Prelude.<*> (x Core..:? "ModelArtifacts")
            Prelude.<*> (x Core..:? "StoppingCondition")
            Prelude.<*> (x Core..:? "AlgorithmSpecification")
            Prelude.<*> ( x Core..:? "DebugRuleConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LabelingJobArn")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "TrainingJobName")
            Prelude.<*> (x Core..:? "TensorBoardOutputConfig")
            Prelude.<*> (x Core..:? "BillableTimeInSeconds")
            Prelude.<*> (x Core..:? "TrainingJobArn")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> ( x Core..:? "HyperParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "TrainingEndTime")
      )

instance Prelude.Hashable TrainingJob where
  hashWithSalt _salt TrainingJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` enableManagedSpotTraining
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` trainingTimeInSeconds
      `Prelude.hashWithSalt` debugRuleEvaluationStatuses
      `Prelude.hashWithSalt` retryStrategy
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` secondaryStatusTransitions
      `Prelude.hashWithSalt` enableNetworkIsolation
      `Prelude.hashWithSalt` resourceConfig
      `Prelude.hashWithSalt` experimentConfig
      `Prelude.hashWithSalt` tuningJobArn
      `Prelude.hashWithSalt` trainingStartTime
      `Prelude.hashWithSalt` checkpointConfig
      `Prelude.hashWithSalt` autoMLJobArn
      `Prelude.hashWithSalt` debugHookConfig
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` secondaryStatus
      `Prelude.hashWithSalt` enableInterContainerTrafficEncryption
      `Prelude.hashWithSalt` finalMetricDataList
      `Prelude.hashWithSalt` trainingJobStatus
      `Prelude.hashWithSalt` modelArtifacts
      `Prelude.hashWithSalt` stoppingCondition
      `Prelude.hashWithSalt` algorithmSpecification
      `Prelude.hashWithSalt` debugRuleConfigurations
      `Prelude.hashWithSalt` labelingJobArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` trainingJobName
      `Prelude.hashWithSalt` tensorBoardOutputConfig
      `Prelude.hashWithSalt` billableTimeInSeconds
      `Prelude.hashWithSalt` trainingJobArn
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` hyperParameters
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` trainingEndTime

instance Prelude.NFData TrainingJob where
  rnf TrainingJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf enableManagedSpotTraining
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf trainingTimeInSeconds
      `Prelude.seq` Prelude.rnf debugRuleEvaluationStatuses
      `Prelude.seq` Prelude.rnf retryStrategy
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf secondaryStatusTransitions
      `Prelude.seq` Prelude.rnf enableNetworkIsolation
      `Prelude.seq` Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf experimentConfig
      `Prelude.seq` Prelude.rnf tuningJobArn
      `Prelude.seq` Prelude.rnf trainingStartTime
      `Prelude.seq` Prelude.rnf checkpointConfig
      `Prelude.seq` Prelude.rnf autoMLJobArn
      `Prelude.seq` Prelude.rnf debugHookConfig
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf secondaryStatus
      `Prelude.seq` Prelude.rnf
        enableInterContainerTrafficEncryption
      `Prelude.seq` Prelude.rnf
        finalMetricDataList
      `Prelude.seq` Prelude.rnf
        trainingJobStatus
      `Prelude.seq` Prelude.rnf
        modelArtifacts
      `Prelude.seq` Prelude.rnf
        stoppingCondition
      `Prelude.seq` Prelude.rnf
        algorithmSpecification
      `Prelude.seq` Prelude.rnf
        debugRuleConfigurations
      `Prelude.seq` Prelude.rnf
        labelingJobArn
      `Prelude.seq` Prelude.rnf
        creationTime
      `Prelude.seq` Prelude.rnf
        trainingJobName
      `Prelude.seq` Prelude.rnf
        tensorBoardOutputConfig
      `Prelude.seq` Prelude.rnf
        billableTimeInSeconds
      `Prelude.seq` Prelude.rnf
        trainingJobArn
      `Prelude.seq` Prelude.rnf
        inputDataConfig
      `Prelude.seq` Prelude.rnf
        hyperParameters
      `Prelude.seq` Prelude.rnf
        failureReason
      `Prelude.seq` Prelude.rnf
        trainingEndTime
