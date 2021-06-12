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
import Network.AWS.SageMaker.Types.VpcConfig

-- | Contains information about a training job.
--
-- /See:/ 'newTrainingJob' smart constructor.
data TrainingJob = TrainingJob'
  { -- | A VpcConfig object that specifies the VPC that this training job has
    -- access to. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | Information about the debug rule configuration.
    debugRuleConfigurations :: Core.Maybe [DebugRuleConfiguration],
    -- | An array of @Channel@ objects that describes each data input channel.
    inputDataConfig :: Core.Maybe (Core.NonEmpty Channel),
    -- | Algorithm-specific parameters.
    hyperParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | When true, enables managed spot training using Amazon EC2 Spot instances
    -- to run training jobs instead of on-demand instances. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
    enableManagedSpotTraining :: Core.Maybe Core.Bool,
    -- | A timestamp that indicates when the training job was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the labeling job.
    labelingJobArn :: Core.Maybe Core.Text,
    -- | The AWS Identity and Access Management (IAM) role configured for the
    -- training job.
    roleArn :: Core.Maybe Core.Text,
    -- | The training time in seconds.
    trainingTimeInSeconds :: Core.Maybe Core.Natural,
    experimentConfig :: Core.Maybe ExperimentConfig,
    -- | If the @TrainingJob@ was created with network isolation, the value is
    -- set to @true@. If network isolation is enabled, nodes can\'t communicate
    -- beyond the VPC they run in.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool,
    -- | The name of the training job.
    trainingJobName :: Core.Maybe Core.Text,
    checkpointConfig :: Core.Maybe CheckpointConfig,
    -- | The S3 path where model artifacts that you configured when creating the
    -- job are stored. Amazon SageMaker creates subfolders for model artifacts.
    outputDataConfig :: Core.Maybe OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
    -- job if the training job was launched by a hyperparameter tuning job.
    tuningJobArn :: Core.Maybe Core.Text,
    -- | Information about the Amazon S3 location that is configured for storing
    -- model artifacts.
    modelArtifacts :: Core.Maybe ModelArtifacts,
    -- | A history of all of the secondary statuses that the training job has
    -- transitioned through.
    secondaryStatusTransitions :: Core.Maybe [SecondaryStatusTransition],
    -- | A list of final metric values that are set when the training job
    -- completes. Used only if the training job was configured to use metrics.
    finalMetricDataList :: Core.Maybe [MetricData],
    -- | The Amazon Resource Name (ARN) of the job.
    autoMLJobArn :: Core.Maybe Core.Text,
    -- | If the training job failed, the reason it failed.
    failureReason :: Core.Maybe Core.Text,
    -- | An array of key-value pairs. You can use tags to categorize your AWS
    -- resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
    tags :: Core.Maybe [Tag],
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
    secondaryStatus :: Core.Maybe SecondaryStatus,
    -- | A timestamp that indicates when the status of the training job was last
    -- modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    tensorBoardOutputConfig :: Core.Maybe TensorBoardOutputConfig,
    -- | Specifies a limit to how long a model training job can run. When the job
    -- reaches the time limit, Amazon SageMaker ends the training job. Use this
    -- API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
    -- signal, which delays job termination for 120 seconds. Algorithms can use
    -- this 120-second window to save the model artifacts, so the results of
    -- training are not lost.
    stoppingCondition :: Core.Maybe StoppingCondition,
    -- | Information about the evaluation status of the rules for the training
    -- job.
    debugRuleEvaluationStatuses :: Core.Maybe [DebugRuleEvaluationStatus],
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
    trainingJobStatus :: Core.Maybe TrainingJobStatus,
    debugHookConfig :: Core.Maybe DebugHookConfig,
    -- | The billable time in seconds.
    billableTimeInSeconds :: Core.Maybe Core.Natural,
    -- | Resources, including ML compute instances and ML storage volumes, that
    -- are configured for model training.
    resourceConfig :: Core.Maybe ResourceConfig,
    -- | Indicates the time when the training job starts on training instances.
    -- You are billed for the time interval between this time and the value of
    -- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
    -- this time. The difference is due to the time it takes to download the
    -- training data and to the size of the training container.
    trainingStartTime :: Core.Maybe Core.POSIX,
    -- | Indicates the time when the training job ends on training instances. You
    -- are billed for the time interval between the value of
    -- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
    -- this is the time after model artifacts are uploaded. For failed jobs,
    -- this is the time when Amazon SageMaker detects a job failure.
    trainingEndTime :: Core.Maybe Core.POSIX,
    -- | Information about the algorithm used for training, and algorithm
    -- metadata.
    algorithmSpecification :: Core.Maybe AlgorithmSpecification,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrainingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'trainingJob_vpcConfig' - A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'debugRuleConfigurations', 'trainingJob_debugRuleConfigurations' - Information about the debug rule configuration.
--
-- 'inputDataConfig', 'trainingJob_inputDataConfig' - An array of @Channel@ objects that describes each data input channel.
--
-- 'hyperParameters', 'trainingJob_hyperParameters' - Algorithm-specific parameters.
--
-- 'enableManagedSpotTraining', 'trainingJob_enableManagedSpotTraining' - When true, enables managed spot training using Amazon EC2 Spot instances
-- to run training jobs instead of on-demand instances. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
--
-- 'creationTime', 'trainingJob_creationTime' - A timestamp that indicates when the training job was created.
--
-- 'labelingJobArn', 'trainingJob_labelingJobArn' - The Amazon Resource Name (ARN) of the labeling job.
--
-- 'roleArn', 'trainingJob_roleArn' - The AWS Identity and Access Management (IAM) role configured for the
-- training job.
--
-- 'trainingTimeInSeconds', 'trainingJob_trainingTimeInSeconds' - The training time in seconds.
--
-- 'experimentConfig', 'trainingJob_experimentConfig' - Undocumented member.
--
-- 'enableNetworkIsolation', 'trainingJob_enableNetworkIsolation' - If the @TrainingJob@ was created with network isolation, the value is
-- set to @true@. If network isolation is enabled, nodes can\'t communicate
-- beyond the VPC they run in.
--
-- 'enableInterContainerTrafficEncryption', 'trainingJob_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
--
-- 'trainingJobName', 'trainingJob_trainingJobName' - The name of the training job.
--
-- 'checkpointConfig', 'trainingJob_checkpointConfig' - Undocumented member.
--
-- 'outputDataConfig', 'trainingJob_outputDataConfig' - The S3 path where model artifacts that you configured when creating the
-- job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- 'tuningJobArn', 'trainingJob_tuningJobArn' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
--
-- 'modelArtifacts', 'trainingJob_modelArtifacts' - Information about the Amazon S3 location that is configured for storing
-- model artifacts.
--
-- 'secondaryStatusTransitions', 'trainingJob_secondaryStatusTransitions' - A history of all of the secondary statuses that the training job has
-- transitioned through.
--
-- 'finalMetricDataList', 'trainingJob_finalMetricDataList' - A list of final metric values that are set when the training job
-- completes. Used only if the training job was configured to use metrics.
--
-- 'autoMLJobArn', 'trainingJob_autoMLJobArn' - The Amazon Resource Name (ARN) of the job.
--
-- 'failureReason', 'trainingJob_failureReason' - If the training job failed, the reason it failed.
--
-- 'tags', 'trainingJob_tags' - An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
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
-- 'lastModifiedTime', 'trainingJob_lastModifiedTime' - A timestamp that indicates when the status of the training job was last
-- modified.
--
-- 'tensorBoardOutputConfig', 'trainingJob_tensorBoardOutputConfig' - Undocumented member.
--
-- 'stoppingCondition', 'trainingJob_stoppingCondition' - Specifies a limit to how long a model training job can run. When the job
-- reaches the time limit, Amazon SageMaker ends the training job. Use this
-- API to cap model training costs.
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
-- 'debugHookConfig', 'trainingJob_debugHookConfig' - Undocumented member.
--
-- 'billableTimeInSeconds', 'trainingJob_billableTimeInSeconds' - The billable time in seconds.
--
-- 'resourceConfig', 'trainingJob_resourceConfig' - Resources, including ML compute instances and ML storage volumes, that
-- are configured for model training.
--
-- 'trainingStartTime', 'trainingJob_trainingStartTime' - Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
--
-- 'trainingEndTime', 'trainingJob_trainingEndTime' - Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
--
-- 'algorithmSpecification', 'trainingJob_algorithmSpecification' - Information about the algorithm used for training, and algorithm
-- metadata.
--
-- 'trainingJobArn', 'trainingJob_trainingJobArn' - The Amazon Resource Name (ARN) of the training job.
newTrainingJob ::
  TrainingJob
newTrainingJob =
  TrainingJob'
    { vpcConfig = Core.Nothing,
      debugRuleConfigurations = Core.Nothing,
      inputDataConfig = Core.Nothing,
      hyperParameters = Core.Nothing,
      enableManagedSpotTraining = Core.Nothing,
      creationTime = Core.Nothing,
      labelingJobArn = Core.Nothing,
      roleArn = Core.Nothing,
      trainingTimeInSeconds = Core.Nothing,
      experimentConfig = Core.Nothing,
      enableNetworkIsolation = Core.Nothing,
      enableInterContainerTrafficEncryption = Core.Nothing,
      trainingJobName = Core.Nothing,
      checkpointConfig = Core.Nothing,
      outputDataConfig = Core.Nothing,
      tuningJobArn = Core.Nothing,
      modelArtifacts = Core.Nothing,
      secondaryStatusTransitions = Core.Nothing,
      finalMetricDataList = Core.Nothing,
      autoMLJobArn = Core.Nothing,
      failureReason = Core.Nothing,
      tags = Core.Nothing,
      secondaryStatus = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      tensorBoardOutputConfig = Core.Nothing,
      stoppingCondition = Core.Nothing,
      debugRuleEvaluationStatuses = Core.Nothing,
      trainingJobStatus = Core.Nothing,
      debugHookConfig = Core.Nothing,
      billableTimeInSeconds = Core.Nothing,
      resourceConfig = Core.Nothing,
      trainingStartTime = Core.Nothing,
      trainingEndTime = Core.Nothing,
      algorithmSpecification = Core.Nothing,
      trainingJobArn = Core.Nothing
    }

-- | A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
trainingJob_vpcConfig :: Lens.Lens' TrainingJob (Core.Maybe VpcConfig)
trainingJob_vpcConfig = Lens.lens (\TrainingJob' {vpcConfig} -> vpcConfig) (\s@TrainingJob' {} a -> s {vpcConfig = a} :: TrainingJob)

-- | Information about the debug rule configuration.
trainingJob_debugRuleConfigurations :: Lens.Lens' TrainingJob (Core.Maybe [DebugRuleConfiguration])
trainingJob_debugRuleConfigurations = Lens.lens (\TrainingJob' {debugRuleConfigurations} -> debugRuleConfigurations) (\s@TrainingJob' {} a -> s {debugRuleConfigurations = a} :: TrainingJob) Core.. Lens.mapping Lens._Coerce

-- | An array of @Channel@ objects that describes each data input channel.
trainingJob_inputDataConfig :: Lens.Lens' TrainingJob (Core.Maybe (Core.NonEmpty Channel))
trainingJob_inputDataConfig = Lens.lens (\TrainingJob' {inputDataConfig} -> inputDataConfig) (\s@TrainingJob' {} a -> s {inputDataConfig = a} :: TrainingJob) Core.. Lens.mapping Lens._Coerce

-- | Algorithm-specific parameters.
trainingJob_hyperParameters :: Lens.Lens' TrainingJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
trainingJob_hyperParameters = Lens.lens (\TrainingJob' {hyperParameters} -> hyperParameters) (\s@TrainingJob' {} a -> s {hyperParameters = a} :: TrainingJob) Core.. Lens.mapping Lens._Coerce

-- | When true, enables managed spot training using Amazon EC2 Spot instances
-- to run training jobs instead of on-demand instances. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
trainingJob_enableManagedSpotTraining :: Lens.Lens' TrainingJob (Core.Maybe Core.Bool)
trainingJob_enableManagedSpotTraining = Lens.lens (\TrainingJob' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@TrainingJob' {} a -> s {enableManagedSpotTraining = a} :: TrainingJob)

-- | A timestamp that indicates when the training job was created.
trainingJob_creationTime :: Lens.Lens' TrainingJob (Core.Maybe Core.UTCTime)
trainingJob_creationTime = Lens.lens (\TrainingJob' {creationTime} -> creationTime) (\s@TrainingJob' {} a -> s {creationTime = a} :: TrainingJob) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the labeling job.
trainingJob_labelingJobArn :: Lens.Lens' TrainingJob (Core.Maybe Core.Text)
trainingJob_labelingJobArn = Lens.lens (\TrainingJob' {labelingJobArn} -> labelingJobArn) (\s@TrainingJob' {} a -> s {labelingJobArn = a} :: TrainingJob)

-- | The AWS Identity and Access Management (IAM) role configured for the
-- training job.
trainingJob_roleArn :: Lens.Lens' TrainingJob (Core.Maybe Core.Text)
trainingJob_roleArn = Lens.lens (\TrainingJob' {roleArn} -> roleArn) (\s@TrainingJob' {} a -> s {roleArn = a} :: TrainingJob)

-- | The training time in seconds.
trainingJob_trainingTimeInSeconds :: Lens.Lens' TrainingJob (Core.Maybe Core.Natural)
trainingJob_trainingTimeInSeconds = Lens.lens (\TrainingJob' {trainingTimeInSeconds} -> trainingTimeInSeconds) (\s@TrainingJob' {} a -> s {trainingTimeInSeconds = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_experimentConfig :: Lens.Lens' TrainingJob (Core.Maybe ExperimentConfig)
trainingJob_experimentConfig = Lens.lens (\TrainingJob' {experimentConfig} -> experimentConfig) (\s@TrainingJob' {} a -> s {experimentConfig = a} :: TrainingJob)

-- | If the @TrainingJob@ was created with network isolation, the value is
-- set to @true@. If network isolation is enabled, nodes can\'t communicate
-- beyond the VPC they run in.
trainingJob_enableNetworkIsolation :: Lens.Lens' TrainingJob (Core.Maybe Core.Bool)
trainingJob_enableNetworkIsolation = Lens.lens (\TrainingJob' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@TrainingJob' {} a -> s {enableNetworkIsolation = a} :: TrainingJob)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
trainingJob_enableInterContainerTrafficEncryption :: Lens.Lens' TrainingJob (Core.Maybe Core.Bool)
trainingJob_enableInterContainerTrafficEncryption = Lens.lens (\TrainingJob' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@TrainingJob' {} a -> s {enableInterContainerTrafficEncryption = a} :: TrainingJob)

-- | The name of the training job.
trainingJob_trainingJobName :: Lens.Lens' TrainingJob (Core.Maybe Core.Text)
trainingJob_trainingJobName = Lens.lens (\TrainingJob' {trainingJobName} -> trainingJobName) (\s@TrainingJob' {} a -> s {trainingJobName = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_checkpointConfig :: Lens.Lens' TrainingJob (Core.Maybe CheckpointConfig)
trainingJob_checkpointConfig = Lens.lens (\TrainingJob' {checkpointConfig} -> checkpointConfig) (\s@TrainingJob' {} a -> s {checkpointConfig = a} :: TrainingJob)

-- | The S3 path where model artifacts that you configured when creating the
-- job are stored. Amazon SageMaker creates subfolders for model artifacts.
trainingJob_outputDataConfig :: Lens.Lens' TrainingJob (Core.Maybe OutputDataConfig)
trainingJob_outputDataConfig = Lens.lens (\TrainingJob' {outputDataConfig} -> outputDataConfig) (\s@TrainingJob' {} a -> s {outputDataConfig = a} :: TrainingJob)

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
trainingJob_tuningJobArn :: Lens.Lens' TrainingJob (Core.Maybe Core.Text)
trainingJob_tuningJobArn = Lens.lens (\TrainingJob' {tuningJobArn} -> tuningJobArn) (\s@TrainingJob' {} a -> s {tuningJobArn = a} :: TrainingJob)

-- | Information about the Amazon S3 location that is configured for storing
-- model artifacts.
trainingJob_modelArtifacts :: Lens.Lens' TrainingJob (Core.Maybe ModelArtifacts)
trainingJob_modelArtifacts = Lens.lens (\TrainingJob' {modelArtifacts} -> modelArtifacts) (\s@TrainingJob' {} a -> s {modelArtifacts = a} :: TrainingJob)

-- | A history of all of the secondary statuses that the training job has
-- transitioned through.
trainingJob_secondaryStatusTransitions :: Lens.Lens' TrainingJob (Core.Maybe [SecondaryStatusTransition])
trainingJob_secondaryStatusTransitions = Lens.lens (\TrainingJob' {secondaryStatusTransitions} -> secondaryStatusTransitions) (\s@TrainingJob' {} a -> s {secondaryStatusTransitions = a} :: TrainingJob) Core.. Lens.mapping Lens._Coerce

-- | A list of final metric values that are set when the training job
-- completes. Used only if the training job was configured to use metrics.
trainingJob_finalMetricDataList :: Lens.Lens' TrainingJob (Core.Maybe [MetricData])
trainingJob_finalMetricDataList = Lens.lens (\TrainingJob' {finalMetricDataList} -> finalMetricDataList) (\s@TrainingJob' {} a -> s {finalMetricDataList = a} :: TrainingJob) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the job.
trainingJob_autoMLJobArn :: Lens.Lens' TrainingJob (Core.Maybe Core.Text)
trainingJob_autoMLJobArn = Lens.lens (\TrainingJob' {autoMLJobArn} -> autoMLJobArn) (\s@TrainingJob' {} a -> s {autoMLJobArn = a} :: TrainingJob)

-- | If the training job failed, the reason it failed.
trainingJob_failureReason :: Lens.Lens' TrainingJob (Core.Maybe Core.Text)
trainingJob_failureReason = Lens.lens (\TrainingJob' {failureReason} -> failureReason) (\s@TrainingJob' {} a -> s {failureReason = a} :: TrainingJob)

-- | An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
trainingJob_tags :: Lens.Lens' TrainingJob (Core.Maybe [Tag])
trainingJob_tags = Lens.lens (\TrainingJob' {tags} -> tags) (\s@TrainingJob' {} a -> s {tags = a} :: TrainingJob) Core.. Lens.mapping Lens._Coerce

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
trainingJob_secondaryStatus :: Lens.Lens' TrainingJob (Core.Maybe SecondaryStatus)
trainingJob_secondaryStatus = Lens.lens (\TrainingJob' {secondaryStatus} -> secondaryStatus) (\s@TrainingJob' {} a -> s {secondaryStatus = a} :: TrainingJob)

-- | A timestamp that indicates when the status of the training job was last
-- modified.
trainingJob_lastModifiedTime :: Lens.Lens' TrainingJob (Core.Maybe Core.UTCTime)
trainingJob_lastModifiedTime = Lens.lens (\TrainingJob' {lastModifiedTime} -> lastModifiedTime) (\s@TrainingJob' {} a -> s {lastModifiedTime = a} :: TrainingJob) Core.. Lens.mapping Core._Time

-- | Undocumented member.
trainingJob_tensorBoardOutputConfig :: Lens.Lens' TrainingJob (Core.Maybe TensorBoardOutputConfig)
trainingJob_tensorBoardOutputConfig = Lens.lens (\TrainingJob' {tensorBoardOutputConfig} -> tensorBoardOutputConfig) (\s@TrainingJob' {} a -> s {tensorBoardOutputConfig = a} :: TrainingJob)

-- | Specifies a limit to how long a model training job can run. When the job
-- reaches the time limit, Amazon SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
trainingJob_stoppingCondition :: Lens.Lens' TrainingJob (Core.Maybe StoppingCondition)
trainingJob_stoppingCondition = Lens.lens (\TrainingJob' {stoppingCondition} -> stoppingCondition) (\s@TrainingJob' {} a -> s {stoppingCondition = a} :: TrainingJob)

-- | Information about the evaluation status of the rules for the training
-- job.
trainingJob_debugRuleEvaluationStatuses :: Lens.Lens' TrainingJob (Core.Maybe [DebugRuleEvaluationStatus])
trainingJob_debugRuleEvaluationStatuses = Lens.lens (\TrainingJob' {debugRuleEvaluationStatuses} -> debugRuleEvaluationStatuses) (\s@TrainingJob' {} a -> s {debugRuleEvaluationStatuses = a} :: TrainingJob) Core.. Lens.mapping Lens._Coerce

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
trainingJob_trainingJobStatus :: Lens.Lens' TrainingJob (Core.Maybe TrainingJobStatus)
trainingJob_trainingJobStatus = Lens.lens (\TrainingJob' {trainingJobStatus} -> trainingJobStatus) (\s@TrainingJob' {} a -> s {trainingJobStatus = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_debugHookConfig :: Lens.Lens' TrainingJob (Core.Maybe DebugHookConfig)
trainingJob_debugHookConfig = Lens.lens (\TrainingJob' {debugHookConfig} -> debugHookConfig) (\s@TrainingJob' {} a -> s {debugHookConfig = a} :: TrainingJob)

-- | The billable time in seconds.
trainingJob_billableTimeInSeconds :: Lens.Lens' TrainingJob (Core.Maybe Core.Natural)
trainingJob_billableTimeInSeconds = Lens.lens (\TrainingJob' {billableTimeInSeconds} -> billableTimeInSeconds) (\s@TrainingJob' {} a -> s {billableTimeInSeconds = a} :: TrainingJob)

-- | Resources, including ML compute instances and ML storage volumes, that
-- are configured for model training.
trainingJob_resourceConfig :: Lens.Lens' TrainingJob (Core.Maybe ResourceConfig)
trainingJob_resourceConfig = Lens.lens (\TrainingJob' {resourceConfig} -> resourceConfig) (\s@TrainingJob' {} a -> s {resourceConfig = a} :: TrainingJob)

-- | Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
trainingJob_trainingStartTime :: Lens.Lens' TrainingJob (Core.Maybe Core.UTCTime)
trainingJob_trainingStartTime = Lens.lens (\TrainingJob' {trainingStartTime} -> trainingStartTime) (\s@TrainingJob' {} a -> s {trainingStartTime = a} :: TrainingJob) Core.. Lens.mapping Core._Time

-- | Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
trainingJob_trainingEndTime :: Lens.Lens' TrainingJob (Core.Maybe Core.UTCTime)
trainingJob_trainingEndTime = Lens.lens (\TrainingJob' {trainingEndTime} -> trainingEndTime) (\s@TrainingJob' {} a -> s {trainingEndTime = a} :: TrainingJob) Core.. Lens.mapping Core._Time

-- | Information about the algorithm used for training, and algorithm
-- metadata.
trainingJob_algorithmSpecification :: Lens.Lens' TrainingJob (Core.Maybe AlgorithmSpecification)
trainingJob_algorithmSpecification = Lens.lens (\TrainingJob' {algorithmSpecification} -> algorithmSpecification) (\s@TrainingJob' {} a -> s {algorithmSpecification = a} :: TrainingJob)

-- | The Amazon Resource Name (ARN) of the training job.
trainingJob_trainingJobArn :: Lens.Lens' TrainingJob (Core.Maybe Core.Text)
trainingJob_trainingJobArn = Lens.lens (\TrainingJob' {trainingJobArn} -> trainingJobArn) (\s@TrainingJob' {} a -> s {trainingJobArn = a} :: TrainingJob)

instance Core.FromJSON TrainingJob where
  parseJSON =
    Core.withObject
      "TrainingJob"
      ( \x ->
          TrainingJob'
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> ( x Core..:? "DebugRuleConfigurations"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "HyperParameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "EnableManagedSpotTraining")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "LabelingJobArn")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "TrainingTimeInSeconds")
            Core.<*> (x Core..:? "ExperimentConfig")
            Core.<*> (x Core..:? "EnableNetworkIsolation")
            Core.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
            Core.<*> (x Core..:? "TrainingJobName")
            Core.<*> (x Core..:? "CheckpointConfig")
            Core.<*> (x Core..:? "OutputDataConfig")
            Core.<*> (x Core..:? "TuningJobArn")
            Core.<*> (x Core..:? "ModelArtifacts")
            Core.<*> ( x Core..:? "SecondaryStatusTransitions"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "FinalMetricDataList"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "AutoMLJobArn")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SecondaryStatus")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "TensorBoardOutputConfig")
            Core.<*> (x Core..:? "StoppingCondition")
            Core.<*> ( x Core..:? "DebugRuleEvaluationStatuses"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "TrainingJobStatus")
            Core.<*> (x Core..:? "DebugHookConfig")
            Core.<*> (x Core..:? "BillableTimeInSeconds")
            Core.<*> (x Core..:? "ResourceConfig")
            Core.<*> (x Core..:? "TrainingStartTime")
            Core.<*> (x Core..:? "TrainingEndTime")
            Core.<*> (x Core..:? "AlgorithmSpecification")
            Core.<*> (x Core..:? "TrainingJobArn")
      )

instance Core.Hashable TrainingJob

instance Core.NFData TrainingJob
