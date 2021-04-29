{-# LANGUAGE DeriveDataTypeable #-}
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
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | Information about the debug rule configuration.
    debugRuleConfigurations :: Prelude.Maybe [DebugRuleConfiguration],
    -- | An array of @Channel@ objects that describes each data input channel.
    inputDataConfig :: Prelude.Maybe (Prelude.NonEmpty Channel),
    -- | Algorithm-specific parameters.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | When true, enables managed spot training using Amazon EC2 Spot instances
    -- to run training jobs instead of on-demand instances. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
    enableManagedSpotTraining :: Prelude.Maybe Prelude.Bool,
    -- | A timestamp that indicates when the training job was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the labeling job.
    labelingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The AWS Identity and Access Management (IAM) role configured for the
    -- training job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The training time in seconds.
    trainingTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | If the @TrainingJob@ was created with network isolation, the value is
    -- set to @true@. If network isolation is enabled, nodes can\'t communicate
    -- beyond the VPC they run in.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    -- | The name of the training job.
    trainingJobName :: Prelude.Maybe Prelude.Text,
    checkpointConfig :: Prelude.Maybe CheckpointConfig,
    -- | The S3 path where model artifacts that you configured when creating the
    -- job are stored. Amazon SageMaker creates subfolders for model artifacts.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
    -- job if the training job was launched by a hyperparameter tuning job.
    tuningJobArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the Amazon S3 location that is configured for storing
    -- model artifacts.
    modelArtifacts :: Prelude.Maybe ModelArtifacts,
    -- | A history of all of the secondary statuses that the training job has
    -- transitioned through.
    secondaryStatusTransitions :: Prelude.Maybe [SecondaryStatusTransition],
    -- | A list of final metric values that are set when the training job
    -- completes. Used only if the training job was configured to use metrics.
    finalMetricDataList :: Prelude.Maybe [MetricData],
    -- | The Amazon Resource Name (ARN) of the job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | If the training job failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs. You can use tags to categorize your AWS
    -- resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
    tags :: Prelude.Maybe [Tag],
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
    -- | A timestamp that indicates when the status of the training job was last
    -- modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    tensorBoardOutputConfig :: Prelude.Maybe TensorBoardOutputConfig,
    -- | Specifies a limit to how long a model training job can run. When the job
    -- reaches the time limit, Amazon SageMaker ends the training job. Use this
    -- API to cap model training costs.
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
    debugHookConfig :: Prelude.Maybe DebugHookConfig,
    -- | The billable time in seconds.
    billableTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Resources, including ML compute instances and ML storage volumes, that
    -- are configured for model training.
    resourceConfig :: Prelude.Maybe ResourceConfig,
    -- | Indicates the time when the training job starts on training instances.
    -- You are billed for the time interval between this time and the value of
    -- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
    -- this time. The difference is due to the time it takes to download the
    -- training data and to the size of the training container.
    trainingStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | Indicates the time when the training job ends on training instances. You
    -- are billed for the time interval between the value of
    -- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
    -- this is the time after model artifacts are uploaded. For failed jobs,
    -- this is the time when Amazon SageMaker detects a job failure.
    trainingEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | Information about the algorithm used for training, and algorithm
    -- metadata.
    algorithmSpecification :: Prelude.Maybe AlgorithmSpecification,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { vpcConfig = Prelude.Nothing,
      debugRuleConfigurations = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      hyperParameters = Prelude.Nothing,
      enableManagedSpotTraining = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      labelingJobArn = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      trainingTimeInSeconds = Prelude.Nothing,
      experimentConfig = Prelude.Nothing,
      enableNetworkIsolation = Prelude.Nothing,
      enableInterContainerTrafficEncryption =
        Prelude.Nothing,
      trainingJobName = Prelude.Nothing,
      checkpointConfig = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      tuningJobArn = Prelude.Nothing,
      modelArtifacts = Prelude.Nothing,
      secondaryStatusTransitions = Prelude.Nothing,
      finalMetricDataList = Prelude.Nothing,
      autoMLJobArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      tags = Prelude.Nothing,
      secondaryStatus = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      tensorBoardOutputConfig = Prelude.Nothing,
      stoppingCondition = Prelude.Nothing,
      debugRuleEvaluationStatuses = Prelude.Nothing,
      trainingJobStatus = Prelude.Nothing,
      debugHookConfig = Prelude.Nothing,
      billableTimeInSeconds = Prelude.Nothing,
      resourceConfig = Prelude.Nothing,
      trainingStartTime = Prelude.Nothing,
      trainingEndTime = Prelude.Nothing,
      algorithmSpecification = Prelude.Nothing,
      trainingJobArn = Prelude.Nothing
    }

-- | A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
trainingJob_vpcConfig :: Lens.Lens' TrainingJob (Prelude.Maybe VpcConfig)
trainingJob_vpcConfig = Lens.lens (\TrainingJob' {vpcConfig} -> vpcConfig) (\s@TrainingJob' {} a -> s {vpcConfig = a} :: TrainingJob)

-- | Information about the debug rule configuration.
trainingJob_debugRuleConfigurations :: Lens.Lens' TrainingJob (Prelude.Maybe [DebugRuleConfiguration])
trainingJob_debugRuleConfigurations = Lens.lens (\TrainingJob' {debugRuleConfigurations} -> debugRuleConfigurations) (\s@TrainingJob' {} a -> s {debugRuleConfigurations = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | An array of @Channel@ objects that describes each data input channel.
trainingJob_inputDataConfig :: Lens.Lens' TrainingJob (Prelude.Maybe (Prelude.NonEmpty Channel))
trainingJob_inputDataConfig = Lens.lens (\TrainingJob' {inputDataConfig} -> inputDataConfig) (\s@TrainingJob' {} a -> s {inputDataConfig = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | Algorithm-specific parameters.
trainingJob_hyperParameters :: Lens.Lens' TrainingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
trainingJob_hyperParameters = Lens.lens (\TrainingJob' {hyperParameters} -> hyperParameters) (\s@TrainingJob' {} a -> s {hyperParameters = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | When true, enables managed spot training using Amazon EC2 Spot instances
-- to run training jobs instead of on-demand instances. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
trainingJob_enableManagedSpotTraining :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Bool)
trainingJob_enableManagedSpotTraining = Lens.lens (\TrainingJob' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@TrainingJob' {} a -> s {enableManagedSpotTraining = a} :: TrainingJob)

-- | A timestamp that indicates when the training job was created.
trainingJob_creationTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_creationTime = Lens.lens (\TrainingJob' {creationTime} -> creationTime) (\s@TrainingJob' {} a -> s {creationTime = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the labeling job.
trainingJob_labelingJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_labelingJobArn = Lens.lens (\TrainingJob' {labelingJobArn} -> labelingJobArn) (\s@TrainingJob' {} a -> s {labelingJobArn = a} :: TrainingJob)

-- | The AWS Identity and Access Management (IAM) role configured for the
-- training job.
trainingJob_roleArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_roleArn = Lens.lens (\TrainingJob' {roleArn} -> roleArn) (\s@TrainingJob' {} a -> s {roleArn = a} :: TrainingJob)

-- | The training time in seconds.
trainingJob_trainingTimeInSeconds :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Natural)
trainingJob_trainingTimeInSeconds = Lens.lens (\TrainingJob' {trainingTimeInSeconds} -> trainingTimeInSeconds) (\s@TrainingJob' {} a -> s {trainingTimeInSeconds = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_experimentConfig :: Lens.Lens' TrainingJob (Prelude.Maybe ExperimentConfig)
trainingJob_experimentConfig = Lens.lens (\TrainingJob' {experimentConfig} -> experimentConfig) (\s@TrainingJob' {} a -> s {experimentConfig = a} :: TrainingJob)

-- | If the @TrainingJob@ was created with network isolation, the value is
-- set to @true@. If network isolation is enabled, nodes can\'t communicate
-- beyond the VPC they run in.
trainingJob_enableNetworkIsolation :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Bool)
trainingJob_enableNetworkIsolation = Lens.lens (\TrainingJob' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@TrainingJob' {} a -> s {enableNetworkIsolation = a} :: TrainingJob)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
trainingJob_enableInterContainerTrafficEncryption :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Bool)
trainingJob_enableInterContainerTrafficEncryption = Lens.lens (\TrainingJob' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@TrainingJob' {} a -> s {enableInterContainerTrafficEncryption = a} :: TrainingJob)

-- | The name of the training job.
trainingJob_trainingJobName :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_trainingJobName = Lens.lens (\TrainingJob' {trainingJobName} -> trainingJobName) (\s@TrainingJob' {} a -> s {trainingJobName = a} :: TrainingJob)

-- | Undocumented member.
trainingJob_checkpointConfig :: Lens.Lens' TrainingJob (Prelude.Maybe CheckpointConfig)
trainingJob_checkpointConfig = Lens.lens (\TrainingJob' {checkpointConfig} -> checkpointConfig) (\s@TrainingJob' {} a -> s {checkpointConfig = a} :: TrainingJob)

-- | The S3 path where model artifacts that you configured when creating the
-- job are stored. Amazon SageMaker creates subfolders for model artifacts.
trainingJob_outputDataConfig :: Lens.Lens' TrainingJob (Prelude.Maybe OutputDataConfig)
trainingJob_outputDataConfig = Lens.lens (\TrainingJob' {outputDataConfig} -> outputDataConfig) (\s@TrainingJob' {} a -> s {outputDataConfig = a} :: TrainingJob)

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
trainingJob_tuningJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_tuningJobArn = Lens.lens (\TrainingJob' {tuningJobArn} -> tuningJobArn) (\s@TrainingJob' {} a -> s {tuningJobArn = a} :: TrainingJob)

-- | Information about the Amazon S3 location that is configured for storing
-- model artifacts.
trainingJob_modelArtifacts :: Lens.Lens' TrainingJob (Prelude.Maybe ModelArtifacts)
trainingJob_modelArtifacts = Lens.lens (\TrainingJob' {modelArtifacts} -> modelArtifacts) (\s@TrainingJob' {} a -> s {modelArtifacts = a} :: TrainingJob)

-- | A history of all of the secondary statuses that the training job has
-- transitioned through.
trainingJob_secondaryStatusTransitions :: Lens.Lens' TrainingJob (Prelude.Maybe [SecondaryStatusTransition])
trainingJob_secondaryStatusTransitions = Lens.lens (\TrainingJob' {secondaryStatusTransitions} -> secondaryStatusTransitions) (\s@TrainingJob' {} a -> s {secondaryStatusTransitions = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of final metric values that are set when the training job
-- completes. Used only if the training job was configured to use metrics.
trainingJob_finalMetricDataList :: Lens.Lens' TrainingJob (Prelude.Maybe [MetricData])
trainingJob_finalMetricDataList = Lens.lens (\TrainingJob' {finalMetricDataList} -> finalMetricDataList) (\s@TrainingJob' {} a -> s {finalMetricDataList = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the job.
trainingJob_autoMLJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_autoMLJobArn = Lens.lens (\TrainingJob' {autoMLJobArn} -> autoMLJobArn) (\s@TrainingJob' {} a -> s {autoMLJobArn = a} :: TrainingJob)

-- | If the training job failed, the reason it failed.
trainingJob_failureReason :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_failureReason = Lens.lens (\TrainingJob' {failureReason} -> failureReason) (\s@TrainingJob' {} a -> s {failureReason = a} :: TrainingJob)

-- | An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
trainingJob_tags :: Lens.Lens' TrainingJob (Prelude.Maybe [Tag])
trainingJob_tags = Lens.lens (\TrainingJob' {tags} -> tags) (\s@TrainingJob' {} a -> s {tags = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Coerce

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

-- | A timestamp that indicates when the status of the training job was last
-- modified.
trainingJob_lastModifiedTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_lastModifiedTime = Lens.lens (\TrainingJob' {lastModifiedTime} -> lastModifiedTime) (\s@TrainingJob' {} a -> s {lastModifiedTime = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
trainingJob_tensorBoardOutputConfig :: Lens.Lens' TrainingJob (Prelude.Maybe TensorBoardOutputConfig)
trainingJob_tensorBoardOutputConfig = Lens.lens (\TrainingJob' {tensorBoardOutputConfig} -> tensorBoardOutputConfig) (\s@TrainingJob' {} a -> s {tensorBoardOutputConfig = a} :: TrainingJob)

-- | Specifies a limit to how long a model training job can run. When the job
-- reaches the time limit, Amazon SageMaker ends the training job. Use this
-- API to cap model training costs.
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
trainingJob_debugRuleEvaluationStatuses = Lens.lens (\TrainingJob' {debugRuleEvaluationStatuses} -> debugRuleEvaluationStatuses) (\s@TrainingJob' {} a -> s {debugRuleEvaluationStatuses = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Coerce

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

-- | Undocumented member.
trainingJob_debugHookConfig :: Lens.Lens' TrainingJob (Prelude.Maybe DebugHookConfig)
trainingJob_debugHookConfig = Lens.lens (\TrainingJob' {debugHookConfig} -> debugHookConfig) (\s@TrainingJob' {} a -> s {debugHookConfig = a} :: TrainingJob)

-- | The billable time in seconds.
trainingJob_billableTimeInSeconds :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Natural)
trainingJob_billableTimeInSeconds = Lens.lens (\TrainingJob' {billableTimeInSeconds} -> billableTimeInSeconds) (\s@TrainingJob' {} a -> s {billableTimeInSeconds = a} :: TrainingJob)

-- | Resources, including ML compute instances and ML storage volumes, that
-- are configured for model training.
trainingJob_resourceConfig :: Lens.Lens' TrainingJob (Prelude.Maybe ResourceConfig)
trainingJob_resourceConfig = Lens.lens (\TrainingJob' {resourceConfig} -> resourceConfig) (\s@TrainingJob' {} a -> s {resourceConfig = a} :: TrainingJob)

-- | Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
trainingJob_trainingStartTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_trainingStartTime = Lens.lens (\TrainingJob' {trainingStartTime} -> trainingStartTime) (\s@TrainingJob' {} a -> s {trainingStartTime = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Time

-- | Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
trainingJob_trainingEndTime :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.UTCTime)
trainingJob_trainingEndTime = Lens.lens (\TrainingJob' {trainingEndTime} -> trainingEndTime) (\s@TrainingJob' {} a -> s {trainingEndTime = a} :: TrainingJob) Prelude.. Lens.mapping Prelude._Time

-- | Information about the algorithm used for training, and algorithm
-- metadata.
trainingJob_algorithmSpecification :: Lens.Lens' TrainingJob (Prelude.Maybe AlgorithmSpecification)
trainingJob_algorithmSpecification = Lens.lens (\TrainingJob' {algorithmSpecification} -> algorithmSpecification) (\s@TrainingJob' {} a -> s {algorithmSpecification = a} :: TrainingJob)

-- | The Amazon Resource Name (ARN) of the training job.
trainingJob_trainingJobArn :: Lens.Lens' TrainingJob (Prelude.Maybe Prelude.Text)
trainingJob_trainingJobArn = Lens.lens (\TrainingJob' {trainingJobArn} -> trainingJobArn) (\s@TrainingJob' {} a -> s {trainingJobArn = a} :: TrainingJob)

instance Prelude.FromJSON TrainingJob where
  parseJSON =
    Prelude.withObject
      "TrainingJob"
      ( \x ->
          TrainingJob'
            Prelude.<$> (x Prelude..:? "VpcConfig")
            Prelude.<*> ( x Prelude..:? "DebugRuleConfigurations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "InputDataConfig")
            Prelude.<*> ( x Prelude..:? "HyperParameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "EnableManagedSpotTraining")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "LabelingJobArn")
            Prelude.<*> (x Prelude..:? "RoleArn")
            Prelude.<*> (x Prelude..:? "TrainingTimeInSeconds")
            Prelude.<*> (x Prelude..:? "ExperimentConfig")
            Prelude.<*> (x Prelude..:? "EnableNetworkIsolation")
            Prelude.<*> ( x
                            Prelude..:? "EnableInterContainerTrafficEncryption"
                        )
            Prelude.<*> (x Prelude..:? "TrainingJobName")
            Prelude.<*> (x Prelude..:? "CheckpointConfig")
            Prelude.<*> (x Prelude..:? "OutputDataConfig")
            Prelude.<*> (x Prelude..:? "TuningJobArn")
            Prelude.<*> (x Prelude..:? "ModelArtifacts")
            Prelude.<*> ( x Prelude..:? "SecondaryStatusTransitions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "FinalMetricDataList"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "AutoMLJobArn")
            Prelude.<*> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "SecondaryStatus")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "TensorBoardOutputConfig")
            Prelude.<*> (x Prelude..:? "StoppingCondition")
            Prelude.<*> ( x Prelude..:? "DebugRuleEvaluationStatuses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "TrainingJobStatus")
            Prelude.<*> (x Prelude..:? "DebugHookConfig")
            Prelude.<*> (x Prelude..:? "BillableTimeInSeconds")
            Prelude.<*> (x Prelude..:? "ResourceConfig")
            Prelude.<*> (x Prelude..:? "TrainingStartTime")
            Prelude.<*> (x Prelude..:? "TrainingEndTime")
            Prelude.<*> (x Prelude..:? "AlgorithmSpecification")
            Prelude.<*> (x Prelude..:? "TrainingJobArn")
      )

instance Prelude.Hashable TrainingJob

instance Prelude.NFData TrainingJob
