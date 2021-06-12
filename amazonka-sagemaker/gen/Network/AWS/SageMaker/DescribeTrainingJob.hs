{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeTrainingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a training job.
--
-- Some of the attributes below only appear if the training job
-- successfully starts. If the training job fails, @TrainingJobStatus@ is
-- @Failed@ and, depending on the @FailureReason@, attributes like
-- @TrainingStartTime@, @TrainingTimeInSeconds@, @TrainingEndTime@, and
-- @BillableTimeInSeconds@ may not be present in the response.
module Network.AWS.SageMaker.DescribeTrainingJob
  ( -- * Creating a Request
    DescribeTrainingJob (..),
    newDescribeTrainingJob,

    -- * Request Lenses
    describeTrainingJob_trainingJobName,

    -- * Destructuring the Response
    DescribeTrainingJobResponse (..),
    newDescribeTrainingJobResponse,

    -- * Response Lenses
    describeTrainingJobResponse_vpcConfig,
    describeTrainingJobResponse_debugRuleConfigurations,
    describeTrainingJobResponse_inputDataConfig,
    describeTrainingJobResponse_hyperParameters,
    describeTrainingJobResponse_enableManagedSpotTraining,
    describeTrainingJobResponse_labelingJobArn,
    describeTrainingJobResponse_roleArn,
    describeTrainingJobResponse_trainingTimeInSeconds,
    describeTrainingJobResponse_profilerConfig,
    describeTrainingJobResponse_experimentConfig,
    describeTrainingJobResponse_profilerRuleEvaluationStatuses,
    describeTrainingJobResponse_enableNetworkIsolation,
    describeTrainingJobResponse_enableInterContainerTrafficEncryption,
    describeTrainingJobResponse_checkpointConfig,
    describeTrainingJobResponse_outputDataConfig,
    describeTrainingJobResponse_tuningJobArn,
    describeTrainingJobResponse_secondaryStatusTransitions,
    describeTrainingJobResponse_finalMetricDataList,
    describeTrainingJobResponse_profilingStatus,
    describeTrainingJobResponse_profilerRuleConfigurations,
    describeTrainingJobResponse_autoMLJobArn,
    describeTrainingJobResponse_failureReason,
    describeTrainingJobResponse_lastModifiedTime,
    describeTrainingJobResponse_tensorBoardOutputConfig,
    describeTrainingJobResponse_debugRuleEvaluationStatuses,
    describeTrainingJobResponse_debugHookConfig,
    describeTrainingJobResponse_billableTimeInSeconds,
    describeTrainingJobResponse_trainingStartTime,
    describeTrainingJobResponse_trainingEndTime,
    describeTrainingJobResponse_httpStatus,
    describeTrainingJobResponse_trainingJobName,
    describeTrainingJobResponse_trainingJobArn,
    describeTrainingJobResponse_modelArtifacts,
    describeTrainingJobResponse_trainingJobStatus,
    describeTrainingJobResponse_secondaryStatus,
    describeTrainingJobResponse_algorithmSpecification,
    describeTrainingJobResponse_resourceConfig,
    describeTrainingJobResponse_stoppingCondition,
    describeTrainingJobResponse_creationTime,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeTrainingJob' smart constructor.
data DescribeTrainingJob = DescribeTrainingJob'
  { -- | The name of the training job.
    trainingJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrainingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingJobName', 'describeTrainingJob_trainingJobName' - The name of the training job.
newDescribeTrainingJob ::
  -- | 'trainingJobName'
  Core.Text ->
  DescribeTrainingJob
newDescribeTrainingJob pTrainingJobName_ =
  DescribeTrainingJob'
    { trainingJobName =
        pTrainingJobName_
    }

-- | The name of the training job.
describeTrainingJob_trainingJobName :: Lens.Lens' DescribeTrainingJob Core.Text
describeTrainingJob_trainingJobName = Lens.lens (\DescribeTrainingJob' {trainingJobName} -> trainingJobName) (\s@DescribeTrainingJob' {} a -> s {trainingJobName = a} :: DescribeTrainingJob)

instance Core.AWSRequest DescribeTrainingJob where
  type
    AWSResponse DescribeTrainingJob =
      DescribeTrainingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrainingJobResponse'
            Core.<$> (x Core..?> "VpcConfig")
            Core.<*> ( x Core..?> "DebugRuleConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "InputDataConfig")
            Core.<*> (x Core..?> "HyperParameters" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "EnableManagedSpotTraining")
            Core.<*> (x Core..?> "LabelingJobArn")
            Core.<*> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "TrainingTimeInSeconds")
            Core.<*> (x Core..?> "ProfilerConfig")
            Core.<*> (x Core..?> "ExperimentConfig")
            Core.<*> ( x Core..?> "ProfilerRuleEvaluationStatuses"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "EnableNetworkIsolation")
            Core.<*> (x Core..?> "EnableInterContainerTrafficEncryption")
            Core.<*> (x Core..?> "CheckpointConfig")
            Core.<*> (x Core..?> "OutputDataConfig")
            Core.<*> (x Core..?> "TuningJobArn")
            Core.<*> ( x Core..?> "SecondaryStatusTransitions"
                         Core..!@ Core.mempty
                     )
            Core.<*> ( x Core..?> "FinalMetricDataList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "ProfilingStatus")
            Core.<*> ( x Core..?> "ProfilerRuleConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "AutoMLJobArn")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "TensorBoardOutputConfig")
            Core.<*> ( x Core..?> "DebugRuleEvaluationStatuses"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "DebugHookConfig")
            Core.<*> (x Core..?> "BillableTimeInSeconds")
            Core.<*> (x Core..?> "TrainingStartTime")
            Core.<*> (x Core..?> "TrainingEndTime")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "TrainingJobName")
            Core.<*> (x Core..:> "TrainingJobArn")
            Core.<*> (x Core..:> "ModelArtifacts")
            Core.<*> (x Core..:> "TrainingJobStatus")
            Core.<*> (x Core..:> "SecondaryStatus")
            Core.<*> (x Core..:> "AlgorithmSpecification")
            Core.<*> (x Core..:> "ResourceConfig")
            Core.<*> (x Core..:> "StoppingCondition")
            Core.<*> (x Core..:> "CreationTime")
      )

instance Core.Hashable DescribeTrainingJob

instance Core.NFData DescribeTrainingJob

instance Core.ToHeaders DescribeTrainingJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeTrainingJob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTrainingJob where
  toJSON DescribeTrainingJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("TrainingJobName" Core..= trainingJobName)
          ]
      )

instance Core.ToPath DescribeTrainingJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTrainingJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTrainingJobResponse' smart constructor.
data DescribeTrainingJobResponse = DescribeTrainingJobResponse'
  { -- | A VpcConfig object that specifies the VPC that this training job has
    -- access to. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | Configuration information for Debugger rules for debugging output
    -- tensors.
    debugRuleConfigurations :: Core.Maybe [DebugRuleConfiguration],
    -- | An array of @Channel@ objects that describes each data input channel.
    inputDataConfig :: Core.Maybe (Core.NonEmpty Channel),
    -- | Algorithm-specific parameters.
    hyperParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A Boolean indicating whether managed spot training is enabled (@True@)
    -- or not (@False@).
    enableManagedSpotTraining :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
    -- labeling job that created the transform or training job.
    labelingJobArn :: Core.Maybe Core.Text,
    -- | The AWS Identity and Access Management (IAM) role configured for the
    -- training job.
    roleArn :: Core.Maybe Core.Text,
    -- | The training time in seconds.
    trainingTimeInSeconds :: Core.Maybe Core.Natural,
    profilerConfig :: Core.Maybe ProfilerConfig,
    experimentConfig :: Core.Maybe ExperimentConfig,
    -- | Evaluation status of Debugger rules for profiling on a training job.
    profilerRuleEvaluationStatuses :: Core.Maybe [ProfilerRuleEvaluationStatus],
    -- | If you want to allow inbound or outbound network calls, except for calls
    -- between peers within a training cluster for distributed training, choose
    -- @True@. If you enable network isolation for training jobs that are
    -- configured to use a VPC, Amazon SageMaker downloads and uploads customer
    -- data and model artifacts through the specified VPC, but the training
    -- container does not have network access.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithms in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool,
    checkpointConfig :: Core.Maybe CheckpointConfig,
    -- | The S3 path where model artifacts that you configured when creating the
    -- job are stored. Amazon SageMaker creates subfolders for model artifacts.
    outputDataConfig :: Core.Maybe OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
    -- job if the training job was launched by a hyperparameter tuning job.
    tuningJobArn :: Core.Maybe Core.Text,
    -- | A history of all of the secondary statuses that the training job has
    -- transitioned through.
    secondaryStatusTransitions :: Core.Maybe [SecondaryStatusTransition],
    -- | A collection of @MetricData@ objects that specify the names, values, and
    -- dates and times that the training algorithm emitted to Amazon
    -- CloudWatch.
    finalMetricDataList :: Core.Maybe [MetricData],
    -- | Profiling status of a training job.
    profilingStatus :: Core.Maybe ProfilingStatus,
    -- | Configuration information for Debugger rules for profiling system and
    -- framework metrics.
    profilerRuleConfigurations :: Core.Maybe [ProfilerRuleConfiguration],
    -- | The Amazon Resource Name (ARN) of an AutoML job.
    autoMLJobArn :: Core.Maybe Core.Text,
    -- | If the training job failed, the reason it failed.
    failureReason :: Core.Maybe Core.Text,
    -- | A timestamp that indicates when the status of the training job was last
    -- modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    tensorBoardOutputConfig :: Core.Maybe TensorBoardOutputConfig,
    -- | Evaluation status of Debugger rules for debugging on a training job.
    debugRuleEvaluationStatuses :: Core.Maybe [DebugRuleEvaluationStatus],
    debugHookConfig :: Core.Maybe DebugHookConfig,
    -- | The billable time in seconds. Billable time refers to the absolute
    -- wall-clock time.
    --
    -- Multiply @BillableTimeInSeconds@ by the number of instances
    -- (@InstanceCount@) in your training cluster to get the total compute time
    -- Amazon SageMaker will bill you if you run distributed training. The
    -- formula is as follows: @BillableTimeInSeconds * InstanceCount@ .
    --
    -- You can calculate the savings from using managed spot training using the
    -- formula @(1 - BillableTimeInSeconds \/ TrainingTimeInSeconds) * 100@.
    -- For example, if @BillableTimeInSeconds@ is 100 and
    -- @TrainingTimeInSeconds@ is 500, the savings is 80%.
    billableTimeInSeconds :: Core.Maybe Core.Natural,
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
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Name of the model training job.
    trainingJobName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Core.Text,
    -- | Information about the Amazon S3 location that is configured for storing
    -- model artifacts.
    modelArtifacts :: ModelArtifacts,
    -- | The status of the training job.
    --
    -- Amazon SageMaker provides the following training job statuses:
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
    trainingJobStatus :: TrainingJobStatus,
    -- | Provides detailed information about the state of the training job. For
    -- detailed information on the secondary status of the training job, see
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
    --     -   @Interrupted@ - The job stopped because the managed spot
    --         training instances were interrupted.
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
    --     -   @MaxWaitTimeExceeded@ - The job stopped because it exceeded the
    --         maximum allowed wait time.
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
    secondaryStatus :: SecondaryStatus,
    -- | Information about the algorithm used for training, and algorithm
    -- metadata.
    algorithmSpecification :: AlgorithmSpecification,
    -- | Resources, including ML compute instances and ML storage volumes, that
    -- are configured for model training.
    resourceConfig :: ResourceConfig,
    -- | Specifies a limit to how long a model training job can run. It also
    -- specifies the maximum time to wait for a spot instance. When the job
    -- reaches the time limit, Amazon SageMaker ends the training job. Use this
    -- API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
    -- signal, which delays job termination for 120 seconds. Algorithms can use
    -- this 120-second window to save the model artifacts, so the results of
    -- training are not lost.
    stoppingCondition :: StoppingCondition,
    -- | A timestamp that indicates when the training job was created.
    creationTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrainingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'describeTrainingJobResponse_vpcConfig' - A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'debugRuleConfigurations', 'describeTrainingJobResponse_debugRuleConfigurations' - Configuration information for Debugger rules for debugging output
-- tensors.
--
-- 'inputDataConfig', 'describeTrainingJobResponse_inputDataConfig' - An array of @Channel@ objects that describes each data input channel.
--
-- 'hyperParameters', 'describeTrainingJobResponse_hyperParameters' - Algorithm-specific parameters.
--
-- 'enableManagedSpotTraining', 'describeTrainingJobResponse_enableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
--
-- 'labelingJobArn', 'describeTrainingJobResponse_labelingJobArn' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
--
-- 'roleArn', 'describeTrainingJobResponse_roleArn' - The AWS Identity and Access Management (IAM) role configured for the
-- training job.
--
-- 'trainingTimeInSeconds', 'describeTrainingJobResponse_trainingTimeInSeconds' - The training time in seconds.
--
-- 'profilerConfig', 'describeTrainingJobResponse_profilerConfig' - Undocumented member.
--
-- 'experimentConfig', 'describeTrainingJobResponse_experimentConfig' - Undocumented member.
--
-- 'profilerRuleEvaluationStatuses', 'describeTrainingJobResponse_profilerRuleEvaluationStatuses' - Evaluation status of Debugger rules for profiling on a training job.
--
-- 'enableNetworkIsolation', 'describeTrainingJobResponse_enableNetworkIsolation' - If you want to allow inbound or outbound network calls, except for calls
-- between peers within a training cluster for distributed training, choose
-- @True@. If you enable network isolation for training jobs that are
-- configured to use a VPC, Amazon SageMaker downloads and uploads customer
-- data and model artifacts through the specified VPC, but the training
-- container does not have network access.
--
-- 'enableInterContainerTrafficEncryption', 'describeTrainingJobResponse_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithms in
-- distributed training.
--
-- 'checkpointConfig', 'describeTrainingJobResponse_checkpointConfig' - Undocumented member.
--
-- 'outputDataConfig', 'describeTrainingJobResponse_outputDataConfig' - The S3 path where model artifacts that you configured when creating the
-- job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- 'tuningJobArn', 'describeTrainingJobResponse_tuningJobArn' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
--
-- 'secondaryStatusTransitions', 'describeTrainingJobResponse_secondaryStatusTransitions' - A history of all of the secondary statuses that the training job has
-- transitioned through.
--
-- 'finalMetricDataList', 'describeTrainingJobResponse_finalMetricDataList' - A collection of @MetricData@ objects that specify the names, values, and
-- dates and times that the training algorithm emitted to Amazon
-- CloudWatch.
--
-- 'profilingStatus', 'describeTrainingJobResponse_profilingStatus' - Profiling status of a training job.
--
-- 'profilerRuleConfigurations', 'describeTrainingJobResponse_profilerRuleConfigurations' - Configuration information for Debugger rules for profiling system and
-- framework metrics.
--
-- 'autoMLJobArn', 'describeTrainingJobResponse_autoMLJobArn' - The Amazon Resource Name (ARN) of an AutoML job.
--
-- 'failureReason', 'describeTrainingJobResponse_failureReason' - If the training job failed, the reason it failed.
--
-- 'lastModifiedTime', 'describeTrainingJobResponse_lastModifiedTime' - A timestamp that indicates when the status of the training job was last
-- modified.
--
-- 'tensorBoardOutputConfig', 'describeTrainingJobResponse_tensorBoardOutputConfig' - Undocumented member.
--
-- 'debugRuleEvaluationStatuses', 'describeTrainingJobResponse_debugRuleEvaluationStatuses' - Evaluation status of Debugger rules for debugging on a training job.
--
-- 'debugHookConfig', 'describeTrainingJobResponse_debugHookConfig' - Undocumented member.
--
-- 'billableTimeInSeconds', 'describeTrainingJobResponse_billableTimeInSeconds' - The billable time in seconds. Billable time refers to the absolute
-- wall-clock time.
--
-- Multiply @BillableTimeInSeconds@ by the number of instances
-- (@InstanceCount@) in your training cluster to get the total compute time
-- Amazon SageMaker will bill you if you run distributed training. The
-- formula is as follows: @BillableTimeInSeconds * InstanceCount@ .
--
-- You can calculate the savings from using managed spot training using the
-- formula @(1 - BillableTimeInSeconds \/ TrainingTimeInSeconds) * 100@.
-- For example, if @BillableTimeInSeconds@ is 100 and
-- @TrainingTimeInSeconds@ is 500, the savings is 80%.
--
-- 'trainingStartTime', 'describeTrainingJobResponse_trainingStartTime' - Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
--
-- 'trainingEndTime', 'describeTrainingJobResponse_trainingEndTime' - Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
--
-- 'httpStatus', 'describeTrainingJobResponse_httpStatus' - The response's http status code.
--
-- 'trainingJobName', 'describeTrainingJobResponse_trainingJobName' - Name of the model training job.
--
-- 'trainingJobArn', 'describeTrainingJobResponse_trainingJobArn' - The Amazon Resource Name (ARN) of the training job.
--
-- 'modelArtifacts', 'describeTrainingJobResponse_modelArtifacts' - Information about the Amazon S3 location that is configured for storing
-- model artifacts.
--
-- 'trainingJobStatus', 'describeTrainingJobResponse_trainingJobStatus' - The status of the training job.
--
-- Amazon SageMaker provides the following training job statuses:
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
-- 'secondaryStatus', 'describeTrainingJobResponse_secondaryStatus' - Provides detailed information about the state of the training job. For
-- detailed information on the secondary status of the training job, see
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
--     -   @Interrupted@ - The job stopped because the managed spot
--         training instances were interrupted.
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
--     -   @MaxWaitTimeExceeded@ - The job stopped because it exceeded the
--         maximum allowed wait time.
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
-- 'algorithmSpecification', 'describeTrainingJobResponse_algorithmSpecification' - Information about the algorithm used for training, and algorithm
-- metadata.
--
-- 'resourceConfig', 'describeTrainingJobResponse_resourceConfig' - Resources, including ML compute instances and ML storage volumes, that
-- are configured for model training.
--
-- 'stoppingCondition', 'describeTrainingJobResponse_stoppingCondition' - Specifies a limit to how long a model training job can run. It also
-- specifies the maximum time to wait for a spot instance. When the job
-- reaches the time limit, Amazon SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
--
-- 'creationTime', 'describeTrainingJobResponse_creationTime' - A timestamp that indicates when the training job was created.
newDescribeTrainingJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'trainingJobName'
  Core.Text ->
  -- | 'trainingJobArn'
  Core.Text ->
  -- | 'modelArtifacts'
  ModelArtifacts ->
  -- | 'trainingJobStatus'
  TrainingJobStatus ->
  -- | 'secondaryStatus'
  SecondaryStatus ->
  -- | 'algorithmSpecification'
  AlgorithmSpecification ->
  -- | 'resourceConfig'
  ResourceConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  -- | 'creationTime'
  Core.UTCTime ->
  DescribeTrainingJobResponse
newDescribeTrainingJobResponse
  pHttpStatus_
  pTrainingJobName_
  pTrainingJobArn_
  pModelArtifacts_
  pTrainingJobStatus_
  pSecondaryStatus_
  pAlgorithmSpecification_
  pResourceConfig_
  pStoppingCondition_
  pCreationTime_ =
    DescribeTrainingJobResponse'
      { vpcConfig =
          Core.Nothing,
        debugRuleConfigurations = Core.Nothing,
        inputDataConfig = Core.Nothing,
        hyperParameters = Core.Nothing,
        enableManagedSpotTraining = Core.Nothing,
        labelingJobArn = Core.Nothing,
        roleArn = Core.Nothing,
        trainingTimeInSeconds = Core.Nothing,
        profilerConfig = Core.Nothing,
        experimentConfig = Core.Nothing,
        profilerRuleEvaluationStatuses = Core.Nothing,
        enableNetworkIsolation = Core.Nothing,
        enableInterContainerTrafficEncryption =
          Core.Nothing,
        checkpointConfig = Core.Nothing,
        outputDataConfig = Core.Nothing,
        tuningJobArn = Core.Nothing,
        secondaryStatusTransitions = Core.Nothing,
        finalMetricDataList = Core.Nothing,
        profilingStatus = Core.Nothing,
        profilerRuleConfigurations = Core.Nothing,
        autoMLJobArn = Core.Nothing,
        failureReason = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        tensorBoardOutputConfig = Core.Nothing,
        debugRuleEvaluationStatuses = Core.Nothing,
        debugHookConfig = Core.Nothing,
        billableTimeInSeconds = Core.Nothing,
        trainingStartTime = Core.Nothing,
        trainingEndTime = Core.Nothing,
        httpStatus = pHttpStatus_,
        trainingJobName = pTrainingJobName_,
        trainingJobArn = pTrainingJobArn_,
        modelArtifacts = pModelArtifacts_,
        trainingJobStatus = pTrainingJobStatus_,
        secondaryStatus = pSecondaryStatus_,
        algorithmSpecification =
          pAlgorithmSpecification_,
        resourceConfig = pResourceConfig_,
        stoppingCondition = pStoppingCondition_,
        creationTime =
          Core._Time Lens.# pCreationTime_
      }

-- | A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
describeTrainingJobResponse_vpcConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe VpcConfig)
describeTrainingJobResponse_vpcConfig = Lens.lens (\DescribeTrainingJobResponse' {vpcConfig} -> vpcConfig) (\s@DescribeTrainingJobResponse' {} a -> s {vpcConfig = a} :: DescribeTrainingJobResponse)

-- | Configuration information for Debugger rules for debugging output
-- tensors.
describeTrainingJobResponse_debugRuleConfigurations :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [DebugRuleConfiguration])
describeTrainingJobResponse_debugRuleConfigurations = Lens.lens (\DescribeTrainingJobResponse' {debugRuleConfigurations} -> debugRuleConfigurations) (\s@DescribeTrainingJobResponse' {} a -> s {debugRuleConfigurations = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | An array of @Channel@ objects that describes each data input channel.
describeTrainingJobResponse_inputDataConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe (Core.NonEmpty Channel))
describeTrainingJobResponse_inputDataConfig = Lens.lens (\DescribeTrainingJobResponse' {inputDataConfig} -> inputDataConfig) (\s@DescribeTrainingJobResponse' {} a -> s {inputDataConfig = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | Algorithm-specific parameters.
describeTrainingJobResponse_hyperParameters :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeTrainingJobResponse_hyperParameters = Lens.lens (\DescribeTrainingJobResponse' {hyperParameters} -> hyperParameters) (\s@DescribeTrainingJobResponse' {} a -> s {hyperParameters = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
describeTrainingJobResponse_enableManagedSpotTraining :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Bool)
describeTrainingJobResponse_enableManagedSpotTraining = Lens.lens (\DescribeTrainingJobResponse' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@DescribeTrainingJobResponse' {} a -> s {enableManagedSpotTraining = a} :: DescribeTrainingJobResponse)

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
describeTrainingJobResponse_labelingJobArn :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Text)
describeTrainingJobResponse_labelingJobArn = Lens.lens (\DescribeTrainingJobResponse' {labelingJobArn} -> labelingJobArn) (\s@DescribeTrainingJobResponse' {} a -> s {labelingJobArn = a} :: DescribeTrainingJobResponse)

-- | The AWS Identity and Access Management (IAM) role configured for the
-- training job.
describeTrainingJobResponse_roleArn :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Text)
describeTrainingJobResponse_roleArn = Lens.lens (\DescribeTrainingJobResponse' {roleArn} -> roleArn) (\s@DescribeTrainingJobResponse' {} a -> s {roleArn = a} :: DescribeTrainingJobResponse)

-- | The training time in seconds.
describeTrainingJobResponse_trainingTimeInSeconds :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Natural)
describeTrainingJobResponse_trainingTimeInSeconds = Lens.lens (\DescribeTrainingJobResponse' {trainingTimeInSeconds} -> trainingTimeInSeconds) (\s@DescribeTrainingJobResponse' {} a -> s {trainingTimeInSeconds = a} :: DescribeTrainingJobResponse)

-- | Undocumented member.
describeTrainingJobResponse_profilerConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe ProfilerConfig)
describeTrainingJobResponse_profilerConfig = Lens.lens (\DescribeTrainingJobResponse' {profilerConfig} -> profilerConfig) (\s@DescribeTrainingJobResponse' {} a -> s {profilerConfig = a} :: DescribeTrainingJobResponse)

-- | Undocumented member.
describeTrainingJobResponse_experimentConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe ExperimentConfig)
describeTrainingJobResponse_experimentConfig = Lens.lens (\DescribeTrainingJobResponse' {experimentConfig} -> experimentConfig) (\s@DescribeTrainingJobResponse' {} a -> s {experimentConfig = a} :: DescribeTrainingJobResponse)

-- | Evaluation status of Debugger rules for profiling on a training job.
describeTrainingJobResponse_profilerRuleEvaluationStatuses :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [ProfilerRuleEvaluationStatus])
describeTrainingJobResponse_profilerRuleEvaluationStatuses = Lens.lens (\DescribeTrainingJobResponse' {profilerRuleEvaluationStatuses} -> profilerRuleEvaluationStatuses) (\s@DescribeTrainingJobResponse' {} a -> s {profilerRuleEvaluationStatuses = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | If you want to allow inbound or outbound network calls, except for calls
-- between peers within a training cluster for distributed training, choose
-- @True@. If you enable network isolation for training jobs that are
-- configured to use a VPC, Amazon SageMaker downloads and uploads customer
-- data and model artifacts through the specified VPC, but the training
-- container does not have network access.
describeTrainingJobResponse_enableNetworkIsolation :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Bool)
describeTrainingJobResponse_enableNetworkIsolation = Lens.lens (\DescribeTrainingJobResponse' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@DescribeTrainingJobResponse' {} a -> s {enableNetworkIsolation = a} :: DescribeTrainingJobResponse)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithms in
-- distributed training.
describeTrainingJobResponse_enableInterContainerTrafficEncryption :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Bool)
describeTrainingJobResponse_enableInterContainerTrafficEncryption = Lens.lens (\DescribeTrainingJobResponse' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@DescribeTrainingJobResponse' {} a -> s {enableInterContainerTrafficEncryption = a} :: DescribeTrainingJobResponse)

-- | Undocumented member.
describeTrainingJobResponse_checkpointConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe CheckpointConfig)
describeTrainingJobResponse_checkpointConfig = Lens.lens (\DescribeTrainingJobResponse' {checkpointConfig} -> checkpointConfig) (\s@DescribeTrainingJobResponse' {} a -> s {checkpointConfig = a} :: DescribeTrainingJobResponse)

-- | The S3 path where model artifacts that you configured when creating the
-- job are stored. Amazon SageMaker creates subfolders for model artifacts.
describeTrainingJobResponse_outputDataConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe OutputDataConfig)
describeTrainingJobResponse_outputDataConfig = Lens.lens (\DescribeTrainingJobResponse' {outputDataConfig} -> outputDataConfig) (\s@DescribeTrainingJobResponse' {} a -> s {outputDataConfig = a} :: DescribeTrainingJobResponse)

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
describeTrainingJobResponse_tuningJobArn :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Text)
describeTrainingJobResponse_tuningJobArn = Lens.lens (\DescribeTrainingJobResponse' {tuningJobArn} -> tuningJobArn) (\s@DescribeTrainingJobResponse' {} a -> s {tuningJobArn = a} :: DescribeTrainingJobResponse)

-- | A history of all of the secondary statuses that the training job has
-- transitioned through.
describeTrainingJobResponse_secondaryStatusTransitions :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [SecondaryStatusTransition])
describeTrainingJobResponse_secondaryStatusTransitions = Lens.lens (\DescribeTrainingJobResponse' {secondaryStatusTransitions} -> secondaryStatusTransitions) (\s@DescribeTrainingJobResponse' {} a -> s {secondaryStatusTransitions = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | A collection of @MetricData@ objects that specify the names, values, and
-- dates and times that the training algorithm emitted to Amazon
-- CloudWatch.
describeTrainingJobResponse_finalMetricDataList :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [MetricData])
describeTrainingJobResponse_finalMetricDataList = Lens.lens (\DescribeTrainingJobResponse' {finalMetricDataList} -> finalMetricDataList) (\s@DescribeTrainingJobResponse' {} a -> s {finalMetricDataList = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | Profiling status of a training job.
describeTrainingJobResponse_profilingStatus :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe ProfilingStatus)
describeTrainingJobResponse_profilingStatus = Lens.lens (\DescribeTrainingJobResponse' {profilingStatus} -> profilingStatus) (\s@DescribeTrainingJobResponse' {} a -> s {profilingStatus = a} :: DescribeTrainingJobResponse)

-- | Configuration information for Debugger rules for profiling system and
-- framework metrics.
describeTrainingJobResponse_profilerRuleConfigurations :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [ProfilerRuleConfiguration])
describeTrainingJobResponse_profilerRuleConfigurations = Lens.lens (\DescribeTrainingJobResponse' {profilerRuleConfigurations} -> profilerRuleConfigurations) (\s@DescribeTrainingJobResponse' {} a -> s {profilerRuleConfigurations = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of an AutoML job.
describeTrainingJobResponse_autoMLJobArn :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Text)
describeTrainingJobResponse_autoMLJobArn = Lens.lens (\DescribeTrainingJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeTrainingJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeTrainingJobResponse)

-- | If the training job failed, the reason it failed.
describeTrainingJobResponse_failureReason :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Text)
describeTrainingJobResponse_failureReason = Lens.lens (\DescribeTrainingJobResponse' {failureReason} -> failureReason) (\s@DescribeTrainingJobResponse' {} a -> s {failureReason = a} :: DescribeTrainingJobResponse)

-- | A timestamp that indicates when the status of the training job was last
-- modified.
describeTrainingJobResponse_lastModifiedTime :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.UTCTime)
describeTrainingJobResponse_lastModifiedTime = Lens.lens (\DescribeTrainingJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeTrainingJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Core._Time

-- | Undocumented member.
describeTrainingJobResponse_tensorBoardOutputConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe TensorBoardOutputConfig)
describeTrainingJobResponse_tensorBoardOutputConfig = Lens.lens (\DescribeTrainingJobResponse' {tensorBoardOutputConfig} -> tensorBoardOutputConfig) (\s@DescribeTrainingJobResponse' {} a -> s {tensorBoardOutputConfig = a} :: DescribeTrainingJobResponse)

-- | Evaluation status of Debugger rules for debugging on a training job.
describeTrainingJobResponse_debugRuleEvaluationStatuses :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe [DebugRuleEvaluationStatus])
describeTrainingJobResponse_debugRuleEvaluationStatuses = Lens.lens (\DescribeTrainingJobResponse' {debugRuleEvaluationStatuses} -> debugRuleEvaluationStatuses) (\s@DescribeTrainingJobResponse' {} a -> s {debugRuleEvaluationStatuses = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
describeTrainingJobResponse_debugHookConfig :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe DebugHookConfig)
describeTrainingJobResponse_debugHookConfig = Lens.lens (\DescribeTrainingJobResponse' {debugHookConfig} -> debugHookConfig) (\s@DescribeTrainingJobResponse' {} a -> s {debugHookConfig = a} :: DescribeTrainingJobResponse)

-- | The billable time in seconds. Billable time refers to the absolute
-- wall-clock time.
--
-- Multiply @BillableTimeInSeconds@ by the number of instances
-- (@InstanceCount@) in your training cluster to get the total compute time
-- Amazon SageMaker will bill you if you run distributed training. The
-- formula is as follows: @BillableTimeInSeconds * InstanceCount@ .
--
-- You can calculate the savings from using managed spot training using the
-- formula @(1 - BillableTimeInSeconds \/ TrainingTimeInSeconds) * 100@.
-- For example, if @BillableTimeInSeconds@ is 100 and
-- @TrainingTimeInSeconds@ is 500, the savings is 80%.
describeTrainingJobResponse_billableTimeInSeconds :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.Natural)
describeTrainingJobResponse_billableTimeInSeconds = Lens.lens (\DescribeTrainingJobResponse' {billableTimeInSeconds} -> billableTimeInSeconds) (\s@DescribeTrainingJobResponse' {} a -> s {billableTimeInSeconds = a} :: DescribeTrainingJobResponse)

-- | Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
describeTrainingJobResponse_trainingStartTime :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.UTCTime)
describeTrainingJobResponse_trainingStartTime = Lens.lens (\DescribeTrainingJobResponse' {trainingStartTime} -> trainingStartTime) (\s@DescribeTrainingJobResponse' {} a -> s {trainingStartTime = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Core._Time

-- | Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
describeTrainingJobResponse_trainingEndTime :: Lens.Lens' DescribeTrainingJobResponse (Core.Maybe Core.UTCTime)
describeTrainingJobResponse_trainingEndTime = Lens.lens (\DescribeTrainingJobResponse' {trainingEndTime} -> trainingEndTime) (\s@DescribeTrainingJobResponse' {} a -> s {trainingEndTime = a} :: DescribeTrainingJobResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
describeTrainingJobResponse_httpStatus :: Lens.Lens' DescribeTrainingJobResponse Core.Int
describeTrainingJobResponse_httpStatus = Lens.lens (\DescribeTrainingJobResponse' {httpStatus} -> httpStatus) (\s@DescribeTrainingJobResponse' {} a -> s {httpStatus = a} :: DescribeTrainingJobResponse)

-- | Name of the model training job.
describeTrainingJobResponse_trainingJobName :: Lens.Lens' DescribeTrainingJobResponse Core.Text
describeTrainingJobResponse_trainingJobName = Lens.lens (\DescribeTrainingJobResponse' {trainingJobName} -> trainingJobName) (\s@DescribeTrainingJobResponse' {} a -> s {trainingJobName = a} :: DescribeTrainingJobResponse)

-- | The Amazon Resource Name (ARN) of the training job.
describeTrainingJobResponse_trainingJobArn :: Lens.Lens' DescribeTrainingJobResponse Core.Text
describeTrainingJobResponse_trainingJobArn = Lens.lens (\DescribeTrainingJobResponse' {trainingJobArn} -> trainingJobArn) (\s@DescribeTrainingJobResponse' {} a -> s {trainingJobArn = a} :: DescribeTrainingJobResponse)

-- | Information about the Amazon S3 location that is configured for storing
-- model artifacts.
describeTrainingJobResponse_modelArtifacts :: Lens.Lens' DescribeTrainingJobResponse ModelArtifacts
describeTrainingJobResponse_modelArtifacts = Lens.lens (\DescribeTrainingJobResponse' {modelArtifacts} -> modelArtifacts) (\s@DescribeTrainingJobResponse' {} a -> s {modelArtifacts = a} :: DescribeTrainingJobResponse)

-- | The status of the training job.
--
-- Amazon SageMaker provides the following training job statuses:
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
describeTrainingJobResponse_trainingJobStatus :: Lens.Lens' DescribeTrainingJobResponse TrainingJobStatus
describeTrainingJobResponse_trainingJobStatus = Lens.lens (\DescribeTrainingJobResponse' {trainingJobStatus} -> trainingJobStatus) (\s@DescribeTrainingJobResponse' {} a -> s {trainingJobStatus = a} :: DescribeTrainingJobResponse)

-- | Provides detailed information about the state of the training job. For
-- detailed information on the secondary status of the training job, see
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
--     -   @Interrupted@ - The job stopped because the managed spot
--         training instances were interrupted.
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
--     -   @MaxWaitTimeExceeded@ - The job stopped because it exceeded the
--         maximum allowed wait time.
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
describeTrainingJobResponse_secondaryStatus :: Lens.Lens' DescribeTrainingJobResponse SecondaryStatus
describeTrainingJobResponse_secondaryStatus = Lens.lens (\DescribeTrainingJobResponse' {secondaryStatus} -> secondaryStatus) (\s@DescribeTrainingJobResponse' {} a -> s {secondaryStatus = a} :: DescribeTrainingJobResponse)

-- | Information about the algorithm used for training, and algorithm
-- metadata.
describeTrainingJobResponse_algorithmSpecification :: Lens.Lens' DescribeTrainingJobResponse AlgorithmSpecification
describeTrainingJobResponse_algorithmSpecification = Lens.lens (\DescribeTrainingJobResponse' {algorithmSpecification} -> algorithmSpecification) (\s@DescribeTrainingJobResponse' {} a -> s {algorithmSpecification = a} :: DescribeTrainingJobResponse)

-- | Resources, including ML compute instances and ML storage volumes, that
-- are configured for model training.
describeTrainingJobResponse_resourceConfig :: Lens.Lens' DescribeTrainingJobResponse ResourceConfig
describeTrainingJobResponse_resourceConfig = Lens.lens (\DescribeTrainingJobResponse' {resourceConfig} -> resourceConfig) (\s@DescribeTrainingJobResponse' {} a -> s {resourceConfig = a} :: DescribeTrainingJobResponse)

-- | Specifies a limit to how long a model training job can run. It also
-- specifies the maximum time to wait for a spot instance. When the job
-- reaches the time limit, Amazon SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
describeTrainingJobResponse_stoppingCondition :: Lens.Lens' DescribeTrainingJobResponse StoppingCondition
describeTrainingJobResponse_stoppingCondition = Lens.lens (\DescribeTrainingJobResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeTrainingJobResponse' {} a -> s {stoppingCondition = a} :: DescribeTrainingJobResponse)

-- | A timestamp that indicates when the training job was created.
describeTrainingJobResponse_creationTime :: Lens.Lens' DescribeTrainingJobResponse Core.UTCTime
describeTrainingJobResponse_creationTime = Lens.lens (\DescribeTrainingJobResponse' {creationTime} -> creationTime) (\s@DescribeTrainingJobResponse' {} a -> s {creationTime = a} :: DescribeTrainingJobResponse) Core.. Core._Time

instance Core.NFData DescribeTrainingJobResponse
