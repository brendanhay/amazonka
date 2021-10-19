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
    describeTrainingJobResponse_labelingJobArn,
    describeTrainingJobResponse_failureReason,
    describeTrainingJobResponse_secondaryStatusTransitions,
    describeTrainingJobResponse_trainingEndTime,
    describeTrainingJobResponse_environment,
    describeTrainingJobResponse_billableTimeInSeconds,
    describeTrainingJobResponse_debugHookConfig,
    describeTrainingJobResponse_checkpointConfig,
    describeTrainingJobResponse_retryStrategy,
    describeTrainingJobResponse_debugRuleEvaluationStatuses,
    describeTrainingJobResponse_profilerConfig,
    describeTrainingJobResponse_profilerRuleEvaluationStatuses,
    describeTrainingJobResponse_enableNetworkIsolation,
    describeTrainingJobResponse_experimentConfig,
    describeTrainingJobResponse_lastModifiedTime,
    describeTrainingJobResponse_debugRuleConfigurations,
    describeTrainingJobResponse_enableManagedSpotTraining,
    describeTrainingJobResponse_autoMLJobArn,
    describeTrainingJobResponse_hyperParameters,
    describeTrainingJobResponse_inputDataConfig,
    describeTrainingJobResponse_profilerRuleConfigurations,
    describeTrainingJobResponse_vpcConfig,
    describeTrainingJobResponse_finalMetricDataList,
    describeTrainingJobResponse_profilingStatus,
    describeTrainingJobResponse_outputDataConfig,
    describeTrainingJobResponse_trainingStartTime,
    describeTrainingJobResponse_tuningJobArn,
    describeTrainingJobResponse_enableInterContainerTrafficEncryption,
    describeTrainingJobResponse_tensorBoardOutputConfig,
    describeTrainingJobResponse_trainingTimeInSeconds,
    describeTrainingJobResponse_roleArn,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeTrainingJob' smart constructor.
data DescribeTrainingJob = DescribeTrainingJob'
  { -- | The name of the training job.
    trainingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeTrainingJob
newDescribeTrainingJob pTrainingJobName_ =
  DescribeTrainingJob'
    { trainingJobName =
        pTrainingJobName_
    }

-- | The name of the training job.
describeTrainingJob_trainingJobName :: Lens.Lens' DescribeTrainingJob Prelude.Text
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
            Prelude.<$> (x Core..?> "LabelingJobArn")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> ( x Core..?> "SecondaryStatusTransitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "TrainingEndTime")
            Prelude.<*> (x Core..?> "Environment" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "BillableTimeInSeconds")
            Prelude.<*> (x Core..?> "DebugHookConfig")
            Prelude.<*> (x Core..?> "CheckpointConfig")
            Prelude.<*> (x Core..?> "RetryStrategy")
            Prelude.<*> ( x Core..?> "DebugRuleEvaluationStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ProfilerConfig")
            Prelude.<*> ( x Core..?> "ProfilerRuleEvaluationStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "EnableNetworkIsolation")
            Prelude.<*> (x Core..?> "ExperimentConfig")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> ( x Core..?> "DebugRuleConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "EnableManagedSpotTraining")
            Prelude.<*> (x Core..?> "AutoMLJobArn")
            Prelude.<*> ( x Core..?> "HyperParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "InputDataConfig")
            Prelude.<*> ( x Core..?> "ProfilerRuleConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "VpcConfig")
            Prelude.<*> ( x Core..?> "FinalMetricDataList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ProfilingStatus")
            Prelude.<*> (x Core..?> "OutputDataConfig")
            Prelude.<*> (x Core..?> "TrainingStartTime")
            Prelude.<*> (x Core..?> "TuningJobArn")
            Prelude.<*> (x Core..?> "EnableInterContainerTrafficEncryption")
            Prelude.<*> (x Core..?> "TensorBoardOutputConfig")
            Prelude.<*> (x Core..?> "TrainingTimeInSeconds")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "TrainingJobName")
            Prelude.<*> (x Core..:> "TrainingJobArn")
            Prelude.<*> (x Core..:> "ModelArtifacts")
            Prelude.<*> (x Core..:> "TrainingJobStatus")
            Prelude.<*> (x Core..:> "SecondaryStatus")
            Prelude.<*> (x Core..:> "AlgorithmSpecification")
            Prelude.<*> (x Core..:> "ResourceConfig")
            Prelude.<*> (x Core..:> "StoppingCondition")
            Prelude.<*> (x Core..:> "CreationTime")
      )

instance Prelude.Hashable DescribeTrainingJob

instance Prelude.NFData DescribeTrainingJob

instance Core.ToHeaders DescribeTrainingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeTrainingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTrainingJob where
  toJSON DescribeTrainingJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TrainingJobName" Core..= trainingJobName)
          ]
      )

instance Core.ToPath DescribeTrainingJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTrainingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTrainingJobResponse' smart constructor.
data DescribeTrainingJobResponse = DescribeTrainingJobResponse'
  { -- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
    -- labeling job that created the transform or training job.
    labelingJobArn :: Prelude.Maybe Prelude.Text,
    -- | If the training job failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A history of all of the secondary statuses that the training job has
    -- transitioned through.
    secondaryStatusTransitions :: Prelude.Maybe [SecondaryStatusTransition],
    -- | Indicates the time when the training job ends on training instances. You
    -- are billed for the time interval between the value of
    -- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
    -- this is the time after model artifacts are uploaded. For failed jobs,
    -- this is the time when Amazon SageMaker detects a job failure.
    trainingEndTime :: Prelude.Maybe Core.POSIX,
    -- | The environment variables to set in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The billable time in seconds. Billable time refers to the absolute
    -- wall-clock time.
    --
    -- Multiply @BillableTimeInSeconds@ by the number of instances
    -- (@InstanceCount@) in your training cluster to get the total compute time
    -- SageMaker will bill you if you run distributed training. The formula is
    -- as follows: @BillableTimeInSeconds * InstanceCount@ .
    --
    -- You can calculate the savings from using managed spot training using the
    -- formula @(1 - BillableTimeInSeconds \/ TrainingTimeInSeconds) * 100@.
    -- For example, if @BillableTimeInSeconds@ is 100 and
    -- @TrainingTimeInSeconds@ is 500, the savings is 80%.
    billableTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    debugHookConfig :: Prelude.Maybe DebugHookConfig,
    checkpointConfig :: Prelude.Maybe CheckpointConfig,
    -- | The number of times to retry the job when the job fails due to an
    -- @InternalServerError@.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | Evaluation status of Debugger rules for debugging on a training job.
    debugRuleEvaluationStatuses :: Prelude.Maybe [DebugRuleEvaluationStatus],
    profilerConfig :: Prelude.Maybe ProfilerConfig,
    -- | Evaluation status of Debugger rules for profiling on a training job.
    profilerRuleEvaluationStatuses :: Prelude.Maybe [ProfilerRuleEvaluationStatus],
    -- | If you want to allow inbound or outbound network calls, except for calls
    -- between peers within a training cluster for distributed training, choose
    -- @True@. If you enable network isolation for training jobs that are
    -- configured to use a VPC, Amazon SageMaker downloads and uploads customer
    -- data and model artifacts through the specified VPC, but the training
    -- container does not have network access.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | A timestamp that indicates when the status of the training job was last
    -- modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | Configuration information for Debugger rules for debugging output
    -- tensors.
    debugRuleConfigurations :: Prelude.Maybe [DebugRuleConfiguration],
    -- | A Boolean indicating whether managed spot training is enabled (@True@)
    -- or not (@False@).
    enableManagedSpotTraining :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of an AutoML job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | Algorithm-specific parameters.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An array of @Channel@ objects that describes each data input channel.
    inputDataConfig :: Prelude.Maybe (Prelude.NonEmpty Channel),
    -- | Configuration information for Debugger rules for profiling system and
    -- framework metrics.
    profilerRuleConfigurations :: Prelude.Maybe [ProfilerRuleConfiguration],
    -- | A VpcConfig object that specifies the VPC that this training job has
    -- access to. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | A collection of @MetricData@ objects that specify the names, values, and
    -- dates and times that the training algorithm emitted to Amazon
    -- CloudWatch.
    finalMetricDataList :: Prelude.Maybe [MetricData],
    -- | Profiling status of a training job.
    profilingStatus :: Prelude.Maybe ProfilingStatus,
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
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithms in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    tensorBoardOutputConfig :: Prelude.Maybe TensorBoardOutputConfig,
    -- | The training time in seconds.
    trainingTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services Identity and Access Management (IAM) role
    -- configured for the training job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Name of the model training job.
    trainingJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Text,
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
    -- -   @PreparingTraining@
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
    -- specifies how long a managed Spot training job has to complete. When the
    -- job reaches the time limit, Amazon SageMaker ends the training job. Use
    -- this API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
    -- signal, which delays job termination for 120 seconds. Algorithms can use
    -- this 120-second window to save the model artifacts, so the results of
    -- training are not lost.
    stoppingCondition :: StoppingCondition,
    -- | A timestamp that indicates when the training job was created.
    creationTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrainingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelingJobArn', 'describeTrainingJobResponse_labelingJobArn' - The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
--
-- 'failureReason', 'describeTrainingJobResponse_failureReason' - If the training job failed, the reason it failed.
--
-- 'secondaryStatusTransitions', 'describeTrainingJobResponse_secondaryStatusTransitions' - A history of all of the secondary statuses that the training job has
-- transitioned through.
--
-- 'trainingEndTime', 'describeTrainingJobResponse_trainingEndTime' - Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
--
-- 'environment', 'describeTrainingJobResponse_environment' - The environment variables to set in the Docker container.
--
-- 'billableTimeInSeconds', 'describeTrainingJobResponse_billableTimeInSeconds' - The billable time in seconds. Billable time refers to the absolute
-- wall-clock time.
--
-- Multiply @BillableTimeInSeconds@ by the number of instances
-- (@InstanceCount@) in your training cluster to get the total compute time
-- SageMaker will bill you if you run distributed training. The formula is
-- as follows: @BillableTimeInSeconds * InstanceCount@ .
--
-- You can calculate the savings from using managed spot training using the
-- formula @(1 - BillableTimeInSeconds \/ TrainingTimeInSeconds) * 100@.
-- For example, if @BillableTimeInSeconds@ is 100 and
-- @TrainingTimeInSeconds@ is 500, the savings is 80%.
--
-- 'debugHookConfig', 'describeTrainingJobResponse_debugHookConfig' - Undocumented member.
--
-- 'checkpointConfig', 'describeTrainingJobResponse_checkpointConfig' - Undocumented member.
--
-- 'retryStrategy', 'describeTrainingJobResponse_retryStrategy' - The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
--
-- 'debugRuleEvaluationStatuses', 'describeTrainingJobResponse_debugRuleEvaluationStatuses' - Evaluation status of Debugger rules for debugging on a training job.
--
-- 'profilerConfig', 'describeTrainingJobResponse_profilerConfig' - Undocumented member.
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
-- 'experimentConfig', 'describeTrainingJobResponse_experimentConfig' - Undocumented member.
--
-- 'lastModifiedTime', 'describeTrainingJobResponse_lastModifiedTime' - A timestamp that indicates when the status of the training job was last
-- modified.
--
-- 'debugRuleConfigurations', 'describeTrainingJobResponse_debugRuleConfigurations' - Configuration information for Debugger rules for debugging output
-- tensors.
--
-- 'enableManagedSpotTraining', 'describeTrainingJobResponse_enableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
--
-- 'autoMLJobArn', 'describeTrainingJobResponse_autoMLJobArn' - The Amazon Resource Name (ARN) of an AutoML job.
--
-- 'hyperParameters', 'describeTrainingJobResponse_hyperParameters' - Algorithm-specific parameters.
--
-- 'inputDataConfig', 'describeTrainingJobResponse_inputDataConfig' - An array of @Channel@ objects that describes each data input channel.
--
-- 'profilerRuleConfigurations', 'describeTrainingJobResponse_profilerRuleConfigurations' - Configuration information for Debugger rules for profiling system and
-- framework metrics.
--
-- 'vpcConfig', 'describeTrainingJobResponse_vpcConfig' - A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'finalMetricDataList', 'describeTrainingJobResponse_finalMetricDataList' - A collection of @MetricData@ objects that specify the names, values, and
-- dates and times that the training algorithm emitted to Amazon
-- CloudWatch.
--
-- 'profilingStatus', 'describeTrainingJobResponse_profilingStatus' - Profiling status of a training job.
--
-- 'outputDataConfig', 'describeTrainingJobResponse_outputDataConfig' - The S3 path where model artifacts that you configured when creating the
-- job are stored. Amazon SageMaker creates subfolders for model artifacts.
--
-- 'trainingStartTime', 'describeTrainingJobResponse_trainingStartTime' - Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
--
-- 'tuningJobArn', 'describeTrainingJobResponse_tuningJobArn' - The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
--
-- 'enableInterContainerTrafficEncryption', 'describeTrainingJobResponse_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithms in
-- distributed training.
--
-- 'tensorBoardOutputConfig', 'describeTrainingJobResponse_tensorBoardOutputConfig' - Undocumented member.
--
-- 'trainingTimeInSeconds', 'describeTrainingJobResponse_trainingTimeInSeconds' - The training time in seconds.
--
-- 'roleArn', 'describeTrainingJobResponse_roleArn' - The Amazon Web Services Identity and Access Management (IAM) role
-- configured for the training job.
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
-- -   @PreparingTraining@
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
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, Amazon SageMaker ends the training job. Use
-- this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
--
-- 'creationTime', 'describeTrainingJobResponse_creationTime' - A timestamp that indicates when the training job was created.
newDescribeTrainingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trainingJobName'
  Prelude.Text ->
  -- | 'trainingJobArn'
  Prelude.Text ->
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
  Prelude.UTCTime ->
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
      { labelingJobArn =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        secondaryStatusTransitions = Prelude.Nothing,
        trainingEndTime = Prelude.Nothing,
        environment = Prelude.Nothing,
        billableTimeInSeconds = Prelude.Nothing,
        debugHookConfig = Prelude.Nothing,
        checkpointConfig = Prelude.Nothing,
        retryStrategy = Prelude.Nothing,
        debugRuleEvaluationStatuses = Prelude.Nothing,
        profilerConfig = Prelude.Nothing,
        profilerRuleEvaluationStatuses =
          Prelude.Nothing,
        enableNetworkIsolation = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        debugRuleConfigurations = Prelude.Nothing,
        enableManagedSpotTraining = Prelude.Nothing,
        autoMLJobArn = Prelude.Nothing,
        hyperParameters = Prelude.Nothing,
        inputDataConfig = Prelude.Nothing,
        profilerRuleConfigurations = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        finalMetricDataList = Prelude.Nothing,
        profilingStatus = Prelude.Nothing,
        outputDataConfig = Prelude.Nothing,
        trainingStartTime = Prelude.Nothing,
        tuningJobArn = Prelude.Nothing,
        enableInterContainerTrafficEncryption =
          Prelude.Nothing,
        tensorBoardOutputConfig = Prelude.Nothing,
        trainingTimeInSeconds = Prelude.Nothing,
        roleArn = Prelude.Nothing,
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

-- | The Amazon Resource Name (ARN) of the Amazon SageMaker Ground Truth
-- labeling job that created the transform or training job.
describeTrainingJobResponse_labelingJobArn :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Text)
describeTrainingJobResponse_labelingJobArn = Lens.lens (\DescribeTrainingJobResponse' {labelingJobArn} -> labelingJobArn) (\s@DescribeTrainingJobResponse' {} a -> s {labelingJobArn = a} :: DescribeTrainingJobResponse)

-- | If the training job failed, the reason it failed.
describeTrainingJobResponse_failureReason :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Text)
describeTrainingJobResponse_failureReason = Lens.lens (\DescribeTrainingJobResponse' {failureReason} -> failureReason) (\s@DescribeTrainingJobResponse' {} a -> s {failureReason = a} :: DescribeTrainingJobResponse)

-- | A history of all of the secondary statuses that the training job has
-- transitioned through.
describeTrainingJobResponse_secondaryStatusTransitions :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe [SecondaryStatusTransition])
describeTrainingJobResponse_secondaryStatusTransitions = Lens.lens (\DescribeTrainingJobResponse' {secondaryStatusTransitions} -> secondaryStatusTransitions) (\s@DescribeTrainingJobResponse' {} a -> s {secondaryStatusTransitions = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the time when the training job ends on training instances. You
-- are billed for the time interval between the value of
-- @TrainingStartTime@ and this time. For successful jobs and stopped jobs,
-- this is the time after model artifacts are uploaded. For failed jobs,
-- this is the time when Amazon SageMaker detects a job failure.
describeTrainingJobResponse_trainingEndTime :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeTrainingJobResponse_trainingEndTime = Lens.lens (\DescribeTrainingJobResponse' {trainingEndTime} -> trainingEndTime) (\s@DescribeTrainingJobResponse' {} a -> s {trainingEndTime = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Core._Time

-- | The environment variables to set in the Docker container.
describeTrainingJobResponse_environment :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeTrainingJobResponse_environment = Lens.lens (\DescribeTrainingJobResponse' {environment} -> environment) (\s@DescribeTrainingJobResponse' {} a -> s {environment = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The billable time in seconds. Billable time refers to the absolute
-- wall-clock time.
--
-- Multiply @BillableTimeInSeconds@ by the number of instances
-- (@InstanceCount@) in your training cluster to get the total compute time
-- SageMaker will bill you if you run distributed training. The formula is
-- as follows: @BillableTimeInSeconds * InstanceCount@ .
--
-- You can calculate the savings from using managed spot training using the
-- formula @(1 - BillableTimeInSeconds \/ TrainingTimeInSeconds) * 100@.
-- For example, if @BillableTimeInSeconds@ is 100 and
-- @TrainingTimeInSeconds@ is 500, the savings is 80%.
describeTrainingJobResponse_billableTimeInSeconds :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Natural)
describeTrainingJobResponse_billableTimeInSeconds = Lens.lens (\DescribeTrainingJobResponse' {billableTimeInSeconds} -> billableTimeInSeconds) (\s@DescribeTrainingJobResponse' {} a -> s {billableTimeInSeconds = a} :: DescribeTrainingJobResponse)

-- | Undocumented member.
describeTrainingJobResponse_debugHookConfig :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe DebugHookConfig)
describeTrainingJobResponse_debugHookConfig = Lens.lens (\DescribeTrainingJobResponse' {debugHookConfig} -> debugHookConfig) (\s@DescribeTrainingJobResponse' {} a -> s {debugHookConfig = a} :: DescribeTrainingJobResponse)

-- | Undocumented member.
describeTrainingJobResponse_checkpointConfig :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe CheckpointConfig)
describeTrainingJobResponse_checkpointConfig = Lens.lens (\DescribeTrainingJobResponse' {checkpointConfig} -> checkpointConfig) (\s@DescribeTrainingJobResponse' {} a -> s {checkpointConfig = a} :: DescribeTrainingJobResponse)

-- | The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
describeTrainingJobResponse_retryStrategy :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe RetryStrategy)
describeTrainingJobResponse_retryStrategy = Lens.lens (\DescribeTrainingJobResponse' {retryStrategy} -> retryStrategy) (\s@DescribeTrainingJobResponse' {} a -> s {retryStrategy = a} :: DescribeTrainingJobResponse)

-- | Evaluation status of Debugger rules for debugging on a training job.
describeTrainingJobResponse_debugRuleEvaluationStatuses :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe [DebugRuleEvaluationStatus])
describeTrainingJobResponse_debugRuleEvaluationStatuses = Lens.lens (\DescribeTrainingJobResponse' {debugRuleEvaluationStatuses} -> debugRuleEvaluationStatuses) (\s@DescribeTrainingJobResponse' {} a -> s {debugRuleEvaluationStatuses = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeTrainingJobResponse_profilerConfig :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe ProfilerConfig)
describeTrainingJobResponse_profilerConfig = Lens.lens (\DescribeTrainingJobResponse' {profilerConfig} -> profilerConfig) (\s@DescribeTrainingJobResponse' {} a -> s {profilerConfig = a} :: DescribeTrainingJobResponse)

-- | Evaluation status of Debugger rules for profiling on a training job.
describeTrainingJobResponse_profilerRuleEvaluationStatuses :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe [ProfilerRuleEvaluationStatus])
describeTrainingJobResponse_profilerRuleEvaluationStatuses = Lens.lens (\DescribeTrainingJobResponse' {profilerRuleEvaluationStatuses} -> profilerRuleEvaluationStatuses) (\s@DescribeTrainingJobResponse' {} a -> s {profilerRuleEvaluationStatuses = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you want to allow inbound or outbound network calls, except for calls
-- between peers within a training cluster for distributed training, choose
-- @True@. If you enable network isolation for training jobs that are
-- configured to use a VPC, Amazon SageMaker downloads and uploads customer
-- data and model artifacts through the specified VPC, but the training
-- container does not have network access.
describeTrainingJobResponse_enableNetworkIsolation :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Bool)
describeTrainingJobResponse_enableNetworkIsolation = Lens.lens (\DescribeTrainingJobResponse' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@DescribeTrainingJobResponse' {} a -> s {enableNetworkIsolation = a} :: DescribeTrainingJobResponse)

-- | Undocumented member.
describeTrainingJobResponse_experimentConfig :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe ExperimentConfig)
describeTrainingJobResponse_experimentConfig = Lens.lens (\DescribeTrainingJobResponse' {experimentConfig} -> experimentConfig) (\s@DescribeTrainingJobResponse' {} a -> s {experimentConfig = a} :: DescribeTrainingJobResponse)

-- | A timestamp that indicates when the status of the training job was last
-- modified.
describeTrainingJobResponse_lastModifiedTime :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeTrainingJobResponse_lastModifiedTime = Lens.lens (\DescribeTrainingJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeTrainingJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Core._Time

-- | Configuration information for Debugger rules for debugging output
-- tensors.
describeTrainingJobResponse_debugRuleConfigurations :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe [DebugRuleConfiguration])
describeTrainingJobResponse_debugRuleConfigurations = Lens.lens (\DescribeTrainingJobResponse' {debugRuleConfigurations} -> debugRuleConfigurations) (\s@DescribeTrainingJobResponse' {} a -> s {debugRuleConfigurations = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
describeTrainingJobResponse_enableManagedSpotTraining :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Bool)
describeTrainingJobResponse_enableManagedSpotTraining = Lens.lens (\DescribeTrainingJobResponse' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@DescribeTrainingJobResponse' {} a -> s {enableManagedSpotTraining = a} :: DescribeTrainingJobResponse)

-- | The Amazon Resource Name (ARN) of an AutoML job.
describeTrainingJobResponse_autoMLJobArn :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Text)
describeTrainingJobResponse_autoMLJobArn = Lens.lens (\DescribeTrainingJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeTrainingJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeTrainingJobResponse)

-- | Algorithm-specific parameters.
describeTrainingJobResponse_hyperParameters :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeTrainingJobResponse_hyperParameters = Lens.lens (\DescribeTrainingJobResponse' {hyperParameters} -> hyperParameters) (\s@DescribeTrainingJobResponse' {} a -> s {hyperParameters = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of @Channel@ objects that describes each data input channel.
describeTrainingJobResponse_inputDataConfig :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe (Prelude.NonEmpty Channel))
describeTrainingJobResponse_inputDataConfig = Lens.lens (\DescribeTrainingJobResponse' {inputDataConfig} -> inputDataConfig) (\s@DescribeTrainingJobResponse' {} a -> s {inputDataConfig = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information for Debugger rules for profiling system and
-- framework metrics.
describeTrainingJobResponse_profilerRuleConfigurations :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe [ProfilerRuleConfiguration])
describeTrainingJobResponse_profilerRuleConfigurations = Lens.lens (\DescribeTrainingJobResponse' {profilerRuleConfigurations} -> profilerRuleConfigurations) (\s@DescribeTrainingJobResponse' {} a -> s {profilerRuleConfigurations = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | A VpcConfig object that specifies the VPC that this training job has
-- access to. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
describeTrainingJobResponse_vpcConfig :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe VpcConfig)
describeTrainingJobResponse_vpcConfig = Lens.lens (\DescribeTrainingJobResponse' {vpcConfig} -> vpcConfig) (\s@DescribeTrainingJobResponse' {} a -> s {vpcConfig = a} :: DescribeTrainingJobResponse)

-- | A collection of @MetricData@ objects that specify the names, values, and
-- dates and times that the training algorithm emitted to Amazon
-- CloudWatch.
describeTrainingJobResponse_finalMetricDataList :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe [MetricData])
describeTrainingJobResponse_finalMetricDataList = Lens.lens (\DescribeTrainingJobResponse' {finalMetricDataList} -> finalMetricDataList) (\s@DescribeTrainingJobResponse' {} a -> s {finalMetricDataList = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Profiling status of a training job.
describeTrainingJobResponse_profilingStatus :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe ProfilingStatus)
describeTrainingJobResponse_profilingStatus = Lens.lens (\DescribeTrainingJobResponse' {profilingStatus} -> profilingStatus) (\s@DescribeTrainingJobResponse' {} a -> s {profilingStatus = a} :: DescribeTrainingJobResponse)

-- | The S3 path where model artifacts that you configured when creating the
-- job are stored. Amazon SageMaker creates subfolders for model artifacts.
describeTrainingJobResponse_outputDataConfig :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe OutputDataConfig)
describeTrainingJobResponse_outputDataConfig = Lens.lens (\DescribeTrainingJobResponse' {outputDataConfig} -> outputDataConfig) (\s@DescribeTrainingJobResponse' {} a -> s {outputDataConfig = a} :: DescribeTrainingJobResponse)

-- | Indicates the time when the training job starts on training instances.
-- You are billed for the time interval between this time and the value of
-- @TrainingEndTime@. The start time in CloudWatch Logs might be later than
-- this time. The difference is due to the time it takes to download the
-- training data and to the size of the training container.
describeTrainingJobResponse_trainingStartTime :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeTrainingJobResponse_trainingStartTime = Lens.lens (\DescribeTrainingJobResponse' {trainingStartTime} -> trainingStartTime) (\s@DescribeTrainingJobResponse' {} a -> s {trainingStartTime = a} :: DescribeTrainingJobResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the associated hyperparameter tuning
-- job if the training job was launched by a hyperparameter tuning job.
describeTrainingJobResponse_tuningJobArn :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Text)
describeTrainingJobResponse_tuningJobArn = Lens.lens (\DescribeTrainingJobResponse' {tuningJobArn} -> tuningJobArn) (\s@DescribeTrainingJobResponse' {} a -> s {tuningJobArn = a} :: DescribeTrainingJobResponse)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithms in
-- distributed training.
describeTrainingJobResponse_enableInterContainerTrafficEncryption :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Bool)
describeTrainingJobResponse_enableInterContainerTrafficEncryption = Lens.lens (\DescribeTrainingJobResponse' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@DescribeTrainingJobResponse' {} a -> s {enableInterContainerTrafficEncryption = a} :: DescribeTrainingJobResponse)

-- | Undocumented member.
describeTrainingJobResponse_tensorBoardOutputConfig :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe TensorBoardOutputConfig)
describeTrainingJobResponse_tensorBoardOutputConfig = Lens.lens (\DescribeTrainingJobResponse' {tensorBoardOutputConfig} -> tensorBoardOutputConfig) (\s@DescribeTrainingJobResponse' {} a -> s {tensorBoardOutputConfig = a} :: DescribeTrainingJobResponse)

-- | The training time in seconds.
describeTrainingJobResponse_trainingTimeInSeconds :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Natural)
describeTrainingJobResponse_trainingTimeInSeconds = Lens.lens (\DescribeTrainingJobResponse' {trainingTimeInSeconds} -> trainingTimeInSeconds) (\s@DescribeTrainingJobResponse' {} a -> s {trainingTimeInSeconds = a} :: DescribeTrainingJobResponse)

-- | The Amazon Web Services Identity and Access Management (IAM) role
-- configured for the training job.
describeTrainingJobResponse_roleArn :: Lens.Lens' DescribeTrainingJobResponse (Prelude.Maybe Prelude.Text)
describeTrainingJobResponse_roleArn = Lens.lens (\DescribeTrainingJobResponse' {roleArn} -> roleArn) (\s@DescribeTrainingJobResponse' {} a -> s {roleArn = a} :: DescribeTrainingJobResponse)

-- | The response's http status code.
describeTrainingJobResponse_httpStatus :: Lens.Lens' DescribeTrainingJobResponse Prelude.Int
describeTrainingJobResponse_httpStatus = Lens.lens (\DescribeTrainingJobResponse' {httpStatus} -> httpStatus) (\s@DescribeTrainingJobResponse' {} a -> s {httpStatus = a} :: DescribeTrainingJobResponse)

-- | Name of the model training job.
describeTrainingJobResponse_trainingJobName :: Lens.Lens' DescribeTrainingJobResponse Prelude.Text
describeTrainingJobResponse_trainingJobName = Lens.lens (\DescribeTrainingJobResponse' {trainingJobName} -> trainingJobName) (\s@DescribeTrainingJobResponse' {} a -> s {trainingJobName = a} :: DescribeTrainingJobResponse)

-- | The Amazon Resource Name (ARN) of the training job.
describeTrainingJobResponse_trainingJobArn :: Lens.Lens' DescribeTrainingJobResponse Prelude.Text
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
-- -   @PreparingTraining@
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
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, Amazon SageMaker ends the training job. Use
-- this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
describeTrainingJobResponse_stoppingCondition :: Lens.Lens' DescribeTrainingJobResponse StoppingCondition
describeTrainingJobResponse_stoppingCondition = Lens.lens (\DescribeTrainingJobResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeTrainingJobResponse' {} a -> s {stoppingCondition = a} :: DescribeTrainingJobResponse)

-- | A timestamp that indicates when the training job was created.
describeTrainingJobResponse_creationTime :: Lens.Lens' DescribeTrainingJobResponse Prelude.UTCTime
describeTrainingJobResponse_creationTime = Lens.lens (\DescribeTrainingJobResponse' {creationTime} -> creationTime) (\s@DescribeTrainingJobResponse' {} a -> s {creationTime = a} :: DescribeTrainingJobResponse) Prelude.. Core._Time

instance Prelude.NFData DescribeTrainingJobResponse
