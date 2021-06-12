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
-- Module      : Network.AWS.SageMaker.DescribeProcessingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a processing job.
module Network.AWS.SageMaker.DescribeProcessingJob
  ( -- * Creating a Request
    DescribeProcessingJob (..),
    newDescribeProcessingJob,

    -- * Request Lenses
    describeProcessingJob_processingJobName,

    -- * Destructuring the Response
    DescribeProcessingJobResponse (..),
    newDescribeProcessingJobResponse,

    -- * Response Lenses
    describeProcessingJobResponse_networkConfig,
    describeProcessingJobResponse_processingEndTime,
    describeProcessingJobResponse_roleArn,
    describeProcessingJobResponse_processingOutputConfig,
    describeProcessingJobResponse_exitMessage,
    describeProcessingJobResponse_experimentConfig,
    describeProcessingJobResponse_environment,
    describeProcessingJobResponse_autoMLJobArn,
    describeProcessingJobResponse_failureReason,
    describeProcessingJobResponse_monitoringScheduleArn,
    describeProcessingJobResponse_lastModifiedTime,
    describeProcessingJobResponse_processingInputs,
    describeProcessingJobResponse_processingStartTime,
    describeProcessingJobResponse_stoppingCondition,
    describeProcessingJobResponse_trainingJobArn,
    describeProcessingJobResponse_httpStatus,
    describeProcessingJobResponse_processingJobName,
    describeProcessingJobResponse_processingResources,
    describeProcessingJobResponse_appSpecification,
    describeProcessingJobResponse_processingJobArn,
    describeProcessingJobResponse_processingJobStatus,
    describeProcessingJobResponse_creationTime,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeProcessingJob' smart constructor.
data DescribeProcessingJob = DescribeProcessingJob'
  { -- | The name of the processing job. The name must be unique within an AWS
    -- Region in the AWS account.
    processingJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProcessingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingJobName', 'describeProcessingJob_processingJobName' - The name of the processing job. The name must be unique within an AWS
-- Region in the AWS account.
newDescribeProcessingJob ::
  -- | 'processingJobName'
  Core.Text ->
  DescribeProcessingJob
newDescribeProcessingJob pProcessingJobName_ =
  DescribeProcessingJob'
    { processingJobName =
        pProcessingJobName_
    }

-- | The name of the processing job. The name must be unique within an AWS
-- Region in the AWS account.
describeProcessingJob_processingJobName :: Lens.Lens' DescribeProcessingJob Core.Text
describeProcessingJob_processingJobName = Lens.lens (\DescribeProcessingJob' {processingJobName} -> processingJobName) (\s@DescribeProcessingJob' {} a -> s {processingJobName = a} :: DescribeProcessingJob)

instance Core.AWSRequest DescribeProcessingJob where
  type
    AWSResponse DescribeProcessingJob =
      DescribeProcessingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProcessingJobResponse'
            Core.<$> (x Core..?> "NetworkConfig")
            Core.<*> (x Core..?> "ProcessingEndTime")
            Core.<*> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "ProcessingOutputConfig")
            Core.<*> (x Core..?> "ExitMessage")
            Core.<*> (x Core..?> "ExperimentConfig")
            Core.<*> (x Core..?> "Environment" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "AutoMLJobArn")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "MonitoringScheduleArn")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "ProcessingInputs" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ProcessingStartTime")
            Core.<*> (x Core..?> "StoppingCondition")
            Core.<*> (x Core..?> "TrainingJobArn")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ProcessingJobName")
            Core.<*> (x Core..:> "ProcessingResources")
            Core.<*> (x Core..:> "AppSpecification")
            Core.<*> (x Core..:> "ProcessingJobArn")
            Core.<*> (x Core..:> "ProcessingJobStatus")
            Core.<*> (x Core..:> "CreationTime")
      )

instance Core.Hashable DescribeProcessingJob

instance Core.NFData DescribeProcessingJob

instance Core.ToHeaders DescribeProcessingJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeProcessingJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProcessingJob where
  toJSON DescribeProcessingJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ProcessingJobName" Core..= processingJobName)
          ]
      )

instance Core.ToPath DescribeProcessingJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProcessingJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProcessingJobResponse' smart constructor.
data DescribeProcessingJobResponse = DescribeProcessingJobResponse'
  { -- | Networking options for a processing job.
    networkConfig :: Core.Maybe NetworkConfig,
    -- | The time at which the processing job completed.
    processingEndTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Core.Maybe Core.Text,
    -- | Output configuration for the processing job.
    processingOutputConfig :: Core.Maybe ProcessingOutputConfig,
    -- | An optional string, up to one KB in size, that contains metadata from
    -- the processing container when the processing job exits.
    exitMessage :: Core.Maybe Core.Text,
    -- | The configuration information used to create an experiment.
    experimentConfig :: Core.Maybe ExperimentConfig,
    -- | The environment variables set in the Docker container.
    environment :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The ARN of an AutoML job associated with this processing job.
    autoMLJobArn :: Core.Maybe Core.Text,
    -- | A string, up to one KB in size, that contains the reason a processing
    -- job failed, if it failed.
    failureReason :: Core.Maybe Core.Text,
    -- | The ARN of a monitoring schedule for an endpoint associated with this
    -- processing job.
    monitoringScheduleArn :: Core.Maybe Core.Text,
    -- | The time at which the processing job was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The inputs for a processing job.
    processingInputs :: Core.Maybe [ProcessingInput],
    -- | The time at which the processing job started.
    processingStartTime :: Core.Maybe Core.POSIX,
    -- | The time limit for how long the processing job is allowed to run.
    stoppingCondition :: Core.Maybe ProcessingStoppingCondition,
    -- | The ARN of a training job associated with this processing job.
    trainingJobArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The name of the processing job. The name must be unique within an AWS
    -- Region in the AWS account.
    processingJobName :: Core.Text,
    -- | Identifies the resources, ML compute instances, and ML storage volumes
    -- to deploy for a processing job. In distributed training, you specify
    -- more than one instance.
    processingResources :: ProcessingResources,
    -- | Configures the processing job to run a specified container image.
    appSpecification :: AppSpecification,
    -- | The Amazon Resource Name (ARN) of the processing job.
    processingJobArn :: Core.Text,
    -- | Provides the status of a processing job.
    processingJobStatus :: ProcessingJobStatus,
    -- | The time at which the processing job was created.
    creationTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProcessingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfig', 'describeProcessingJobResponse_networkConfig' - Networking options for a processing job.
--
-- 'processingEndTime', 'describeProcessingJobResponse_processingEndTime' - The time at which the processing job completed.
--
-- 'roleArn', 'describeProcessingJobResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
--
-- 'processingOutputConfig', 'describeProcessingJobResponse_processingOutputConfig' - Output configuration for the processing job.
--
-- 'exitMessage', 'describeProcessingJobResponse_exitMessage' - An optional string, up to one KB in size, that contains metadata from
-- the processing container when the processing job exits.
--
-- 'experimentConfig', 'describeProcessingJobResponse_experimentConfig' - The configuration information used to create an experiment.
--
-- 'environment', 'describeProcessingJobResponse_environment' - The environment variables set in the Docker container.
--
-- 'autoMLJobArn', 'describeProcessingJobResponse_autoMLJobArn' - The ARN of an AutoML job associated with this processing job.
--
-- 'failureReason', 'describeProcessingJobResponse_failureReason' - A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
--
-- 'monitoringScheduleArn', 'describeProcessingJobResponse_monitoringScheduleArn' - The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
--
-- 'lastModifiedTime', 'describeProcessingJobResponse_lastModifiedTime' - The time at which the processing job was last modified.
--
-- 'processingInputs', 'describeProcessingJobResponse_processingInputs' - The inputs for a processing job.
--
-- 'processingStartTime', 'describeProcessingJobResponse_processingStartTime' - The time at which the processing job started.
--
-- 'stoppingCondition', 'describeProcessingJobResponse_stoppingCondition' - The time limit for how long the processing job is allowed to run.
--
-- 'trainingJobArn', 'describeProcessingJobResponse_trainingJobArn' - The ARN of a training job associated with this processing job.
--
-- 'httpStatus', 'describeProcessingJobResponse_httpStatus' - The response's http status code.
--
-- 'processingJobName', 'describeProcessingJobResponse_processingJobName' - The name of the processing job. The name must be unique within an AWS
-- Region in the AWS account.
--
-- 'processingResources', 'describeProcessingJobResponse_processingResources' - Identifies the resources, ML compute instances, and ML storage volumes
-- to deploy for a processing job. In distributed training, you specify
-- more than one instance.
--
-- 'appSpecification', 'describeProcessingJobResponse_appSpecification' - Configures the processing job to run a specified container image.
--
-- 'processingJobArn', 'describeProcessingJobResponse_processingJobArn' - The Amazon Resource Name (ARN) of the processing job.
--
-- 'processingJobStatus', 'describeProcessingJobResponse_processingJobStatus' - Provides the status of a processing job.
--
-- 'creationTime', 'describeProcessingJobResponse_creationTime' - The time at which the processing job was created.
newDescribeProcessingJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'processingJobName'
  Core.Text ->
  -- | 'processingResources'
  ProcessingResources ->
  -- | 'appSpecification'
  AppSpecification ->
  -- | 'processingJobArn'
  Core.Text ->
  -- | 'processingJobStatus'
  ProcessingJobStatus ->
  -- | 'creationTime'
  Core.UTCTime ->
  DescribeProcessingJobResponse
newDescribeProcessingJobResponse
  pHttpStatus_
  pProcessingJobName_
  pProcessingResources_
  pAppSpecification_
  pProcessingJobArn_
  pProcessingJobStatus_
  pCreationTime_ =
    DescribeProcessingJobResponse'
      { networkConfig =
          Core.Nothing,
        processingEndTime = Core.Nothing,
        roleArn = Core.Nothing,
        processingOutputConfig = Core.Nothing,
        exitMessage = Core.Nothing,
        experimentConfig = Core.Nothing,
        environment = Core.Nothing,
        autoMLJobArn = Core.Nothing,
        failureReason = Core.Nothing,
        monitoringScheduleArn = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        processingInputs = Core.Nothing,
        processingStartTime = Core.Nothing,
        stoppingCondition = Core.Nothing,
        trainingJobArn = Core.Nothing,
        httpStatus = pHttpStatus_,
        processingJobName = pProcessingJobName_,
        processingResources = pProcessingResources_,
        appSpecification = pAppSpecification_,
        processingJobArn = pProcessingJobArn_,
        processingJobStatus = pProcessingJobStatus_,
        creationTime =
          Core._Time Lens.# pCreationTime_
      }

-- | Networking options for a processing job.
describeProcessingJobResponse_networkConfig :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe NetworkConfig)
describeProcessingJobResponse_networkConfig = Lens.lens (\DescribeProcessingJobResponse' {networkConfig} -> networkConfig) (\s@DescribeProcessingJobResponse' {} a -> s {networkConfig = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job completed.
describeProcessingJobResponse_processingEndTime :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.UTCTime)
describeProcessingJobResponse_processingEndTime = Lens.lens (\DescribeProcessingJobResponse' {processingEndTime} -> processingEndTime) (\s@DescribeProcessingJobResponse' {} a -> s {processingEndTime = a} :: DescribeProcessingJobResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
describeProcessingJobResponse_roleArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.Text)
describeProcessingJobResponse_roleArn = Lens.lens (\DescribeProcessingJobResponse' {roleArn} -> roleArn) (\s@DescribeProcessingJobResponse' {} a -> s {roleArn = a} :: DescribeProcessingJobResponse)

-- | Output configuration for the processing job.
describeProcessingJobResponse_processingOutputConfig :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe ProcessingOutputConfig)
describeProcessingJobResponse_processingOutputConfig = Lens.lens (\DescribeProcessingJobResponse' {processingOutputConfig} -> processingOutputConfig) (\s@DescribeProcessingJobResponse' {} a -> s {processingOutputConfig = a} :: DescribeProcessingJobResponse)

-- | An optional string, up to one KB in size, that contains metadata from
-- the processing container when the processing job exits.
describeProcessingJobResponse_exitMessage :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.Text)
describeProcessingJobResponse_exitMessage = Lens.lens (\DescribeProcessingJobResponse' {exitMessage} -> exitMessage) (\s@DescribeProcessingJobResponse' {} a -> s {exitMessage = a} :: DescribeProcessingJobResponse)

-- | The configuration information used to create an experiment.
describeProcessingJobResponse_experimentConfig :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe ExperimentConfig)
describeProcessingJobResponse_experimentConfig = Lens.lens (\DescribeProcessingJobResponse' {experimentConfig} -> experimentConfig) (\s@DescribeProcessingJobResponse' {} a -> s {experimentConfig = a} :: DescribeProcessingJobResponse)

-- | The environment variables set in the Docker container.
describeProcessingJobResponse_environment :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeProcessingJobResponse_environment = Lens.lens (\DescribeProcessingJobResponse' {environment} -> environment) (\s@DescribeProcessingJobResponse' {} a -> s {environment = a} :: DescribeProcessingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | The ARN of an AutoML job associated with this processing job.
describeProcessingJobResponse_autoMLJobArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.Text)
describeProcessingJobResponse_autoMLJobArn = Lens.lens (\DescribeProcessingJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeProcessingJobResponse)

-- | A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
describeProcessingJobResponse_failureReason :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.Text)
describeProcessingJobResponse_failureReason = Lens.lens (\DescribeProcessingJobResponse' {failureReason} -> failureReason) (\s@DescribeProcessingJobResponse' {} a -> s {failureReason = a} :: DescribeProcessingJobResponse)

-- | The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
describeProcessingJobResponse_monitoringScheduleArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.Text)
describeProcessingJobResponse_monitoringScheduleArn = Lens.lens (\DescribeProcessingJobResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@DescribeProcessingJobResponse' {} a -> s {monitoringScheduleArn = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job was last modified.
describeProcessingJobResponse_lastModifiedTime :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.UTCTime)
describeProcessingJobResponse_lastModifiedTime = Lens.lens (\DescribeProcessingJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeProcessingJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeProcessingJobResponse) Core.. Lens.mapping Core._Time

-- | The inputs for a processing job.
describeProcessingJobResponse_processingInputs :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe [ProcessingInput])
describeProcessingJobResponse_processingInputs = Lens.lens (\DescribeProcessingJobResponse' {processingInputs} -> processingInputs) (\s@DescribeProcessingJobResponse' {} a -> s {processingInputs = a} :: DescribeProcessingJobResponse) Core.. Lens.mapping Lens._Coerce

-- | The time at which the processing job started.
describeProcessingJobResponse_processingStartTime :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.UTCTime)
describeProcessingJobResponse_processingStartTime = Lens.lens (\DescribeProcessingJobResponse' {processingStartTime} -> processingStartTime) (\s@DescribeProcessingJobResponse' {} a -> s {processingStartTime = a} :: DescribeProcessingJobResponse) Core.. Lens.mapping Core._Time

-- | The time limit for how long the processing job is allowed to run.
describeProcessingJobResponse_stoppingCondition :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe ProcessingStoppingCondition)
describeProcessingJobResponse_stoppingCondition = Lens.lens (\DescribeProcessingJobResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeProcessingJobResponse' {} a -> s {stoppingCondition = a} :: DescribeProcessingJobResponse)

-- | The ARN of a training job associated with this processing job.
describeProcessingJobResponse_trainingJobArn :: Lens.Lens' DescribeProcessingJobResponse (Core.Maybe Core.Text)
describeProcessingJobResponse_trainingJobArn = Lens.lens (\DescribeProcessingJobResponse' {trainingJobArn} -> trainingJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {trainingJobArn = a} :: DescribeProcessingJobResponse)

-- | The response's http status code.
describeProcessingJobResponse_httpStatus :: Lens.Lens' DescribeProcessingJobResponse Core.Int
describeProcessingJobResponse_httpStatus = Lens.lens (\DescribeProcessingJobResponse' {httpStatus} -> httpStatus) (\s@DescribeProcessingJobResponse' {} a -> s {httpStatus = a} :: DescribeProcessingJobResponse)

-- | The name of the processing job. The name must be unique within an AWS
-- Region in the AWS account.
describeProcessingJobResponse_processingJobName :: Lens.Lens' DescribeProcessingJobResponse Core.Text
describeProcessingJobResponse_processingJobName = Lens.lens (\DescribeProcessingJobResponse' {processingJobName} -> processingJobName) (\s@DescribeProcessingJobResponse' {} a -> s {processingJobName = a} :: DescribeProcessingJobResponse)

-- | Identifies the resources, ML compute instances, and ML storage volumes
-- to deploy for a processing job. In distributed training, you specify
-- more than one instance.
describeProcessingJobResponse_processingResources :: Lens.Lens' DescribeProcessingJobResponse ProcessingResources
describeProcessingJobResponse_processingResources = Lens.lens (\DescribeProcessingJobResponse' {processingResources} -> processingResources) (\s@DescribeProcessingJobResponse' {} a -> s {processingResources = a} :: DescribeProcessingJobResponse)

-- | Configures the processing job to run a specified container image.
describeProcessingJobResponse_appSpecification :: Lens.Lens' DescribeProcessingJobResponse AppSpecification
describeProcessingJobResponse_appSpecification = Lens.lens (\DescribeProcessingJobResponse' {appSpecification} -> appSpecification) (\s@DescribeProcessingJobResponse' {} a -> s {appSpecification = a} :: DescribeProcessingJobResponse)

-- | The Amazon Resource Name (ARN) of the processing job.
describeProcessingJobResponse_processingJobArn :: Lens.Lens' DescribeProcessingJobResponse Core.Text
describeProcessingJobResponse_processingJobArn = Lens.lens (\DescribeProcessingJobResponse' {processingJobArn} -> processingJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {processingJobArn = a} :: DescribeProcessingJobResponse)

-- | Provides the status of a processing job.
describeProcessingJobResponse_processingJobStatus :: Lens.Lens' DescribeProcessingJobResponse ProcessingJobStatus
describeProcessingJobResponse_processingJobStatus = Lens.lens (\DescribeProcessingJobResponse' {processingJobStatus} -> processingJobStatus) (\s@DescribeProcessingJobResponse' {} a -> s {processingJobStatus = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job was created.
describeProcessingJobResponse_creationTime :: Lens.Lens' DescribeProcessingJobResponse Core.UTCTime
describeProcessingJobResponse_creationTime = Lens.lens (\DescribeProcessingJobResponse' {creationTime} -> creationTime) (\s@DescribeProcessingJobResponse' {} a -> s {creationTime = a} :: DescribeProcessingJobResponse) Core.. Core._Time

instance Core.NFData DescribeProcessingJobResponse
