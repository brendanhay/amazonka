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
-- Module      : Amazonka.SageMaker.DescribeProcessingJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a processing job.
module Amazonka.SageMaker.DescribeProcessingJob
  ( -- * Creating a Request
    DescribeProcessingJob (..),
    newDescribeProcessingJob,

    -- * Request Lenses
    describeProcessingJob_processingJobName,

    -- * Destructuring the Response
    DescribeProcessingJobResponse (..),
    newDescribeProcessingJobResponse,

    -- * Response Lenses
    describeProcessingJobResponse_roleArn,
    describeProcessingJobResponse_environment,
    describeProcessingJobResponse_monitoringScheduleArn,
    describeProcessingJobResponse_networkConfig,
    describeProcessingJobResponse_experimentConfig,
    describeProcessingJobResponse_autoMLJobArn,
    describeProcessingJobResponse_processingInputs,
    describeProcessingJobResponse_lastModifiedTime,
    describeProcessingJobResponse_stoppingCondition,
    describeProcessingJobResponse_processingStartTime,
    describeProcessingJobResponse_processingEndTime,
    describeProcessingJobResponse_trainingJobArn,
    describeProcessingJobResponse_exitMessage,
    describeProcessingJobResponse_failureReason,
    describeProcessingJobResponse_processingOutputConfig,
    describeProcessingJobResponse_httpStatus,
    describeProcessingJobResponse_processingJobName,
    describeProcessingJobResponse_processingResources,
    describeProcessingJobResponse_appSpecification,
    describeProcessingJobResponse_processingJobArn,
    describeProcessingJobResponse_processingJobStatus,
    describeProcessingJobResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeProcessingJob' smart constructor.
data DescribeProcessingJob = DescribeProcessingJob'
  { -- | The name of the processing job. The name must be unique within an Amazon
    -- Web Services Region in the Amazon Web Services account.
    processingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProcessingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingJobName', 'describeProcessingJob_processingJobName' - The name of the processing job. The name must be unique within an Amazon
-- Web Services Region in the Amazon Web Services account.
newDescribeProcessingJob ::
  -- | 'processingJobName'
  Prelude.Text ->
  DescribeProcessingJob
newDescribeProcessingJob pProcessingJobName_ =
  DescribeProcessingJob'
    { processingJobName =
        pProcessingJobName_
    }

-- | The name of the processing job. The name must be unique within an Amazon
-- Web Services Region in the Amazon Web Services account.
describeProcessingJob_processingJobName :: Lens.Lens' DescribeProcessingJob Prelude.Text
describeProcessingJob_processingJobName = Lens.lens (\DescribeProcessingJob' {processingJobName} -> processingJobName) (\s@DescribeProcessingJob' {} a -> s {processingJobName = a} :: DescribeProcessingJob)

instance Core.AWSRequest DescribeProcessingJob where
  type
    AWSResponse DescribeProcessingJob =
      DescribeProcessingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProcessingJobResponse'
            Prelude.<$> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "Environment" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "MonitoringScheduleArn")
            Prelude.<*> (x Core..?> "NetworkConfig")
            Prelude.<*> (x Core..?> "ExperimentConfig")
            Prelude.<*> (x Core..?> "AutoMLJobArn")
            Prelude.<*> ( x Core..?> "ProcessingInputs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "StoppingCondition")
            Prelude.<*> (x Core..?> "ProcessingStartTime")
            Prelude.<*> (x Core..?> "ProcessingEndTime")
            Prelude.<*> (x Core..?> "TrainingJobArn")
            Prelude.<*> (x Core..?> "ExitMessage")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "ProcessingOutputConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ProcessingJobName")
            Prelude.<*> (x Core..:> "ProcessingResources")
            Prelude.<*> (x Core..:> "AppSpecification")
            Prelude.<*> (x Core..:> "ProcessingJobArn")
            Prelude.<*> (x Core..:> "ProcessingJobStatus")
            Prelude.<*> (x Core..:> "CreationTime")
      )

instance Prelude.Hashable DescribeProcessingJob where
  hashWithSalt _salt DescribeProcessingJob' {..} =
    _salt `Prelude.hashWithSalt` processingJobName

instance Prelude.NFData DescribeProcessingJob where
  rnf DescribeProcessingJob' {..} =
    Prelude.rnf processingJobName

instance Core.ToHeaders DescribeProcessingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeProcessingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProcessingJob where
  toJSON DescribeProcessingJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProcessingJobName" Core..= processingJobName)
          ]
      )

instance Core.ToPath DescribeProcessingJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProcessingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProcessingJobResponse' smart constructor.
data DescribeProcessingJobResponse = DescribeProcessingJobResponse'
  { -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The environment variables set in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of a monitoring schedule for an endpoint associated with this
    -- processing job.
    monitoringScheduleArn :: Prelude.Maybe Prelude.Text,
    -- | Networking options for a processing job.
    networkConfig :: Prelude.Maybe NetworkConfig,
    -- | The configuration information used to create an experiment.
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | The ARN of an AutoML job associated with this processing job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | The inputs for a processing job.
    processingInputs :: Prelude.Maybe [ProcessingInput],
    -- | The time at which the processing job was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The time limit for how long the processing job is allowed to run.
    stoppingCondition :: Prelude.Maybe ProcessingStoppingCondition,
    -- | The time at which the processing job started.
    processingStartTime :: Prelude.Maybe Core.POSIX,
    -- | The time at which the processing job completed.
    processingEndTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of a training job associated with this processing job.
    trainingJobArn :: Prelude.Maybe Prelude.Text,
    -- | An optional string, up to one KB in size, that contains metadata from
    -- the processing container when the processing job exits.
    exitMessage :: Prelude.Maybe Prelude.Text,
    -- | A string, up to one KB in size, that contains the reason a processing
    -- job failed, if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Output configuration for the processing job.
    processingOutputConfig :: Prelude.Maybe ProcessingOutputConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the processing job. The name must be unique within an Amazon
    -- Web Services Region in the Amazon Web Services account.
    processingJobName :: Prelude.Text,
    -- | Identifies the resources, ML compute instances, and ML storage volumes
    -- to deploy for a processing job. In distributed training, you specify
    -- more than one instance.
    processingResources :: ProcessingResources,
    -- | Configures the processing job to run a specified container image.
    appSpecification :: AppSpecification,
    -- | The Amazon Resource Name (ARN) of the processing job.
    processingJobArn :: Prelude.Text,
    -- | Provides the status of a processing job.
    processingJobStatus :: ProcessingJobStatus,
    -- | The time at which the processing job was created.
    creationTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProcessingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'describeProcessingJobResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
--
-- 'environment', 'describeProcessingJobResponse_environment' - The environment variables set in the Docker container.
--
-- 'monitoringScheduleArn', 'describeProcessingJobResponse_monitoringScheduleArn' - The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
--
-- 'networkConfig', 'describeProcessingJobResponse_networkConfig' - Networking options for a processing job.
--
-- 'experimentConfig', 'describeProcessingJobResponse_experimentConfig' - The configuration information used to create an experiment.
--
-- 'autoMLJobArn', 'describeProcessingJobResponse_autoMLJobArn' - The ARN of an AutoML job associated with this processing job.
--
-- 'processingInputs', 'describeProcessingJobResponse_processingInputs' - The inputs for a processing job.
--
-- 'lastModifiedTime', 'describeProcessingJobResponse_lastModifiedTime' - The time at which the processing job was last modified.
--
-- 'stoppingCondition', 'describeProcessingJobResponse_stoppingCondition' - The time limit for how long the processing job is allowed to run.
--
-- 'processingStartTime', 'describeProcessingJobResponse_processingStartTime' - The time at which the processing job started.
--
-- 'processingEndTime', 'describeProcessingJobResponse_processingEndTime' - The time at which the processing job completed.
--
-- 'trainingJobArn', 'describeProcessingJobResponse_trainingJobArn' - The ARN of a training job associated with this processing job.
--
-- 'exitMessage', 'describeProcessingJobResponse_exitMessage' - An optional string, up to one KB in size, that contains metadata from
-- the processing container when the processing job exits.
--
-- 'failureReason', 'describeProcessingJobResponse_failureReason' - A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
--
-- 'processingOutputConfig', 'describeProcessingJobResponse_processingOutputConfig' - Output configuration for the processing job.
--
-- 'httpStatus', 'describeProcessingJobResponse_httpStatus' - The response's http status code.
--
-- 'processingJobName', 'describeProcessingJobResponse_processingJobName' - The name of the processing job. The name must be unique within an Amazon
-- Web Services Region in the Amazon Web Services account.
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
  Prelude.Int ->
  -- | 'processingJobName'
  Prelude.Text ->
  -- | 'processingResources'
  ProcessingResources ->
  -- | 'appSpecification'
  AppSpecification ->
  -- | 'processingJobArn'
  Prelude.Text ->
  -- | 'processingJobStatus'
  ProcessingJobStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
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
      { roleArn =
          Prelude.Nothing,
        environment = Prelude.Nothing,
        monitoringScheduleArn = Prelude.Nothing,
        networkConfig = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        autoMLJobArn = Prelude.Nothing,
        processingInputs = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        processingStartTime = Prelude.Nothing,
        processingEndTime = Prelude.Nothing,
        trainingJobArn = Prelude.Nothing,
        exitMessage = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        processingOutputConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        processingJobName = pProcessingJobName_,
        processingResources = pProcessingResources_,
        appSpecification = pAppSpecification_,
        processingJobArn = pProcessingJobArn_,
        processingJobStatus = pProcessingJobStatus_,
        creationTime =
          Core._Time Lens.# pCreationTime_
      }

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
describeProcessingJobResponse_roleArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_roleArn = Lens.lens (\DescribeProcessingJobResponse' {roleArn} -> roleArn) (\s@DescribeProcessingJobResponse' {} a -> s {roleArn = a} :: DescribeProcessingJobResponse)

-- | The environment variables set in the Docker container.
describeProcessingJobResponse_environment :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeProcessingJobResponse_environment = Lens.lens (\DescribeProcessingJobResponse' {environment} -> environment) (\s@DescribeProcessingJobResponse' {} a -> s {environment = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
describeProcessingJobResponse_monitoringScheduleArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_monitoringScheduleArn = Lens.lens (\DescribeProcessingJobResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@DescribeProcessingJobResponse' {} a -> s {monitoringScheduleArn = a} :: DescribeProcessingJobResponse)

-- | Networking options for a processing job.
describeProcessingJobResponse_networkConfig :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe NetworkConfig)
describeProcessingJobResponse_networkConfig = Lens.lens (\DescribeProcessingJobResponse' {networkConfig} -> networkConfig) (\s@DescribeProcessingJobResponse' {} a -> s {networkConfig = a} :: DescribeProcessingJobResponse)

-- | The configuration information used to create an experiment.
describeProcessingJobResponse_experimentConfig :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe ExperimentConfig)
describeProcessingJobResponse_experimentConfig = Lens.lens (\DescribeProcessingJobResponse' {experimentConfig} -> experimentConfig) (\s@DescribeProcessingJobResponse' {} a -> s {experimentConfig = a} :: DescribeProcessingJobResponse)

-- | The ARN of an AutoML job associated with this processing job.
describeProcessingJobResponse_autoMLJobArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_autoMLJobArn = Lens.lens (\DescribeProcessingJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeProcessingJobResponse)

-- | The inputs for a processing job.
describeProcessingJobResponse_processingInputs :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe [ProcessingInput])
describeProcessingJobResponse_processingInputs = Lens.lens (\DescribeProcessingJobResponse' {processingInputs} -> processingInputs) (\s@DescribeProcessingJobResponse' {} a -> s {processingInputs = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the processing job was last modified.
describeProcessingJobResponse_lastModifiedTime :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeProcessingJobResponse_lastModifiedTime = Lens.lens (\DescribeProcessingJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeProcessingJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Core._Time

-- | The time limit for how long the processing job is allowed to run.
describeProcessingJobResponse_stoppingCondition :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe ProcessingStoppingCondition)
describeProcessingJobResponse_stoppingCondition = Lens.lens (\DescribeProcessingJobResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeProcessingJobResponse' {} a -> s {stoppingCondition = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job started.
describeProcessingJobResponse_processingStartTime :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeProcessingJobResponse_processingStartTime = Lens.lens (\DescribeProcessingJobResponse' {processingStartTime} -> processingStartTime) (\s@DescribeProcessingJobResponse' {} a -> s {processingStartTime = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Core._Time

-- | The time at which the processing job completed.
describeProcessingJobResponse_processingEndTime :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeProcessingJobResponse_processingEndTime = Lens.lens (\DescribeProcessingJobResponse' {processingEndTime} -> processingEndTime) (\s@DescribeProcessingJobResponse' {} a -> s {processingEndTime = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN of a training job associated with this processing job.
describeProcessingJobResponse_trainingJobArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_trainingJobArn = Lens.lens (\DescribeProcessingJobResponse' {trainingJobArn} -> trainingJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {trainingJobArn = a} :: DescribeProcessingJobResponse)

-- | An optional string, up to one KB in size, that contains metadata from
-- the processing container when the processing job exits.
describeProcessingJobResponse_exitMessage :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_exitMessage = Lens.lens (\DescribeProcessingJobResponse' {exitMessage} -> exitMessage) (\s@DescribeProcessingJobResponse' {} a -> s {exitMessage = a} :: DescribeProcessingJobResponse)

-- | A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
describeProcessingJobResponse_failureReason :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_failureReason = Lens.lens (\DescribeProcessingJobResponse' {failureReason} -> failureReason) (\s@DescribeProcessingJobResponse' {} a -> s {failureReason = a} :: DescribeProcessingJobResponse)

-- | Output configuration for the processing job.
describeProcessingJobResponse_processingOutputConfig :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe ProcessingOutputConfig)
describeProcessingJobResponse_processingOutputConfig = Lens.lens (\DescribeProcessingJobResponse' {processingOutputConfig} -> processingOutputConfig) (\s@DescribeProcessingJobResponse' {} a -> s {processingOutputConfig = a} :: DescribeProcessingJobResponse)

-- | The response's http status code.
describeProcessingJobResponse_httpStatus :: Lens.Lens' DescribeProcessingJobResponse Prelude.Int
describeProcessingJobResponse_httpStatus = Lens.lens (\DescribeProcessingJobResponse' {httpStatus} -> httpStatus) (\s@DescribeProcessingJobResponse' {} a -> s {httpStatus = a} :: DescribeProcessingJobResponse)

-- | The name of the processing job. The name must be unique within an Amazon
-- Web Services Region in the Amazon Web Services account.
describeProcessingJobResponse_processingJobName :: Lens.Lens' DescribeProcessingJobResponse Prelude.Text
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
describeProcessingJobResponse_processingJobArn :: Lens.Lens' DescribeProcessingJobResponse Prelude.Text
describeProcessingJobResponse_processingJobArn = Lens.lens (\DescribeProcessingJobResponse' {processingJobArn} -> processingJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {processingJobArn = a} :: DescribeProcessingJobResponse)

-- | Provides the status of a processing job.
describeProcessingJobResponse_processingJobStatus :: Lens.Lens' DescribeProcessingJobResponse ProcessingJobStatus
describeProcessingJobResponse_processingJobStatus = Lens.lens (\DescribeProcessingJobResponse' {processingJobStatus} -> processingJobStatus) (\s@DescribeProcessingJobResponse' {} a -> s {processingJobStatus = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job was created.
describeProcessingJobResponse_creationTime :: Lens.Lens' DescribeProcessingJobResponse Prelude.UTCTime
describeProcessingJobResponse_creationTime = Lens.lens (\DescribeProcessingJobResponse' {creationTime} -> creationTime) (\s@DescribeProcessingJobResponse' {} a -> s {creationTime = a} :: DescribeProcessingJobResponse) Prelude.. Core._Time

instance Prelude.NFData DescribeProcessingJobResponse where
  rnf DescribeProcessingJobResponse' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf monitoringScheduleArn
      `Prelude.seq` Prelude.rnf networkConfig
      `Prelude.seq` Prelude.rnf experimentConfig
      `Prelude.seq` Prelude.rnf autoMLJobArn
      `Prelude.seq` Prelude.rnf processingInputs
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf stoppingCondition
      `Prelude.seq` Prelude.rnf processingStartTime
      `Prelude.seq` Prelude.rnf processingEndTime
      `Prelude.seq` Prelude.rnf trainingJobArn
      `Prelude.seq` Prelude.rnf exitMessage
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf processingOutputConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf processingJobName
      `Prelude.seq` Prelude.rnf processingResources
      `Prelude.seq` Prelude.rnf appSpecification
      `Prelude.seq` Prelude.rnf processingJobArn
      `Prelude.seq` Prelude.rnf
        processingJobStatus
      `Prelude.seq` Prelude.rnf creationTime
