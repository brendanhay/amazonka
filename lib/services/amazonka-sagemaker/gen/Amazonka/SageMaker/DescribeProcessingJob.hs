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
    describeProcessingJobResponse_autoMLJobArn,
    describeProcessingJobResponse_environment,
    describeProcessingJobResponse_exitMessage,
    describeProcessingJobResponse_experimentConfig,
    describeProcessingJobResponse_failureReason,
    describeProcessingJobResponse_lastModifiedTime,
    describeProcessingJobResponse_monitoringScheduleArn,
    describeProcessingJobResponse_networkConfig,
    describeProcessingJobResponse_processingEndTime,
    describeProcessingJobResponse_processingInputs,
    describeProcessingJobResponse_processingOutputConfig,
    describeProcessingJobResponse_processingStartTime,
    describeProcessingJobResponse_roleArn,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "AutoMLJobArn")
            Prelude.<*> (x Data..?> "Environment" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ExitMessage")
            Prelude.<*> (x Data..?> "ExperimentConfig")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "MonitoringScheduleArn")
            Prelude.<*> (x Data..?> "NetworkConfig")
            Prelude.<*> (x Data..?> "ProcessingEndTime")
            Prelude.<*> ( x Data..?> "ProcessingInputs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ProcessingOutputConfig")
            Prelude.<*> (x Data..?> "ProcessingStartTime")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "StoppingCondition")
            Prelude.<*> (x Data..?> "TrainingJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProcessingJobName")
            Prelude.<*> (x Data..:> "ProcessingResources")
            Prelude.<*> (x Data..:> "AppSpecification")
            Prelude.<*> (x Data..:> "ProcessingJobArn")
            Prelude.<*> (x Data..:> "ProcessingJobStatus")
            Prelude.<*> (x Data..:> "CreationTime")
      )

instance Prelude.Hashable DescribeProcessingJob where
  hashWithSalt _salt DescribeProcessingJob' {..} =
    _salt `Prelude.hashWithSalt` processingJobName

instance Prelude.NFData DescribeProcessingJob where
  rnf DescribeProcessingJob' {..} =
    Prelude.rnf processingJobName

instance Data.ToHeaders DescribeProcessingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeProcessingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProcessingJob where
  toJSON DescribeProcessingJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProcessingJobName" Data..= processingJobName)
          ]
      )

instance Data.ToPath DescribeProcessingJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProcessingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProcessingJobResponse' smart constructor.
data DescribeProcessingJobResponse = DescribeProcessingJobResponse'
  { -- | The ARN of an AutoML job associated with this processing job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | The environment variables set in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An optional string, up to one KB in size, that contains metadata from
    -- the processing container when the processing job exits.
    exitMessage :: Prelude.Maybe Prelude.Text,
    -- | The configuration information used to create an experiment.
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | A string, up to one KB in size, that contains the reason a processing
    -- job failed, if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The time at which the processing job was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of a monitoring schedule for an endpoint associated with this
    -- processing job.
    monitoringScheduleArn :: Prelude.Maybe Prelude.Text,
    -- | Networking options for a processing job.
    networkConfig :: Prelude.Maybe NetworkConfig,
    -- | The time at which the processing job completed.
    processingEndTime :: Prelude.Maybe Data.POSIX,
    -- | The inputs for a processing job.
    processingInputs :: Prelude.Maybe [ProcessingInput],
    -- | Output configuration for the processing job.
    processingOutputConfig :: Prelude.Maybe ProcessingOutputConfig,
    -- | The time at which the processing job started.
    processingStartTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The time limit for how long the processing job is allowed to run.
    stoppingCondition :: Prelude.Maybe ProcessingStoppingCondition,
    -- | The ARN of a training job associated with this processing job.
    trainingJobArn :: Prelude.Maybe Prelude.Text,
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
    creationTime :: Data.POSIX
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
-- 'autoMLJobArn', 'describeProcessingJobResponse_autoMLJobArn' - The ARN of an AutoML job associated with this processing job.
--
-- 'environment', 'describeProcessingJobResponse_environment' - The environment variables set in the Docker container.
--
-- 'exitMessage', 'describeProcessingJobResponse_exitMessage' - An optional string, up to one KB in size, that contains metadata from
-- the processing container when the processing job exits.
--
-- 'experimentConfig', 'describeProcessingJobResponse_experimentConfig' - The configuration information used to create an experiment.
--
-- 'failureReason', 'describeProcessingJobResponse_failureReason' - A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
--
-- 'lastModifiedTime', 'describeProcessingJobResponse_lastModifiedTime' - The time at which the processing job was last modified.
--
-- 'monitoringScheduleArn', 'describeProcessingJobResponse_monitoringScheduleArn' - The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
--
-- 'networkConfig', 'describeProcessingJobResponse_networkConfig' - Networking options for a processing job.
--
-- 'processingEndTime', 'describeProcessingJobResponse_processingEndTime' - The time at which the processing job completed.
--
-- 'processingInputs', 'describeProcessingJobResponse_processingInputs' - The inputs for a processing job.
--
-- 'processingOutputConfig', 'describeProcessingJobResponse_processingOutputConfig' - Output configuration for the processing job.
--
-- 'processingStartTime', 'describeProcessingJobResponse_processingStartTime' - The time at which the processing job started.
--
-- 'roleArn', 'describeProcessingJobResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
--
-- 'stoppingCondition', 'describeProcessingJobResponse_stoppingCondition' - The time limit for how long the processing job is allowed to run.
--
-- 'trainingJobArn', 'describeProcessingJobResponse_trainingJobArn' - The ARN of a training job associated with this processing job.
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
      { autoMLJobArn =
          Prelude.Nothing,
        environment = Prelude.Nothing,
        exitMessage = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        monitoringScheduleArn = Prelude.Nothing,
        networkConfig = Prelude.Nothing,
        processingEndTime = Prelude.Nothing,
        processingInputs = Prelude.Nothing,
        processingOutputConfig = Prelude.Nothing,
        processingStartTime = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        trainingJobArn = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        processingJobName = pProcessingJobName_,
        processingResources = pProcessingResources_,
        appSpecification = pAppSpecification_,
        processingJobArn = pProcessingJobArn_,
        processingJobStatus = pProcessingJobStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_
      }

-- | The ARN of an AutoML job associated with this processing job.
describeProcessingJobResponse_autoMLJobArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_autoMLJobArn = Lens.lens (\DescribeProcessingJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeProcessingJobResponse)

-- | The environment variables set in the Docker container.
describeProcessingJobResponse_environment :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeProcessingJobResponse_environment = Lens.lens (\DescribeProcessingJobResponse' {environment} -> environment) (\s@DescribeProcessingJobResponse' {} a -> s {environment = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional string, up to one KB in size, that contains metadata from
-- the processing container when the processing job exits.
describeProcessingJobResponse_exitMessage :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_exitMessage = Lens.lens (\DescribeProcessingJobResponse' {exitMessage} -> exitMessage) (\s@DescribeProcessingJobResponse' {} a -> s {exitMessage = a} :: DescribeProcessingJobResponse)

-- | The configuration information used to create an experiment.
describeProcessingJobResponse_experimentConfig :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe ExperimentConfig)
describeProcessingJobResponse_experimentConfig = Lens.lens (\DescribeProcessingJobResponse' {experimentConfig} -> experimentConfig) (\s@DescribeProcessingJobResponse' {} a -> s {experimentConfig = a} :: DescribeProcessingJobResponse)

-- | A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
describeProcessingJobResponse_failureReason :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_failureReason = Lens.lens (\DescribeProcessingJobResponse' {failureReason} -> failureReason) (\s@DescribeProcessingJobResponse' {} a -> s {failureReason = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job was last modified.
describeProcessingJobResponse_lastModifiedTime :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeProcessingJobResponse_lastModifiedTime = Lens.lens (\DescribeProcessingJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeProcessingJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
describeProcessingJobResponse_monitoringScheduleArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_monitoringScheduleArn = Lens.lens (\DescribeProcessingJobResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@DescribeProcessingJobResponse' {} a -> s {monitoringScheduleArn = a} :: DescribeProcessingJobResponse)

-- | Networking options for a processing job.
describeProcessingJobResponse_networkConfig :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe NetworkConfig)
describeProcessingJobResponse_networkConfig = Lens.lens (\DescribeProcessingJobResponse' {networkConfig} -> networkConfig) (\s@DescribeProcessingJobResponse' {} a -> s {networkConfig = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job completed.
describeProcessingJobResponse_processingEndTime :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeProcessingJobResponse_processingEndTime = Lens.lens (\DescribeProcessingJobResponse' {processingEndTime} -> processingEndTime) (\s@DescribeProcessingJobResponse' {} a -> s {processingEndTime = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Data._Time

-- | The inputs for a processing job.
describeProcessingJobResponse_processingInputs :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe [ProcessingInput])
describeProcessingJobResponse_processingInputs = Lens.lens (\DescribeProcessingJobResponse' {processingInputs} -> processingInputs) (\s@DescribeProcessingJobResponse' {} a -> s {processingInputs = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Output configuration for the processing job.
describeProcessingJobResponse_processingOutputConfig :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe ProcessingOutputConfig)
describeProcessingJobResponse_processingOutputConfig = Lens.lens (\DescribeProcessingJobResponse' {processingOutputConfig} -> processingOutputConfig) (\s@DescribeProcessingJobResponse' {} a -> s {processingOutputConfig = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job started.
describeProcessingJobResponse_processingStartTime :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeProcessingJobResponse_processingStartTime = Lens.lens (\DescribeProcessingJobResponse' {processingStartTime} -> processingStartTime) (\s@DescribeProcessingJobResponse' {} a -> s {processingStartTime = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
describeProcessingJobResponse_roleArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_roleArn = Lens.lens (\DescribeProcessingJobResponse' {roleArn} -> roleArn) (\s@DescribeProcessingJobResponse' {} a -> s {roleArn = a} :: DescribeProcessingJobResponse)

-- | The time limit for how long the processing job is allowed to run.
describeProcessingJobResponse_stoppingCondition :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe ProcessingStoppingCondition)
describeProcessingJobResponse_stoppingCondition = Lens.lens (\DescribeProcessingJobResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeProcessingJobResponse' {} a -> s {stoppingCondition = a} :: DescribeProcessingJobResponse)

-- | The ARN of a training job associated with this processing job.
describeProcessingJobResponse_trainingJobArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_trainingJobArn = Lens.lens (\DescribeProcessingJobResponse' {trainingJobArn} -> trainingJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {trainingJobArn = a} :: DescribeProcessingJobResponse)

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
describeProcessingJobResponse_creationTime = Lens.lens (\DescribeProcessingJobResponse' {creationTime} -> creationTime) (\s@DescribeProcessingJobResponse' {} a -> s {creationTime = a} :: DescribeProcessingJobResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeProcessingJobResponse where
  rnf DescribeProcessingJobResponse' {..} =
    Prelude.rnf autoMLJobArn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf exitMessage
      `Prelude.seq` Prelude.rnf experimentConfig
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf monitoringScheduleArn
      `Prelude.seq` Prelude.rnf networkConfig
      `Prelude.seq` Prelude.rnf processingEndTime
      `Prelude.seq` Prelude.rnf processingInputs
      `Prelude.seq` Prelude.rnf processingOutputConfig
      `Prelude.seq` Prelude.rnf processingStartTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf stoppingCondition
      `Prelude.seq` Prelude.rnf trainingJobArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf processingJobName
      `Prelude.seq` Prelude.rnf processingResources
      `Prelude.seq` Prelude.rnf appSpecification
      `Prelude.seq` Prelude.rnf processingJobArn
      `Prelude.seq` Prelude.rnf
        processingJobStatus
      `Prelude.seq` Prelude.rnf creationTime
