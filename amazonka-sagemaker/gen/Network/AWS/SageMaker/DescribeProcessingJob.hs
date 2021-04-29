{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeProcessingJob' smart constructor.
data DescribeProcessingJob = DescribeProcessingJob'
  { -- | The name of the processing job. The name must be unique within an AWS
    -- Region in the AWS account.
    processingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeProcessingJob
newDescribeProcessingJob pProcessingJobName_ =
  DescribeProcessingJob'
    { processingJobName =
        pProcessingJobName_
    }

-- | The name of the processing job. The name must be unique within an AWS
-- Region in the AWS account.
describeProcessingJob_processingJobName :: Lens.Lens' DescribeProcessingJob Prelude.Text
describeProcessingJob_processingJobName = Lens.lens (\DescribeProcessingJob' {processingJobName} -> processingJobName) (\s@DescribeProcessingJob' {} a -> s {processingJobName = a} :: DescribeProcessingJob)

instance Prelude.AWSRequest DescribeProcessingJob where
  type
    Rs DescribeProcessingJob =
      DescribeProcessingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProcessingJobResponse'
            Prelude.<$> (x Prelude..?> "NetworkConfig")
            Prelude.<*> (x Prelude..?> "ProcessingEndTime")
            Prelude.<*> (x Prelude..?> "RoleArn")
            Prelude.<*> (x Prelude..?> "ProcessingOutputConfig")
            Prelude.<*> (x Prelude..?> "ExitMessage")
            Prelude.<*> (x Prelude..?> "ExperimentConfig")
            Prelude.<*> ( x Prelude..?> "Environment"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "AutoMLJobArn")
            Prelude.<*> (x Prelude..?> "FailureReason")
            Prelude.<*> (x Prelude..?> "MonitoringScheduleArn")
            Prelude.<*> (x Prelude..?> "LastModifiedTime")
            Prelude.<*> ( x Prelude..?> "ProcessingInputs"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "ProcessingStartTime")
            Prelude.<*> (x Prelude..?> "StoppingCondition")
            Prelude.<*> (x Prelude..?> "TrainingJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "ProcessingJobName")
            Prelude.<*> (x Prelude..:> "ProcessingResources")
            Prelude.<*> (x Prelude..:> "AppSpecification")
            Prelude.<*> (x Prelude..:> "ProcessingJobArn")
            Prelude.<*> (x Prelude..:> "ProcessingJobStatus")
            Prelude.<*> (x Prelude..:> "CreationTime")
      )

instance Prelude.Hashable DescribeProcessingJob

instance Prelude.NFData DescribeProcessingJob

instance Prelude.ToHeaders DescribeProcessingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DescribeProcessingJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeProcessingJob where
  toJSON DescribeProcessingJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProcessingJobName" Prelude..= processingJobName)
          ]
      )

instance Prelude.ToPath DescribeProcessingJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeProcessingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProcessingJobResponse' smart constructor.
data DescribeProcessingJobResponse = DescribeProcessingJobResponse'
  { -- | Networking options for a processing job.
    networkConfig :: Prelude.Maybe NetworkConfig,
    -- | The time at which the processing job completed.
    processingEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Output configuration for the processing job.
    processingOutputConfig :: Prelude.Maybe ProcessingOutputConfig,
    -- | An optional string, up to one KB in size, that contains metadata from
    -- the processing container when the processing job exits.
    exitMessage :: Prelude.Maybe Prelude.Text,
    -- | The configuration information used to create an experiment.
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | The environment variables set in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of an AutoML job associated with this processing job.
    autoMLJobArn :: Prelude.Maybe Prelude.Text,
    -- | A string, up to one KB in size, that contains the reason a processing
    -- job failed, if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a monitoring schedule for an endpoint associated with this
    -- processing job.
    monitoringScheduleArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the processing job was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The inputs for a processing job.
    processingInputs :: Prelude.Maybe [ProcessingInput],
    -- | The time at which the processing job started.
    processingStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time limit for how long the processing job is allowed to run.
    stoppingCondition :: Prelude.Maybe ProcessingStoppingCondition,
    -- | The ARN of a training job associated with this processing job.
    trainingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the processing job. The name must be unique within an AWS
    -- Region in the AWS account.
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
    creationTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      { networkConfig =
          Prelude.Nothing,
        processingEndTime = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        processingOutputConfig = Prelude.Nothing,
        exitMessage = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        environment = Prelude.Nothing,
        autoMLJobArn = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        monitoringScheduleArn = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        processingInputs = Prelude.Nothing,
        processingStartTime = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        trainingJobArn = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        processingJobName = pProcessingJobName_,
        processingResources = pProcessingResources_,
        appSpecification = pAppSpecification_,
        processingJobArn = pProcessingJobArn_,
        processingJobStatus = pProcessingJobStatus_,
        creationTime =
          Prelude._Time Lens.# pCreationTime_
      }

-- | Networking options for a processing job.
describeProcessingJobResponse_networkConfig :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe NetworkConfig)
describeProcessingJobResponse_networkConfig = Lens.lens (\DescribeProcessingJobResponse' {networkConfig} -> networkConfig) (\s@DescribeProcessingJobResponse' {} a -> s {networkConfig = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job completed.
describeProcessingJobResponse_processingEndTime :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeProcessingJobResponse_processingEndTime = Lens.lens (\DescribeProcessingJobResponse' {processingEndTime} -> processingEndTime) (\s@DescribeProcessingJobResponse' {} a -> s {processingEndTime = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
describeProcessingJobResponse_roleArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_roleArn = Lens.lens (\DescribeProcessingJobResponse' {roleArn} -> roleArn) (\s@DescribeProcessingJobResponse' {} a -> s {roleArn = a} :: DescribeProcessingJobResponse)

-- | Output configuration for the processing job.
describeProcessingJobResponse_processingOutputConfig :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe ProcessingOutputConfig)
describeProcessingJobResponse_processingOutputConfig = Lens.lens (\DescribeProcessingJobResponse' {processingOutputConfig} -> processingOutputConfig) (\s@DescribeProcessingJobResponse' {} a -> s {processingOutputConfig = a} :: DescribeProcessingJobResponse)

-- | An optional string, up to one KB in size, that contains metadata from
-- the processing container when the processing job exits.
describeProcessingJobResponse_exitMessage :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_exitMessage = Lens.lens (\DescribeProcessingJobResponse' {exitMessage} -> exitMessage) (\s@DescribeProcessingJobResponse' {} a -> s {exitMessage = a} :: DescribeProcessingJobResponse)

-- | The configuration information used to create an experiment.
describeProcessingJobResponse_experimentConfig :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe ExperimentConfig)
describeProcessingJobResponse_experimentConfig = Lens.lens (\DescribeProcessingJobResponse' {experimentConfig} -> experimentConfig) (\s@DescribeProcessingJobResponse' {} a -> s {experimentConfig = a} :: DescribeProcessingJobResponse)

-- | The environment variables set in the Docker container.
describeProcessingJobResponse_environment :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeProcessingJobResponse_environment = Lens.lens (\DescribeProcessingJobResponse' {environment} -> environment) (\s@DescribeProcessingJobResponse' {} a -> s {environment = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The ARN of an AutoML job associated with this processing job.
describeProcessingJobResponse_autoMLJobArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_autoMLJobArn = Lens.lens (\DescribeProcessingJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeProcessingJobResponse)

-- | A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
describeProcessingJobResponse_failureReason :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_failureReason = Lens.lens (\DescribeProcessingJobResponse' {failureReason} -> failureReason) (\s@DescribeProcessingJobResponse' {} a -> s {failureReason = a} :: DescribeProcessingJobResponse)

-- | The ARN of a monitoring schedule for an endpoint associated with this
-- processing job.
describeProcessingJobResponse_monitoringScheduleArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_monitoringScheduleArn = Lens.lens (\DescribeProcessingJobResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@DescribeProcessingJobResponse' {} a -> s {monitoringScheduleArn = a} :: DescribeProcessingJobResponse)

-- | The time at which the processing job was last modified.
describeProcessingJobResponse_lastModifiedTime :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeProcessingJobResponse_lastModifiedTime = Lens.lens (\DescribeProcessingJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeProcessingJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Prelude._Time

-- | The inputs for a processing job.
describeProcessingJobResponse_processingInputs :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe [ProcessingInput])
describeProcessingJobResponse_processingInputs = Lens.lens (\DescribeProcessingJobResponse' {processingInputs} -> processingInputs) (\s@DescribeProcessingJobResponse' {} a -> s {processingInputs = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The time at which the processing job started.
describeProcessingJobResponse_processingStartTime :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeProcessingJobResponse_processingStartTime = Lens.lens (\DescribeProcessingJobResponse' {processingStartTime} -> processingStartTime) (\s@DescribeProcessingJobResponse' {} a -> s {processingStartTime = a} :: DescribeProcessingJobResponse) Prelude.. Lens.mapping Prelude._Time

-- | The time limit for how long the processing job is allowed to run.
describeProcessingJobResponse_stoppingCondition :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe ProcessingStoppingCondition)
describeProcessingJobResponse_stoppingCondition = Lens.lens (\DescribeProcessingJobResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeProcessingJobResponse' {} a -> s {stoppingCondition = a} :: DescribeProcessingJobResponse)

-- | The ARN of a training job associated with this processing job.
describeProcessingJobResponse_trainingJobArn :: Lens.Lens' DescribeProcessingJobResponse (Prelude.Maybe Prelude.Text)
describeProcessingJobResponse_trainingJobArn = Lens.lens (\DescribeProcessingJobResponse' {trainingJobArn} -> trainingJobArn) (\s@DescribeProcessingJobResponse' {} a -> s {trainingJobArn = a} :: DescribeProcessingJobResponse)

-- | The response's http status code.
describeProcessingJobResponse_httpStatus :: Lens.Lens' DescribeProcessingJobResponse Prelude.Int
describeProcessingJobResponse_httpStatus = Lens.lens (\DescribeProcessingJobResponse' {httpStatus} -> httpStatus) (\s@DescribeProcessingJobResponse' {} a -> s {httpStatus = a} :: DescribeProcessingJobResponse)

-- | The name of the processing job. The name must be unique within an AWS
-- Region in the AWS account.
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
describeProcessingJobResponse_creationTime = Lens.lens (\DescribeProcessingJobResponse' {creationTime} -> creationTime) (\s@DescribeProcessingJobResponse' {} a -> s {creationTime = a} :: DescribeProcessingJobResponse) Prelude.. Prelude._Time

instance Prelude.NFData DescribeProcessingJobResponse
