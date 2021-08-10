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
-- Module      : Network.AWS.SageMaker.DescribeCompilationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a model compilation job.
--
-- To create a model compilation job, use CreateCompilationJob. To get
-- information about multiple model compilation jobs, use
-- ListCompilationJobs.
module Network.AWS.SageMaker.DescribeCompilationJob
  ( -- * Creating a Request
    DescribeCompilationJob (..),
    newDescribeCompilationJob,

    -- * Request Lenses
    describeCompilationJob_compilationJobName,

    -- * Destructuring the Response
    DescribeCompilationJobResponse (..),
    newDescribeCompilationJobResponse,

    -- * Response Lenses
    describeCompilationJobResponse_modelDigests,
    describeCompilationJobResponse_compilationStartTime,
    describeCompilationJobResponse_compilationEndTime,
    describeCompilationJobResponse_httpStatus,
    describeCompilationJobResponse_compilationJobName,
    describeCompilationJobResponse_compilationJobArn,
    describeCompilationJobResponse_compilationJobStatus,
    describeCompilationJobResponse_stoppingCondition,
    describeCompilationJobResponse_creationTime,
    describeCompilationJobResponse_lastModifiedTime,
    describeCompilationJobResponse_failureReason,
    describeCompilationJobResponse_modelArtifacts,
    describeCompilationJobResponse_roleArn,
    describeCompilationJobResponse_inputConfig,
    describeCompilationJobResponse_outputConfig,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeCompilationJob' smart constructor.
data DescribeCompilationJob = DescribeCompilationJob'
  { -- | The name of the model compilation job that you want information about.
    compilationJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCompilationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilationJobName', 'describeCompilationJob_compilationJobName' - The name of the model compilation job that you want information about.
newDescribeCompilationJob ::
  -- | 'compilationJobName'
  Prelude.Text ->
  DescribeCompilationJob
newDescribeCompilationJob pCompilationJobName_ =
  DescribeCompilationJob'
    { compilationJobName =
        pCompilationJobName_
    }

-- | The name of the model compilation job that you want information about.
describeCompilationJob_compilationJobName :: Lens.Lens' DescribeCompilationJob Prelude.Text
describeCompilationJob_compilationJobName = Lens.lens (\DescribeCompilationJob' {compilationJobName} -> compilationJobName) (\s@DescribeCompilationJob' {} a -> s {compilationJobName = a} :: DescribeCompilationJob)

instance Core.AWSRequest DescribeCompilationJob where
  type
    AWSResponse DescribeCompilationJob =
      DescribeCompilationJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCompilationJobResponse'
            Prelude.<$> (x Core..?> "ModelDigests")
            Prelude.<*> (x Core..?> "CompilationStartTime")
            Prelude.<*> (x Core..?> "CompilationEndTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "CompilationJobName")
            Prelude.<*> (x Core..:> "CompilationJobArn")
            Prelude.<*> (x Core..:> "CompilationJobStatus")
            Prelude.<*> (x Core..:> "StoppingCondition")
            Prelude.<*> (x Core..:> "CreationTime")
            Prelude.<*> (x Core..:> "LastModifiedTime")
            Prelude.<*> (x Core..:> "FailureReason")
            Prelude.<*> (x Core..:> "ModelArtifacts")
            Prelude.<*> (x Core..:> "RoleArn")
            Prelude.<*> (x Core..:> "InputConfig")
            Prelude.<*> (x Core..:> "OutputConfig")
      )

instance Prelude.Hashable DescribeCompilationJob

instance Prelude.NFData DescribeCompilationJob

instance Core.ToHeaders DescribeCompilationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeCompilationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCompilationJob where
  toJSON DescribeCompilationJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CompilationJobName" Core..= compilationJobName)
          ]
      )

instance Core.ToPath DescribeCompilationJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCompilationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCompilationJobResponse' smart constructor.
data DescribeCompilationJobResponse = DescribeCompilationJobResponse'
  { -- | Provides a BLAKE2 hash value that identifies the compiled model
    -- artifacts in Amazon S3.
    modelDigests :: Prelude.Maybe ModelDigests,
    -- | The time when the model compilation job started the @CompilationJob@
    -- instances.
    --
    -- You are billed for the time between this timestamp and the timestamp in
    -- the DescribeCompilationJobResponse$CompilationEndTime field. In Amazon
    -- CloudWatch Logs, the start time might be later than this time. That\'s
    -- because it takes time to download the compilation job, which depends on
    -- the size of the compilation job container.
    compilationStartTime :: Prelude.Maybe Core.POSIX,
    -- | The time when the model compilation job on a compilation job instance
    -- ended. For a successful or stopped job, this is when the job\'s model
    -- artifacts have finished uploading. For a failed job, this is when Amazon
    -- SageMaker detected that the job failed.
    compilationEndTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the model compilation job.
    compilationJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model compilation job.
    compilationJobArn :: Prelude.Text,
    -- | The status of the model compilation job.
    compilationJobStatus :: CompilationJobStatus,
    -- | Specifies a limit to how long a model compilation job can run. When the
    -- job reaches the time limit, Amazon SageMaker ends the compilation job.
    -- Use this API to cap model training costs.
    stoppingCondition :: StoppingCondition,
    -- | The time that the model compilation job was created.
    creationTime :: Core.POSIX,
    -- | The time that the status of the model compilation job was last modified.
    lastModifiedTime :: Core.POSIX,
    -- | If a model compilation job failed, the reason it failed.
    failureReason :: Prelude.Text,
    -- | Information about the location in Amazon S3 that has been configured for
    -- storing the model artifacts used in the compilation job.
    modelArtifacts :: ModelArtifacts,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker
    -- assumes to perform the model compilation job.
    roleArn :: Prelude.Text,
    -- | Information about the location in Amazon S3 of the input model
    -- artifacts, the name and shape of the expected data inputs, and the
    -- framework in which the model was trained.
    inputConfig :: InputConfig,
    -- | Information about the output location for the compiled model and the
    -- target device that the model runs on.
    outputConfig :: OutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCompilationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelDigests', 'describeCompilationJobResponse_modelDigests' - Provides a BLAKE2 hash value that identifies the compiled model
-- artifacts in Amazon S3.
--
-- 'compilationStartTime', 'describeCompilationJobResponse_compilationStartTime' - The time when the model compilation job started the @CompilationJob@
-- instances.
--
-- You are billed for the time between this timestamp and the timestamp in
-- the DescribeCompilationJobResponse$CompilationEndTime field. In Amazon
-- CloudWatch Logs, the start time might be later than this time. That\'s
-- because it takes time to download the compilation job, which depends on
-- the size of the compilation job container.
--
-- 'compilationEndTime', 'describeCompilationJobResponse_compilationEndTime' - The time when the model compilation job on a compilation job instance
-- ended. For a successful or stopped job, this is when the job\'s model
-- artifacts have finished uploading. For a failed job, this is when Amazon
-- SageMaker detected that the job failed.
--
-- 'httpStatus', 'describeCompilationJobResponse_httpStatus' - The response's http status code.
--
-- 'compilationJobName', 'describeCompilationJobResponse_compilationJobName' - The name of the model compilation job.
--
-- 'compilationJobArn', 'describeCompilationJobResponse_compilationJobArn' - The Amazon Resource Name (ARN) of the model compilation job.
--
-- 'compilationJobStatus', 'describeCompilationJobResponse_compilationJobStatus' - The status of the model compilation job.
--
-- 'stoppingCondition', 'describeCompilationJobResponse_stoppingCondition' - Specifies a limit to how long a model compilation job can run. When the
-- job reaches the time limit, Amazon SageMaker ends the compilation job.
-- Use this API to cap model training costs.
--
-- 'creationTime', 'describeCompilationJobResponse_creationTime' - The time that the model compilation job was created.
--
-- 'lastModifiedTime', 'describeCompilationJobResponse_lastModifiedTime' - The time that the status of the model compilation job was last modified.
--
-- 'failureReason', 'describeCompilationJobResponse_failureReason' - If a model compilation job failed, the reason it failed.
--
-- 'modelArtifacts', 'describeCompilationJobResponse_modelArtifacts' - Information about the location in Amazon S3 that has been configured for
-- storing the model artifacts used in the compilation job.
--
-- 'roleArn', 'describeCompilationJobResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker
-- assumes to perform the model compilation job.
--
-- 'inputConfig', 'describeCompilationJobResponse_inputConfig' - Information about the location in Amazon S3 of the input model
-- artifacts, the name and shape of the expected data inputs, and the
-- framework in which the model was trained.
--
-- 'outputConfig', 'describeCompilationJobResponse_outputConfig' - Information about the output location for the compiled model and the
-- target device that the model runs on.
newDescribeCompilationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'compilationJobName'
  Prelude.Text ->
  -- | 'compilationJobArn'
  Prelude.Text ->
  -- | 'compilationJobStatus'
  CompilationJobStatus ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'failureReason'
  Prelude.Text ->
  -- | 'modelArtifacts'
  ModelArtifacts ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'inputConfig'
  InputConfig ->
  -- | 'outputConfig'
  OutputConfig ->
  DescribeCompilationJobResponse
newDescribeCompilationJobResponse
  pHttpStatus_
  pCompilationJobName_
  pCompilationJobArn_
  pCompilationJobStatus_
  pStoppingCondition_
  pCreationTime_
  pLastModifiedTime_
  pFailureReason_
  pModelArtifacts_
  pRoleArn_
  pInputConfig_
  pOutputConfig_ =
    DescribeCompilationJobResponse'
      { modelDigests =
          Prelude.Nothing,
        compilationStartTime = Prelude.Nothing,
        compilationEndTime = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        compilationJobName = pCompilationJobName_,
        compilationJobArn = pCompilationJobArn_,
        compilationJobStatus =
          pCompilationJobStatus_,
        stoppingCondition = pStoppingCondition_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
        failureReason = pFailureReason_,
        modelArtifacts = pModelArtifacts_,
        roleArn = pRoleArn_,
        inputConfig = pInputConfig_,
        outputConfig = pOutputConfig_
      }

-- | Provides a BLAKE2 hash value that identifies the compiled model
-- artifacts in Amazon S3.
describeCompilationJobResponse_modelDigests :: Lens.Lens' DescribeCompilationJobResponse (Prelude.Maybe ModelDigests)
describeCompilationJobResponse_modelDigests = Lens.lens (\DescribeCompilationJobResponse' {modelDigests} -> modelDigests) (\s@DescribeCompilationJobResponse' {} a -> s {modelDigests = a} :: DescribeCompilationJobResponse)

-- | The time when the model compilation job started the @CompilationJob@
-- instances.
--
-- You are billed for the time between this timestamp and the timestamp in
-- the DescribeCompilationJobResponse$CompilationEndTime field. In Amazon
-- CloudWatch Logs, the start time might be later than this time. That\'s
-- because it takes time to download the compilation job, which depends on
-- the size of the compilation job container.
describeCompilationJobResponse_compilationStartTime :: Lens.Lens' DescribeCompilationJobResponse (Prelude.Maybe Prelude.UTCTime)
describeCompilationJobResponse_compilationStartTime = Lens.lens (\DescribeCompilationJobResponse' {compilationStartTime} -> compilationStartTime) (\s@DescribeCompilationJobResponse' {} a -> s {compilationStartTime = a} :: DescribeCompilationJobResponse) Prelude.. Lens.mapping Core._Time

-- | The time when the model compilation job on a compilation job instance
-- ended. For a successful or stopped job, this is when the job\'s model
-- artifacts have finished uploading. For a failed job, this is when Amazon
-- SageMaker detected that the job failed.
describeCompilationJobResponse_compilationEndTime :: Lens.Lens' DescribeCompilationJobResponse (Prelude.Maybe Prelude.UTCTime)
describeCompilationJobResponse_compilationEndTime = Lens.lens (\DescribeCompilationJobResponse' {compilationEndTime} -> compilationEndTime) (\s@DescribeCompilationJobResponse' {} a -> s {compilationEndTime = a} :: DescribeCompilationJobResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeCompilationJobResponse_httpStatus :: Lens.Lens' DescribeCompilationJobResponse Prelude.Int
describeCompilationJobResponse_httpStatus = Lens.lens (\DescribeCompilationJobResponse' {httpStatus} -> httpStatus) (\s@DescribeCompilationJobResponse' {} a -> s {httpStatus = a} :: DescribeCompilationJobResponse)

-- | The name of the model compilation job.
describeCompilationJobResponse_compilationJobName :: Lens.Lens' DescribeCompilationJobResponse Prelude.Text
describeCompilationJobResponse_compilationJobName = Lens.lens (\DescribeCompilationJobResponse' {compilationJobName} -> compilationJobName) (\s@DescribeCompilationJobResponse' {} a -> s {compilationJobName = a} :: DescribeCompilationJobResponse)

-- | The Amazon Resource Name (ARN) of the model compilation job.
describeCompilationJobResponse_compilationJobArn :: Lens.Lens' DescribeCompilationJobResponse Prelude.Text
describeCompilationJobResponse_compilationJobArn = Lens.lens (\DescribeCompilationJobResponse' {compilationJobArn} -> compilationJobArn) (\s@DescribeCompilationJobResponse' {} a -> s {compilationJobArn = a} :: DescribeCompilationJobResponse)

-- | The status of the model compilation job.
describeCompilationJobResponse_compilationJobStatus :: Lens.Lens' DescribeCompilationJobResponse CompilationJobStatus
describeCompilationJobResponse_compilationJobStatus = Lens.lens (\DescribeCompilationJobResponse' {compilationJobStatus} -> compilationJobStatus) (\s@DescribeCompilationJobResponse' {} a -> s {compilationJobStatus = a} :: DescribeCompilationJobResponse)

-- | Specifies a limit to how long a model compilation job can run. When the
-- job reaches the time limit, Amazon SageMaker ends the compilation job.
-- Use this API to cap model training costs.
describeCompilationJobResponse_stoppingCondition :: Lens.Lens' DescribeCompilationJobResponse StoppingCondition
describeCompilationJobResponse_stoppingCondition = Lens.lens (\DescribeCompilationJobResponse' {stoppingCondition} -> stoppingCondition) (\s@DescribeCompilationJobResponse' {} a -> s {stoppingCondition = a} :: DescribeCompilationJobResponse)

-- | The time that the model compilation job was created.
describeCompilationJobResponse_creationTime :: Lens.Lens' DescribeCompilationJobResponse Prelude.UTCTime
describeCompilationJobResponse_creationTime = Lens.lens (\DescribeCompilationJobResponse' {creationTime} -> creationTime) (\s@DescribeCompilationJobResponse' {} a -> s {creationTime = a} :: DescribeCompilationJobResponse) Prelude.. Core._Time

-- | The time that the status of the model compilation job was last modified.
describeCompilationJobResponse_lastModifiedTime :: Lens.Lens' DescribeCompilationJobResponse Prelude.UTCTime
describeCompilationJobResponse_lastModifiedTime = Lens.lens (\DescribeCompilationJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeCompilationJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeCompilationJobResponse) Prelude.. Core._Time

-- | If a model compilation job failed, the reason it failed.
describeCompilationJobResponse_failureReason :: Lens.Lens' DescribeCompilationJobResponse Prelude.Text
describeCompilationJobResponse_failureReason = Lens.lens (\DescribeCompilationJobResponse' {failureReason} -> failureReason) (\s@DescribeCompilationJobResponse' {} a -> s {failureReason = a} :: DescribeCompilationJobResponse)

-- | Information about the location in Amazon S3 that has been configured for
-- storing the model artifacts used in the compilation job.
describeCompilationJobResponse_modelArtifacts :: Lens.Lens' DescribeCompilationJobResponse ModelArtifacts
describeCompilationJobResponse_modelArtifacts = Lens.lens (\DescribeCompilationJobResponse' {modelArtifacts} -> modelArtifacts) (\s@DescribeCompilationJobResponse' {} a -> s {modelArtifacts = a} :: DescribeCompilationJobResponse)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker
-- assumes to perform the model compilation job.
describeCompilationJobResponse_roleArn :: Lens.Lens' DescribeCompilationJobResponse Prelude.Text
describeCompilationJobResponse_roleArn = Lens.lens (\DescribeCompilationJobResponse' {roleArn} -> roleArn) (\s@DescribeCompilationJobResponse' {} a -> s {roleArn = a} :: DescribeCompilationJobResponse)

-- | Information about the location in Amazon S3 of the input model
-- artifacts, the name and shape of the expected data inputs, and the
-- framework in which the model was trained.
describeCompilationJobResponse_inputConfig :: Lens.Lens' DescribeCompilationJobResponse InputConfig
describeCompilationJobResponse_inputConfig = Lens.lens (\DescribeCompilationJobResponse' {inputConfig} -> inputConfig) (\s@DescribeCompilationJobResponse' {} a -> s {inputConfig = a} :: DescribeCompilationJobResponse)

-- | Information about the output location for the compiled model and the
-- target device that the model runs on.
describeCompilationJobResponse_outputConfig :: Lens.Lens' DescribeCompilationJobResponse OutputConfig
describeCompilationJobResponse_outputConfig = Lens.lens (\DescribeCompilationJobResponse' {outputConfig} -> outputConfig) (\s@DescribeCompilationJobResponse' {} a -> s {outputConfig = a} :: DescribeCompilationJobResponse)

instance
  Prelude.NFData
    DescribeCompilationJobResponse
