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
-- Module      : Amazonka.SageMaker.DescribeEdgePackagingJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A description of edge packaging jobs.
module Amazonka.SageMaker.DescribeEdgePackagingJob
  ( -- * Creating a Request
    DescribeEdgePackagingJob (..),
    newDescribeEdgePackagingJob,

    -- * Request Lenses
    describeEdgePackagingJob_edgePackagingJobName,

    -- * Destructuring the Response
    DescribeEdgePackagingJobResponse (..),
    newDescribeEdgePackagingJobResponse,

    -- * Response Lenses
    describeEdgePackagingJobResponse_compilationJobName,
    describeEdgePackagingJobResponse_creationTime,
    describeEdgePackagingJobResponse_edgePackagingJobStatusMessage,
    describeEdgePackagingJobResponse_lastModifiedTime,
    describeEdgePackagingJobResponse_modelArtifact,
    describeEdgePackagingJobResponse_modelName,
    describeEdgePackagingJobResponse_modelSignature,
    describeEdgePackagingJobResponse_modelVersion,
    describeEdgePackagingJobResponse_outputConfig,
    describeEdgePackagingJobResponse_presetDeploymentOutput,
    describeEdgePackagingJobResponse_resourceKey,
    describeEdgePackagingJobResponse_roleArn,
    describeEdgePackagingJobResponse_httpStatus,
    describeEdgePackagingJobResponse_edgePackagingJobArn,
    describeEdgePackagingJobResponse_edgePackagingJobName,
    describeEdgePackagingJobResponse_edgePackagingJobStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeEdgePackagingJob' smart constructor.
data DescribeEdgePackagingJob = DescribeEdgePackagingJob'
  { -- | The name of the edge packaging job.
    edgePackagingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEdgePackagingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgePackagingJobName', 'describeEdgePackagingJob_edgePackagingJobName' - The name of the edge packaging job.
newDescribeEdgePackagingJob ::
  -- | 'edgePackagingJobName'
  Prelude.Text ->
  DescribeEdgePackagingJob
newDescribeEdgePackagingJob pEdgePackagingJobName_ =
  DescribeEdgePackagingJob'
    { edgePackagingJobName =
        pEdgePackagingJobName_
    }

-- | The name of the edge packaging job.
describeEdgePackagingJob_edgePackagingJobName :: Lens.Lens' DescribeEdgePackagingJob Prelude.Text
describeEdgePackagingJob_edgePackagingJobName = Lens.lens (\DescribeEdgePackagingJob' {edgePackagingJobName} -> edgePackagingJobName) (\s@DescribeEdgePackagingJob' {} a -> s {edgePackagingJobName = a} :: DescribeEdgePackagingJob)

instance Core.AWSRequest DescribeEdgePackagingJob where
  type
    AWSResponse DescribeEdgePackagingJob =
      DescribeEdgePackagingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEdgePackagingJobResponse'
            Prelude.<$> (x Data..?> "CompilationJobName")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "EdgePackagingJobStatusMessage")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "ModelArtifact")
            Prelude.<*> (x Data..?> "ModelName")
            Prelude.<*> (x Data..?> "ModelSignature")
            Prelude.<*> (x Data..?> "ModelVersion")
            Prelude.<*> (x Data..?> "OutputConfig")
            Prelude.<*> (x Data..?> "PresetDeploymentOutput")
            Prelude.<*> (x Data..?> "ResourceKey")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EdgePackagingJobArn")
            Prelude.<*> (x Data..:> "EdgePackagingJobName")
            Prelude.<*> (x Data..:> "EdgePackagingJobStatus")
      )

instance Prelude.Hashable DescribeEdgePackagingJob where
  hashWithSalt _salt DescribeEdgePackagingJob' {..} =
    _salt `Prelude.hashWithSalt` edgePackagingJobName

instance Prelude.NFData DescribeEdgePackagingJob where
  rnf DescribeEdgePackagingJob' {..} =
    Prelude.rnf edgePackagingJobName

instance Data.ToHeaders DescribeEdgePackagingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeEdgePackagingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEdgePackagingJob where
  toJSON DescribeEdgePackagingJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EdgePackagingJobName"
                  Data..= edgePackagingJobName
              )
          ]
      )

instance Data.ToPath DescribeEdgePackagingJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEdgePackagingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEdgePackagingJobResponse' smart constructor.
data DescribeEdgePackagingJobResponse = DescribeEdgePackagingJobResponse'
  { -- | The name of the SageMaker Neo compilation job that is used to locate
    -- model artifacts that are being packaged.
    compilationJobName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the packaging job was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Returns a message describing the job status and error messages.
    edgePackagingJobStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the job was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Simple Storage (S3) URI where model artifacts ares stored.
    modelArtifact :: Prelude.Maybe Prelude.Text,
    -- | The name of the model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The signature document of files in the model artifact.
    modelSignature :: Prelude.Maybe Prelude.Text,
    -- | The version of the model.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The output configuration for the edge packaging job.
    outputConfig :: Prelude.Maybe EdgeOutputConfig,
    -- | The output of a SageMaker Edge Manager deployable resource.
    presetDeploymentOutput :: Prelude.Maybe EdgePresetDeploymentOutput,
    -- | The Amazon Web Services KMS key to use when encrypting the EBS volume
    -- the job run on.
    resourceKey :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
    -- SageMaker to download and upload the model, and to contact Neo.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the edge packaging job.
    edgePackagingJobArn :: Prelude.Text,
    -- | The name of the edge packaging job.
    edgePackagingJobName :: Prelude.Text,
    -- | The current status of the packaging job.
    edgePackagingJobStatus :: EdgePackagingJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEdgePackagingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilationJobName', 'describeEdgePackagingJobResponse_compilationJobName' - The name of the SageMaker Neo compilation job that is used to locate
-- model artifacts that are being packaged.
--
-- 'creationTime', 'describeEdgePackagingJobResponse_creationTime' - The timestamp of when the packaging job was created.
--
-- 'edgePackagingJobStatusMessage', 'describeEdgePackagingJobResponse_edgePackagingJobStatusMessage' - Returns a message describing the job status and error messages.
--
-- 'lastModifiedTime', 'describeEdgePackagingJobResponse_lastModifiedTime' - The timestamp of when the job was last updated.
--
-- 'modelArtifact', 'describeEdgePackagingJobResponse_modelArtifact' - The Amazon Simple Storage (S3) URI where model artifacts ares stored.
--
-- 'modelName', 'describeEdgePackagingJobResponse_modelName' - The name of the model.
--
-- 'modelSignature', 'describeEdgePackagingJobResponse_modelSignature' - The signature document of files in the model artifact.
--
-- 'modelVersion', 'describeEdgePackagingJobResponse_modelVersion' - The version of the model.
--
-- 'outputConfig', 'describeEdgePackagingJobResponse_outputConfig' - The output configuration for the edge packaging job.
--
-- 'presetDeploymentOutput', 'describeEdgePackagingJobResponse_presetDeploymentOutput' - The output of a SageMaker Edge Manager deployable resource.
--
-- 'resourceKey', 'describeEdgePackagingJobResponse_resourceKey' - The Amazon Web Services KMS key to use when encrypting the EBS volume
-- the job run on.
--
-- 'roleArn', 'describeEdgePackagingJobResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to download and upload the model, and to contact Neo.
--
-- 'httpStatus', 'describeEdgePackagingJobResponse_httpStatus' - The response's http status code.
--
-- 'edgePackagingJobArn', 'describeEdgePackagingJobResponse_edgePackagingJobArn' - The Amazon Resource Name (ARN) of the edge packaging job.
--
-- 'edgePackagingJobName', 'describeEdgePackagingJobResponse_edgePackagingJobName' - The name of the edge packaging job.
--
-- 'edgePackagingJobStatus', 'describeEdgePackagingJobResponse_edgePackagingJobStatus' - The current status of the packaging job.
newDescribeEdgePackagingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'edgePackagingJobArn'
  Prelude.Text ->
  -- | 'edgePackagingJobName'
  Prelude.Text ->
  -- | 'edgePackagingJobStatus'
  EdgePackagingJobStatus ->
  DescribeEdgePackagingJobResponse
newDescribeEdgePackagingJobResponse
  pHttpStatus_
  pEdgePackagingJobArn_
  pEdgePackagingJobName_
  pEdgePackagingJobStatus_ =
    DescribeEdgePackagingJobResponse'
      { compilationJobName =
          Prelude.Nothing,
        creationTime = Prelude.Nothing,
        edgePackagingJobStatusMessage =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        modelArtifact = Prelude.Nothing,
        modelName = Prelude.Nothing,
        modelSignature = Prelude.Nothing,
        modelVersion = Prelude.Nothing,
        outputConfig = Prelude.Nothing,
        presetDeploymentOutput = Prelude.Nothing,
        resourceKey = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        edgePackagingJobArn =
          pEdgePackagingJobArn_,
        edgePackagingJobName =
          pEdgePackagingJobName_,
        edgePackagingJobStatus =
          pEdgePackagingJobStatus_
      }

-- | The name of the SageMaker Neo compilation job that is used to locate
-- model artifacts that are being packaged.
describeEdgePackagingJobResponse_compilationJobName :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.Text)
describeEdgePackagingJobResponse_compilationJobName = Lens.lens (\DescribeEdgePackagingJobResponse' {compilationJobName} -> compilationJobName) (\s@DescribeEdgePackagingJobResponse' {} a -> s {compilationJobName = a} :: DescribeEdgePackagingJobResponse)

-- | The timestamp of when the packaging job was created.
describeEdgePackagingJobResponse_creationTime :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeEdgePackagingJobResponse_creationTime = Lens.lens (\DescribeEdgePackagingJobResponse' {creationTime} -> creationTime) (\s@DescribeEdgePackagingJobResponse' {} a -> s {creationTime = a} :: DescribeEdgePackagingJobResponse) Prelude.. Lens.mapping Data._Time

-- | Returns a message describing the job status and error messages.
describeEdgePackagingJobResponse_edgePackagingJobStatusMessage :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.Text)
describeEdgePackagingJobResponse_edgePackagingJobStatusMessage = Lens.lens (\DescribeEdgePackagingJobResponse' {edgePackagingJobStatusMessage} -> edgePackagingJobStatusMessage) (\s@DescribeEdgePackagingJobResponse' {} a -> s {edgePackagingJobStatusMessage = a} :: DescribeEdgePackagingJobResponse)

-- | The timestamp of when the job was last updated.
describeEdgePackagingJobResponse_lastModifiedTime :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.UTCTime)
describeEdgePackagingJobResponse_lastModifiedTime = Lens.lens (\DescribeEdgePackagingJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeEdgePackagingJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeEdgePackagingJobResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Simple Storage (S3) URI where model artifacts ares stored.
describeEdgePackagingJobResponse_modelArtifact :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.Text)
describeEdgePackagingJobResponse_modelArtifact = Lens.lens (\DescribeEdgePackagingJobResponse' {modelArtifact} -> modelArtifact) (\s@DescribeEdgePackagingJobResponse' {} a -> s {modelArtifact = a} :: DescribeEdgePackagingJobResponse)

-- | The name of the model.
describeEdgePackagingJobResponse_modelName :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.Text)
describeEdgePackagingJobResponse_modelName = Lens.lens (\DescribeEdgePackagingJobResponse' {modelName} -> modelName) (\s@DescribeEdgePackagingJobResponse' {} a -> s {modelName = a} :: DescribeEdgePackagingJobResponse)

-- | The signature document of files in the model artifact.
describeEdgePackagingJobResponse_modelSignature :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.Text)
describeEdgePackagingJobResponse_modelSignature = Lens.lens (\DescribeEdgePackagingJobResponse' {modelSignature} -> modelSignature) (\s@DescribeEdgePackagingJobResponse' {} a -> s {modelSignature = a} :: DescribeEdgePackagingJobResponse)

-- | The version of the model.
describeEdgePackagingJobResponse_modelVersion :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.Text)
describeEdgePackagingJobResponse_modelVersion = Lens.lens (\DescribeEdgePackagingJobResponse' {modelVersion} -> modelVersion) (\s@DescribeEdgePackagingJobResponse' {} a -> s {modelVersion = a} :: DescribeEdgePackagingJobResponse)

-- | The output configuration for the edge packaging job.
describeEdgePackagingJobResponse_outputConfig :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe EdgeOutputConfig)
describeEdgePackagingJobResponse_outputConfig = Lens.lens (\DescribeEdgePackagingJobResponse' {outputConfig} -> outputConfig) (\s@DescribeEdgePackagingJobResponse' {} a -> s {outputConfig = a} :: DescribeEdgePackagingJobResponse)

-- | The output of a SageMaker Edge Manager deployable resource.
describeEdgePackagingJobResponse_presetDeploymentOutput :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe EdgePresetDeploymentOutput)
describeEdgePackagingJobResponse_presetDeploymentOutput = Lens.lens (\DescribeEdgePackagingJobResponse' {presetDeploymentOutput} -> presetDeploymentOutput) (\s@DescribeEdgePackagingJobResponse' {} a -> s {presetDeploymentOutput = a} :: DescribeEdgePackagingJobResponse)

-- | The Amazon Web Services KMS key to use when encrypting the EBS volume
-- the job run on.
describeEdgePackagingJobResponse_resourceKey :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.Text)
describeEdgePackagingJobResponse_resourceKey = Lens.lens (\DescribeEdgePackagingJobResponse' {resourceKey} -> resourceKey) (\s@DescribeEdgePackagingJobResponse' {} a -> s {resourceKey = a} :: DescribeEdgePackagingJobResponse)

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to download and upload the model, and to contact Neo.
describeEdgePackagingJobResponse_roleArn :: Lens.Lens' DescribeEdgePackagingJobResponse (Prelude.Maybe Prelude.Text)
describeEdgePackagingJobResponse_roleArn = Lens.lens (\DescribeEdgePackagingJobResponse' {roleArn} -> roleArn) (\s@DescribeEdgePackagingJobResponse' {} a -> s {roleArn = a} :: DescribeEdgePackagingJobResponse)

-- | The response's http status code.
describeEdgePackagingJobResponse_httpStatus :: Lens.Lens' DescribeEdgePackagingJobResponse Prelude.Int
describeEdgePackagingJobResponse_httpStatus = Lens.lens (\DescribeEdgePackagingJobResponse' {httpStatus} -> httpStatus) (\s@DescribeEdgePackagingJobResponse' {} a -> s {httpStatus = a} :: DescribeEdgePackagingJobResponse)

-- | The Amazon Resource Name (ARN) of the edge packaging job.
describeEdgePackagingJobResponse_edgePackagingJobArn :: Lens.Lens' DescribeEdgePackagingJobResponse Prelude.Text
describeEdgePackagingJobResponse_edgePackagingJobArn = Lens.lens (\DescribeEdgePackagingJobResponse' {edgePackagingJobArn} -> edgePackagingJobArn) (\s@DescribeEdgePackagingJobResponse' {} a -> s {edgePackagingJobArn = a} :: DescribeEdgePackagingJobResponse)

-- | The name of the edge packaging job.
describeEdgePackagingJobResponse_edgePackagingJobName :: Lens.Lens' DescribeEdgePackagingJobResponse Prelude.Text
describeEdgePackagingJobResponse_edgePackagingJobName = Lens.lens (\DescribeEdgePackagingJobResponse' {edgePackagingJobName} -> edgePackagingJobName) (\s@DescribeEdgePackagingJobResponse' {} a -> s {edgePackagingJobName = a} :: DescribeEdgePackagingJobResponse)

-- | The current status of the packaging job.
describeEdgePackagingJobResponse_edgePackagingJobStatus :: Lens.Lens' DescribeEdgePackagingJobResponse EdgePackagingJobStatus
describeEdgePackagingJobResponse_edgePackagingJobStatus = Lens.lens (\DescribeEdgePackagingJobResponse' {edgePackagingJobStatus} -> edgePackagingJobStatus) (\s@DescribeEdgePackagingJobResponse' {} a -> s {edgePackagingJobStatus = a} :: DescribeEdgePackagingJobResponse)

instance
  Prelude.NFData
    DescribeEdgePackagingJobResponse
  where
  rnf DescribeEdgePackagingJobResponse' {..} =
    Prelude.rnf compilationJobName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf edgePackagingJobStatusMessage
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf modelArtifact
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf modelSignature
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf presetDeploymentOutput
      `Prelude.seq` Prelude.rnf resourceKey
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf edgePackagingJobArn
      `Prelude.seq` Prelude.rnf edgePackagingJobName
      `Prelude.seq` Prelude.rnf edgePackagingJobStatus
