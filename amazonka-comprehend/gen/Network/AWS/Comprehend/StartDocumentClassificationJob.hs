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
-- Module      : Network.AWS.Comprehend.StartDocumentClassificationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous document classification job. Use the operation to
-- track the progress of the job.
module Network.AWS.Comprehend.StartDocumentClassificationJob
  ( -- * Creating a Request
    StartDocumentClassificationJob (..),
    newStartDocumentClassificationJob,

    -- * Request Lenses
    startDocumentClassificationJob_vpcConfig,
    startDocumentClassificationJob_volumeKmsKeyId,
    startDocumentClassificationJob_clientRequestToken,
    startDocumentClassificationJob_jobName,
    startDocumentClassificationJob_documentClassifierArn,
    startDocumentClassificationJob_inputDataConfig,
    startDocumentClassificationJob_outputDataConfig,
    startDocumentClassificationJob_dataAccessRoleArn,

    -- * Destructuring the Response
    StartDocumentClassificationJobResponse (..),
    newStartDocumentClassificationJobResponse,

    -- * Response Lenses
    startDocumentClassificationJobResponse_jobStatus,
    startDocumentClassificationJobResponse_jobId,
    startDocumentClassificationJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartDocumentClassificationJob' smart constructor.
data StartDocumentClassificationJob = StartDocumentClassificationJob'
  { -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your document
    -- classification job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that process the analysis job. The VolumeKmsKeyId can be
    -- either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Core.Maybe Core.Text,
    -- | A unique identifier for the request. If you do not set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The identifier of the job.
    jobName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the document classifier to use to
    -- process the job.
    documentClassifierArn :: Core.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartDocumentClassificationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'startDocumentClassificationJob_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your document
-- classification job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'volumeKmsKeyId', 'startDocumentClassificationJob_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'clientRequestToken', 'startDocumentClassificationJob_clientRequestToken' - A unique identifier for the request. If you do not set the client
-- request token, Amazon Comprehend generates one.
--
-- 'jobName', 'startDocumentClassificationJob_jobName' - The identifier of the job.
--
-- 'documentClassifierArn', 'startDocumentClassificationJob_documentClassifierArn' - The Amazon Resource Name (ARN) of the document classifier to use to
-- process the job.
--
-- 'inputDataConfig', 'startDocumentClassificationJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startDocumentClassificationJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startDocumentClassificationJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
newStartDocumentClassificationJob ::
  -- | 'documentClassifierArn'
  Core.Text ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Core.Text ->
  StartDocumentClassificationJob
newStartDocumentClassificationJob
  pDocumentClassifierArn_
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_ =
    StartDocumentClassificationJob'
      { vpcConfig =
          Core.Nothing,
        volumeKmsKeyId = Core.Nothing,
        clientRequestToken = Core.Nothing,
        jobName = Core.Nothing,
        documentClassifierArn =
          pDocumentClassifierArn_,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_
      }

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your document
-- classification job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
startDocumentClassificationJob_vpcConfig :: Lens.Lens' StartDocumentClassificationJob (Core.Maybe VpcConfig)
startDocumentClassificationJob_vpcConfig = Lens.lens (\StartDocumentClassificationJob' {vpcConfig} -> vpcConfig) (\s@StartDocumentClassificationJob' {} a -> s {vpcConfig = a} :: StartDocumentClassificationJob)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
startDocumentClassificationJob_volumeKmsKeyId :: Lens.Lens' StartDocumentClassificationJob (Core.Maybe Core.Text)
startDocumentClassificationJob_volumeKmsKeyId = Lens.lens (\StartDocumentClassificationJob' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@StartDocumentClassificationJob' {} a -> s {volumeKmsKeyId = a} :: StartDocumentClassificationJob)

-- | A unique identifier for the request. If you do not set the client
-- request token, Amazon Comprehend generates one.
startDocumentClassificationJob_clientRequestToken :: Lens.Lens' StartDocumentClassificationJob (Core.Maybe Core.Text)
startDocumentClassificationJob_clientRequestToken = Lens.lens (\StartDocumentClassificationJob' {clientRequestToken} -> clientRequestToken) (\s@StartDocumentClassificationJob' {} a -> s {clientRequestToken = a} :: StartDocumentClassificationJob)

-- | The identifier of the job.
startDocumentClassificationJob_jobName :: Lens.Lens' StartDocumentClassificationJob (Core.Maybe Core.Text)
startDocumentClassificationJob_jobName = Lens.lens (\StartDocumentClassificationJob' {jobName} -> jobName) (\s@StartDocumentClassificationJob' {} a -> s {jobName = a} :: StartDocumentClassificationJob)

-- | The Amazon Resource Name (ARN) of the document classifier to use to
-- process the job.
startDocumentClassificationJob_documentClassifierArn :: Lens.Lens' StartDocumentClassificationJob Core.Text
startDocumentClassificationJob_documentClassifierArn = Lens.lens (\StartDocumentClassificationJob' {documentClassifierArn} -> documentClassifierArn) (\s@StartDocumentClassificationJob' {} a -> s {documentClassifierArn = a} :: StartDocumentClassificationJob)

-- | Specifies the format and location of the input data for the job.
startDocumentClassificationJob_inputDataConfig :: Lens.Lens' StartDocumentClassificationJob InputDataConfig
startDocumentClassificationJob_inputDataConfig = Lens.lens (\StartDocumentClassificationJob' {inputDataConfig} -> inputDataConfig) (\s@StartDocumentClassificationJob' {} a -> s {inputDataConfig = a} :: StartDocumentClassificationJob)

-- | Specifies where to send the output files.
startDocumentClassificationJob_outputDataConfig :: Lens.Lens' StartDocumentClassificationJob OutputDataConfig
startDocumentClassificationJob_outputDataConfig = Lens.lens (\StartDocumentClassificationJob' {outputDataConfig} -> outputDataConfig) (\s@StartDocumentClassificationJob' {} a -> s {outputDataConfig = a} :: StartDocumentClassificationJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
startDocumentClassificationJob_dataAccessRoleArn :: Lens.Lens' StartDocumentClassificationJob Core.Text
startDocumentClassificationJob_dataAccessRoleArn = Lens.lens (\StartDocumentClassificationJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartDocumentClassificationJob' {} a -> s {dataAccessRoleArn = a} :: StartDocumentClassificationJob)

instance
  Core.AWSRequest
    StartDocumentClassificationJob
  where
  type
    AWSResponse StartDocumentClassificationJob =
      StartDocumentClassificationJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDocumentClassificationJobResponse'
            Core.<$> (x Core..?> "JobStatus")
            Core.<*> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartDocumentClassificationJob

instance Core.NFData StartDocumentClassificationJob

instance
  Core.ToHeaders
    StartDocumentClassificationJob
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StartDocumentClassificationJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartDocumentClassificationJob where
  toJSON StartDocumentClassificationJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VpcConfig" Core..=) Core.<$> vpcConfig,
            ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("JobName" Core..=) Core.<$> jobName,
            Core.Just
              ( "DocumentClassifierArn"
                  Core..= documentClassifierArn
              ),
            Core.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Core.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn)
          ]
      )

instance Core.ToPath StartDocumentClassificationJob where
  toPath = Core.const "/"

instance Core.ToQuery StartDocumentClassificationJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartDocumentClassificationJobResponse' smart constructor.
data StartDocumentClassificationJobResponse = StartDocumentClassificationJobResponse'
  { -- | The status of the job:
    --
    -- -   SUBMITTED - The job has been received and queued for processing.
    --
    -- -   IN_PROGRESS - Amazon Comprehend is processing the job.
    --
    -- -   COMPLETED - The job was successfully completed and the output is
    --     available.
    --
    -- -   FAILED - The job did not complete. For details, use the operation.
    --
    -- -   STOP_REQUESTED - Amazon Comprehend has received a stop request for
    --     the job and is processing the request.
    --
    -- -   STOPPED - The job was successfully stopped without completing.
    jobStatus :: Core.Maybe JobStatus,
    -- | The identifier generated for the job. To get the status of the job, use
    -- this identifier with the operation.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartDocumentClassificationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'startDocumentClassificationJobResponse_jobStatus' - The status of the job:
--
-- -   SUBMITTED - The job has been received and queued for processing.
--
-- -   IN_PROGRESS - Amazon Comprehend is processing the job.
--
-- -   COMPLETED - The job was successfully completed and the output is
--     available.
--
-- -   FAILED - The job did not complete. For details, use the operation.
--
-- -   STOP_REQUESTED - Amazon Comprehend has received a stop request for
--     the job and is processing the request.
--
-- -   STOPPED - The job was successfully stopped without completing.
--
-- 'jobId', 'startDocumentClassificationJobResponse_jobId' - The identifier generated for the job. To get the status of the job, use
-- this identifier with the operation.
--
-- 'httpStatus', 'startDocumentClassificationJobResponse_httpStatus' - The response's http status code.
newStartDocumentClassificationJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartDocumentClassificationJobResponse
newStartDocumentClassificationJobResponse
  pHttpStatus_ =
    StartDocumentClassificationJobResponse'
      { jobStatus =
          Core.Nothing,
        jobId = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the job:
--
-- -   SUBMITTED - The job has been received and queued for processing.
--
-- -   IN_PROGRESS - Amazon Comprehend is processing the job.
--
-- -   COMPLETED - The job was successfully completed and the output is
--     available.
--
-- -   FAILED - The job did not complete. For details, use the operation.
--
-- -   STOP_REQUESTED - Amazon Comprehend has received a stop request for
--     the job and is processing the request.
--
-- -   STOPPED - The job was successfully stopped without completing.
startDocumentClassificationJobResponse_jobStatus :: Lens.Lens' StartDocumentClassificationJobResponse (Core.Maybe JobStatus)
startDocumentClassificationJobResponse_jobStatus = Lens.lens (\StartDocumentClassificationJobResponse' {jobStatus} -> jobStatus) (\s@StartDocumentClassificationJobResponse' {} a -> s {jobStatus = a} :: StartDocumentClassificationJobResponse)

-- | The identifier generated for the job. To get the status of the job, use
-- this identifier with the operation.
startDocumentClassificationJobResponse_jobId :: Lens.Lens' StartDocumentClassificationJobResponse (Core.Maybe Core.Text)
startDocumentClassificationJobResponse_jobId = Lens.lens (\StartDocumentClassificationJobResponse' {jobId} -> jobId) (\s@StartDocumentClassificationJobResponse' {} a -> s {jobId = a} :: StartDocumentClassificationJobResponse)

-- | The response's http status code.
startDocumentClassificationJobResponse_httpStatus :: Lens.Lens' StartDocumentClassificationJobResponse Core.Int
startDocumentClassificationJobResponse_httpStatus = Lens.lens (\StartDocumentClassificationJobResponse' {httpStatus} -> httpStatus) (\s@StartDocumentClassificationJobResponse' {} a -> s {httpStatus = a} :: StartDocumentClassificationJobResponse)

instance
  Core.NFData
    StartDocumentClassificationJobResponse
