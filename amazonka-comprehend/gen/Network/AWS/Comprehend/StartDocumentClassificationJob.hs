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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartDocumentClassificationJob' smart constructor.
data StartDocumentClassificationJob = StartDocumentClassificationJob'
  { -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your document
    -- classification job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that process the analysis job. The VolumeKmsKeyId can be
    -- either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the request. If you do not set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the document classifier to use to
    -- process the job.
    documentClassifierArn :: Prelude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  StartDocumentClassificationJob
newStartDocumentClassificationJob
  pDocumentClassifierArn_
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_ =
    StartDocumentClassificationJob'
      { vpcConfig =
          Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        jobName = Prelude.Nothing,
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
startDocumentClassificationJob_vpcConfig :: Lens.Lens' StartDocumentClassificationJob (Prelude.Maybe VpcConfig)
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
startDocumentClassificationJob_volumeKmsKeyId :: Lens.Lens' StartDocumentClassificationJob (Prelude.Maybe Prelude.Text)
startDocumentClassificationJob_volumeKmsKeyId = Lens.lens (\StartDocumentClassificationJob' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@StartDocumentClassificationJob' {} a -> s {volumeKmsKeyId = a} :: StartDocumentClassificationJob)

-- | A unique identifier for the request. If you do not set the client
-- request token, Amazon Comprehend generates one.
startDocumentClassificationJob_clientRequestToken :: Lens.Lens' StartDocumentClassificationJob (Prelude.Maybe Prelude.Text)
startDocumentClassificationJob_clientRequestToken = Lens.lens (\StartDocumentClassificationJob' {clientRequestToken} -> clientRequestToken) (\s@StartDocumentClassificationJob' {} a -> s {clientRequestToken = a} :: StartDocumentClassificationJob)

-- | The identifier of the job.
startDocumentClassificationJob_jobName :: Lens.Lens' StartDocumentClassificationJob (Prelude.Maybe Prelude.Text)
startDocumentClassificationJob_jobName = Lens.lens (\StartDocumentClassificationJob' {jobName} -> jobName) (\s@StartDocumentClassificationJob' {} a -> s {jobName = a} :: StartDocumentClassificationJob)

-- | The Amazon Resource Name (ARN) of the document classifier to use to
-- process the job.
startDocumentClassificationJob_documentClassifierArn :: Lens.Lens' StartDocumentClassificationJob Prelude.Text
startDocumentClassificationJob_documentClassifierArn = Lens.lens (\StartDocumentClassificationJob' {documentClassifierArn} -> documentClassifierArn) (\s@StartDocumentClassificationJob' {} a -> s {documentClassifierArn = a} :: StartDocumentClassificationJob)

-- | Specifies the format and location of the input data for the job.
startDocumentClassificationJob_inputDataConfig :: Lens.Lens' StartDocumentClassificationJob InputDataConfig
startDocumentClassificationJob_inputDataConfig = Lens.lens (\StartDocumentClassificationJob' {inputDataConfig} -> inputDataConfig) (\s@StartDocumentClassificationJob' {} a -> s {inputDataConfig = a} :: StartDocumentClassificationJob)

-- | Specifies where to send the output files.
startDocumentClassificationJob_outputDataConfig :: Lens.Lens' StartDocumentClassificationJob OutputDataConfig
startDocumentClassificationJob_outputDataConfig = Lens.lens (\StartDocumentClassificationJob' {outputDataConfig} -> outputDataConfig) (\s@StartDocumentClassificationJob' {} a -> s {outputDataConfig = a} :: StartDocumentClassificationJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
startDocumentClassificationJob_dataAccessRoleArn :: Lens.Lens' StartDocumentClassificationJob Prelude.Text
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
            Prelude.<$> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartDocumentClassificationJob

instance
  Prelude.NFData
    StartDocumentClassificationJob

instance
  Core.ToHeaders
    StartDocumentClassificationJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StartDocumentClassificationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartDocumentClassificationJob where
  toJSON StartDocumentClassificationJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("JobName" Core..=) Prelude.<$> jobName,
            Prelude.Just
              ( "DocumentClassifierArn"
                  Core..= documentClassifierArn
              ),
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn)
          ]
      )

instance Core.ToPath StartDocumentClassificationJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartDocumentClassificationJob where
  toQuery = Prelude.const Prelude.mempty

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
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier generated for the job. To get the status of the job, use
    -- this identifier with the operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartDocumentClassificationJobResponse
newStartDocumentClassificationJobResponse
  pHttpStatus_ =
    StartDocumentClassificationJobResponse'
      { jobStatus =
          Prelude.Nothing,
        jobId = Prelude.Nothing,
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
startDocumentClassificationJobResponse_jobStatus :: Lens.Lens' StartDocumentClassificationJobResponse (Prelude.Maybe JobStatus)
startDocumentClassificationJobResponse_jobStatus = Lens.lens (\StartDocumentClassificationJobResponse' {jobStatus} -> jobStatus) (\s@StartDocumentClassificationJobResponse' {} a -> s {jobStatus = a} :: StartDocumentClassificationJobResponse)

-- | The identifier generated for the job. To get the status of the job, use
-- this identifier with the operation.
startDocumentClassificationJobResponse_jobId :: Lens.Lens' StartDocumentClassificationJobResponse (Prelude.Maybe Prelude.Text)
startDocumentClassificationJobResponse_jobId = Lens.lens (\StartDocumentClassificationJobResponse' {jobId} -> jobId) (\s@StartDocumentClassificationJobResponse' {} a -> s {jobId = a} :: StartDocumentClassificationJobResponse)

-- | The response's http status code.
startDocumentClassificationJobResponse_httpStatus :: Lens.Lens' StartDocumentClassificationJobResponse Prelude.Int
startDocumentClassificationJobResponse_httpStatus = Lens.lens (\StartDocumentClassificationJobResponse' {httpStatus} -> httpStatus) (\s@StartDocumentClassificationJobResponse' {} a -> s {httpStatus = a} :: StartDocumentClassificationJobResponse)

instance
  Prelude.NFData
    StartDocumentClassificationJobResponse
