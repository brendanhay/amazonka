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
-- Module      : Amazonka.Comprehend.StartDocumentClassificationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous document classification job. Use the operation to
-- track the progress of the job.
module Amazonka.Comprehend.StartDocumentClassificationJob
  ( -- * Creating a Request
    StartDocumentClassificationJob (..),
    newStartDocumentClassificationJob,

    -- * Request Lenses
    startDocumentClassificationJob_jobName,
    startDocumentClassificationJob_vpcConfig,
    startDocumentClassificationJob_volumeKmsKeyId,
    startDocumentClassificationJob_clientRequestToken,
    startDocumentClassificationJob_tags,
    startDocumentClassificationJob_documentClassifierArn,
    startDocumentClassificationJob_inputDataConfig,
    startDocumentClassificationJob_outputDataConfig,
    startDocumentClassificationJob_dataAccessRoleArn,

    -- * Destructuring the Response
    StartDocumentClassificationJobResponse (..),
    newStartDocumentClassificationJobResponse,

    -- * Response Lenses
    startDocumentClassificationJobResponse_jobId,
    startDocumentClassificationJobResponse_jobArn,
    startDocumentClassificationJobResponse_jobStatus,
    startDocumentClassificationJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDocumentClassificationJob' smart constructor.
data StartDocumentClassificationJob = StartDocumentClassificationJob'
  { -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Configuration parameters for an optional private Virtual Private Cloud
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
    -- | Tags to be associated with the document classification job. A tag is a
    -- key-value pair that adds metadata to a resource used by Amazon
    -- Comprehend. For example, a tag with \"Sales\" as the key might be added
    -- to a resource to indicate its use by the sales department.
    tags :: Prelude.Maybe [Tag],
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
-- 'jobName', 'startDocumentClassificationJob_jobName' - The identifier of the job.
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
-- 'tags', 'startDocumentClassificationJob_tags' - Tags to be associated with the document classification job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
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
      { jobName =
          Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        tags = Prelude.Nothing,
        documentClassifierArn =
          pDocumentClassifierArn_,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_
      }

-- | The identifier of the job.
startDocumentClassificationJob_jobName :: Lens.Lens' StartDocumentClassificationJob (Prelude.Maybe Prelude.Text)
startDocumentClassificationJob_jobName = Lens.lens (\StartDocumentClassificationJob' {jobName} -> jobName) (\s@StartDocumentClassificationJob' {} a -> s {jobName = a} :: StartDocumentClassificationJob)

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

-- | Tags to be associated with the document classification job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
startDocumentClassificationJob_tags :: Lens.Lens' StartDocumentClassificationJob (Prelude.Maybe [Tag])
startDocumentClassificationJob_tags = Lens.lens (\StartDocumentClassificationJob' {tags} -> tags) (\s@StartDocumentClassificationJob' {} a -> s {tags = a} :: StartDocumentClassificationJob) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Core..?> "JobId")
            Prelude.<*> (x Core..?> "JobArn")
            Prelude.<*> (x Core..?> "JobStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartDocumentClassificationJob
  where
  hashWithSalt
    salt'
    StartDocumentClassificationJob' {..} =
      salt' `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` documentClassifierArn
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` volumeKmsKeyId
        `Prelude.hashWithSalt` vpcConfig
        `Prelude.hashWithSalt` jobName

instance
  Prelude.NFData
    StartDocumentClassificationJob
  where
  rnf StartDocumentClassificationJob' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf documentClassifierArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig

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
          [ ("JobName" Core..=) Prelude.<$> jobName,
            ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("Tags" Core..=) Prelude.<$> tags,
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
  { -- | The identifier generated for the job. To get the status of the job, use
    -- this identifier with the operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the document classification job. It is
    -- a unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:document-classification-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:document-classification-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
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
    jobStatus :: Prelude.Maybe JobStatus,
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
-- 'jobId', 'startDocumentClassificationJobResponse_jobId' - The identifier generated for the job. To get the status of the job, use
-- this identifier with the operation.
--
-- 'jobArn', 'startDocumentClassificationJobResponse_jobArn' - The Amazon Resource Name (ARN) of the document classification job. It is
-- a unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:document-classification-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:document-classification-job\/1234abcd12ab34cd56ef1234567890ab@
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
-- 'httpStatus', 'startDocumentClassificationJobResponse_httpStatus' - The response's http status code.
newStartDocumentClassificationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDocumentClassificationJobResponse
newStartDocumentClassificationJobResponse
  pHttpStatus_ =
    StartDocumentClassificationJobResponse'
      { jobId =
          Prelude.Nothing,
        jobArn = Prelude.Nothing,
        jobStatus = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identifier generated for the job. To get the status of the job, use
-- this identifier with the operation.
startDocumentClassificationJobResponse_jobId :: Lens.Lens' StartDocumentClassificationJobResponse (Prelude.Maybe Prelude.Text)
startDocumentClassificationJobResponse_jobId = Lens.lens (\StartDocumentClassificationJobResponse' {jobId} -> jobId) (\s@StartDocumentClassificationJobResponse' {} a -> s {jobId = a} :: StartDocumentClassificationJobResponse)

-- | The Amazon Resource Name (ARN) of the document classification job. It is
-- a unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:document-classification-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:document-classification-job\/1234abcd12ab34cd56ef1234567890ab@
startDocumentClassificationJobResponse_jobArn :: Lens.Lens' StartDocumentClassificationJobResponse (Prelude.Maybe Prelude.Text)
startDocumentClassificationJobResponse_jobArn = Lens.lens (\StartDocumentClassificationJobResponse' {jobArn} -> jobArn) (\s@StartDocumentClassificationJobResponse' {} a -> s {jobArn = a} :: StartDocumentClassificationJobResponse)

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

-- | The response's http status code.
startDocumentClassificationJobResponse_httpStatus :: Lens.Lens' StartDocumentClassificationJobResponse Prelude.Int
startDocumentClassificationJobResponse_httpStatus = Lens.lens (\StartDocumentClassificationJobResponse' {httpStatus} -> httpStatus) (\s@StartDocumentClassificationJobResponse' {} a -> s {httpStatus = a} :: StartDocumentClassificationJobResponse)

instance
  Prelude.NFData
    StartDocumentClassificationJobResponse
  where
  rnf StartDocumentClassificationJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobArn
