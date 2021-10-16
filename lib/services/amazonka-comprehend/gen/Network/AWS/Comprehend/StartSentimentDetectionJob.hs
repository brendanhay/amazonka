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
-- Module      : Network.AWS.Comprehend.StartSentimentDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous sentiment detection job for a collection of
-- documents. use the operation to track the status of a job.
module Network.AWS.Comprehend.StartSentimentDetectionJob
  ( -- * Creating a Request
    StartSentimentDetectionJob (..),
    newStartSentimentDetectionJob,

    -- * Request Lenses
    startSentimentDetectionJob_vpcConfig,
    startSentimentDetectionJob_volumeKmsKeyId,
    startSentimentDetectionJob_tags,
    startSentimentDetectionJob_clientRequestToken,
    startSentimentDetectionJob_jobName,
    startSentimentDetectionJob_inputDataConfig,
    startSentimentDetectionJob_outputDataConfig,
    startSentimentDetectionJob_dataAccessRoleArn,
    startSentimentDetectionJob_languageCode,

    -- * Destructuring the Response
    StartSentimentDetectionJobResponse (..),
    newStartSentimentDetectionJobResponse,

    -- * Response Lenses
    startSentimentDetectionJobResponse_jobStatus,
    startSentimentDetectionJobResponse_jobArn,
    startSentimentDetectionJobResponse_jobId,
    startSentimentDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartSentimentDetectionJob' smart constructor.
data StartSentimentDetectionJob = StartSentimentDetectionJob'
  { -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your sentiment
    -- detection job. For more information, see
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
    -- | Tags to be associated with the sentiment detection job. A tag is a
    -- key-value pair that adds metadata to a resource used by Amazon
    -- Comprehend. For example, a tag with \"Sales\" as the key might be added
    -- to a resource to indicate its use by the sales department.
    tags :: Prelude.Maybe [Tag],
    -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    -- For more information, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
    dataAccessRoleArn :: Prelude.Text,
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSentimentDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'startSentimentDetectionJob_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your sentiment
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'volumeKmsKeyId', 'startSentimentDetectionJob_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'tags', 'startSentimentDetectionJob_tags' - Tags to be associated with the sentiment detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
--
-- 'clientRequestToken', 'startSentimentDetectionJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'jobName', 'startSentimentDetectionJob_jobName' - The identifier of the job.
--
-- 'inputDataConfig', 'startSentimentDetectionJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startSentimentDetectionJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startSentimentDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
--
-- 'languageCode', 'startSentimentDetectionJob_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
newStartSentimentDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartSentimentDetectionJob
newStartSentimentDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartSentimentDetectionJob'
      { vpcConfig =
          Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        jobName = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_
      }

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your sentiment
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
startSentimentDetectionJob_vpcConfig :: Lens.Lens' StartSentimentDetectionJob (Prelude.Maybe VpcConfig)
startSentimentDetectionJob_vpcConfig = Lens.lens (\StartSentimentDetectionJob' {vpcConfig} -> vpcConfig) (\s@StartSentimentDetectionJob' {} a -> s {vpcConfig = a} :: StartSentimentDetectionJob)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
startSentimentDetectionJob_volumeKmsKeyId :: Lens.Lens' StartSentimentDetectionJob (Prelude.Maybe Prelude.Text)
startSentimentDetectionJob_volumeKmsKeyId = Lens.lens (\StartSentimentDetectionJob' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@StartSentimentDetectionJob' {} a -> s {volumeKmsKeyId = a} :: StartSentimentDetectionJob)

-- | Tags to be associated with the sentiment detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
startSentimentDetectionJob_tags :: Lens.Lens' StartSentimentDetectionJob (Prelude.Maybe [Tag])
startSentimentDetectionJob_tags = Lens.lens (\StartSentimentDetectionJob' {tags} -> tags) (\s@StartSentimentDetectionJob' {} a -> s {tags = a} :: StartSentimentDetectionJob) Prelude.. Lens.mapping Lens._Coerce

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startSentimentDetectionJob_clientRequestToken :: Lens.Lens' StartSentimentDetectionJob (Prelude.Maybe Prelude.Text)
startSentimentDetectionJob_clientRequestToken = Lens.lens (\StartSentimentDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartSentimentDetectionJob' {} a -> s {clientRequestToken = a} :: StartSentimentDetectionJob)

-- | The identifier of the job.
startSentimentDetectionJob_jobName :: Lens.Lens' StartSentimentDetectionJob (Prelude.Maybe Prelude.Text)
startSentimentDetectionJob_jobName = Lens.lens (\StartSentimentDetectionJob' {jobName} -> jobName) (\s@StartSentimentDetectionJob' {} a -> s {jobName = a} :: StartSentimentDetectionJob)

-- | Specifies the format and location of the input data for the job.
startSentimentDetectionJob_inputDataConfig :: Lens.Lens' StartSentimentDetectionJob InputDataConfig
startSentimentDetectionJob_inputDataConfig = Lens.lens (\StartSentimentDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartSentimentDetectionJob' {} a -> s {inputDataConfig = a} :: StartSentimentDetectionJob)

-- | Specifies where to send the output files.
startSentimentDetectionJob_outputDataConfig :: Lens.Lens' StartSentimentDetectionJob OutputDataConfig
startSentimentDetectionJob_outputDataConfig = Lens.lens (\StartSentimentDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartSentimentDetectionJob' {} a -> s {outputDataConfig = a} :: StartSentimentDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
startSentimentDetectionJob_dataAccessRoleArn :: Lens.Lens' StartSentimentDetectionJob Prelude.Text
startSentimentDetectionJob_dataAccessRoleArn = Lens.lens (\StartSentimentDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartSentimentDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartSentimentDetectionJob)

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
startSentimentDetectionJob_languageCode :: Lens.Lens' StartSentimentDetectionJob LanguageCode
startSentimentDetectionJob_languageCode = Lens.lens (\StartSentimentDetectionJob' {languageCode} -> languageCode) (\s@StartSentimentDetectionJob' {} a -> s {languageCode = a} :: StartSentimentDetectionJob)

instance Core.AWSRequest StartSentimentDetectionJob where
  type
    AWSResponse StartSentimentDetectionJob =
      StartSentimentDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSentimentDetectionJobResponse'
            Prelude.<$> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "JobArn")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSentimentDetectionJob

instance Prelude.NFData StartSentimentDetectionJob

instance Core.ToHeaders StartSentimentDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StartSentimentDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartSentimentDetectionJob where
  toJSON StartSentimentDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            ("Tags" Core..=) Prelude.<$> tags,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("JobName" Core..=) Prelude.<$> jobName,
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath StartSentimentDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartSentimentDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSentimentDetectionJobResponse' smart constructor.
data StartSentimentDetectionJobResponse = StartSentimentDetectionJobResponse'
  { -- | The status of the job.
    --
    -- -   SUBMITTED - The job has been received and is queued for processing.
    --
    -- -   IN_PROGRESS - Amazon Comprehend is processing the job.
    --
    -- -   COMPLETED - The job was successfully completed and the output is
    --     available.
    --
    -- -   FAILED - The job did not complete. To get details, use the
    --     operation.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The Amazon Resource Name (ARN) of the sentiment detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:sentiment-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier generated for the job. To get the status of a job, use
    -- this identifier with the operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSentimentDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'startSentimentDetectionJobResponse_jobStatus' - The status of the job.
--
-- -   SUBMITTED - The job has been received and is queued for processing.
--
-- -   IN_PROGRESS - Amazon Comprehend is processing the job.
--
-- -   COMPLETED - The job was successfully completed and the output is
--     available.
--
-- -   FAILED - The job did not complete. To get details, use the
--     operation.
--
-- 'jobArn', 'startSentimentDetectionJobResponse_jobArn' - The Amazon Resource Name (ARN) of the sentiment detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:sentiment-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobId', 'startSentimentDetectionJobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
--
-- 'httpStatus', 'startSentimentDetectionJobResponse_httpStatus' - The response's http status code.
newStartSentimentDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSentimentDetectionJobResponse
newStartSentimentDetectionJobResponse pHttpStatus_ =
  StartSentimentDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the job.
--
-- -   SUBMITTED - The job has been received and is queued for processing.
--
-- -   IN_PROGRESS - Amazon Comprehend is processing the job.
--
-- -   COMPLETED - The job was successfully completed and the output is
--     available.
--
-- -   FAILED - The job did not complete. To get details, use the
--     operation.
startSentimentDetectionJobResponse_jobStatus :: Lens.Lens' StartSentimentDetectionJobResponse (Prelude.Maybe JobStatus)
startSentimentDetectionJobResponse_jobStatus = Lens.lens (\StartSentimentDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartSentimentDetectionJobResponse' {} a -> s {jobStatus = a} :: StartSentimentDetectionJobResponse)

-- | The Amazon Resource Name (ARN) of the sentiment detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:sentiment-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
startSentimentDetectionJobResponse_jobArn :: Lens.Lens' StartSentimentDetectionJobResponse (Prelude.Maybe Prelude.Text)
startSentimentDetectionJobResponse_jobArn = Lens.lens (\StartSentimentDetectionJobResponse' {jobArn} -> jobArn) (\s@StartSentimentDetectionJobResponse' {} a -> s {jobArn = a} :: StartSentimentDetectionJobResponse)

-- | The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
startSentimentDetectionJobResponse_jobId :: Lens.Lens' StartSentimentDetectionJobResponse (Prelude.Maybe Prelude.Text)
startSentimentDetectionJobResponse_jobId = Lens.lens (\StartSentimentDetectionJobResponse' {jobId} -> jobId) (\s@StartSentimentDetectionJobResponse' {} a -> s {jobId = a} :: StartSentimentDetectionJobResponse)

-- | The response's http status code.
startSentimentDetectionJobResponse_httpStatus :: Lens.Lens' StartSentimentDetectionJobResponse Prelude.Int
startSentimentDetectionJobResponse_httpStatus = Lens.lens (\StartSentimentDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartSentimentDetectionJobResponse' {} a -> s {httpStatus = a} :: StartSentimentDetectionJobResponse)

instance
  Prelude.NFData
    StartSentimentDetectionJobResponse
