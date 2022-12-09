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
-- Module      : Amazonka.Comprehend.StartKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous key phrase detection job for a collection of
-- documents. Use the operation to track the status of a job.
module Amazonka.Comprehend.StartKeyPhrasesDetectionJob
  ( -- * Creating a Request
    StartKeyPhrasesDetectionJob (..),
    newStartKeyPhrasesDetectionJob,

    -- * Request Lenses
    startKeyPhrasesDetectionJob_clientRequestToken,
    startKeyPhrasesDetectionJob_jobName,
    startKeyPhrasesDetectionJob_tags,
    startKeyPhrasesDetectionJob_volumeKmsKeyId,
    startKeyPhrasesDetectionJob_vpcConfig,
    startKeyPhrasesDetectionJob_inputDataConfig,
    startKeyPhrasesDetectionJob_outputDataConfig,
    startKeyPhrasesDetectionJob_dataAccessRoleArn,
    startKeyPhrasesDetectionJob_languageCode,

    -- * Destructuring the Response
    StartKeyPhrasesDetectionJobResponse (..),
    newStartKeyPhrasesDetectionJobResponse,

    -- * Response Lenses
    startKeyPhrasesDetectionJobResponse_jobArn,
    startKeyPhrasesDetectionJobResponse_jobId,
    startKeyPhrasesDetectionJobResponse_jobStatus,
    startKeyPhrasesDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartKeyPhrasesDetectionJob' smart constructor.
data StartKeyPhrasesDetectionJob = StartKeyPhrasesDetectionJob'
  { -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Tags to be associated with the key phrases detection job. A tag is a
    -- key-value pair that adds metadata to a resource used by Amazon
    -- Comprehend. For example, a tag with \"Sales\" as the key might be added
    -- to a resource to indicate its use by the sales department.
    tags :: Prelude.Maybe [Tag],
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
    -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your key phrases
    -- detection job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
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
-- Create a value of 'StartKeyPhrasesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startKeyPhrasesDetectionJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'jobName', 'startKeyPhrasesDetectionJob_jobName' - The identifier of the job.
--
-- 'tags', 'startKeyPhrasesDetectionJob_tags' - Tags to be associated with the key phrases detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
--
-- 'volumeKmsKeyId', 'startKeyPhrasesDetectionJob_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'vpcConfig', 'startKeyPhrasesDetectionJob_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your key phrases
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'inputDataConfig', 'startKeyPhrasesDetectionJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startKeyPhrasesDetectionJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startKeyPhrasesDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
--
-- 'languageCode', 'startKeyPhrasesDetectionJob_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
newStartKeyPhrasesDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartKeyPhrasesDetectionJob
newStartKeyPhrasesDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartKeyPhrasesDetectionJob'
      { clientRequestToken =
          Prelude.Nothing,
        jobName = Prelude.Nothing,
        tags = Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_
      }

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startKeyPhrasesDetectionJob_clientRequestToken :: Lens.Lens' StartKeyPhrasesDetectionJob (Prelude.Maybe Prelude.Text)
startKeyPhrasesDetectionJob_clientRequestToken = Lens.lens (\StartKeyPhrasesDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartKeyPhrasesDetectionJob' {} a -> s {clientRequestToken = a} :: StartKeyPhrasesDetectionJob)

-- | The identifier of the job.
startKeyPhrasesDetectionJob_jobName :: Lens.Lens' StartKeyPhrasesDetectionJob (Prelude.Maybe Prelude.Text)
startKeyPhrasesDetectionJob_jobName = Lens.lens (\StartKeyPhrasesDetectionJob' {jobName} -> jobName) (\s@StartKeyPhrasesDetectionJob' {} a -> s {jobName = a} :: StartKeyPhrasesDetectionJob)

-- | Tags to be associated with the key phrases detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
startKeyPhrasesDetectionJob_tags :: Lens.Lens' StartKeyPhrasesDetectionJob (Prelude.Maybe [Tag])
startKeyPhrasesDetectionJob_tags = Lens.lens (\StartKeyPhrasesDetectionJob' {tags} -> tags) (\s@StartKeyPhrasesDetectionJob' {} a -> s {tags = a} :: StartKeyPhrasesDetectionJob) Prelude.. Lens.mapping Lens.coerced

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
startKeyPhrasesDetectionJob_volumeKmsKeyId :: Lens.Lens' StartKeyPhrasesDetectionJob (Prelude.Maybe Prelude.Text)
startKeyPhrasesDetectionJob_volumeKmsKeyId = Lens.lens (\StartKeyPhrasesDetectionJob' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@StartKeyPhrasesDetectionJob' {} a -> s {volumeKmsKeyId = a} :: StartKeyPhrasesDetectionJob)

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your key phrases
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
startKeyPhrasesDetectionJob_vpcConfig :: Lens.Lens' StartKeyPhrasesDetectionJob (Prelude.Maybe VpcConfig)
startKeyPhrasesDetectionJob_vpcConfig = Lens.lens (\StartKeyPhrasesDetectionJob' {vpcConfig} -> vpcConfig) (\s@StartKeyPhrasesDetectionJob' {} a -> s {vpcConfig = a} :: StartKeyPhrasesDetectionJob)

-- | Specifies the format and location of the input data for the job.
startKeyPhrasesDetectionJob_inputDataConfig :: Lens.Lens' StartKeyPhrasesDetectionJob InputDataConfig
startKeyPhrasesDetectionJob_inputDataConfig = Lens.lens (\StartKeyPhrasesDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartKeyPhrasesDetectionJob' {} a -> s {inputDataConfig = a} :: StartKeyPhrasesDetectionJob)

-- | Specifies where to send the output files.
startKeyPhrasesDetectionJob_outputDataConfig :: Lens.Lens' StartKeyPhrasesDetectionJob OutputDataConfig
startKeyPhrasesDetectionJob_outputDataConfig = Lens.lens (\StartKeyPhrasesDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartKeyPhrasesDetectionJob' {} a -> s {outputDataConfig = a} :: StartKeyPhrasesDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
startKeyPhrasesDetectionJob_dataAccessRoleArn :: Lens.Lens' StartKeyPhrasesDetectionJob Prelude.Text
startKeyPhrasesDetectionJob_dataAccessRoleArn = Lens.lens (\StartKeyPhrasesDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartKeyPhrasesDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartKeyPhrasesDetectionJob)

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
startKeyPhrasesDetectionJob_languageCode :: Lens.Lens' StartKeyPhrasesDetectionJob LanguageCode
startKeyPhrasesDetectionJob_languageCode = Lens.lens (\StartKeyPhrasesDetectionJob' {languageCode} -> languageCode) (\s@StartKeyPhrasesDetectionJob' {} a -> s {languageCode = a} :: StartKeyPhrasesDetectionJob)

instance Core.AWSRequest StartKeyPhrasesDetectionJob where
  type
    AWSResponse StartKeyPhrasesDetectionJob =
      StartKeyPhrasesDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartKeyPhrasesDetectionJobResponse'
            Prelude.<$> (x Data..?> "JobArn")
            Prelude.<*> (x Data..?> "JobId")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartKeyPhrasesDetectionJob where
  hashWithSalt _salt StartKeyPhrasesDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData StartKeyPhrasesDetectionJob where
  rnf StartKeyPhrasesDetectionJob' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders StartKeyPhrasesDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.StartKeyPhrasesDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartKeyPhrasesDetectionJob where
  toJSON StartKeyPhrasesDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("JobName" Data..=) Prelude.<$> jobName,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VolumeKmsKeyId" Data..=)
              Prelude.<$> volumeKmsKeyId,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath StartKeyPhrasesDetectionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartKeyPhrasesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartKeyPhrasesDetectionJobResponse' smart constructor.
data StartKeyPhrasesDetectionJobResponse = StartKeyPhrasesDetectionJobResponse'
  { -- | The Amazon Resource Name (ARN) of the key phrase detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:key-phrases-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:key-phrases-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier generated for the job. To get the status of a job, use
    -- this identifier with the operation.
    jobId :: Prelude.Maybe Prelude.Text,
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
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartKeyPhrasesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobArn', 'startKeyPhrasesDetectionJobResponse_jobArn' - The Amazon Resource Name (ARN) of the key phrase detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:key-phrases-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:key-phrases-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobId', 'startKeyPhrasesDetectionJobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
--
-- 'jobStatus', 'startKeyPhrasesDetectionJobResponse_jobStatus' - The status of the job.
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
-- 'httpStatus', 'startKeyPhrasesDetectionJobResponse_httpStatus' - The response's http status code.
newStartKeyPhrasesDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartKeyPhrasesDetectionJobResponse
newStartKeyPhrasesDetectionJobResponse pHttpStatus_ =
  StartKeyPhrasesDetectionJobResponse'
    { jobArn =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the key phrase detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:key-phrases-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:key-phrases-detection-job\/1234abcd12ab34cd56ef1234567890ab@
startKeyPhrasesDetectionJobResponse_jobArn :: Lens.Lens' StartKeyPhrasesDetectionJobResponse (Prelude.Maybe Prelude.Text)
startKeyPhrasesDetectionJobResponse_jobArn = Lens.lens (\StartKeyPhrasesDetectionJobResponse' {jobArn} -> jobArn) (\s@StartKeyPhrasesDetectionJobResponse' {} a -> s {jobArn = a} :: StartKeyPhrasesDetectionJobResponse)

-- | The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
startKeyPhrasesDetectionJobResponse_jobId :: Lens.Lens' StartKeyPhrasesDetectionJobResponse (Prelude.Maybe Prelude.Text)
startKeyPhrasesDetectionJobResponse_jobId = Lens.lens (\StartKeyPhrasesDetectionJobResponse' {jobId} -> jobId) (\s@StartKeyPhrasesDetectionJobResponse' {} a -> s {jobId = a} :: StartKeyPhrasesDetectionJobResponse)

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
startKeyPhrasesDetectionJobResponse_jobStatus :: Lens.Lens' StartKeyPhrasesDetectionJobResponse (Prelude.Maybe JobStatus)
startKeyPhrasesDetectionJobResponse_jobStatus = Lens.lens (\StartKeyPhrasesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartKeyPhrasesDetectionJobResponse' {} a -> s {jobStatus = a} :: StartKeyPhrasesDetectionJobResponse)

-- | The response's http status code.
startKeyPhrasesDetectionJobResponse_httpStatus :: Lens.Lens' StartKeyPhrasesDetectionJobResponse Prelude.Int
startKeyPhrasesDetectionJobResponse_httpStatus = Lens.lens (\StartKeyPhrasesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartKeyPhrasesDetectionJobResponse' {} a -> s {httpStatus = a} :: StartKeyPhrasesDetectionJobResponse)

instance
  Prelude.NFData
    StartKeyPhrasesDetectionJobResponse
  where
  rnf StartKeyPhrasesDetectionJobResponse' {..} =
    Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf httpStatus
