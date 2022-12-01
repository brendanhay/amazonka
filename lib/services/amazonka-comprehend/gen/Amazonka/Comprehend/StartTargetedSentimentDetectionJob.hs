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
-- Module      : Amazonka.Comprehend.StartTargetedSentimentDetectionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous targeted sentiment detection job for a collection
-- of documents. Use the operation to track the status of a job.
module Amazonka.Comprehend.StartTargetedSentimentDetectionJob
  ( -- * Creating a Request
    StartTargetedSentimentDetectionJob (..),
    newStartTargetedSentimentDetectionJob,

    -- * Request Lenses
    startTargetedSentimentDetectionJob_tags,
    startTargetedSentimentDetectionJob_clientRequestToken,
    startTargetedSentimentDetectionJob_vpcConfig,
    startTargetedSentimentDetectionJob_jobName,
    startTargetedSentimentDetectionJob_volumeKmsKeyId,
    startTargetedSentimentDetectionJob_inputDataConfig,
    startTargetedSentimentDetectionJob_outputDataConfig,
    startTargetedSentimentDetectionJob_dataAccessRoleArn,
    startTargetedSentimentDetectionJob_languageCode,

    -- * Destructuring the Response
    StartTargetedSentimentDetectionJobResponse (..),
    newStartTargetedSentimentDetectionJobResponse,

    -- * Response Lenses
    startTargetedSentimentDetectionJobResponse_jobStatus,
    startTargetedSentimentDetectionJobResponse_jobId,
    startTargetedSentimentDetectionJobResponse_jobArn,
    startTargetedSentimentDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartTargetedSentimentDetectionJob' smart constructor.
data StartTargetedSentimentDetectionJob = StartTargetedSentimentDetectionJob'
  { -- | Tags to be associated with the targeted sentiment detection job. A tag
    -- is a key-value pair that adds metadata to a resource used by Amazon
    -- Comprehend. For example, a tag with \"Sales\" as the key might be added
    -- to a resource to indicate its use by the sales department.
    tags :: Prelude.Maybe [Tag],
    -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | ID for the KMS key that Amazon Comprehend uses to encrypt data on the
    -- storage volume attached to the ML compute instance(s) that process the
    -- analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    -- For more information, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions Role-based permissions>.
    dataAccessRoleArn :: Prelude.Text,
    -- | The language of the input documents. Currently, English is the only
    -- supported language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTargetedSentimentDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startTargetedSentimentDetectionJob_tags' - Tags to be associated with the targeted sentiment detection job. A tag
-- is a key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
--
-- 'clientRequestToken', 'startTargetedSentimentDetectionJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'vpcConfig', 'startTargetedSentimentDetectionJob_vpcConfig' - Undocumented member.
--
-- 'jobName', 'startTargetedSentimentDetectionJob_jobName' - The identifier of the job.
--
-- 'volumeKmsKeyId', 'startTargetedSentimentDetectionJob_volumeKmsKeyId' - ID for the KMS key that Amazon Comprehend uses to encrypt data on the
-- storage volume attached to the ML compute instance(s) that process the
-- analysis job. The VolumeKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'inputDataConfig', 'startTargetedSentimentDetectionJob_inputDataConfig' - Undocumented member.
--
-- 'outputDataConfig', 'startTargetedSentimentDetectionJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startTargetedSentimentDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions Role-based permissions>.
--
-- 'languageCode', 'startTargetedSentimentDetectionJob_languageCode' - The language of the input documents. Currently, English is the only
-- supported language.
newStartTargetedSentimentDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartTargetedSentimentDetectionJob
newStartTargetedSentimentDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartTargetedSentimentDetectionJob'
      { tags =
          Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        jobName = Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_
      }

-- | Tags to be associated with the targeted sentiment detection job. A tag
-- is a key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
startTargetedSentimentDetectionJob_tags :: Lens.Lens' StartTargetedSentimentDetectionJob (Prelude.Maybe [Tag])
startTargetedSentimentDetectionJob_tags = Lens.lens (\StartTargetedSentimentDetectionJob' {tags} -> tags) (\s@StartTargetedSentimentDetectionJob' {} a -> s {tags = a} :: StartTargetedSentimentDetectionJob) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startTargetedSentimentDetectionJob_clientRequestToken :: Lens.Lens' StartTargetedSentimentDetectionJob (Prelude.Maybe Prelude.Text)
startTargetedSentimentDetectionJob_clientRequestToken = Lens.lens (\StartTargetedSentimentDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartTargetedSentimentDetectionJob' {} a -> s {clientRequestToken = a} :: StartTargetedSentimentDetectionJob)

-- | Undocumented member.
startTargetedSentimentDetectionJob_vpcConfig :: Lens.Lens' StartTargetedSentimentDetectionJob (Prelude.Maybe VpcConfig)
startTargetedSentimentDetectionJob_vpcConfig = Lens.lens (\StartTargetedSentimentDetectionJob' {vpcConfig} -> vpcConfig) (\s@StartTargetedSentimentDetectionJob' {} a -> s {vpcConfig = a} :: StartTargetedSentimentDetectionJob)

-- | The identifier of the job.
startTargetedSentimentDetectionJob_jobName :: Lens.Lens' StartTargetedSentimentDetectionJob (Prelude.Maybe Prelude.Text)
startTargetedSentimentDetectionJob_jobName = Lens.lens (\StartTargetedSentimentDetectionJob' {jobName} -> jobName) (\s@StartTargetedSentimentDetectionJob' {} a -> s {jobName = a} :: StartTargetedSentimentDetectionJob)

-- | ID for the KMS key that Amazon Comprehend uses to encrypt data on the
-- storage volume attached to the ML compute instance(s) that process the
-- analysis job. The VolumeKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
startTargetedSentimentDetectionJob_volumeKmsKeyId :: Lens.Lens' StartTargetedSentimentDetectionJob (Prelude.Maybe Prelude.Text)
startTargetedSentimentDetectionJob_volumeKmsKeyId = Lens.lens (\StartTargetedSentimentDetectionJob' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@StartTargetedSentimentDetectionJob' {} a -> s {volumeKmsKeyId = a} :: StartTargetedSentimentDetectionJob)

-- | Undocumented member.
startTargetedSentimentDetectionJob_inputDataConfig :: Lens.Lens' StartTargetedSentimentDetectionJob InputDataConfig
startTargetedSentimentDetectionJob_inputDataConfig = Lens.lens (\StartTargetedSentimentDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartTargetedSentimentDetectionJob' {} a -> s {inputDataConfig = a} :: StartTargetedSentimentDetectionJob)

-- | Specifies where to send the output files.
startTargetedSentimentDetectionJob_outputDataConfig :: Lens.Lens' StartTargetedSentimentDetectionJob OutputDataConfig
startTargetedSentimentDetectionJob_outputDataConfig = Lens.lens (\StartTargetedSentimentDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartTargetedSentimentDetectionJob' {} a -> s {outputDataConfig = a} :: StartTargetedSentimentDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions Role-based permissions>.
startTargetedSentimentDetectionJob_dataAccessRoleArn :: Lens.Lens' StartTargetedSentimentDetectionJob Prelude.Text
startTargetedSentimentDetectionJob_dataAccessRoleArn = Lens.lens (\StartTargetedSentimentDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartTargetedSentimentDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartTargetedSentimentDetectionJob)

-- | The language of the input documents. Currently, English is the only
-- supported language.
startTargetedSentimentDetectionJob_languageCode :: Lens.Lens' StartTargetedSentimentDetectionJob LanguageCode
startTargetedSentimentDetectionJob_languageCode = Lens.lens (\StartTargetedSentimentDetectionJob' {languageCode} -> languageCode) (\s@StartTargetedSentimentDetectionJob' {} a -> s {languageCode = a} :: StartTargetedSentimentDetectionJob)

instance
  Core.AWSRequest
    StartTargetedSentimentDetectionJob
  where
  type
    AWSResponse StartTargetedSentimentDetectionJob =
      StartTargetedSentimentDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTargetedSentimentDetectionJobResponse'
            Prelude.<$> (x Core..?> "JobStatus")
              Prelude.<*> (x Core..?> "JobId")
              Prelude.<*> (x Core..?> "JobArn")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartTargetedSentimentDetectionJob
  where
  hashWithSalt
    _salt
    StartTargetedSentimentDetectionJob' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` vpcConfig
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` volumeKmsKeyId
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` languageCode

instance
  Prelude.NFData
    StartTargetedSentimentDetectionJob
  where
  rnf StartTargetedSentimentDetectionJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf languageCode

instance
  Core.ToHeaders
    StartTargetedSentimentDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StartTargetedSentimentDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    StartTargetedSentimentDetectionJob
  where
  toJSON StartTargetedSentimentDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("JobName" Core..=) Prelude.<$> jobName,
            ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance
  Core.ToPath
    StartTargetedSentimentDetectionJob
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    StartTargetedSentimentDetectionJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTargetedSentimentDetectionJobResponse' smart constructor.
data StartTargetedSentimentDetectionJobResponse = StartTargetedSentimentDetectionJobResponse'
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
    -- | The identifier generated for the job. To get the status of a job, use
    -- this identifier with the operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the targeted sentiment detection job.
    -- It is a unique, fully qualified identifier for the job. It includes the
    -- AWS account, Region, and the job ID. The format of the ARN is as
    -- follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:targeted-sentiment-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:targeted-sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTargetedSentimentDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'startTargetedSentimentDetectionJobResponse_jobStatus' - The status of the job.
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
-- 'jobId', 'startTargetedSentimentDetectionJobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
--
-- 'jobArn', 'startTargetedSentimentDetectionJobResponse_jobArn' - The Amazon Resource Name (ARN) of the targeted sentiment detection job.
-- It is a unique, fully qualified identifier for the job. It includes the
-- AWS account, Region, and the job ID. The format of the ARN is as
-- follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:targeted-sentiment-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:targeted-sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'httpStatus', 'startTargetedSentimentDetectionJobResponse_httpStatus' - The response's http status code.
newStartTargetedSentimentDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTargetedSentimentDetectionJobResponse
newStartTargetedSentimentDetectionJobResponse
  pHttpStatus_ =
    StartTargetedSentimentDetectionJobResponse'
      { jobStatus =
          Prelude.Nothing,
        jobId = Prelude.Nothing,
        jobArn = Prelude.Nothing,
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
startTargetedSentimentDetectionJobResponse_jobStatus :: Lens.Lens' StartTargetedSentimentDetectionJobResponse (Prelude.Maybe JobStatus)
startTargetedSentimentDetectionJobResponse_jobStatus = Lens.lens (\StartTargetedSentimentDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartTargetedSentimentDetectionJobResponse' {} a -> s {jobStatus = a} :: StartTargetedSentimentDetectionJobResponse)

-- | The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
startTargetedSentimentDetectionJobResponse_jobId :: Lens.Lens' StartTargetedSentimentDetectionJobResponse (Prelude.Maybe Prelude.Text)
startTargetedSentimentDetectionJobResponse_jobId = Lens.lens (\StartTargetedSentimentDetectionJobResponse' {jobId} -> jobId) (\s@StartTargetedSentimentDetectionJobResponse' {} a -> s {jobId = a} :: StartTargetedSentimentDetectionJobResponse)

-- | The Amazon Resource Name (ARN) of the targeted sentiment detection job.
-- It is a unique, fully qualified identifier for the job. It includes the
-- AWS account, Region, and the job ID. The format of the ARN is as
-- follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:targeted-sentiment-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:targeted-sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
startTargetedSentimentDetectionJobResponse_jobArn :: Lens.Lens' StartTargetedSentimentDetectionJobResponse (Prelude.Maybe Prelude.Text)
startTargetedSentimentDetectionJobResponse_jobArn = Lens.lens (\StartTargetedSentimentDetectionJobResponse' {jobArn} -> jobArn) (\s@StartTargetedSentimentDetectionJobResponse' {} a -> s {jobArn = a} :: StartTargetedSentimentDetectionJobResponse)

-- | The response's http status code.
startTargetedSentimentDetectionJobResponse_httpStatus :: Lens.Lens' StartTargetedSentimentDetectionJobResponse Prelude.Int
startTargetedSentimentDetectionJobResponse_httpStatus = Lens.lens (\StartTargetedSentimentDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartTargetedSentimentDetectionJobResponse' {} a -> s {httpStatus = a} :: StartTargetedSentimentDetectionJobResponse)

instance
  Prelude.NFData
    StartTargetedSentimentDetectionJobResponse
  where
  rnf StartTargetedSentimentDetectionJobResponse' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf httpStatus
