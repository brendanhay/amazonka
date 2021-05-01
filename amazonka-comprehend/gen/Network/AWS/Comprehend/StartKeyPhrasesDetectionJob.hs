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
-- Module      : Network.AWS.Comprehend.StartKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous key phrase detection job for a collection of
-- documents. Use the operation to track the status of a job.
module Network.AWS.Comprehend.StartKeyPhrasesDetectionJob
  ( -- * Creating a Request
    StartKeyPhrasesDetectionJob (..),
    newStartKeyPhrasesDetectionJob,

    -- * Request Lenses
    startKeyPhrasesDetectionJob_vpcConfig,
    startKeyPhrasesDetectionJob_volumeKmsKeyId,
    startKeyPhrasesDetectionJob_clientRequestToken,
    startKeyPhrasesDetectionJob_jobName,
    startKeyPhrasesDetectionJob_inputDataConfig,
    startKeyPhrasesDetectionJob_outputDataConfig,
    startKeyPhrasesDetectionJob_dataAccessRoleArn,
    startKeyPhrasesDetectionJob_languageCode,

    -- * Destructuring the Response
    StartKeyPhrasesDetectionJobResponse (..),
    newStartKeyPhrasesDetectionJobResponse,

    -- * Response Lenses
    startKeyPhrasesDetectionJobResponse_jobStatus,
    startKeyPhrasesDetectionJobResponse_jobId,
    startKeyPhrasesDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartKeyPhrasesDetectionJob' smart constructor.
data StartKeyPhrasesDetectionJob = StartKeyPhrasesDetectionJob'
  { -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your key phrases
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartKeyPhrasesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'startKeyPhrasesDetectionJob_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your key phrases
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
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
-- 'clientRequestToken', 'startKeyPhrasesDetectionJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'jobName', 'startKeyPhrasesDetectionJob_jobName' - The identifier of the job.
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
      { vpcConfig =
          Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        jobName = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_
      }

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your key phrases
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
startKeyPhrasesDetectionJob_vpcConfig :: Lens.Lens' StartKeyPhrasesDetectionJob (Prelude.Maybe VpcConfig)
startKeyPhrasesDetectionJob_vpcConfig = Lens.lens (\StartKeyPhrasesDetectionJob' {vpcConfig} -> vpcConfig) (\s@StartKeyPhrasesDetectionJob' {} a -> s {vpcConfig = a} :: StartKeyPhrasesDetectionJob)

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

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startKeyPhrasesDetectionJob_clientRequestToken :: Lens.Lens' StartKeyPhrasesDetectionJob (Prelude.Maybe Prelude.Text)
startKeyPhrasesDetectionJob_clientRequestToken = Lens.lens (\StartKeyPhrasesDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartKeyPhrasesDetectionJob' {} a -> s {clientRequestToken = a} :: StartKeyPhrasesDetectionJob)

-- | The identifier of the job.
startKeyPhrasesDetectionJob_jobName :: Lens.Lens' StartKeyPhrasesDetectionJob (Prelude.Maybe Prelude.Text)
startKeyPhrasesDetectionJob_jobName = Lens.lens (\StartKeyPhrasesDetectionJob' {jobName} -> jobName) (\s@StartKeyPhrasesDetectionJob' {} a -> s {jobName = a} :: StartKeyPhrasesDetectionJob)

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

instance
  Prelude.AWSRequest
    StartKeyPhrasesDetectionJob
  where
  type
    Rs StartKeyPhrasesDetectionJob =
      StartKeyPhrasesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartKeyPhrasesDetectionJobResponse'
            Prelude.<$> (x Prelude..?> "JobStatus")
            Prelude.<*> (x Prelude..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartKeyPhrasesDetectionJob

instance Prelude.NFData StartKeyPhrasesDetectionJob

instance
  Prelude.ToHeaders
    StartKeyPhrasesDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.StartKeyPhrasesDetectionJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartKeyPhrasesDetectionJob where
  toJSON StartKeyPhrasesDetectionJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Prelude..=) Prelude.<$> vpcConfig,
            ("VolumeKmsKeyId" Prelude..=)
              Prelude.<$> volumeKmsKeyId,
            ("ClientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            ("JobName" Prelude..=) Prelude.<$> jobName,
            Prelude.Just
              ("InputDataConfig" Prelude..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Prelude..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Prelude..= dataAccessRoleArn),
            Prelude.Just
              ("LanguageCode" Prelude..= languageCode)
          ]
      )

instance Prelude.ToPath StartKeyPhrasesDetectionJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartKeyPhrasesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartKeyPhrasesDetectionJobResponse' smart constructor.
data StartKeyPhrasesDetectionJobResponse = StartKeyPhrasesDetectionJobResponse'
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartKeyPhrasesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'jobId', 'startKeyPhrasesDetectionJobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
--
-- 'httpStatus', 'startKeyPhrasesDetectionJobResponse_httpStatus' - The response's http status code.
newStartKeyPhrasesDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartKeyPhrasesDetectionJobResponse
newStartKeyPhrasesDetectionJobResponse pHttpStatus_ =
  StartKeyPhrasesDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
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
startKeyPhrasesDetectionJobResponse_jobStatus :: Lens.Lens' StartKeyPhrasesDetectionJobResponse (Prelude.Maybe JobStatus)
startKeyPhrasesDetectionJobResponse_jobStatus = Lens.lens (\StartKeyPhrasesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartKeyPhrasesDetectionJobResponse' {} a -> s {jobStatus = a} :: StartKeyPhrasesDetectionJobResponse)

-- | The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
startKeyPhrasesDetectionJobResponse_jobId :: Lens.Lens' StartKeyPhrasesDetectionJobResponse (Prelude.Maybe Prelude.Text)
startKeyPhrasesDetectionJobResponse_jobId = Lens.lens (\StartKeyPhrasesDetectionJobResponse' {jobId} -> jobId) (\s@StartKeyPhrasesDetectionJobResponse' {} a -> s {jobId = a} :: StartKeyPhrasesDetectionJobResponse)

-- | The response's http status code.
startKeyPhrasesDetectionJobResponse_httpStatus :: Lens.Lens' StartKeyPhrasesDetectionJobResponse Prelude.Int
startKeyPhrasesDetectionJobResponse_httpStatus = Lens.lens (\StartKeyPhrasesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartKeyPhrasesDetectionJobResponse' {} a -> s {httpStatus = a} :: StartKeyPhrasesDetectionJobResponse)

instance
  Prelude.NFData
    StartKeyPhrasesDetectionJobResponse
