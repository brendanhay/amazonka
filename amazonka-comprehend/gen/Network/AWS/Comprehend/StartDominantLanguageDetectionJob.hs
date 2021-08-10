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
-- Module      : Network.AWS.Comprehend.StartDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous dominant language detection job for a collection
-- of documents. Use the operation to track the status of a job.
module Network.AWS.Comprehend.StartDominantLanguageDetectionJob
  ( -- * Creating a Request
    StartDominantLanguageDetectionJob (..),
    newStartDominantLanguageDetectionJob,

    -- * Request Lenses
    startDominantLanguageDetectionJob_vpcConfig,
    startDominantLanguageDetectionJob_volumeKmsKeyId,
    startDominantLanguageDetectionJob_clientRequestToken,
    startDominantLanguageDetectionJob_jobName,
    startDominantLanguageDetectionJob_inputDataConfig,
    startDominantLanguageDetectionJob_outputDataConfig,
    startDominantLanguageDetectionJob_dataAccessRoleArn,

    -- * Destructuring the Response
    StartDominantLanguageDetectionJobResponse (..),
    newStartDominantLanguageDetectionJobResponse,

    -- * Response Lenses
    startDominantLanguageDetectionJobResponse_jobStatus,
    startDominantLanguageDetectionJobResponse_jobId,
    startDominantLanguageDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartDominantLanguageDetectionJob' smart constructor.
data StartDominantLanguageDetectionJob = StartDominantLanguageDetectionJob'
  { -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your dominant language
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
    -- | A unique identifier for the request. If you do not set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An identifier for the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    -- For more information, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
    dataAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDominantLanguageDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'startDominantLanguageDetectionJob_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your dominant language
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'volumeKmsKeyId', 'startDominantLanguageDetectionJob_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'clientRequestToken', 'startDominantLanguageDetectionJob_clientRequestToken' - A unique identifier for the request. If you do not set the client
-- request token, Amazon Comprehend generates one.
--
-- 'jobName', 'startDominantLanguageDetectionJob_jobName' - An identifier for the job.
--
-- 'inputDataConfig', 'startDominantLanguageDetectionJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startDominantLanguageDetectionJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startDominantLanguageDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
newStartDominantLanguageDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  StartDominantLanguageDetectionJob
newStartDominantLanguageDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_ =
    StartDominantLanguageDetectionJob'
      { vpcConfig =
          Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        jobName = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_
      }

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your dominant language
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
startDominantLanguageDetectionJob_vpcConfig :: Lens.Lens' StartDominantLanguageDetectionJob (Prelude.Maybe VpcConfig)
startDominantLanguageDetectionJob_vpcConfig = Lens.lens (\StartDominantLanguageDetectionJob' {vpcConfig} -> vpcConfig) (\s@StartDominantLanguageDetectionJob' {} a -> s {vpcConfig = a} :: StartDominantLanguageDetectionJob)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
startDominantLanguageDetectionJob_volumeKmsKeyId :: Lens.Lens' StartDominantLanguageDetectionJob (Prelude.Maybe Prelude.Text)
startDominantLanguageDetectionJob_volumeKmsKeyId = Lens.lens (\StartDominantLanguageDetectionJob' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@StartDominantLanguageDetectionJob' {} a -> s {volumeKmsKeyId = a} :: StartDominantLanguageDetectionJob)

-- | A unique identifier for the request. If you do not set the client
-- request token, Amazon Comprehend generates one.
startDominantLanguageDetectionJob_clientRequestToken :: Lens.Lens' StartDominantLanguageDetectionJob (Prelude.Maybe Prelude.Text)
startDominantLanguageDetectionJob_clientRequestToken = Lens.lens (\StartDominantLanguageDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartDominantLanguageDetectionJob' {} a -> s {clientRequestToken = a} :: StartDominantLanguageDetectionJob)

-- | An identifier for the job.
startDominantLanguageDetectionJob_jobName :: Lens.Lens' StartDominantLanguageDetectionJob (Prelude.Maybe Prelude.Text)
startDominantLanguageDetectionJob_jobName = Lens.lens (\StartDominantLanguageDetectionJob' {jobName} -> jobName) (\s@StartDominantLanguageDetectionJob' {} a -> s {jobName = a} :: StartDominantLanguageDetectionJob)

-- | Specifies the format and location of the input data for the job.
startDominantLanguageDetectionJob_inputDataConfig :: Lens.Lens' StartDominantLanguageDetectionJob InputDataConfig
startDominantLanguageDetectionJob_inputDataConfig = Lens.lens (\StartDominantLanguageDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartDominantLanguageDetectionJob' {} a -> s {inputDataConfig = a} :: StartDominantLanguageDetectionJob)

-- | Specifies where to send the output files.
startDominantLanguageDetectionJob_outputDataConfig :: Lens.Lens' StartDominantLanguageDetectionJob OutputDataConfig
startDominantLanguageDetectionJob_outputDataConfig = Lens.lens (\StartDominantLanguageDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartDominantLanguageDetectionJob' {} a -> s {outputDataConfig = a} :: StartDominantLanguageDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
startDominantLanguageDetectionJob_dataAccessRoleArn :: Lens.Lens' StartDominantLanguageDetectionJob Prelude.Text
startDominantLanguageDetectionJob_dataAccessRoleArn = Lens.lens (\StartDominantLanguageDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartDominantLanguageDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartDominantLanguageDetectionJob)

instance
  Core.AWSRequest
    StartDominantLanguageDetectionJob
  where
  type
    AWSResponse StartDominantLanguageDetectionJob =
      StartDominantLanguageDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDominantLanguageDetectionJobResponse'
            Prelude.<$> (x Core..?> "JobStatus")
              Prelude.<*> (x Core..?> "JobId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartDominantLanguageDetectionJob

instance
  Prelude.NFData
    StartDominantLanguageDetectionJob

instance
  Core.ToHeaders
    StartDominantLanguageDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StartDominantLanguageDetectionJob" ::
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
    StartDominantLanguageDetectionJob
  where
  toJSON StartDominantLanguageDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("JobName" Core..=) Prelude.<$> jobName,
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn)
          ]
      )

instance
  Core.ToPath
    StartDominantLanguageDetectionJob
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    StartDominantLanguageDetectionJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDominantLanguageDetectionJobResponse' smart constructor.
data StartDominantLanguageDetectionJobResponse = StartDominantLanguageDetectionJobResponse'
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDominantLanguageDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'startDominantLanguageDetectionJobResponse_jobStatus' - The status of the job.
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
-- 'jobId', 'startDominantLanguageDetectionJobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
--
-- 'httpStatus', 'startDominantLanguageDetectionJobResponse_httpStatus' - The response's http status code.
newStartDominantLanguageDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDominantLanguageDetectionJobResponse
newStartDominantLanguageDetectionJobResponse
  pHttpStatus_ =
    StartDominantLanguageDetectionJobResponse'
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
startDominantLanguageDetectionJobResponse_jobStatus :: Lens.Lens' StartDominantLanguageDetectionJobResponse (Prelude.Maybe JobStatus)
startDominantLanguageDetectionJobResponse_jobStatus = Lens.lens (\StartDominantLanguageDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartDominantLanguageDetectionJobResponse' {} a -> s {jobStatus = a} :: StartDominantLanguageDetectionJobResponse)

-- | The identifier generated for the job. To get the status of a job, use
-- this identifier with the operation.
startDominantLanguageDetectionJobResponse_jobId :: Lens.Lens' StartDominantLanguageDetectionJobResponse (Prelude.Maybe Prelude.Text)
startDominantLanguageDetectionJobResponse_jobId = Lens.lens (\StartDominantLanguageDetectionJobResponse' {jobId} -> jobId) (\s@StartDominantLanguageDetectionJobResponse' {} a -> s {jobId = a} :: StartDominantLanguageDetectionJobResponse)

-- | The response's http status code.
startDominantLanguageDetectionJobResponse_httpStatus :: Lens.Lens' StartDominantLanguageDetectionJobResponse Prelude.Int
startDominantLanguageDetectionJobResponse_httpStatus = Lens.lens (\StartDominantLanguageDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartDominantLanguageDetectionJobResponse' {} a -> s {httpStatus = a} :: StartDominantLanguageDetectionJobResponse)

instance
  Prelude.NFData
    StartDominantLanguageDetectionJobResponse
