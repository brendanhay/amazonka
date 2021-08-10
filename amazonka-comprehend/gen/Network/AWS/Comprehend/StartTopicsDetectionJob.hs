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
-- Module      : Network.AWS.Comprehend.StartTopicsDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous topic detection job. Use the
-- @DescribeTopicDetectionJob@ operation to track the status of a job.
module Network.AWS.Comprehend.StartTopicsDetectionJob
  ( -- * Creating a Request
    StartTopicsDetectionJob (..),
    newStartTopicsDetectionJob,

    -- * Request Lenses
    startTopicsDetectionJob_vpcConfig,
    startTopicsDetectionJob_volumeKmsKeyId,
    startTopicsDetectionJob_clientRequestToken,
    startTopicsDetectionJob_numberOfTopics,
    startTopicsDetectionJob_jobName,
    startTopicsDetectionJob_inputDataConfig,
    startTopicsDetectionJob_outputDataConfig,
    startTopicsDetectionJob_dataAccessRoleArn,

    -- * Destructuring the Response
    StartTopicsDetectionJobResponse (..),
    newStartTopicsDetectionJobResponse,

    -- * Response Lenses
    startTopicsDetectionJobResponse_jobStatus,
    startTopicsDetectionJobResponse_jobId,
    startTopicsDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartTopicsDetectionJob' smart constructor.
data StartTopicsDetectionJob = StartTopicsDetectionJob'
  { -- | Configuration parameters for an optional private Virtual Private Cloud
    -- (VPC) containing the resources you are using for your topic detection
    -- job. For more information, see
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
    -- | The number of topics to detect.
    numberOfTopics :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files. The output is a compressed
    -- archive with two files, @topic-terms.csv@ that lists the terms
    -- associated with each topic, and @doc-topics.csv@ that lists the
    -- documents associated with each topic
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    -- For more information, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
    dataAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTopicsDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'startTopicsDetectionJob_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your topic detection
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'volumeKmsKeyId', 'startTopicsDetectionJob_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'clientRequestToken', 'startTopicsDetectionJob_clientRequestToken' - A unique identifier for the request. If you do not set the client
-- request token, Amazon Comprehend generates one.
--
-- 'numberOfTopics', 'startTopicsDetectionJob_numberOfTopics' - The number of topics to detect.
--
-- 'jobName', 'startTopicsDetectionJob_jobName' - The identifier of the job.
--
-- 'inputDataConfig', 'startTopicsDetectionJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startTopicsDetectionJob_outputDataConfig' - Specifies where to send the output files. The output is a compressed
-- archive with two files, @topic-terms.csv@ that lists the terms
-- associated with each topic, and @doc-topics.csv@ that lists the
-- documents associated with each topic
--
-- 'dataAccessRoleArn', 'startTopicsDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
newStartTopicsDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  StartTopicsDetectionJob
newStartTopicsDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_ =
    StartTopicsDetectionJob'
      { vpcConfig =
          Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        numberOfTopics = Prelude.Nothing,
        jobName = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_
      }

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your topic detection
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
startTopicsDetectionJob_vpcConfig :: Lens.Lens' StartTopicsDetectionJob (Prelude.Maybe VpcConfig)
startTopicsDetectionJob_vpcConfig = Lens.lens (\StartTopicsDetectionJob' {vpcConfig} -> vpcConfig) (\s@StartTopicsDetectionJob' {} a -> s {vpcConfig = a} :: StartTopicsDetectionJob)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
startTopicsDetectionJob_volumeKmsKeyId :: Lens.Lens' StartTopicsDetectionJob (Prelude.Maybe Prelude.Text)
startTopicsDetectionJob_volumeKmsKeyId = Lens.lens (\StartTopicsDetectionJob' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@StartTopicsDetectionJob' {} a -> s {volumeKmsKeyId = a} :: StartTopicsDetectionJob)

-- | A unique identifier for the request. If you do not set the client
-- request token, Amazon Comprehend generates one.
startTopicsDetectionJob_clientRequestToken :: Lens.Lens' StartTopicsDetectionJob (Prelude.Maybe Prelude.Text)
startTopicsDetectionJob_clientRequestToken = Lens.lens (\StartTopicsDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartTopicsDetectionJob' {} a -> s {clientRequestToken = a} :: StartTopicsDetectionJob)

-- | The number of topics to detect.
startTopicsDetectionJob_numberOfTopics :: Lens.Lens' StartTopicsDetectionJob (Prelude.Maybe Prelude.Natural)
startTopicsDetectionJob_numberOfTopics = Lens.lens (\StartTopicsDetectionJob' {numberOfTopics} -> numberOfTopics) (\s@StartTopicsDetectionJob' {} a -> s {numberOfTopics = a} :: StartTopicsDetectionJob)

-- | The identifier of the job.
startTopicsDetectionJob_jobName :: Lens.Lens' StartTopicsDetectionJob (Prelude.Maybe Prelude.Text)
startTopicsDetectionJob_jobName = Lens.lens (\StartTopicsDetectionJob' {jobName} -> jobName) (\s@StartTopicsDetectionJob' {} a -> s {jobName = a} :: StartTopicsDetectionJob)

-- | Specifies the format and location of the input data for the job.
startTopicsDetectionJob_inputDataConfig :: Lens.Lens' StartTopicsDetectionJob InputDataConfig
startTopicsDetectionJob_inputDataConfig = Lens.lens (\StartTopicsDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartTopicsDetectionJob' {} a -> s {inputDataConfig = a} :: StartTopicsDetectionJob)

-- | Specifies where to send the output files. The output is a compressed
-- archive with two files, @topic-terms.csv@ that lists the terms
-- associated with each topic, and @doc-topics.csv@ that lists the
-- documents associated with each topic
startTopicsDetectionJob_outputDataConfig :: Lens.Lens' StartTopicsDetectionJob OutputDataConfig
startTopicsDetectionJob_outputDataConfig = Lens.lens (\StartTopicsDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartTopicsDetectionJob' {} a -> s {outputDataConfig = a} :: StartTopicsDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
startTopicsDetectionJob_dataAccessRoleArn :: Lens.Lens' StartTopicsDetectionJob Prelude.Text
startTopicsDetectionJob_dataAccessRoleArn = Lens.lens (\StartTopicsDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartTopicsDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartTopicsDetectionJob)

instance Core.AWSRequest StartTopicsDetectionJob where
  type
    AWSResponse StartTopicsDetectionJob =
      StartTopicsDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTopicsDetectionJobResponse'
            Prelude.<$> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTopicsDetectionJob

instance Prelude.NFData StartTopicsDetectionJob

instance Core.ToHeaders StartTopicsDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StartTopicsDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartTopicsDetectionJob where
  toJSON StartTopicsDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("NumberOfTopics" Core..=)
              Prelude.<$> numberOfTopics,
            ("JobName" Core..=) Prelude.<$> jobName,
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn)
          ]
      )

instance Core.ToPath StartTopicsDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartTopicsDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTopicsDetectionJobResponse' smart constructor.
data StartTopicsDetectionJobResponse = StartTopicsDetectionJobResponse'
  { -- | The status of the job:
    --
    -- -   SUBMITTED - The job has been received and is queued for processing.
    --
    -- -   IN_PROGRESS - Amazon Comprehend is processing the job.
    --
    -- -   COMPLETED - The job was successfully completed and the output is
    --     available.
    --
    -- -   FAILED - The job did not complete. To get details, use the
    --     @DescribeTopicDetectionJob@ operation.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier generated for the job. To get the status of the job, use
    -- this identifier with the @DescribeTopicDetectionJob@ operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTopicsDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'startTopicsDetectionJobResponse_jobStatus' - The status of the job:
--
-- -   SUBMITTED - The job has been received and is queued for processing.
--
-- -   IN_PROGRESS - Amazon Comprehend is processing the job.
--
-- -   COMPLETED - The job was successfully completed and the output is
--     available.
--
-- -   FAILED - The job did not complete. To get details, use the
--     @DescribeTopicDetectionJob@ operation.
--
-- 'jobId', 'startTopicsDetectionJobResponse_jobId' - The identifier generated for the job. To get the status of the job, use
-- this identifier with the @DescribeTopicDetectionJob@ operation.
--
-- 'httpStatus', 'startTopicsDetectionJobResponse_httpStatus' - The response's http status code.
newStartTopicsDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTopicsDetectionJobResponse
newStartTopicsDetectionJobResponse pHttpStatus_ =
  StartTopicsDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the job:
--
-- -   SUBMITTED - The job has been received and is queued for processing.
--
-- -   IN_PROGRESS - Amazon Comprehend is processing the job.
--
-- -   COMPLETED - The job was successfully completed and the output is
--     available.
--
-- -   FAILED - The job did not complete. To get details, use the
--     @DescribeTopicDetectionJob@ operation.
startTopicsDetectionJobResponse_jobStatus :: Lens.Lens' StartTopicsDetectionJobResponse (Prelude.Maybe JobStatus)
startTopicsDetectionJobResponse_jobStatus = Lens.lens (\StartTopicsDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartTopicsDetectionJobResponse' {} a -> s {jobStatus = a} :: StartTopicsDetectionJobResponse)

-- | The identifier generated for the job. To get the status of the job, use
-- this identifier with the @DescribeTopicDetectionJob@ operation.
startTopicsDetectionJobResponse_jobId :: Lens.Lens' StartTopicsDetectionJobResponse (Prelude.Maybe Prelude.Text)
startTopicsDetectionJobResponse_jobId = Lens.lens (\StartTopicsDetectionJobResponse' {jobId} -> jobId) (\s@StartTopicsDetectionJobResponse' {} a -> s {jobId = a} :: StartTopicsDetectionJobResponse)

-- | The response's http status code.
startTopicsDetectionJobResponse_httpStatus :: Lens.Lens' StartTopicsDetectionJobResponse Prelude.Int
startTopicsDetectionJobResponse_httpStatus = Lens.lens (\StartTopicsDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartTopicsDetectionJobResponse' {} a -> s {httpStatus = a} :: StartTopicsDetectionJobResponse)

instance
  Prelude.NFData
    StartTopicsDetectionJobResponse
