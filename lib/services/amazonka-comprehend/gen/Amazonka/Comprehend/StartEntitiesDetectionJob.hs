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
-- Module      : Amazonka.Comprehend.StartEntitiesDetectionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous entity detection job for a collection of
-- documents. Use the operation to track the status of a job.
--
-- This API can be used for either standard entity detection or custom
-- entity recognition. In order to be used for custom entity recognition,
-- the optional @EntityRecognizerArn@ must be used in order to provide
-- access to the recognizer being used to detect the custom entity.
module Amazonka.Comprehend.StartEntitiesDetectionJob
  ( -- * Creating a Request
    StartEntitiesDetectionJob (..),
    newStartEntitiesDetectionJob,

    -- * Request Lenses
    startEntitiesDetectionJob_clientRequestToken,
    startEntitiesDetectionJob_entityRecognizerArn,
    startEntitiesDetectionJob_jobName,
    startEntitiesDetectionJob_tags,
    startEntitiesDetectionJob_volumeKmsKeyId,
    startEntitiesDetectionJob_vpcConfig,
    startEntitiesDetectionJob_inputDataConfig,
    startEntitiesDetectionJob_outputDataConfig,
    startEntitiesDetectionJob_dataAccessRoleArn,
    startEntitiesDetectionJob_languageCode,

    -- * Destructuring the Response
    StartEntitiesDetectionJobResponse (..),
    newStartEntitiesDetectionJobResponse,

    -- * Response Lenses
    startEntitiesDetectionJobResponse_jobArn,
    startEntitiesDetectionJobResponse_jobId,
    startEntitiesDetectionJobResponse_jobStatus,
    startEntitiesDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartEntitiesDetectionJob' smart constructor.
data StartEntitiesDetectionJob = StartEntitiesDetectionJob'
  { -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the specific entity
    -- recognizer to be used by the @StartEntitiesDetectionJob@. This ARN is
    -- optional and is only used for a custom entity recognition job.
    entityRecognizerArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Tags to be associated with the entities detection job. A tag is a
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
    -- (VPC) containing the resources you are using for your entity detection
    -- job. For more information, see
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
    -- | The language of the input documents. All documents must be in the same
    -- language. You can specify any of the languages supported by Amazon
    -- Comprehend. If custom entities recognition is used, this parameter is
    -- ignored and the language used for training the model is used instead.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEntitiesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startEntitiesDetectionJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'entityRecognizerArn', 'startEntitiesDetectionJob_entityRecognizerArn' - The Amazon Resource Name (ARN) that identifies the specific entity
-- recognizer to be used by the @StartEntitiesDetectionJob@. This ARN is
-- optional and is only used for a custom entity recognition job.
--
-- 'jobName', 'startEntitiesDetectionJob_jobName' - The identifier of the job.
--
-- 'tags', 'startEntitiesDetectionJob_tags' - Tags to be associated with the entities detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
--
-- 'volumeKmsKeyId', 'startEntitiesDetectionJob_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'vpcConfig', 'startEntitiesDetectionJob_vpcConfig' - Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your entity detection
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'inputDataConfig', 'startEntitiesDetectionJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startEntitiesDetectionJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startEntitiesDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
--
-- 'languageCode', 'startEntitiesDetectionJob_languageCode' - The language of the input documents. All documents must be in the same
-- language. You can specify any of the languages supported by Amazon
-- Comprehend. If custom entities recognition is used, this parameter is
-- ignored and the language used for training the model is used instead.
newStartEntitiesDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartEntitiesDetectionJob
newStartEntitiesDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartEntitiesDetectionJob'
      { clientRequestToken =
          Prelude.Nothing,
        entityRecognizerArn = Prelude.Nothing,
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
startEntitiesDetectionJob_clientRequestToken :: Lens.Lens' StartEntitiesDetectionJob (Prelude.Maybe Prelude.Text)
startEntitiesDetectionJob_clientRequestToken = Lens.lens (\StartEntitiesDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartEntitiesDetectionJob' {} a -> s {clientRequestToken = a} :: StartEntitiesDetectionJob)

-- | The Amazon Resource Name (ARN) that identifies the specific entity
-- recognizer to be used by the @StartEntitiesDetectionJob@. This ARN is
-- optional and is only used for a custom entity recognition job.
startEntitiesDetectionJob_entityRecognizerArn :: Lens.Lens' StartEntitiesDetectionJob (Prelude.Maybe Prelude.Text)
startEntitiesDetectionJob_entityRecognizerArn = Lens.lens (\StartEntitiesDetectionJob' {entityRecognizerArn} -> entityRecognizerArn) (\s@StartEntitiesDetectionJob' {} a -> s {entityRecognizerArn = a} :: StartEntitiesDetectionJob)

-- | The identifier of the job.
startEntitiesDetectionJob_jobName :: Lens.Lens' StartEntitiesDetectionJob (Prelude.Maybe Prelude.Text)
startEntitiesDetectionJob_jobName = Lens.lens (\StartEntitiesDetectionJob' {jobName} -> jobName) (\s@StartEntitiesDetectionJob' {} a -> s {jobName = a} :: StartEntitiesDetectionJob)

-- | Tags to be associated with the entities detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
startEntitiesDetectionJob_tags :: Lens.Lens' StartEntitiesDetectionJob (Prelude.Maybe [Tag])
startEntitiesDetectionJob_tags = Lens.lens (\StartEntitiesDetectionJob' {tags} -> tags) (\s@StartEntitiesDetectionJob' {} a -> s {tags = a} :: StartEntitiesDetectionJob) Prelude.. Lens.mapping Lens.coerced

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
startEntitiesDetectionJob_volumeKmsKeyId :: Lens.Lens' StartEntitiesDetectionJob (Prelude.Maybe Prelude.Text)
startEntitiesDetectionJob_volumeKmsKeyId = Lens.lens (\StartEntitiesDetectionJob' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@StartEntitiesDetectionJob' {} a -> s {volumeKmsKeyId = a} :: StartEntitiesDetectionJob)

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for your entity detection
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
startEntitiesDetectionJob_vpcConfig :: Lens.Lens' StartEntitiesDetectionJob (Prelude.Maybe VpcConfig)
startEntitiesDetectionJob_vpcConfig = Lens.lens (\StartEntitiesDetectionJob' {vpcConfig} -> vpcConfig) (\s@StartEntitiesDetectionJob' {} a -> s {vpcConfig = a} :: StartEntitiesDetectionJob)

-- | Specifies the format and location of the input data for the job.
startEntitiesDetectionJob_inputDataConfig :: Lens.Lens' StartEntitiesDetectionJob InputDataConfig
startEntitiesDetectionJob_inputDataConfig = Lens.lens (\StartEntitiesDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartEntitiesDetectionJob' {} a -> s {inputDataConfig = a} :: StartEntitiesDetectionJob)

-- | Specifies where to send the output files.
startEntitiesDetectionJob_outputDataConfig :: Lens.Lens' StartEntitiesDetectionJob OutputDataConfig
startEntitiesDetectionJob_outputDataConfig = Lens.lens (\StartEntitiesDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartEntitiesDetectionJob' {} a -> s {outputDataConfig = a} :: StartEntitiesDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions>.
startEntitiesDetectionJob_dataAccessRoleArn :: Lens.Lens' StartEntitiesDetectionJob Prelude.Text
startEntitiesDetectionJob_dataAccessRoleArn = Lens.lens (\StartEntitiesDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartEntitiesDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartEntitiesDetectionJob)

-- | The language of the input documents. All documents must be in the same
-- language. You can specify any of the languages supported by Amazon
-- Comprehend. If custom entities recognition is used, this parameter is
-- ignored and the language used for training the model is used instead.
startEntitiesDetectionJob_languageCode :: Lens.Lens' StartEntitiesDetectionJob LanguageCode
startEntitiesDetectionJob_languageCode = Lens.lens (\StartEntitiesDetectionJob' {languageCode} -> languageCode) (\s@StartEntitiesDetectionJob' {} a -> s {languageCode = a} :: StartEntitiesDetectionJob)

instance Core.AWSRequest StartEntitiesDetectionJob where
  type
    AWSResponse StartEntitiesDetectionJob =
      StartEntitiesDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartEntitiesDetectionJobResponse'
            Prelude.<$> (x Data..?> "JobArn")
            Prelude.<*> (x Data..?> "JobId")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartEntitiesDetectionJob where
  hashWithSalt _salt StartEntitiesDetectionJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` entityRecognizerArn
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData StartEntitiesDetectionJob where
  rnf StartEntitiesDetectionJob' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf entityRecognizerArn
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders StartEntitiesDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.StartEntitiesDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartEntitiesDetectionJob where
  toJSON StartEntitiesDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("EntityRecognizerArn" Data..=)
              Prelude.<$> entityRecognizerArn,
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

instance Data.ToPath StartEntitiesDetectionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartEntitiesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartEntitiesDetectionJobResponse' smart constructor.
data StartEntitiesDetectionJobResponse = StartEntitiesDetectionJobResponse'
  { -- | The Amazon Resource Name (ARN) of the entities detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:entities-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier generated for the job. To get the status of job, use this
    -- identifier with the operation.
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
-- Create a value of 'StartEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobArn', 'startEntitiesDetectionJobResponse_jobArn' - The Amazon Resource Name (ARN) of the entities detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:entities-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobId', 'startEntitiesDetectionJobResponse_jobId' - The identifier generated for the job. To get the status of job, use this
-- identifier with the operation.
--
-- 'jobStatus', 'startEntitiesDetectionJobResponse_jobStatus' - The status of the job.
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
-- -   STOP_REQUESTED - Amazon Comprehend has received a stop request for
--     the job and is processing the request.
--
-- -   STOPPED - The job was successfully stopped without completing.
--
-- 'httpStatus', 'startEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newStartEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartEntitiesDetectionJobResponse
newStartEntitiesDetectionJobResponse pHttpStatus_ =
  StartEntitiesDetectionJobResponse'
    { jobArn =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the entities detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:entities-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
startEntitiesDetectionJobResponse_jobArn :: Lens.Lens' StartEntitiesDetectionJobResponse (Prelude.Maybe Prelude.Text)
startEntitiesDetectionJobResponse_jobArn = Lens.lens (\StartEntitiesDetectionJobResponse' {jobArn} -> jobArn) (\s@StartEntitiesDetectionJobResponse' {} a -> s {jobArn = a} :: StartEntitiesDetectionJobResponse)

-- | The identifier generated for the job. To get the status of job, use this
-- identifier with the operation.
startEntitiesDetectionJobResponse_jobId :: Lens.Lens' StartEntitiesDetectionJobResponse (Prelude.Maybe Prelude.Text)
startEntitiesDetectionJobResponse_jobId = Lens.lens (\StartEntitiesDetectionJobResponse' {jobId} -> jobId) (\s@StartEntitiesDetectionJobResponse' {} a -> s {jobId = a} :: StartEntitiesDetectionJobResponse)

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
--
-- -   STOP_REQUESTED - Amazon Comprehend has received a stop request for
--     the job and is processing the request.
--
-- -   STOPPED - The job was successfully stopped without completing.
startEntitiesDetectionJobResponse_jobStatus :: Lens.Lens' StartEntitiesDetectionJobResponse (Prelude.Maybe JobStatus)
startEntitiesDetectionJobResponse_jobStatus = Lens.lens (\StartEntitiesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartEntitiesDetectionJobResponse' {} a -> s {jobStatus = a} :: StartEntitiesDetectionJobResponse)

-- | The response's http status code.
startEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' StartEntitiesDetectionJobResponse Prelude.Int
startEntitiesDetectionJobResponse_httpStatus = Lens.lens (\StartEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: StartEntitiesDetectionJobResponse)

instance
  Prelude.NFData
    StartEntitiesDetectionJobResponse
  where
  rnf StartEntitiesDetectionJobResponse' {..} =
    Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf httpStatus
