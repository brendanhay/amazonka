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
-- Module      : Network.AWS.ComprehendMedical.StartEntitiesDetectionV2Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous medical entity detection job for a collection of
-- documents. Use the @DescribeEntitiesDetectionV2Job@ operation to track
-- the status of a job.
module Network.AWS.ComprehendMedical.StartEntitiesDetectionV2Job
  ( -- * Creating a Request
    StartEntitiesDetectionV2Job (..),
    newStartEntitiesDetectionV2Job,

    -- * Request Lenses
    startEntitiesDetectionV2Job_kmsKey,
    startEntitiesDetectionV2Job_jobName,
    startEntitiesDetectionV2Job_clientRequestToken,
    startEntitiesDetectionV2Job_inputDataConfig,
    startEntitiesDetectionV2Job_outputDataConfig,
    startEntitiesDetectionV2Job_dataAccessRoleArn,
    startEntitiesDetectionV2Job_languageCode,

    -- * Destructuring the Response
    StartEntitiesDetectionV2JobResponse (..),
    newStartEntitiesDetectionV2JobResponse,

    -- * Response Lenses
    startEntitiesDetectionV2JobResponse_jobId,
    startEntitiesDetectionV2JobResponse_httpStatus,
  )
where

import Network.AWS.ComprehendMedical.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartEntitiesDetectionV2Job' smart constructor.
data StartEntitiesDetectionV2Job = StartEntitiesDetectionV2Job'
  { -- | An AWS Key Management Service key to encrypt your output files. If you
    -- do not specify a key, the files are written in plain text.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend Medical generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend Medical read access to your
    -- input data. For more information, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
    dataAccessRoleArn :: Prelude.Text,
    -- | The language of the input documents. All documents must be in the same
    -- language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEntitiesDetectionV2Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'startEntitiesDetectionV2Job_kmsKey' - An AWS Key Management Service key to encrypt your output files. If you
-- do not specify a key, the files are written in plain text.
--
-- 'jobName', 'startEntitiesDetectionV2Job_jobName' - The identifier of the job.
--
-- 'clientRequestToken', 'startEntitiesDetectionV2Job_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend Medical generates one.
--
-- 'inputDataConfig', 'startEntitiesDetectionV2Job_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startEntitiesDetectionV2Job_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startEntitiesDetectionV2Job_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend Medical read access to your
-- input data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
--
-- 'languageCode', 'startEntitiesDetectionV2Job_languageCode' - The language of the input documents. All documents must be in the same
-- language.
newStartEntitiesDetectionV2Job ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartEntitiesDetectionV2Job
newStartEntitiesDetectionV2Job
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartEntitiesDetectionV2Job'
      { kmsKey =
          Prelude.Nothing,
        jobName = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_
      }

-- | An AWS Key Management Service key to encrypt your output files. If you
-- do not specify a key, the files are written in plain text.
startEntitiesDetectionV2Job_kmsKey :: Lens.Lens' StartEntitiesDetectionV2Job (Prelude.Maybe Prelude.Text)
startEntitiesDetectionV2Job_kmsKey = Lens.lens (\StartEntitiesDetectionV2Job' {kmsKey} -> kmsKey) (\s@StartEntitiesDetectionV2Job' {} a -> s {kmsKey = a} :: StartEntitiesDetectionV2Job)

-- | The identifier of the job.
startEntitiesDetectionV2Job_jobName :: Lens.Lens' StartEntitiesDetectionV2Job (Prelude.Maybe Prelude.Text)
startEntitiesDetectionV2Job_jobName = Lens.lens (\StartEntitiesDetectionV2Job' {jobName} -> jobName) (\s@StartEntitiesDetectionV2Job' {} a -> s {jobName = a} :: StartEntitiesDetectionV2Job)

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend Medical generates one.
startEntitiesDetectionV2Job_clientRequestToken :: Lens.Lens' StartEntitiesDetectionV2Job (Prelude.Maybe Prelude.Text)
startEntitiesDetectionV2Job_clientRequestToken = Lens.lens (\StartEntitiesDetectionV2Job' {clientRequestToken} -> clientRequestToken) (\s@StartEntitiesDetectionV2Job' {} a -> s {clientRequestToken = a} :: StartEntitiesDetectionV2Job)

-- | Specifies the format and location of the input data for the job.
startEntitiesDetectionV2Job_inputDataConfig :: Lens.Lens' StartEntitiesDetectionV2Job InputDataConfig
startEntitiesDetectionV2Job_inputDataConfig = Lens.lens (\StartEntitiesDetectionV2Job' {inputDataConfig} -> inputDataConfig) (\s@StartEntitiesDetectionV2Job' {} a -> s {inputDataConfig = a} :: StartEntitiesDetectionV2Job)

-- | Specifies where to send the output files.
startEntitiesDetectionV2Job_outputDataConfig :: Lens.Lens' StartEntitiesDetectionV2Job OutputDataConfig
startEntitiesDetectionV2Job_outputDataConfig = Lens.lens (\StartEntitiesDetectionV2Job' {outputDataConfig} -> outputDataConfig) (\s@StartEntitiesDetectionV2Job' {} a -> s {outputDataConfig = a} :: StartEntitiesDetectionV2Job)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend Medical read access to your
-- input data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
startEntitiesDetectionV2Job_dataAccessRoleArn :: Lens.Lens' StartEntitiesDetectionV2Job Prelude.Text
startEntitiesDetectionV2Job_dataAccessRoleArn = Lens.lens (\StartEntitiesDetectionV2Job' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartEntitiesDetectionV2Job' {} a -> s {dataAccessRoleArn = a} :: StartEntitiesDetectionV2Job)

-- | The language of the input documents. All documents must be in the same
-- language.
startEntitiesDetectionV2Job_languageCode :: Lens.Lens' StartEntitiesDetectionV2Job LanguageCode
startEntitiesDetectionV2Job_languageCode = Lens.lens (\StartEntitiesDetectionV2Job' {languageCode} -> languageCode) (\s@StartEntitiesDetectionV2Job' {} a -> s {languageCode = a} :: StartEntitiesDetectionV2Job)

instance Core.AWSRequest StartEntitiesDetectionV2Job where
  type
    AWSResponse StartEntitiesDetectionV2Job =
      StartEntitiesDetectionV2JobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartEntitiesDetectionV2JobResponse'
            Prelude.<$> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartEntitiesDetectionV2Job

instance Prelude.NFData StartEntitiesDetectionV2Job

instance Core.ToHeaders StartEntitiesDetectionV2Job where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComprehendMedical_20181030.StartEntitiesDetectionV2Job" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartEntitiesDetectionV2Job where
  toJSON StartEntitiesDetectionV2Job' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KMSKey" Core..=) Prelude.<$> kmsKey,
            ("JobName" Core..=) Prelude.<$> jobName,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath StartEntitiesDetectionV2Job where
  toPath = Prelude.const "/"

instance Core.ToQuery StartEntitiesDetectionV2Job where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartEntitiesDetectionV2JobResponse' smart constructor.
data StartEntitiesDetectionV2JobResponse = StartEntitiesDetectionV2JobResponse'
  { -- | The identifier generated for the job. To get the status of a job, use
    -- this identifier with the @DescribeEntitiesDetectionV2Job@ operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEntitiesDetectionV2JobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startEntitiesDetectionV2JobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this identifier with the @DescribeEntitiesDetectionV2Job@ operation.
--
-- 'httpStatus', 'startEntitiesDetectionV2JobResponse_httpStatus' - The response's http status code.
newStartEntitiesDetectionV2JobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartEntitiesDetectionV2JobResponse
newStartEntitiesDetectionV2JobResponse pHttpStatus_ =
  StartEntitiesDetectionV2JobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use
-- this identifier with the @DescribeEntitiesDetectionV2Job@ operation.
startEntitiesDetectionV2JobResponse_jobId :: Lens.Lens' StartEntitiesDetectionV2JobResponse (Prelude.Maybe Prelude.Text)
startEntitiesDetectionV2JobResponse_jobId = Lens.lens (\StartEntitiesDetectionV2JobResponse' {jobId} -> jobId) (\s@StartEntitiesDetectionV2JobResponse' {} a -> s {jobId = a} :: StartEntitiesDetectionV2JobResponse)

-- | The response's http status code.
startEntitiesDetectionV2JobResponse_httpStatus :: Lens.Lens' StartEntitiesDetectionV2JobResponse Prelude.Int
startEntitiesDetectionV2JobResponse_httpStatus = Lens.lens (\StartEntitiesDetectionV2JobResponse' {httpStatus} -> httpStatus) (\s@StartEntitiesDetectionV2JobResponse' {} a -> s {httpStatus = a} :: StartEntitiesDetectionV2JobResponse)

instance
  Prelude.NFData
    StartEntitiesDetectionV2JobResponse
