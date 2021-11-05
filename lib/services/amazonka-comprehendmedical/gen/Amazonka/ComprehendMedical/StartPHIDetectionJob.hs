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
-- Module      : Amazonka.ComprehendMedical.StartPHIDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous job to detect protected health information (PHI).
-- Use the @DescribePHIDetectionJob@ operation to track the status of a
-- job.
module Amazonka.ComprehendMedical.StartPHIDetectionJob
  ( -- * Creating a Request
    StartPHIDetectionJob (..),
    newStartPHIDetectionJob,

    -- * Request Lenses
    startPHIDetectionJob_kmsKey,
    startPHIDetectionJob_jobName,
    startPHIDetectionJob_clientRequestToken,
    startPHIDetectionJob_inputDataConfig,
    startPHIDetectionJob_outputDataConfig,
    startPHIDetectionJob_dataAccessRoleArn,
    startPHIDetectionJob_languageCode,

    -- * Destructuring the Response
    StartPHIDetectionJobResponse (..),
    newStartPHIDetectionJobResponse,

    -- * Response Lenses
    startPHIDetectionJobResponse_jobId,
    startPHIDetectionJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartPHIDetectionJob' smart constructor.
data StartPHIDetectionJob = StartPHIDetectionJob'
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
-- Create a value of 'StartPHIDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'startPHIDetectionJob_kmsKey' - An AWS Key Management Service key to encrypt your output files. If you
-- do not specify a key, the files are written in plain text.
--
-- 'jobName', 'startPHIDetectionJob_jobName' - The identifier of the job.
--
-- 'clientRequestToken', 'startPHIDetectionJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend Medical generates one.
--
-- 'inputDataConfig', 'startPHIDetectionJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startPHIDetectionJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startPHIDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend Medical read access to your
-- input data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
--
-- 'languageCode', 'startPHIDetectionJob_languageCode' - The language of the input documents. All documents must be in the same
-- language.
newStartPHIDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartPHIDetectionJob
newStartPHIDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartPHIDetectionJob'
      { kmsKey = Prelude.Nothing,
        jobName = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_
      }

-- | An AWS Key Management Service key to encrypt your output files. If you
-- do not specify a key, the files are written in plain text.
startPHIDetectionJob_kmsKey :: Lens.Lens' StartPHIDetectionJob (Prelude.Maybe Prelude.Text)
startPHIDetectionJob_kmsKey = Lens.lens (\StartPHIDetectionJob' {kmsKey} -> kmsKey) (\s@StartPHIDetectionJob' {} a -> s {kmsKey = a} :: StartPHIDetectionJob)

-- | The identifier of the job.
startPHIDetectionJob_jobName :: Lens.Lens' StartPHIDetectionJob (Prelude.Maybe Prelude.Text)
startPHIDetectionJob_jobName = Lens.lens (\StartPHIDetectionJob' {jobName} -> jobName) (\s@StartPHIDetectionJob' {} a -> s {jobName = a} :: StartPHIDetectionJob)

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend Medical generates one.
startPHIDetectionJob_clientRequestToken :: Lens.Lens' StartPHIDetectionJob (Prelude.Maybe Prelude.Text)
startPHIDetectionJob_clientRequestToken = Lens.lens (\StartPHIDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartPHIDetectionJob' {} a -> s {clientRequestToken = a} :: StartPHIDetectionJob)

-- | Specifies the format and location of the input data for the job.
startPHIDetectionJob_inputDataConfig :: Lens.Lens' StartPHIDetectionJob InputDataConfig
startPHIDetectionJob_inputDataConfig = Lens.lens (\StartPHIDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartPHIDetectionJob' {} a -> s {inputDataConfig = a} :: StartPHIDetectionJob)

-- | Specifies where to send the output files.
startPHIDetectionJob_outputDataConfig :: Lens.Lens' StartPHIDetectionJob OutputDataConfig
startPHIDetectionJob_outputDataConfig = Lens.lens (\StartPHIDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartPHIDetectionJob' {} a -> s {outputDataConfig = a} :: StartPHIDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend Medical read access to your
-- input data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
startPHIDetectionJob_dataAccessRoleArn :: Lens.Lens' StartPHIDetectionJob Prelude.Text
startPHIDetectionJob_dataAccessRoleArn = Lens.lens (\StartPHIDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartPHIDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartPHIDetectionJob)

-- | The language of the input documents. All documents must be in the same
-- language.
startPHIDetectionJob_languageCode :: Lens.Lens' StartPHIDetectionJob LanguageCode
startPHIDetectionJob_languageCode = Lens.lens (\StartPHIDetectionJob' {languageCode} -> languageCode) (\s@StartPHIDetectionJob' {} a -> s {languageCode = a} :: StartPHIDetectionJob)

instance Core.AWSRequest StartPHIDetectionJob where
  type
    AWSResponse StartPHIDetectionJob =
      StartPHIDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPHIDetectionJobResponse'
            Prelude.<$> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartPHIDetectionJob

instance Prelude.NFData StartPHIDetectionJob

instance Core.ToHeaders StartPHIDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComprehendMedical_20181030.StartPHIDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartPHIDetectionJob where
  toJSON StartPHIDetectionJob' {..} =
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

instance Core.ToPath StartPHIDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartPHIDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartPHIDetectionJobResponse' smart constructor.
data StartPHIDetectionJobResponse = StartPHIDetectionJobResponse'
  { -- | The identifier generated for the job. To get the status of a job, use
    -- this identifier with the @DescribePHIDetectionJob@ operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPHIDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startPHIDetectionJobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this identifier with the @DescribePHIDetectionJob@ operation.
--
-- 'httpStatus', 'startPHIDetectionJobResponse_httpStatus' - The response's http status code.
newStartPHIDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartPHIDetectionJobResponse
newStartPHIDetectionJobResponse pHttpStatus_ =
  StartPHIDetectionJobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use
-- this identifier with the @DescribePHIDetectionJob@ operation.
startPHIDetectionJobResponse_jobId :: Lens.Lens' StartPHIDetectionJobResponse (Prelude.Maybe Prelude.Text)
startPHIDetectionJobResponse_jobId = Lens.lens (\StartPHIDetectionJobResponse' {jobId} -> jobId) (\s@StartPHIDetectionJobResponse' {} a -> s {jobId = a} :: StartPHIDetectionJobResponse)

-- | The response's http status code.
startPHIDetectionJobResponse_httpStatus :: Lens.Lens' StartPHIDetectionJobResponse Prelude.Int
startPHIDetectionJobResponse_httpStatus = Lens.lens (\StartPHIDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartPHIDetectionJobResponse' {} a -> s {httpStatus = a} :: StartPHIDetectionJobResponse)

instance Prelude.NFData StartPHIDetectionJobResponse
