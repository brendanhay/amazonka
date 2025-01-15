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
-- Module      : Amazonka.ComprehendMedical.StartEntitiesDetectionV2Job
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous medical entity detection job for a collection of
-- documents. Use the @DescribeEntitiesDetectionV2Job@ operation to track
-- the status of a job.
module Amazonka.ComprehendMedical.StartEntitiesDetectionV2Job
  ( -- * Creating a Request
    StartEntitiesDetectionV2Job (..),
    newStartEntitiesDetectionV2Job,

    -- * Request Lenses
    startEntitiesDetectionV2Job_clientRequestToken,
    startEntitiesDetectionV2Job_jobName,
    startEntitiesDetectionV2Job_kmsKey,
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

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartEntitiesDetectionV2Job' smart constructor.
data StartEntitiesDetectionV2Job = StartEntitiesDetectionV2Job'
  { -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Comprehend Medical; generates one for you.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | An AWS Key Management Service key to encrypt your output files. If you
    -- do not specify a key, the files are written in plain text.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The input configuration that specifies the format and location of the
    -- input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | The output configuration that specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Comprehend Medical; read access to your input
    -- data. For more information, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
    dataAccessRoleArn :: Prelude.Text,
    -- | The language of the input documents. All documents must be in the same
    -- language. Comprehend Medical; processes files in US English (en).
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
-- 'clientRequestToken', 'startEntitiesDetectionV2Job_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Comprehend Medical; generates one for you.
--
-- 'jobName', 'startEntitiesDetectionV2Job_jobName' - The identifier of the job.
--
-- 'kmsKey', 'startEntitiesDetectionV2Job_kmsKey' - An AWS Key Management Service key to encrypt your output files. If you
-- do not specify a key, the files are written in plain text.
--
-- 'inputDataConfig', 'startEntitiesDetectionV2Job_inputDataConfig' - The input configuration that specifies the format and location of the
-- input data for the job.
--
-- 'outputDataConfig', 'startEntitiesDetectionV2Job_outputDataConfig' - The output configuration that specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startEntitiesDetectionV2Job_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Comprehend Medical; read access to your input
-- data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
--
-- 'languageCode', 'startEntitiesDetectionV2Job_languageCode' - The language of the input documents. All documents must be in the same
-- language. Comprehend Medical; processes files in US English (en).
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
      { clientRequestToken =
          Prelude.Nothing,
        jobName = Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_
      }

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Comprehend Medical; generates one for you.
startEntitiesDetectionV2Job_clientRequestToken :: Lens.Lens' StartEntitiesDetectionV2Job (Prelude.Maybe Prelude.Text)
startEntitiesDetectionV2Job_clientRequestToken = Lens.lens (\StartEntitiesDetectionV2Job' {clientRequestToken} -> clientRequestToken) (\s@StartEntitiesDetectionV2Job' {} a -> s {clientRequestToken = a} :: StartEntitiesDetectionV2Job)

-- | The identifier of the job.
startEntitiesDetectionV2Job_jobName :: Lens.Lens' StartEntitiesDetectionV2Job (Prelude.Maybe Prelude.Text)
startEntitiesDetectionV2Job_jobName = Lens.lens (\StartEntitiesDetectionV2Job' {jobName} -> jobName) (\s@StartEntitiesDetectionV2Job' {} a -> s {jobName = a} :: StartEntitiesDetectionV2Job)

-- | An AWS Key Management Service key to encrypt your output files. If you
-- do not specify a key, the files are written in plain text.
startEntitiesDetectionV2Job_kmsKey :: Lens.Lens' StartEntitiesDetectionV2Job (Prelude.Maybe Prelude.Text)
startEntitiesDetectionV2Job_kmsKey = Lens.lens (\StartEntitiesDetectionV2Job' {kmsKey} -> kmsKey) (\s@StartEntitiesDetectionV2Job' {} a -> s {kmsKey = a} :: StartEntitiesDetectionV2Job)

-- | The input configuration that specifies the format and location of the
-- input data for the job.
startEntitiesDetectionV2Job_inputDataConfig :: Lens.Lens' StartEntitiesDetectionV2Job InputDataConfig
startEntitiesDetectionV2Job_inputDataConfig = Lens.lens (\StartEntitiesDetectionV2Job' {inputDataConfig} -> inputDataConfig) (\s@StartEntitiesDetectionV2Job' {} a -> s {inputDataConfig = a} :: StartEntitiesDetectionV2Job)

-- | The output configuration that specifies where to send the output files.
startEntitiesDetectionV2Job_outputDataConfig :: Lens.Lens' StartEntitiesDetectionV2Job OutputDataConfig
startEntitiesDetectionV2Job_outputDataConfig = Lens.lens (\StartEntitiesDetectionV2Job' {outputDataConfig} -> outputDataConfig) (\s@StartEntitiesDetectionV2Job' {} a -> s {outputDataConfig = a} :: StartEntitiesDetectionV2Job)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Comprehend Medical; read access to your input
-- data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
startEntitiesDetectionV2Job_dataAccessRoleArn :: Lens.Lens' StartEntitiesDetectionV2Job Prelude.Text
startEntitiesDetectionV2Job_dataAccessRoleArn = Lens.lens (\StartEntitiesDetectionV2Job' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartEntitiesDetectionV2Job' {} a -> s {dataAccessRoleArn = a} :: StartEntitiesDetectionV2Job)

-- | The language of the input documents. All documents must be in the same
-- language. Comprehend Medical; processes files in US English (en).
startEntitiesDetectionV2Job_languageCode :: Lens.Lens' StartEntitiesDetectionV2Job LanguageCode
startEntitiesDetectionV2Job_languageCode = Lens.lens (\StartEntitiesDetectionV2Job' {languageCode} -> languageCode) (\s@StartEntitiesDetectionV2Job' {} a -> s {languageCode = a} :: StartEntitiesDetectionV2Job)

instance Core.AWSRequest StartEntitiesDetectionV2Job where
  type
    AWSResponse StartEntitiesDetectionV2Job =
      StartEntitiesDetectionV2JobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartEntitiesDetectionV2JobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartEntitiesDetectionV2Job where
  hashWithSalt _salt StartEntitiesDetectionV2Job' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData StartEntitiesDetectionV2Job where
  rnf StartEntitiesDetectionV2Job' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf jobName `Prelude.seq`
        Prelude.rnf kmsKey `Prelude.seq`
          Prelude.rnf inputDataConfig `Prelude.seq`
            Prelude.rnf outputDataConfig `Prelude.seq`
              Prelude.rnf dataAccessRoleArn `Prelude.seq`
                Prelude.rnf languageCode

instance Data.ToHeaders StartEntitiesDetectionV2Job where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.StartEntitiesDetectionV2Job" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartEntitiesDetectionV2Job where
  toJSON StartEntitiesDetectionV2Job' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("JobName" Data..=) Prelude.<$> jobName,
            ("KMSKey" Data..=) Prelude.<$> kmsKey,
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath StartEntitiesDetectionV2Job where
  toPath = Prelude.const "/"

instance Data.ToQuery StartEntitiesDetectionV2Job where
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
  where
  rnf StartEntitiesDetectionV2JobResponse' {..} =
    Prelude.rnf jobId `Prelude.seq`
      Prelude.rnf httpStatus
