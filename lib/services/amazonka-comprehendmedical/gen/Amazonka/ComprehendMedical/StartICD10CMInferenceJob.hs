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
-- Module      : Amazonka.ComprehendMedical.StartICD10CMInferenceJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous job to detect medical conditions and link them to
-- the ICD-10-CM ontology. Use the @DescribeICD10CMInferenceJob@ operation
-- to track the status of a job.
module Amazonka.ComprehendMedical.StartICD10CMInferenceJob
  ( -- * Creating a Request
    StartICD10CMInferenceJob (..),
    newStartICD10CMInferenceJob,

    -- * Request Lenses
    startICD10CMInferenceJob_clientRequestToken,
    startICD10CMInferenceJob_jobName,
    startICD10CMInferenceJob_kmsKey,
    startICD10CMInferenceJob_inputDataConfig,
    startICD10CMInferenceJob_outputDataConfig,
    startICD10CMInferenceJob_dataAccessRoleArn,
    startICD10CMInferenceJob_languageCode,

    -- * Destructuring the Response
    StartICD10CMInferenceJobResponse (..),
    newStartICD10CMInferenceJobResponse,

    -- * Response Lenses
    startICD10CMInferenceJobResponse_jobId,
    startICD10CMInferenceJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartICD10CMInferenceJob' smart constructor.
data StartICD10CMInferenceJob = StartICD10CMInferenceJob'
  { -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Comprehend Medical; generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | An AWS Key Management Service key to encrypt your output files. If you
    -- do not specify a key, the files are written in plain text.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Comprehend Medical; read access to your input
    -- data. For more information, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
    dataAccessRoleArn :: Prelude.Text,
    -- | The language of the input documents. All documents must be in the same
    -- language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartICD10CMInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startICD10CMInferenceJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Comprehend Medical; generates one.
--
-- 'jobName', 'startICD10CMInferenceJob_jobName' - The identifier of the job.
--
-- 'kmsKey', 'startICD10CMInferenceJob_kmsKey' - An AWS Key Management Service key to encrypt your output files. If you
-- do not specify a key, the files are written in plain text.
--
-- 'inputDataConfig', 'startICD10CMInferenceJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startICD10CMInferenceJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startICD10CMInferenceJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Comprehend Medical; read access to your input
-- data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
--
-- 'languageCode', 'startICD10CMInferenceJob_languageCode' - The language of the input documents. All documents must be in the same
-- language.
newStartICD10CMInferenceJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartICD10CMInferenceJob
newStartICD10CMInferenceJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartICD10CMInferenceJob'
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
-- request token, Comprehend Medical; generates one.
startICD10CMInferenceJob_clientRequestToken :: Lens.Lens' StartICD10CMInferenceJob (Prelude.Maybe Prelude.Text)
startICD10CMInferenceJob_clientRequestToken = Lens.lens (\StartICD10CMInferenceJob' {clientRequestToken} -> clientRequestToken) (\s@StartICD10CMInferenceJob' {} a -> s {clientRequestToken = a} :: StartICD10CMInferenceJob)

-- | The identifier of the job.
startICD10CMInferenceJob_jobName :: Lens.Lens' StartICD10CMInferenceJob (Prelude.Maybe Prelude.Text)
startICD10CMInferenceJob_jobName = Lens.lens (\StartICD10CMInferenceJob' {jobName} -> jobName) (\s@StartICD10CMInferenceJob' {} a -> s {jobName = a} :: StartICD10CMInferenceJob)

-- | An AWS Key Management Service key to encrypt your output files. If you
-- do not specify a key, the files are written in plain text.
startICD10CMInferenceJob_kmsKey :: Lens.Lens' StartICD10CMInferenceJob (Prelude.Maybe Prelude.Text)
startICD10CMInferenceJob_kmsKey = Lens.lens (\StartICD10CMInferenceJob' {kmsKey} -> kmsKey) (\s@StartICD10CMInferenceJob' {} a -> s {kmsKey = a} :: StartICD10CMInferenceJob)

-- | Specifies the format and location of the input data for the job.
startICD10CMInferenceJob_inputDataConfig :: Lens.Lens' StartICD10CMInferenceJob InputDataConfig
startICD10CMInferenceJob_inputDataConfig = Lens.lens (\StartICD10CMInferenceJob' {inputDataConfig} -> inputDataConfig) (\s@StartICD10CMInferenceJob' {} a -> s {inputDataConfig = a} :: StartICD10CMInferenceJob)

-- | Specifies where to send the output files.
startICD10CMInferenceJob_outputDataConfig :: Lens.Lens' StartICD10CMInferenceJob OutputDataConfig
startICD10CMInferenceJob_outputDataConfig = Lens.lens (\StartICD10CMInferenceJob' {outputDataConfig} -> outputDataConfig) (\s@StartICD10CMInferenceJob' {} a -> s {outputDataConfig = a} :: StartICD10CMInferenceJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Comprehend Medical; read access to your input
-- data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
startICD10CMInferenceJob_dataAccessRoleArn :: Lens.Lens' StartICD10CMInferenceJob Prelude.Text
startICD10CMInferenceJob_dataAccessRoleArn = Lens.lens (\StartICD10CMInferenceJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartICD10CMInferenceJob' {} a -> s {dataAccessRoleArn = a} :: StartICD10CMInferenceJob)

-- | The language of the input documents. All documents must be in the same
-- language.
startICD10CMInferenceJob_languageCode :: Lens.Lens' StartICD10CMInferenceJob LanguageCode
startICD10CMInferenceJob_languageCode = Lens.lens (\StartICD10CMInferenceJob' {languageCode} -> languageCode) (\s@StartICD10CMInferenceJob' {} a -> s {languageCode = a} :: StartICD10CMInferenceJob)

instance Core.AWSRequest StartICD10CMInferenceJob where
  type
    AWSResponse StartICD10CMInferenceJob =
      StartICD10CMInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartICD10CMInferenceJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartICD10CMInferenceJob where
  hashWithSalt _salt StartICD10CMInferenceJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData StartICD10CMInferenceJob where
  rnf StartICD10CMInferenceJob' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf jobName `Prelude.seq`
        Prelude.rnf kmsKey `Prelude.seq`
          Prelude.rnf inputDataConfig `Prelude.seq`
            Prelude.rnf outputDataConfig `Prelude.seq`
              Prelude.rnf dataAccessRoleArn `Prelude.seq`
                Prelude.rnf languageCode

instance Data.ToHeaders StartICD10CMInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.StartICD10CMInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartICD10CMInferenceJob where
  toJSON StartICD10CMInferenceJob' {..} =
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

instance Data.ToPath StartICD10CMInferenceJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartICD10CMInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartICD10CMInferenceJobResponse' smart constructor.
data StartICD10CMInferenceJobResponse = StartICD10CMInferenceJobResponse'
  { -- | The identifier generated for the job. To get the status of a job, use
    -- this identifier with the @StartICD10CMInferenceJob@ operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartICD10CMInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startICD10CMInferenceJobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this identifier with the @StartICD10CMInferenceJob@ operation.
--
-- 'httpStatus', 'startICD10CMInferenceJobResponse_httpStatus' - The response's http status code.
newStartICD10CMInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartICD10CMInferenceJobResponse
newStartICD10CMInferenceJobResponse pHttpStatus_ =
  StartICD10CMInferenceJobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use
-- this identifier with the @StartICD10CMInferenceJob@ operation.
startICD10CMInferenceJobResponse_jobId :: Lens.Lens' StartICD10CMInferenceJobResponse (Prelude.Maybe Prelude.Text)
startICD10CMInferenceJobResponse_jobId = Lens.lens (\StartICD10CMInferenceJobResponse' {jobId} -> jobId) (\s@StartICD10CMInferenceJobResponse' {} a -> s {jobId = a} :: StartICD10CMInferenceJobResponse)

-- | The response's http status code.
startICD10CMInferenceJobResponse_httpStatus :: Lens.Lens' StartICD10CMInferenceJobResponse Prelude.Int
startICD10CMInferenceJobResponse_httpStatus = Lens.lens (\StartICD10CMInferenceJobResponse' {httpStatus} -> httpStatus) (\s@StartICD10CMInferenceJobResponse' {} a -> s {httpStatus = a} :: StartICD10CMInferenceJobResponse)

instance
  Prelude.NFData
    StartICD10CMInferenceJobResponse
  where
  rnf StartICD10CMInferenceJobResponse' {..} =
    Prelude.rnf jobId `Prelude.seq`
      Prelude.rnf httpStatus
