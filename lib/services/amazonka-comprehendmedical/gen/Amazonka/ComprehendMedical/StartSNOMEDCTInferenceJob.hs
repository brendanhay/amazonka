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
-- Module      : Amazonka.ComprehendMedical.StartSNOMEDCTInferenceJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous job to detect medical concepts and link them to
-- the SNOMED-CT ontology. Use the DescribeSNOMEDCTInferenceJob operation
-- to track the status of a job.
module Amazonka.ComprehendMedical.StartSNOMEDCTInferenceJob
  ( -- * Creating a Request
    StartSNOMEDCTInferenceJob (..),
    newStartSNOMEDCTInferenceJob,

    -- * Request Lenses
    startSNOMEDCTInferenceJob_clientRequestToken,
    startSNOMEDCTInferenceJob_jobName,
    startSNOMEDCTInferenceJob_kmsKey,
    startSNOMEDCTInferenceJob_inputDataConfig,
    startSNOMEDCTInferenceJob_outputDataConfig,
    startSNOMEDCTInferenceJob_dataAccessRoleArn,
    startSNOMEDCTInferenceJob_languageCode,

    -- * Destructuring the Response
    StartSNOMEDCTInferenceJobResponse (..),
    newStartSNOMEDCTInferenceJobResponse,

    -- * Response Lenses
    startSNOMEDCTInferenceJobResponse_jobId,
    startSNOMEDCTInferenceJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSNOMEDCTInferenceJob' smart constructor.
data StartSNOMEDCTInferenceJob = StartSNOMEDCTInferenceJob'
  { -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend Medical generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The user generated name the asynchronous InferSNOMEDCT job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | An AWS Key Management Service key used to encrypt your output files. If
    -- you do not specify a key, the files are written in plain text.
    kmsKey :: Prelude.Maybe Prelude.Text,
    inputDataConfig :: InputDataConfig,
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend Medical read access to your
    -- input data.
    dataAccessRoleArn :: Prelude.Text,
    -- | The language of the input documents. All documents must be in the same
    -- language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSNOMEDCTInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startSNOMEDCTInferenceJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend Medical generates one.
--
-- 'jobName', 'startSNOMEDCTInferenceJob_jobName' - The user generated name the asynchronous InferSNOMEDCT job.
--
-- 'kmsKey', 'startSNOMEDCTInferenceJob_kmsKey' - An AWS Key Management Service key used to encrypt your output files. If
-- you do not specify a key, the files are written in plain text.
--
-- 'inputDataConfig', 'startSNOMEDCTInferenceJob_inputDataConfig' - Undocumented member.
--
-- 'outputDataConfig', 'startSNOMEDCTInferenceJob_outputDataConfig' - Undocumented member.
--
-- 'dataAccessRoleArn', 'startSNOMEDCTInferenceJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend Medical read access to your
-- input data.
--
-- 'languageCode', 'startSNOMEDCTInferenceJob_languageCode' - The language of the input documents. All documents must be in the same
-- language.
newStartSNOMEDCTInferenceJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartSNOMEDCTInferenceJob
newStartSNOMEDCTInferenceJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartSNOMEDCTInferenceJob'
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
-- request token, Amazon Comprehend Medical generates one.
startSNOMEDCTInferenceJob_clientRequestToken :: Lens.Lens' StartSNOMEDCTInferenceJob (Prelude.Maybe Prelude.Text)
startSNOMEDCTInferenceJob_clientRequestToken = Lens.lens (\StartSNOMEDCTInferenceJob' {clientRequestToken} -> clientRequestToken) (\s@StartSNOMEDCTInferenceJob' {} a -> s {clientRequestToken = a} :: StartSNOMEDCTInferenceJob)

-- | The user generated name the asynchronous InferSNOMEDCT job.
startSNOMEDCTInferenceJob_jobName :: Lens.Lens' StartSNOMEDCTInferenceJob (Prelude.Maybe Prelude.Text)
startSNOMEDCTInferenceJob_jobName = Lens.lens (\StartSNOMEDCTInferenceJob' {jobName} -> jobName) (\s@StartSNOMEDCTInferenceJob' {} a -> s {jobName = a} :: StartSNOMEDCTInferenceJob)

-- | An AWS Key Management Service key used to encrypt your output files. If
-- you do not specify a key, the files are written in plain text.
startSNOMEDCTInferenceJob_kmsKey :: Lens.Lens' StartSNOMEDCTInferenceJob (Prelude.Maybe Prelude.Text)
startSNOMEDCTInferenceJob_kmsKey = Lens.lens (\StartSNOMEDCTInferenceJob' {kmsKey} -> kmsKey) (\s@StartSNOMEDCTInferenceJob' {} a -> s {kmsKey = a} :: StartSNOMEDCTInferenceJob)

-- | Undocumented member.
startSNOMEDCTInferenceJob_inputDataConfig :: Lens.Lens' StartSNOMEDCTInferenceJob InputDataConfig
startSNOMEDCTInferenceJob_inputDataConfig = Lens.lens (\StartSNOMEDCTInferenceJob' {inputDataConfig} -> inputDataConfig) (\s@StartSNOMEDCTInferenceJob' {} a -> s {inputDataConfig = a} :: StartSNOMEDCTInferenceJob)

-- | Undocumented member.
startSNOMEDCTInferenceJob_outputDataConfig :: Lens.Lens' StartSNOMEDCTInferenceJob OutputDataConfig
startSNOMEDCTInferenceJob_outputDataConfig = Lens.lens (\StartSNOMEDCTInferenceJob' {outputDataConfig} -> outputDataConfig) (\s@StartSNOMEDCTInferenceJob' {} a -> s {outputDataConfig = a} :: StartSNOMEDCTInferenceJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend Medical read access to your
-- input data.
startSNOMEDCTInferenceJob_dataAccessRoleArn :: Lens.Lens' StartSNOMEDCTInferenceJob Prelude.Text
startSNOMEDCTInferenceJob_dataAccessRoleArn = Lens.lens (\StartSNOMEDCTInferenceJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartSNOMEDCTInferenceJob' {} a -> s {dataAccessRoleArn = a} :: StartSNOMEDCTInferenceJob)

-- | The language of the input documents. All documents must be in the same
-- language.
startSNOMEDCTInferenceJob_languageCode :: Lens.Lens' StartSNOMEDCTInferenceJob LanguageCode
startSNOMEDCTInferenceJob_languageCode = Lens.lens (\StartSNOMEDCTInferenceJob' {languageCode} -> languageCode) (\s@StartSNOMEDCTInferenceJob' {} a -> s {languageCode = a} :: StartSNOMEDCTInferenceJob)

instance Core.AWSRequest StartSNOMEDCTInferenceJob where
  type
    AWSResponse StartSNOMEDCTInferenceJob =
      StartSNOMEDCTInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSNOMEDCTInferenceJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSNOMEDCTInferenceJob where
  hashWithSalt _salt StartSNOMEDCTInferenceJob' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData StartSNOMEDCTInferenceJob where
  rnf StartSNOMEDCTInferenceJob' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders StartSNOMEDCTInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.StartSNOMEDCTInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSNOMEDCTInferenceJob where
  toJSON StartSNOMEDCTInferenceJob' {..} =
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

instance Data.ToPath StartSNOMEDCTInferenceJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartSNOMEDCTInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSNOMEDCTInferenceJobResponse' smart constructor.
data StartSNOMEDCTInferenceJobResponse = StartSNOMEDCTInferenceJobResponse'
  { -- | The identifier generated for the job. To get the status of a job, use
    -- this identifier with the StartSNOMEDCTInferenceJob operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSNOMEDCTInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startSNOMEDCTInferenceJobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this identifier with the StartSNOMEDCTInferenceJob operation.
--
-- 'httpStatus', 'startSNOMEDCTInferenceJobResponse_httpStatus' - The response's http status code.
newStartSNOMEDCTInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSNOMEDCTInferenceJobResponse
newStartSNOMEDCTInferenceJobResponse pHttpStatus_ =
  StartSNOMEDCTInferenceJobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use
-- this identifier with the StartSNOMEDCTInferenceJob operation.
startSNOMEDCTInferenceJobResponse_jobId :: Lens.Lens' StartSNOMEDCTInferenceJobResponse (Prelude.Maybe Prelude.Text)
startSNOMEDCTInferenceJobResponse_jobId = Lens.lens (\StartSNOMEDCTInferenceJobResponse' {jobId} -> jobId) (\s@StartSNOMEDCTInferenceJobResponse' {} a -> s {jobId = a} :: StartSNOMEDCTInferenceJobResponse)

-- | The response's http status code.
startSNOMEDCTInferenceJobResponse_httpStatus :: Lens.Lens' StartSNOMEDCTInferenceJobResponse Prelude.Int
startSNOMEDCTInferenceJobResponse_httpStatus = Lens.lens (\StartSNOMEDCTInferenceJobResponse' {httpStatus} -> httpStatus) (\s@StartSNOMEDCTInferenceJobResponse' {} a -> s {httpStatus = a} :: StartSNOMEDCTInferenceJobResponse)

instance
  Prelude.NFData
    StartSNOMEDCTInferenceJobResponse
  where
  rnf StartSNOMEDCTInferenceJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
