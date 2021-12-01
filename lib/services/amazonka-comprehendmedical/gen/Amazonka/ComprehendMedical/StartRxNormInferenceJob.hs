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
-- Module      : Amazonka.ComprehendMedical.StartRxNormInferenceJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous job to detect medication entities and link them
-- to the RxNorm ontology. Use the @DescribeRxNormInferenceJob@ operation
-- to track the status of a job.
module Amazonka.ComprehendMedical.StartRxNormInferenceJob
  ( -- * Creating a Request
    StartRxNormInferenceJob (..),
    newStartRxNormInferenceJob,

    -- * Request Lenses
    startRxNormInferenceJob_kmsKey,
    startRxNormInferenceJob_jobName,
    startRxNormInferenceJob_clientRequestToken,
    startRxNormInferenceJob_inputDataConfig,
    startRxNormInferenceJob_outputDataConfig,
    startRxNormInferenceJob_dataAccessRoleArn,
    startRxNormInferenceJob_languageCode,

    -- * Destructuring the Response
    StartRxNormInferenceJobResponse (..),
    newStartRxNormInferenceJobResponse,

    -- * Response Lenses
    startRxNormInferenceJobResponse_jobId,
    startRxNormInferenceJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartRxNormInferenceJob' smart constructor.
data StartRxNormInferenceJob = StartRxNormInferenceJob'
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
-- Create a value of 'StartRxNormInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'startRxNormInferenceJob_kmsKey' - An AWS Key Management Service key to encrypt your output files. If you
-- do not specify a key, the files are written in plain text.
--
-- 'jobName', 'startRxNormInferenceJob_jobName' - The identifier of the job.
--
-- 'clientRequestToken', 'startRxNormInferenceJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend Medical generates one.
--
-- 'inputDataConfig', 'startRxNormInferenceJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startRxNormInferenceJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startRxNormInferenceJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend Medical read access to your
-- input data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
--
-- 'languageCode', 'startRxNormInferenceJob_languageCode' - The language of the input documents. All documents must be in the same
-- language.
newStartRxNormInferenceJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartRxNormInferenceJob
newStartRxNormInferenceJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartRxNormInferenceJob'
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
startRxNormInferenceJob_kmsKey :: Lens.Lens' StartRxNormInferenceJob (Prelude.Maybe Prelude.Text)
startRxNormInferenceJob_kmsKey = Lens.lens (\StartRxNormInferenceJob' {kmsKey} -> kmsKey) (\s@StartRxNormInferenceJob' {} a -> s {kmsKey = a} :: StartRxNormInferenceJob)

-- | The identifier of the job.
startRxNormInferenceJob_jobName :: Lens.Lens' StartRxNormInferenceJob (Prelude.Maybe Prelude.Text)
startRxNormInferenceJob_jobName = Lens.lens (\StartRxNormInferenceJob' {jobName} -> jobName) (\s@StartRxNormInferenceJob' {} a -> s {jobName = a} :: StartRxNormInferenceJob)

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend Medical generates one.
startRxNormInferenceJob_clientRequestToken :: Lens.Lens' StartRxNormInferenceJob (Prelude.Maybe Prelude.Text)
startRxNormInferenceJob_clientRequestToken = Lens.lens (\StartRxNormInferenceJob' {clientRequestToken} -> clientRequestToken) (\s@StartRxNormInferenceJob' {} a -> s {clientRequestToken = a} :: StartRxNormInferenceJob)

-- | Specifies the format and location of the input data for the job.
startRxNormInferenceJob_inputDataConfig :: Lens.Lens' StartRxNormInferenceJob InputDataConfig
startRxNormInferenceJob_inputDataConfig = Lens.lens (\StartRxNormInferenceJob' {inputDataConfig} -> inputDataConfig) (\s@StartRxNormInferenceJob' {} a -> s {inputDataConfig = a} :: StartRxNormInferenceJob)

-- | Specifies where to send the output files.
startRxNormInferenceJob_outputDataConfig :: Lens.Lens' StartRxNormInferenceJob OutputDataConfig
startRxNormInferenceJob_outputDataConfig = Lens.lens (\StartRxNormInferenceJob' {outputDataConfig} -> outputDataConfig) (\s@StartRxNormInferenceJob' {} a -> s {outputDataConfig = a} :: StartRxNormInferenceJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend Medical read access to your
-- input data. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions-med.html#auth-role-permissions-med Role-Based Permissions Required for Asynchronous Operations>.
startRxNormInferenceJob_dataAccessRoleArn :: Lens.Lens' StartRxNormInferenceJob Prelude.Text
startRxNormInferenceJob_dataAccessRoleArn = Lens.lens (\StartRxNormInferenceJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartRxNormInferenceJob' {} a -> s {dataAccessRoleArn = a} :: StartRxNormInferenceJob)

-- | The language of the input documents. All documents must be in the same
-- language.
startRxNormInferenceJob_languageCode :: Lens.Lens' StartRxNormInferenceJob LanguageCode
startRxNormInferenceJob_languageCode = Lens.lens (\StartRxNormInferenceJob' {languageCode} -> languageCode) (\s@StartRxNormInferenceJob' {} a -> s {languageCode = a} :: StartRxNormInferenceJob)

instance Core.AWSRequest StartRxNormInferenceJob where
  type
    AWSResponse StartRxNormInferenceJob =
      StartRxNormInferenceJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRxNormInferenceJobResponse'
            Prelude.<$> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartRxNormInferenceJob where
  hashWithSalt salt' StartRxNormInferenceJob' {..} =
    salt' `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` kmsKey

instance Prelude.NFData StartRxNormInferenceJob where
  rnf StartRxNormInferenceJob' {..} =
    Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf jobName

instance Core.ToHeaders StartRxNormInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComprehendMedical_20181030.StartRxNormInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartRxNormInferenceJob where
  toJSON StartRxNormInferenceJob' {..} =
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

instance Core.ToPath StartRxNormInferenceJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartRxNormInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartRxNormInferenceJobResponse' smart constructor.
data StartRxNormInferenceJobResponse = StartRxNormInferenceJobResponse'
  { -- | The identifier of the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRxNormInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startRxNormInferenceJobResponse_jobId' - The identifier of the job.
--
-- 'httpStatus', 'startRxNormInferenceJobResponse_httpStatus' - The response's http status code.
newStartRxNormInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartRxNormInferenceJobResponse
newStartRxNormInferenceJobResponse pHttpStatus_ =
  StartRxNormInferenceJobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the job.
startRxNormInferenceJobResponse_jobId :: Lens.Lens' StartRxNormInferenceJobResponse (Prelude.Maybe Prelude.Text)
startRxNormInferenceJobResponse_jobId = Lens.lens (\StartRxNormInferenceJobResponse' {jobId} -> jobId) (\s@StartRxNormInferenceJobResponse' {} a -> s {jobId = a} :: StartRxNormInferenceJobResponse)

-- | The response's http status code.
startRxNormInferenceJobResponse_httpStatus :: Lens.Lens' StartRxNormInferenceJobResponse Prelude.Int
startRxNormInferenceJobResponse_httpStatus = Lens.lens (\StartRxNormInferenceJobResponse' {httpStatus} -> httpStatus) (\s@StartRxNormInferenceJobResponse' {} a -> s {httpStatus = a} :: StartRxNormInferenceJobResponse)

instance
  Prelude.NFData
    StartRxNormInferenceJobResponse
  where
  rnf StartRxNormInferenceJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
