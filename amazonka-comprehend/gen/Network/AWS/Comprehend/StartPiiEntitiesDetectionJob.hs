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
-- Module      : Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous PII entity detection job for a collection of
-- documents.
module Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
  ( -- * Creating a Request
    StartPiiEntitiesDetectionJob (..),
    newStartPiiEntitiesDetectionJob,

    -- * Request Lenses
    startPiiEntitiesDetectionJob_redactionConfig,
    startPiiEntitiesDetectionJob_clientRequestToken,
    startPiiEntitiesDetectionJob_jobName,
    startPiiEntitiesDetectionJob_inputDataConfig,
    startPiiEntitiesDetectionJob_outputDataConfig,
    startPiiEntitiesDetectionJob_mode,
    startPiiEntitiesDetectionJob_dataAccessRoleArn,
    startPiiEntitiesDetectionJob_languageCode,

    -- * Destructuring the Response
    StartPiiEntitiesDetectionJobResponse (..),
    newStartPiiEntitiesDetectionJobResponse,

    -- * Response Lenses
    startPiiEntitiesDetectionJobResponse_jobStatus,
    startPiiEntitiesDetectionJobResponse_jobId,
    startPiiEntitiesDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartPiiEntitiesDetectionJob' smart constructor.
data StartPiiEntitiesDetectionJob = StartPiiEntitiesDetectionJob'
  { -- | Provides configuration parameters for PII entity redaction.
    --
    -- This parameter is required if you set the @Mode@ parameter to
    -- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
    -- definition that includes the @PiiEntityTypes@ parameter.
    redactionConfig :: Prelude.Maybe RedactionConfig,
    -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The input properties for a PII entities detection job.
    inputDataConfig :: InputDataConfig,
    -- | Provides conﬁguration parameters for the output of PII entity detection
    -- jobs.
    outputDataConfig :: OutputDataConfig,
    -- | Specifies whether the output provides the locations (offsets) of PII
    -- entities or a file in which PII entities are redacted.
    mode :: PiiEntitiesDetectionMode,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Text,
    -- | The language of the input documents.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPiiEntitiesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redactionConfig', 'startPiiEntitiesDetectionJob_redactionConfig' - Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
--
-- 'clientRequestToken', 'startPiiEntitiesDetectionJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'jobName', 'startPiiEntitiesDetectionJob_jobName' - The identifier of the job.
--
-- 'inputDataConfig', 'startPiiEntitiesDetectionJob_inputDataConfig' - The input properties for a PII entities detection job.
--
-- 'outputDataConfig', 'startPiiEntitiesDetectionJob_outputDataConfig' - Provides conﬁguration parameters for the output of PII entity detection
-- jobs.
--
-- 'mode', 'startPiiEntitiesDetectionJob_mode' - Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
--
-- 'dataAccessRoleArn', 'startPiiEntitiesDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- 'languageCode', 'startPiiEntitiesDetectionJob_languageCode' - The language of the input documents.
newStartPiiEntitiesDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'mode'
  PiiEntitiesDetectionMode ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartPiiEntitiesDetectionJob
newStartPiiEntitiesDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pMode_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartPiiEntitiesDetectionJob'
      { redactionConfig =
          Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        jobName = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        mode = pMode_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_
      }

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
startPiiEntitiesDetectionJob_redactionConfig :: Lens.Lens' StartPiiEntitiesDetectionJob (Prelude.Maybe RedactionConfig)
startPiiEntitiesDetectionJob_redactionConfig = Lens.lens (\StartPiiEntitiesDetectionJob' {redactionConfig} -> redactionConfig) (\s@StartPiiEntitiesDetectionJob' {} a -> s {redactionConfig = a} :: StartPiiEntitiesDetectionJob)

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startPiiEntitiesDetectionJob_clientRequestToken :: Lens.Lens' StartPiiEntitiesDetectionJob (Prelude.Maybe Prelude.Text)
startPiiEntitiesDetectionJob_clientRequestToken = Lens.lens (\StartPiiEntitiesDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartPiiEntitiesDetectionJob' {} a -> s {clientRequestToken = a} :: StartPiiEntitiesDetectionJob)

-- | The identifier of the job.
startPiiEntitiesDetectionJob_jobName :: Lens.Lens' StartPiiEntitiesDetectionJob (Prelude.Maybe Prelude.Text)
startPiiEntitiesDetectionJob_jobName = Lens.lens (\StartPiiEntitiesDetectionJob' {jobName} -> jobName) (\s@StartPiiEntitiesDetectionJob' {} a -> s {jobName = a} :: StartPiiEntitiesDetectionJob)

-- | The input properties for a PII entities detection job.
startPiiEntitiesDetectionJob_inputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob InputDataConfig
startPiiEntitiesDetectionJob_inputDataConfig = Lens.lens (\StartPiiEntitiesDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartPiiEntitiesDetectionJob' {} a -> s {inputDataConfig = a} :: StartPiiEntitiesDetectionJob)

-- | Provides conﬁguration parameters for the output of PII entity detection
-- jobs.
startPiiEntitiesDetectionJob_outputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob OutputDataConfig
startPiiEntitiesDetectionJob_outputDataConfig = Lens.lens (\StartPiiEntitiesDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartPiiEntitiesDetectionJob' {} a -> s {outputDataConfig = a} :: StartPiiEntitiesDetectionJob)

-- | Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
startPiiEntitiesDetectionJob_mode :: Lens.Lens' StartPiiEntitiesDetectionJob PiiEntitiesDetectionMode
startPiiEntitiesDetectionJob_mode = Lens.lens (\StartPiiEntitiesDetectionJob' {mode} -> mode) (\s@StartPiiEntitiesDetectionJob' {} a -> s {mode = a} :: StartPiiEntitiesDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
startPiiEntitiesDetectionJob_dataAccessRoleArn :: Lens.Lens' StartPiiEntitiesDetectionJob Prelude.Text
startPiiEntitiesDetectionJob_dataAccessRoleArn = Lens.lens (\StartPiiEntitiesDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartPiiEntitiesDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartPiiEntitiesDetectionJob)

-- | The language of the input documents.
startPiiEntitiesDetectionJob_languageCode :: Lens.Lens' StartPiiEntitiesDetectionJob LanguageCode
startPiiEntitiesDetectionJob_languageCode = Lens.lens (\StartPiiEntitiesDetectionJob' {languageCode} -> languageCode) (\s@StartPiiEntitiesDetectionJob' {} a -> s {languageCode = a} :: StartPiiEntitiesDetectionJob)

instance Core.AWSRequest StartPiiEntitiesDetectionJob where
  type
    AWSResponse StartPiiEntitiesDetectionJob =
      StartPiiEntitiesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPiiEntitiesDetectionJobResponse'
            Prelude.<$> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartPiiEntitiesDetectionJob

instance Prelude.NFData StartPiiEntitiesDetectionJob

instance Core.ToHeaders StartPiiEntitiesDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StartPiiEntitiesDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartPiiEntitiesDetectionJob where
  toJSON StartPiiEntitiesDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RedactionConfig" Core..=)
              Prelude.<$> redactionConfig,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("JobName" Core..=) Prelude.<$> jobName,
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just ("Mode" Core..= mode),
            Prelude.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath StartPiiEntitiesDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartPiiEntitiesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartPiiEntitiesDetectionJobResponse' smart constructor.
data StartPiiEntitiesDetectionJobResponse = StartPiiEntitiesDetectionJobResponse'
  { -- | The status of the job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier generated for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPiiEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'startPiiEntitiesDetectionJobResponse_jobStatus' - The status of the job.
--
-- 'jobId', 'startPiiEntitiesDetectionJobResponse_jobId' - The identifier generated for the job.
--
-- 'httpStatus', 'startPiiEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newStartPiiEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartPiiEntitiesDetectionJobResponse
newStartPiiEntitiesDetectionJobResponse pHttpStatus_ =
  StartPiiEntitiesDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the job.
startPiiEntitiesDetectionJobResponse_jobStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Prelude.Maybe JobStatus)
startPiiEntitiesDetectionJobResponse_jobStatus = Lens.lens (\StartPiiEntitiesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartPiiEntitiesDetectionJobResponse' {} a -> s {jobStatus = a} :: StartPiiEntitiesDetectionJobResponse)

-- | The identifier generated for the job.
startPiiEntitiesDetectionJobResponse_jobId :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Prelude.Maybe Prelude.Text)
startPiiEntitiesDetectionJobResponse_jobId = Lens.lens (\StartPiiEntitiesDetectionJobResponse' {jobId} -> jobId) (\s@StartPiiEntitiesDetectionJobResponse' {} a -> s {jobId = a} :: StartPiiEntitiesDetectionJobResponse)

-- | The response's http status code.
startPiiEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse Prelude.Int
startPiiEntitiesDetectionJobResponse_httpStatus = Lens.lens (\StartPiiEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartPiiEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: StartPiiEntitiesDetectionJobResponse)

instance
  Prelude.NFData
    StartPiiEntitiesDetectionJobResponse
