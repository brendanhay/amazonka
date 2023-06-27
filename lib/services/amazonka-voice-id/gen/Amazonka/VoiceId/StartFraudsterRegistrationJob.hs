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
-- Module      : Amazonka.VoiceId.StartFraudsterRegistrationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new batch fraudster registration job using provided details.
module Amazonka.VoiceId.StartFraudsterRegistrationJob
  ( -- * Creating a Request
    StartFraudsterRegistrationJob (..),
    newStartFraudsterRegistrationJob,

    -- * Request Lenses
    startFraudsterRegistrationJob_clientToken,
    startFraudsterRegistrationJob_jobName,
    startFraudsterRegistrationJob_registrationConfig,
    startFraudsterRegistrationJob_dataAccessRoleArn,
    startFraudsterRegistrationJob_domainId,
    startFraudsterRegistrationJob_inputDataConfig,
    startFraudsterRegistrationJob_outputDataConfig,

    -- * Destructuring the Response
    StartFraudsterRegistrationJobResponse (..),
    newStartFraudsterRegistrationJobResponse,

    -- * Response Lenses
    startFraudsterRegistrationJobResponse_job,
    startFraudsterRegistrationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newStartFraudsterRegistrationJob' smart constructor.
data StartFraudsterRegistrationJob = StartFraudsterRegistrationJob'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the new fraudster registration job.
    jobName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The registration config containing details such as the action to take
    -- when a duplicate fraudster is detected, and the similarity threshold to
    -- use for detecting a duplicate fraudster.
    registrationConfig :: Prelude.Maybe RegistrationConfig,
    -- | The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
    -- to access customer\'s buckets to read the input manifest file and write
    -- the Job output file. Refer to the
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/voiceid-fraudster-watchlist.html Create and edit a fraudster watchlist>
    -- documentation for the permissions needed in this role.
    dataAccessRoleArn :: Prelude.Text,
    -- | The identifier of the domain that contains the fraudster registration
    -- job and in which the fraudsters are registered.
    domainId :: Prelude.Text,
    -- | The input data config containing an S3 URI for the input manifest file
    -- that contains the list of fraudster registration requests.
    inputDataConfig :: InputDataConfig,
    -- | The output data config containing the S3 location where Voice ID writes
    -- the job output file; you must also include a KMS key ID to encrypt the
    -- file.
    outputDataConfig :: OutputDataConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFraudsterRegistrationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startFraudsterRegistrationJob_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'jobName', 'startFraudsterRegistrationJob_jobName' - The name of the new fraudster registration job.
--
-- 'registrationConfig', 'startFraudsterRegistrationJob_registrationConfig' - The registration config containing details such as the action to take
-- when a duplicate fraudster is detected, and the similarity threshold to
-- use for detecting a duplicate fraudster.
--
-- 'dataAccessRoleArn', 'startFraudsterRegistrationJob_dataAccessRoleArn' - The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
-- to access customer\'s buckets to read the input manifest file and write
-- the Job output file. Refer to the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/voiceid-fraudster-watchlist.html Create and edit a fraudster watchlist>
-- documentation for the permissions needed in this role.
--
-- 'domainId', 'startFraudsterRegistrationJob_domainId' - The identifier of the domain that contains the fraudster registration
-- job and in which the fraudsters are registered.
--
-- 'inputDataConfig', 'startFraudsterRegistrationJob_inputDataConfig' - The input data config containing an S3 URI for the input manifest file
-- that contains the list of fraudster registration requests.
--
-- 'outputDataConfig', 'startFraudsterRegistrationJob_outputDataConfig' - The output data config containing the S3 location where Voice ID writes
-- the job output file; you must also include a KMS key ID to encrypt the
-- file.
newStartFraudsterRegistrationJob ::
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'domainId'
  Prelude.Text ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  StartFraudsterRegistrationJob
newStartFraudsterRegistrationJob
  pDataAccessRoleArn_
  pDomainId_
  pInputDataConfig_
  pOutputDataConfig_ =
    StartFraudsterRegistrationJob'
      { clientToken =
          Prelude.Nothing,
        jobName = Prelude.Nothing,
        registrationConfig = Prelude.Nothing,
        dataAccessRoleArn = pDataAccessRoleArn_,
        domainId = pDomainId_,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
startFraudsterRegistrationJob_clientToken :: Lens.Lens' StartFraudsterRegistrationJob (Prelude.Maybe Prelude.Text)
startFraudsterRegistrationJob_clientToken = Lens.lens (\StartFraudsterRegistrationJob' {clientToken} -> clientToken) (\s@StartFraudsterRegistrationJob' {} a -> s {clientToken = a} :: StartFraudsterRegistrationJob)

-- | The name of the new fraudster registration job.
startFraudsterRegistrationJob_jobName :: Lens.Lens' StartFraudsterRegistrationJob (Prelude.Maybe Prelude.Text)
startFraudsterRegistrationJob_jobName = Lens.lens (\StartFraudsterRegistrationJob' {jobName} -> jobName) (\s@StartFraudsterRegistrationJob' {} a -> s {jobName = a} :: StartFraudsterRegistrationJob) Prelude.. Lens.mapping Data._Sensitive

-- | The registration config containing details such as the action to take
-- when a duplicate fraudster is detected, and the similarity threshold to
-- use for detecting a duplicate fraudster.
startFraudsterRegistrationJob_registrationConfig :: Lens.Lens' StartFraudsterRegistrationJob (Prelude.Maybe RegistrationConfig)
startFraudsterRegistrationJob_registrationConfig = Lens.lens (\StartFraudsterRegistrationJob' {registrationConfig} -> registrationConfig) (\s@StartFraudsterRegistrationJob' {} a -> s {registrationConfig = a} :: StartFraudsterRegistrationJob)

-- | The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
-- to access customer\'s buckets to read the input manifest file and write
-- the Job output file. Refer to the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/voiceid-fraudster-watchlist.html Create and edit a fraudster watchlist>
-- documentation for the permissions needed in this role.
startFraudsterRegistrationJob_dataAccessRoleArn :: Lens.Lens' StartFraudsterRegistrationJob Prelude.Text
startFraudsterRegistrationJob_dataAccessRoleArn = Lens.lens (\StartFraudsterRegistrationJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartFraudsterRegistrationJob' {} a -> s {dataAccessRoleArn = a} :: StartFraudsterRegistrationJob)

-- | The identifier of the domain that contains the fraudster registration
-- job and in which the fraudsters are registered.
startFraudsterRegistrationJob_domainId :: Lens.Lens' StartFraudsterRegistrationJob Prelude.Text
startFraudsterRegistrationJob_domainId = Lens.lens (\StartFraudsterRegistrationJob' {domainId} -> domainId) (\s@StartFraudsterRegistrationJob' {} a -> s {domainId = a} :: StartFraudsterRegistrationJob)

-- | The input data config containing an S3 URI for the input manifest file
-- that contains the list of fraudster registration requests.
startFraudsterRegistrationJob_inputDataConfig :: Lens.Lens' StartFraudsterRegistrationJob InputDataConfig
startFraudsterRegistrationJob_inputDataConfig = Lens.lens (\StartFraudsterRegistrationJob' {inputDataConfig} -> inputDataConfig) (\s@StartFraudsterRegistrationJob' {} a -> s {inputDataConfig = a} :: StartFraudsterRegistrationJob)

-- | The output data config containing the S3 location where Voice ID writes
-- the job output file; you must also include a KMS key ID to encrypt the
-- file.
startFraudsterRegistrationJob_outputDataConfig :: Lens.Lens' StartFraudsterRegistrationJob OutputDataConfig
startFraudsterRegistrationJob_outputDataConfig = Lens.lens (\StartFraudsterRegistrationJob' {outputDataConfig} -> outputDataConfig) (\s@StartFraudsterRegistrationJob' {} a -> s {outputDataConfig = a} :: StartFraudsterRegistrationJob)

instance
  Core.AWSRequest
    StartFraudsterRegistrationJob
  where
  type
    AWSResponse StartFraudsterRegistrationJob =
      StartFraudsterRegistrationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFraudsterRegistrationJobResponse'
            Prelude.<$> (x Data..?> "Job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartFraudsterRegistrationJob
  where
  hashWithSalt _salt StartFraudsterRegistrationJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` registrationConfig
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig

instance Prelude.NFData StartFraudsterRegistrationJob where
  rnf StartFraudsterRegistrationJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf registrationConfig
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig

instance Data.ToHeaders StartFraudsterRegistrationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VoiceID.StartFraudsterRegistrationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFraudsterRegistrationJob where
  toJSON StartFraudsterRegistrationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("JobName" Data..=) Prelude.<$> jobName,
            ("RegistrationConfig" Data..=)
              Prelude.<$> registrationConfig,
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig)
          ]
      )

instance Data.ToPath StartFraudsterRegistrationJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartFraudsterRegistrationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFraudsterRegistrationJobResponse' smart constructor.
data StartFraudsterRegistrationJobResponse = StartFraudsterRegistrationJobResponse'
  { -- | Details about the started fraudster registration job.
    job :: Prelude.Maybe FraudsterRegistrationJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFraudsterRegistrationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'startFraudsterRegistrationJobResponse_job' - Details about the started fraudster registration job.
--
-- 'httpStatus', 'startFraudsterRegistrationJobResponse_httpStatus' - The response's http status code.
newStartFraudsterRegistrationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFraudsterRegistrationJobResponse
newStartFraudsterRegistrationJobResponse pHttpStatus_ =
  StartFraudsterRegistrationJobResponse'
    { job =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the started fraudster registration job.
startFraudsterRegistrationJobResponse_job :: Lens.Lens' StartFraudsterRegistrationJobResponse (Prelude.Maybe FraudsterRegistrationJob)
startFraudsterRegistrationJobResponse_job = Lens.lens (\StartFraudsterRegistrationJobResponse' {job} -> job) (\s@StartFraudsterRegistrationJobResponse' {} a -> s {job = a} :: StartFraudsterRegistrationJobResponse)

-- | The response's http status code.
startFraudsterRegistrationJobResponse_httpStatus :: Lens.Lens' StartFraudsterRegistrationJobResponse Prelude.Int
startFraudsterRegistrationJobResponse_httpStatus = Lens.lens (\StartFraudsterRegistrationJobResponse' {httpStatus} -> httpStatus) (\s@StartFraudsterRegistrationJobResponse' {} a -> s {httpStatus = a} :: StartFraudsterRegistrationJobResponse)

instance
  Prelude.NFData
    StartFraudsterRegistrationJobResponse
  where
  rnf StartFraudsterRegistrationJobResponse' {..} =
    Prelude.rnf job
      `Prelude.seq` Prelude.rnf httpStatus
