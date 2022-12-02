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
-- Module      : Amazonka.VoiceId.StartSpeakerEnrollmentJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new batch speaker enrollment job using specified details.
module Amazonka.VoiceId.StartSpeakerEnrollmentJob
  ( -- * Creating a Request
    StartSpeakerEnrollmentJob (..),
    newStartSpeakerEnrollmentJob,

    -- * Request Lenses
    startSpeakerEnrollmentJob_enrollmentConfig,
    startSpeakerEnrollmentJob_clientToken,
    startSpeakerEnrollmentJob_jobName,
    startSpeakerEnrollmentJob_dataAccessRoleArn,
    startSpeakerEnrollmentJob_domainId,
    startSpeakerEnrollmentJob_inputDataConfig,
    startSpeakerEnrollmentJob_outputDataConfig,

    -- * Destructuring the Response
    StartSpeakerEnrollmentJobResponse (..),
    newStartSpeakerEnrollmentJobResponse,

    -- * Response Lenses
    startSpeakerEnrollmentJobResponse_job,
    startSpeakerEnrollmentJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newStartSpeakerEnrollmentJob' smart constructor.
data StartSpeakerEnrollmentJob = StartSpeakerEnrollmentJob'
  { -- | The enrollment config that contains details such as the action to take
    -- when a speaker is already enrolled in Voice ID or when a speaker is
    -- identified as a fraudster.
    enrollmentConfig :: Prelude.Maybe EnrollmentConfig,
    -- | The idempotency token for starting a new speaker enrollment Job. If not
    -- provided, Amazon Web Services SDK populates this field.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A name for your speaker enrollment job.
    jobName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
    -- to access customer\'s buckets to read the input manifest file and write
    -- the job output file. Refer to
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/voiceid-batch-enrollment.html Batch enrollment using audio data from prior calls>
    -- for the permissions needed in this role.
    dataAccessRoleArn :: Prelude.Text,
    -- | The identifier of the domain that contains the speaker enrollment job
    -- and in which the speakers are enrolled.
    domainId :: Prelude.Text,
    -- | The input data config containing the S3 location for the input manifest
    -- file that contains the list of speaker enrollment requests.
    inputDataConfig :: InputDataConfig,
    -- | The output data config containing the S3 location where Voice ID writes
    -- the job output file; you must also include a KMS key ID to encrypt the
    -- file.
    outputDataConfig :: OutputDataConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSpeakerEnrollmentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enrollmentConfig', 'startSpeakerEnrollmentJob_enrollmentConfig' - The enrollment config that contains details such as the action to take
-- when a speaker is already enrolled in Voice ID or when a speaker is
-- identified as a fraudster.
--
-- 'clientToken', 'startSpeakerEnrollmentJob_clientToken' - The idempotency token for starting a new speaker enrollment Job. If not
-- provided, Amazon Web Services SDK populates this field.
--
-- 'jobName', 'startSpeakerEnrollmentJob_jobName' - A name for your speaker enrollment job.
--
-- 'dataAccessRoleArn', 'startSpeakerEnrollmentJob_dataAccessRoleArn' - The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
-- to access customer\'s buckets to read the input manifest file and write
-- the job output file. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/voiceid-batch-enrollment.html Batch enrollment using audio data from prior calls>
-- for the permissions needed in this role.
--
-- 'domainId', 'startSpeakerEnrollmentJob_domainId' - The identifier of the domain that contains the speaker enrollment job
-- and in which the speakers are enrolled.
--
-- 'inputDataConfig', 'startSpeakerEnrollmentJob_inputDataConfig' - The input data config containing the S3 location for the input manifest
-- file that contains the list of speaker enrollment requests.
--
-- 'outputDataConfig', 'startSpeakerEnrollmentJob_outputDataConfig' - The output data config containing the S3 location where Voice ID writes
-- the job output file; you must also include a KMS key ID to encrypt the
-- file.
newStartSpeakerEnrollmentJob ::
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'domainId'
  Prelude.Text ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  StartSpeakerEnrollmentJob
newStartSpeakerEnrollmentJob
  pDataAccessRoleArn_
  pDomainId_
  pInputDataConfig_
  pOutputDataConfig_ =
    StartSpeakerEnrollmentJob'
      { enrollmentConfig =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        jobName = Prelude.Nothing,
        dataAccessRoleArn = pDataAccessRoleArn_,
        domainId = pDomainId_,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_
      }

-- | The enrollment config that contains details such as the action to take
-- when a speaker is already enrolled in Voice ID or when a speaker is
-- identified as a fraudster.
startSpeakerEnrollmentJob_enrollmentConfig :: Lens.Lens' StartSpeakerEnrollmentJob (Prelude.Maybe EnrollmentConfig)
startSpeakerEnrollmentJob_enrollmentConfig = Lens.lens (\StartSpeakerEnrollmentJob' {enrollmentConfig} -> enrollmentConfig) (\s@StartSpeakerEnrollmentJob' {} a -> s {enrollmentConfig = a} :: StartSpeakerEnrollmentJob)

-- | The idempotency token for starting a new speaker enrollment Job. If not
-- provided, Amazon Web Services SDK populates this field.
startSpeakerEnrollmentJob_clientToken :: Lens.Lens' StartSpeakerEnrollmentJob (Prelude.Maybe Prelude.Text)
startSpeakerEnrollmentJob_clientToken = Lens.lens (\StartSpeakerEnrollmentJob' {clientToken} -> clientToken) (\s@StartSpeakerEnrollmentJob' {} a -> s {clientToken = a} :: StartSpeakerEnrollmentJob)

-- | A name for your speaker enrollment job.
startSpeakerEnrollmentJob_jobName :: Lens.Lens' StartSpeakerEnrollmentJob (Prelude.Maybe Prelude.Text)
startSpeakerEnrollmentJob_jobName = Lens.lens (\StartSpeakerEnrollmentJob' {jobName} -> jobName) (\s@StartSpeakerEnrollmentJob' {} a -> s {jobName = a} :: StartSpeakerEnrollmentJob) Prelude.. Lens.mapping Data._Sensitive

-- | The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
-- to access customer\'s buckets to read the input manifest file and write
-- the job output file. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/voiceid-batch-enrollment.html Batch enrollment using audio data from prior calls>
-- for the permissions needed in this role.
startSpeakerEnrollmentJob_dataAccessRoleArn :: Lens.Lens' StartSpeakerEnrollmentJob Prelude.Text
startSpeakerEnrollmentJob_dataAccessRoleArn = Lens.lens (\StartSpeakerEnrollmentJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartSpeakerEnrollmentJob' {} a -> s {dataAccessRoleArn = a} :: StartSpeakerEnrollmentJob)

-- | The identifier of the domain that contains the speaker enrollment job
-- and in which the speakers are enrolled.
startSpeakerEnrollmentJob_domainId :: Lens.Lens' StartSpeakerEnrollmentJob Prelude.Text
startSpeakerEnrollmentJob_domainId = Lens.lens (\StartSpeakerEnrollmentJob' {domainId} -> domainId) (\s@StartSpeakerEnrollmentJob' {} a -> s {domainId = a} :: StartSpeakerEnrollmentJob)

-- | The input data config containing the S3 location for the input manifest
-- file that contains the list of speaker enrollment requests.
startSpeakerEnrollmentJob_inputDataConfig :: Lens.Lens' StartSpeakerEnrollmentJob InputDataConfig
startSpeakerEnrollmentJob_inputDataConfig = Lens.lens (\StartSpeakerEnrollmentJob' {inputDataConfig} -> inputDataConfig) (\s@StartSpeakerEnrollmentJob' {} a -> s {inputDataConfig = a} :: StartSpeakerEnrollmentJob)

-- | The output data config containing the S3 location where Voice ID writes
-- the job output file; you must also include a KMS key ID to encrypt the
-- file.
startSpeakerEnrollmentJob_outputDataConfig :: Lens.Lens' StartSpeakerEnrollmentJob OutputDataConfig
startSpeakerEnrollmentJob_outputDataConfig = Lens.lens (\StartSpeakerEnrollmentJob' {outputDataConfig} -> outputDataConfig) (\s@StartSpeakerEnrollmentJob' {} a -> s {outputDataConfig = a} :: StartSpeakerEnrollmentJob)

instance Core.AWSRequest StartSpeakerEnrollmentJob where
  type
    AWSResponse StartSpeakerEnrollmentJob =
      StartSpeakerEnrollmentJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSpeakerEnrollmentJobResponse'
            Prelude.<$> (x Data..?> "Job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSpeakerEnrollmentJob where
  hashWithSalt _salt StartSpeakerEnrollmentJob' {..} =
    _salt `Prelude.hashWithSalt` enrollmentConfig
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig

instance Prelude.NFData StartSpeakerEnrollmentJob where
  rnf StartSpeakerEnrollmentJob' {..} =
    Prelude.rnf enrollmentConfig
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig

instance Data.ToHeaders StartSpeakerEnrollmentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VoiceID.StartSpeakerEnrollmentJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSpeakerEnrollmentJob where
  toJSON StartSpeakerEnrollmentJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnrollmentConfig" Data..=)
              Prelude.<$> enrollmentConfig,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("JobName" Data..=) Prelude.<$> jobName,
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig)
          ]
      )

instance Data.ToPath StartSpeakerEnrollmentJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartSpeakerEnrollmentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSpeakerEnrollmentJobResponse' smart constructor.
data StartSpeakerEnrollmentJobResponse = StartSpeakerEnrollmentJobResponse'
  { -- | Details about the started speaker enrollment job.
    job :: Prelude.Maybe SpeakerEnrollmentJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSpeakerEnrollmentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'startSpeakerEnrollmentJobResponse_job' - Details about the started speaker enrollment job.
--
-- 'httpStatus', 'startSpeakerEnrollmentJobResponse_httpStatus' - The response's http status code.
newStartSpeakerEnrollmentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSpeakerEnrollmentJobResponse
newStartSpeakerEnrollmentJobResponse pHttpStatus_ =
  StartSpeakerEnrollmentJobResponse'
    { job =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the started speaker enrollment job.
startSpeakerEnrollmentJobResponse_job :: Lens.Lens' StartSpeakerEnrollmentJobResponse (Prelude.Maybe SpeakerEnrollmentJob)
startSpeakerEnrollmentJobResponse_job = Lens.lens (\StartSpeakerEnrollmentJobResponse' {job} -> job) (\s@StartSpeakerEnrollmentJobResponse' {} a -> s {job = a} :: StartSpeakerEnrollmentJobResponse)

-- | The response's http status code.
startSpeakerEnrollmentJobResponse_httpStatus :: Lens.Lens' StartSpeakerEnrollmentJobResponse Prelude.Int
startSpeakerEnrollmentJobResponse_httpStatus = Lens.lens (\StartSpeakerEnrollmentJobResponse' {httpStatus} -> httpStatus) (\s@StartSpeakerEnrollmentJobResponse' {} a -> s {httpStatus = a} :: StartSpeakerEnrollmentJobResponse)

instance
  Prelude.NFData
    StartSpeakerEnrollmentJobResponse
  where
  rnf StartSpeakerEnrollmentJobResponse' {..} =
    Prelude.rnf job
      `Prelude.seq` Prelude.rnf httpStatus
