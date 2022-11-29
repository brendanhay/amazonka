{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VoiceId.Types.FraudsterRegistrationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.FraudsterRegistrationJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.FailureDetails
import Amazonka.VoiceId.Types.FraudsterRegistrationJobStatus
import Amazonka.VoiceId.Types.InputDataConfig
import Amazonka.VoiceId.Types.JobProgress
import Amazonka.VoiceId.Types.OutputDataConfig
import Amazonka.VoiceId.Types.RegistrationConfig

-- | Contains all the information about a fraudster registration job.
--
-- /See:/ 'newFraudsterRegistrationJob' smart constructor.
data FraudsterRegistrationJob = FraudsterRegistrationJob'
  { -- | The output data config containing the S3 location where you want Voice
    -- ID to write your job output file; you must also include a KMS key ID in
    -- order to encrypt the file.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The current status of the fraudster registration job.
    jobStatus :: Prelude.Maybe FraudsterRegistrationJobStatus,
    -- | The registration config containing details such as the action to take
    -- when a duplicate fraudster is detected, and the similarity threshold to
    -- use for detecting a duplicate fraudster.
    registrationConfig :: Prelude.Maybe RegistrationConfig,
    -- | The client-provided name for the fraudster registration job.
    jobName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A timestamp showing when the fraudster registration job ended.
    endedAt :: Prelude.Maybe Core.POSIX,
    -- | The service-generated identifier for the fraudster registration job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
    -- to access customer\'s buckets to read the input manifest file and write
    -- the job output file.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Contains details that are populated when an entire batch job fails. In
    -- cases of individual registration job failures, the batch job as a whole
    -- doesn\'t fail; it is completed with a @JobStatus@ of
    -- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
    -- individual registration requests that failed.
    failureDetails :: Prelude.Maybe FailureDetails,
    -- | The identifier of the domain containing the fraudster registration job.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | Shows the completed percentage of registration requests listed in the
    -- input file.
    jobProgress :: Prelude.Maybe JobProgress,
    -- | The input data config containing an S3 URI for the input manifest file
    -- that contains the list of fraudster registration job requests.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | A timestamp showing the creation time of the fraudster registration job.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FraudsterRegistrationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputDataConfig', 'fraudsterRegistrationJob_outputDataConfig' - The output data config containing the S3 location where you want Voice
-- ID to write your job output file; you must also include a KMS key ID in
-- order to encrypt the file.
--
-- 'jobStatus', 'fraudsterRegistrationJob_jobStatus' - The current status of the fraudster registration job.
--
-- 'registrationConfig', 'fraudsterRegistrationJob_registrationConfig' - The registration config containing details such as the action to take
-- when a duplicate fraudster is detected, and the similarity threshold to
-- use for detecting a duplicate fraudster.
--
-- 'jobName', 'fraudsterRegistrationJob_jobName' - The client-provided name for the fraudster registration job.
--
-- 'endedAt', 'fraudsterRegistrationJob_endedAt' - A timestamp showing when the fraudster registration job ended.
--
-- 'jobId', 'fraudsterRegistrationJob_jobId' - The service-generated identifier for the fraudster registration job.
--
-- 'dataAccessRoleArn', 'fraudsterRegistrationJob_dataAccessRoleArn' - The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
-- to access customer\'s buckets to read the input manifest file and write
-- the job output file.
--
-- 'failureDetails', 'fraudsterRegistrationJob_failureDetails' - Contains details that are populated when an entire batch job fails. In
-- cases of individual registration job failures, the batch job as a whole
-- doesn\'t fail; it is completed with a @JobStatus@ of
-- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
-- individual registration requests that failed.
--
-- 'domainId', 'fraudsterRegistrationJob_domainId' - The identifier of the domain containing the fraudster registration job.
--
-- 'jobProgress', 'fraudsterRegistrationJob_jobProgress' - Shows the completed percentage of registration requests listed in the
-- input file.
--
-- 'inputDataConfig', 'fraudsterRegistrationJob_inputDataConfig' - The input data config containing an S3 URI for the input manifest file
-- that contains the list of fraudster registration job requests.
--
-- 'createdAt', 'fraudsterRegistrationJob_createdAt' - A timestamp showing the creation time of the fraudster registration job.
newFraudsterRegistrationJob ::
  FraudsterRegistrationJob
newFraudsterRegistrationJob =
  FraudsterRegistrationJob'
    { outputDataConfig =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      registrationConfig = Prelude.Nothing,
      jobName = Prelude.Nothing,
      endedAt = Prelude.Nothing,
      jobId = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      failureDetails = Prelude.Nothing,
      domainId = Prelude.Nothing,
      jobProgress = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The output data config containing the S3 location where you want Voice
-- ID to write your job output file; you must also include a KMS key ID in
-- order to encrypt the file.
fraudsterRegistrationJob_outputDataConfig :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe OutputDataConfig)
fraudsterRegistrationJob_outputDataConfig = Lens.lens (\FraudsterRegistrationJob' {outputDataConfig} -> outputDataConfig) (\s@FraudsterRegistrationJob' {} a -> s {outputDataConfig = a} :: FraudsterRegistrationJob)

-- | The current status of the fraudster registration job.
fraudsterRegistrationJob_jobStatus :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe FraudsterRegistrationJobStatus)
fraudsterRegistrationJob_jobStatus = Lens.lens (\FraudsterRegistrationJob' {jobStatus} -> jobStatus) (\s@FraudsterRegistrationJob' {} a -> s {jobStatus = a} :: FraudsterRegistrationJob)

-- | The registration config containing details such as the action to take
-- when a duplicate fraudster is detected, and the similarity threshold to
-- use for detecting a duplicate fraudster.
fraudsterRegistrationJob_registrationConfig :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe RegistrationConfig)
fraudsterRegistrationJob_registrationConfig = Lens.lens (\FraudsterRegistrationJob' {registrationConfig} -> registrationConfig) (\s@FraudsterRegistrationJob' {} a -> s {registrationConfig = a} :: FraudsterRegistrationJob)

-- | The client-provided name for the fraudster registration job.
fraudsterRegistrationJob_jobName :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe Prelude.Text)
fraudsterRegistrationJob_jobName = Lens.lens (\FraudsterRegistrationJob' {jobName} -> jobName) (\s@FraudsterRegistrationJob' {} a -> s {jobName = a} :: FraudsterRegistrationJob) Prelude.. Lens.mapping Core._Sensitive

-- | A timestamp showing when the fraudster registration job ended.
fraudsterRegistrationJob_endedAt :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe Prelude.UTCTime)
fraudsterRegistrationJob_endedAt = Lens.lens (\FraudsterRegistrationJob' {endedAt} -> endedAt) (\s@FraudsterRegistrationJob' {} a -> s {endedAt = a} :: FraudsterRegistrationJob) Prelude.. Lens.mapping Core._Time

-- | The service-generated identifier for the fraudster registration job.
fraudsterRegistrationJob_jobId :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe Prelude.Text)
fraudsterRegistrationJob_jobId = Lens.lens (\FraudsterRegistrationJob' {jobId} -> jobId) (\s@FraudsterRegistrationJob' {} a -> s {jobId = a} :: FraudsterRegistrationJob)

-- | The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
-- to access customer\'s buckets to read the input manifest file and write
-- the job output file.
fraudsterRegistrationJob_dataAccessRoleArn :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe Prelude.Text)
fraudsterRegistrationJob_dataAccessRoleArn = Lens.lens (\FraudsterRegistrationJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@FraudsterRegistrationJob' {} a -> s {dataAccessRoleArn = a} :: FraudsterRegistrationJob)

-- | Contains details that are populated when an entire batch job fails. In
-- cases of individual registration job failures, the batch job as a whole
-- doesn\'t fail; it is completed with a @JobStatus@ of
-- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
-- individual registration requests that failed.
fraudsterRegistrationJob_failureDetails :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe FailureDetails)
fraudsterRegistrationJob_failureDetails = Lens.lens (\FraudsterRegistrationJob' {failureDetails} -> failureDetails) (\s@FraudsterRegistrationJob' {} a -> s {failureDetails = a} :: FraudsterRegistrationJob)

-- | The identifier of the domain containing the fraudster registration job.
fraudsterRegistrationJob_domainId :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe Prelude.Text)
fraudsterRegistrationJob_domainId = Lens.lens (\FraudsterRegistrationJob' {domainId} -> domainId) (\s@FraudsterRegistrationJob' {} a -> s {domainId = a} :: FraudsterRegistrationJob)

-- | Shows the completed percentage of registration requests listed in the
-- input file.
fraudsterRegistrationJob_jobProgress :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe JobProgress)
fraudsterRegistrationJob_jobProgress = Lens.lens (\FraudsterRegistrationJob' {jobProgress} -> jobProgress) (\s@FraudsterRegistrationJob' {} a -> s {jobProgress = a} :: FraudsterRegistrationJob)

-- | The input data config containing an S3 URI for the input manifest file
-- that contains the list of fraudster registration job requests.
fraudsterRegistrationJob_inputDataConfig :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe InputDataConfig)
fraudsterRegistrationJob_inputDataConfig = Lens.lens (\FraudsterRegistrationJob' {inputDataConfig} -> inputDataConfig) (\s@FraudsterRegistrationJob' {} a -> s {inputDataConfig = a} :: FraudsterRegistrationJob)

-- | A timestamp showing the creation time of the fraudster registration job.
fraudsterRegistrationJob_createdAt :: Lens.Lens' FraudsterRegistrationJob (Prelude.Maybe Prelude.UTCTime)
fraudsterRegistrationJob_createdAt = Lens.lens (\FraudsterRegistrationJob' {createdAt} -> createdAt) (\s@FraudsterRegistrationJob' {} a -> s {createdAt = a} :: FraudsterRegistrationJob) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON FraudsterRegistrationJob where
  parseJSON =
    Core.withObject
      "FraudsterRegistrationJob"
      ( \x ->
          FraudsterRegistrationJob'
            Prelude.<$> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "JobStatus")
            Prelude.<*> (x Core..:? "RegistrationConfig")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "EndedAt")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "FailureDetails")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "JobProgress")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..:? "CreatedAt")
      )

instance Prelude.Hashable FraudsterRegistrationJob where
  hashWithSalt _salt FraudsterRegistrationJob' {..} =
    _salt `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` registrationConfig
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` endedAt
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` jobProgress
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData FraudsterRegistrationJob where
  rnf FraudsterRegistrationJob' {..} =
    Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf registrationConfig
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf endedAt
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf jobProgress
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf createdAt
