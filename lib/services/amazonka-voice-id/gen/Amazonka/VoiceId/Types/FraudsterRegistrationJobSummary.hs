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
-- Module      : Amazonka.VoiceId.Types.FraudsterRegistrationJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.FraudsterRegistrationJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.FailureDetails
import Amazonka.VoiceId.Types.FraudsterRegistrationJobStatus
import Amazonka.VoiceId.Types.JobProgress

-- | Contains a summary of information about a fraudster registration job.
--
-- /See:/ 'newFraudsterRegistrationJobSummary' smart constructor.
data FraudsterRegistrationJobSummary = FraudsterRegistrationJobSummary'
  { -- | A timestamp showing when the fraudster registration job is created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the domain containing the fraudster registration job.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp showing when the fraudster registration job ended.
    endedAt :: Prelude.Maybe Data.POSIX,
    -- | Contains details that are populated when an entire batch job fails. In
    -- cases of individual registration job failures, the batch job as a whole
    -- doesn\'t fail; it is completed with a @JobStatus@ of
    -- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
    -- individual registration requests that failed.
    failureDetails :: Prelude.Maybe FailureDetails,
    -- | The service-generated identifier for the fraudster registration job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The client-provided name for the fraudster registration job.
    jobName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Shows the completed percentage of registration requests listed in the
    -- input file.
    jobProgress :: Prelude.Maybe JobProgress,
    -- | The current status of the fraudster registration job.
    jobStatus :: Prelude.Maybe FraudsterRegistrationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FraudsterRegistrationJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'fraudsterRegistrationJobSummary_createdAt' - A timestamp showing when the fraudster registration job is created.
--
-- 'domainId', 'fraudsterRegistrationJobSummary_domainId' - The identifier of the domain containing the fraudster registration job.
--
-- 'endedAt', 'fraudsterRegistrationJobSummary_endedAt' - A timestamp showing when the fraudster registration job ended.
--
-- 'failureDetails', 'fraudsterRegistrationJobSummary_failureDetails' - Contains details that are populated when an entire batch job fails. In
-- cases of individual registration job failures, the batch job as a whole
-- doesn\'t fail; it is completed with a @JobStatus@ of
-- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
-- individual registration requests that failed.
--
-- 'jobId', 'fraudsterRegistrationJobSummary_jobId' - The service-generated identifier for the fraudster registration job.
--
-- 'jobName', 'fraudsterRegistrationJobSummary_jobName' - The client-provided name for the fraudster registration job.
--
-- 'jobProgress', 'fraudsterRegistrationJobSummary_jobProgress' - Shows the completed percentage of registration requests listed in the
-- input file.
--
-- 'jobStatus', 'fraudsterRegistrationJobSummary_jobStatus' - The current status of the fraudster registration job.
newFraudsterRegistrationJobSummary ::
  FraudsterRegistrationJobSummary
newFraudsterRegistrationJobSummary =
  FraudsterRegistrationJobSummary'
    { createdAt =
        Prelude.Nothing,
      domainId = Prelude.Nothing,
      endedAt = Prelude.Nothing,
      failureDetails = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobProgress = Prelude.Nothing,
      jobStatus = Prelude.Nothing
    }

-- | A timestamp showing when the fraudster registration job is created.
fraudsterRegistrationJobSummary_createdAt :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe Prelude.UTCTime)
fraudsterRegistrationJobSummary_createdAt = Lens.lens (\FraudsterRegistrationJobSummary' {createdAt} -> createdAt) (\s@FraudsterRegistrationJobSummary' {} a -> s {createdAt = a} :: FraudsterRegistrationJobSummary) Prelude.. Lens.mapping Data._Time

-- | The identifier of the domain containing the fraudster registration job.
fraudsterRegistrationJobSummary_domainId :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe Prelude.Text)
fraudsterRegistrationJobSummary_domainId = Lens.lens (\FraudsterRegistrationJobSummary' {domainId} -> domainId) (\s@FraudsterRegistrationJobSummary' {} a -> s {domainId = a} :: FraudsterRegistrationJobSummary)

-- | A timestamp showing when the fraudster registration job ended.
fraudsterRegistrationJobSummary_endedAt :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe Prelude.UTCTime)
fraudsterRegistrationJobSummary_endedAt = Lens.lens (\FraudsterRegistrationJobSummary' {endedAt} -> endedAt) (\s@FraudsterRegistrationJobSummary' {} a -> s {endedAt = a} :: FraudsterRegistrationJobSummary) Prelude.. Lens.mapping Data._Time

-- | Contains details that are populated when an entire batch job fails. In
-- cases of individual registration job failures, the batch job as a whole
-- doesn\'t fail; it is completed with a @JobStatus@ of
-- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
-- individual registration requests that failed.
fraudsterRegistrationJobSummary_failureDetails :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe FailureDetails)
fraudsterRegistrationJobSummary_failureDetails = Lens.lens (\FraudsterRegistrationJobSummary' {failureDetails} -> failureDetails) (\s@FraudsterRegistrationJobSummary' {} a -> s {failureDetails = a} :: FraudsterRegistrationJobSummary)

-- | The service-generated identifier for the fraudster registration job.
fraudsterRegistrationJobSummary_jobId :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe Prelude.Text)
fraudsterRegistrationJobSummary_jobId = Lens.lens (\FraudsterRegistrationJobSummary' {jobId} -> jobId) (\s@FraudsterRegistrationJobSummary' {} a -> s {jobId = a} :: FraudsterRegistrationJobSummary)

-- | The client-provided name for the fraudster registration job.
fraudsterRegistrationJobSummary_jobName :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe Prelude.Text)
fraudsterRegistrationJobSummary_jobName = Lens.lens (\FraudsterRegistrationJobSummary' {jobName} -> jobName) (\s@FraudsterRegistrationJobSummary' {} a -> s {jobName = a} :: FraudsterRegistrationJobSummary) Prelude.. Lens.mapping Data._Sensitive

-- | Shows the completed percentage of registration requests listed in the
-- input file.
fraudsterRegistrationJobSummary_jobProgress :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe JobProgress)
fraudsterRegistrationJobSummary_jobProgress = Lens.lens (\FraudsterRegistrationJobSummary' {jobProgress} -> jobProgress) (\s@FraudsterRegistrationJobSummary' {} a -> s {jobProgress = a} :: FraudsterRegistrationJobSummary)

-- | The current status of the fraudster registration job.
fraudsterRegistrationJobSummary_jobStatus :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe FraudsterRegistrationJobStatus)
fraudsterRegistrationJobSummary_jobStatus = Lens.lens (\FraudsterRegistrationJobSummary' {jobStatus} -> jobStatus) (\s@FraudsterRegistrationJobSummary' {} a -> s {jobStatus = a} :: FraudsterRegistrationJobSummary)

instance
  Data.FromJSON
    FraudsterRegistrationJobSummary
  where
  parseJSON =
    Data.withObject
      "FraudsterRegistrationJobSummary"
      ( \x ->
          FraudsterRegistrationJobSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "EndedAt")
            Prelude.<*> (x Data..:? "FailureDetails")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobProgress")
            Prelude.<*> (x Data..:? "JobStatus")
      )

instance
  Prelude.Hashable
    FraudsterRegistrationJobSummary
  where
  hashWithSalt
    _salt
    FraudsterRegistrationJobSummary' {..} =
      _salt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` domainId
        `Prelude.hashWithSalt` endedAt
        `Prelude.hashWithSalt` failureDetails
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobProgress
        `Prelude.hashWithSalt` jobStatus

instance
  Prelude.NFData
    FraudsterRegistrationJobSummary
  where
  rnf FraudsterRegistrationJobSummary' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf domainId `Prelude.seq`
        Prelude.rnf endedAt `Prelude.seq`
          Prelude.rnf failureDetails `Prelude.seq`
            Prelude.rnf jobId `Prelude.seq`
              Prelude.rnf jobName `Prelude.seq`
                Prelude.rnf jobProgress `Prelude.seq`
                  Prelude.rnf jobStatus
