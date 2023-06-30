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
-- Module      : Amazonka.VoiceId.Types.SpeakerEnrollmentJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.SpeakerEnrollmentJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.FailureDetails
import Amazonka.VoiceId.Types.JobProgress
import Amazonka.VoiceId.Types.SpeakerEnrollmentJobStatus

-- | Contains a summary of information about a speaker enrollment job.
--
-- /See:/ 'newSpeakerEnrollmentJobSummary' smart constructor.
data SpeakerEnrollmentJobSummary = SpeakerEnrollmentJobSummary'
  { -- | A timestamp showing the creation time of the speaker enrollment job.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the domain that contains the speaker enrollment job.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp showing when the speaker enrollment job ended.
    endedAt :: Prelude.Maybe Data.POSIX,
    -- | Contains details that are populated when an entire batch job fails. In
    -- cases of individual registration job failures, the batch job as a whole
    -- doesn\'t fail; it is completed with a @JobStatus@ of
    -- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
    -- individual registration requests that failed.
    failureDetails :: Prelude.Maybe FailureDetails,
    -- | The service-generated identifier for the speaker enrollment job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The client-provided name for the speaker enrollment job.
    jobName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Provides details regarding job progress. This field shows the completed
    -- percentage of enrollment requests listed in the input file.
    jobProgress :: Prelude.Maybe JobProgress,
    -- | The current status of the speaker enrollment job.
    jobStatus :: Prelude.Maybe SpeakerEnrollmentJobStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpeakerEnrollmentJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'speakerEnrollmentJobSummary_createdAt' - A timestamp showing the creation time of the speaker enrollment job.
--
-- 'domainId', 'speakerEnrollmentJobSummary_domainId' - The identifier of the domain that contains the speaker enrollment job.
--
-- 'endedAt', 'speakerEnrollmentJobSummary_endedAt' - A timestamp showing when the speaker enrollment job ended.
--
-- 'failureDetails', 'speakerEnrollmentJobSummary_failureDetails' - Contains details that are populated when an entire batch job fails. In
-- cases of individual registration job failures, the batch job as a whole
-- doesn\'t fail; it is completed with a @JobStatus@ of
-- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
-- individual registration requests that failed.
--
-- 'jobId', 'speakerEnrollmentJobSummary_jobId' - The service-generated identifier for the speaker enrollment job.
--
-- 'jobName', 'speakerEnrollmentJobSummary_jobName' - The client-provided name for the speaker enrollment job.
--
-- 'jobProgress', 'speakerEnrollmentJobSummary_jobProgress' - Provides details regarding job progress. This field shows the completed
-- percentage of enrollment requests listed in the input file.
--
-- 'jobStatus', 'speakerEnrollmentJobSummary_jobStatus' - The current status of the speaker enrollment job.
newSpeakerEnrollmentJobSummary ::
  SpeakerEnrollmentJobSummary
newSpeakerEnrollmentJobSummary =
  SpeakerEnrollmentJobSummary'
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

-- | A timestamp showing the creation time of the speaker enrollment job.
speakerEnrollmentJobSummary_createdAt :: Lens.Lens' SpeakerEnrollmentJobSummary (Prelude.Maybe Prelude.UTCTime)
speakerEnrollmentJobSummary_createdAt = Lens.lens (\SpeakerEnrollmentJobSummary' {createdAt} -> createdAt) (\s@SpeakerEnrollmentJobSummary' {} a -> s {createdAt = a} :: SpeakerEnrollmentJobSummary) Prelude.. Lens.mapping Data._Time

-- | The identifier of the domain that contains the speaker enrollment job.
speakerEnrollmentJobSummary_domainId :: Lens.Lens' SpeakerEnrollmentJobSummary (Prelude.Maybe Prelude.Text)
speakerEnrollmentJobSummary_domainId = Lens.lens (\SpeakerEnrollmentJobSummary' {domainId} -> domainId) (\s@SpeakerEnrollmentJobSummary' {} a -> s {domainId = a} :: SpeakerEnrollmentJobSummary)

-- | A timestamp showing when the speaker enrollment job ended.
speakerEnrollmentJobSummary_endedAt :: Lens.Lens' SpeakerEnrollmentJobSummary (Prelude.Maybe Prelude.UTCTime)
speakerEnrollmentJobSummary_endedAt = Lens.lens (\SpeakerEnrollmentJobSummary' {endedAt} -> endedAt) (\s@SpeakerEnrollmentJobSummary' {} a -> s {endedAt = a} :: SpeakerEnrollmentJobSummary) Prelude.. Lens.mapping Data._Time

-- | Contains details that are populated when an entire batch job fails. In
-- cases of individual registration job failures, the batch job as a whole
-- doesn\'t fail; it is completed with a @JobStatus@ of
-- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
-- individual registration requests that failed.
speakerEnrollmentJobSummary_failureDetails :: Lens.Lens' SpeakerEnrollmentJobSummary (Prelude.Maybe FailureDetails)
speakerEnrollmentJobSummary_failureDetails = Lens.lens (\SpeakerEnrollmentJobSummary' {failureDetails} -> failureDetails) (\s@SpeakerEnrollmentJobSummary' {} a -> s {failureDetails = a} :: SpeakerEnrollmentJobSummary)

-- | The service-generated identifier for the speaker enrollment job.
speakerEnrollmentJobSummary_jobId :: Lens.Lens' SpeakerEnrollmentJobSummary (Prelude.Maybe Prelude.Text)
speakerEnrollmentJobSummary_jobId = Lens.lens (\SpeakerEnrollmentJobSummary' {jobId} -> jobId) (\s@SpeakerEnrollmentJobSummary' {} a -> s {jobId = a} :: SpeakerEnrollmentJobSummary)

-- | The client-provided name for the speaker enrollment job.
speakerEnrollmentJobSummary_jobName :: Lens.Lens' SpeakerEnrollmentJobSummary (Prelude.Maybe Prelude.Text)
speakerEnrollmentJobSummary_jobName = Lens.lens (\SpeakerEnrollmentJobSummary' {jobName} -> jobName) (\s@SpeakerEnrollmentJobSummary' {} a -> s {jobName = a} :: SpeakerEnrollmentJobSummary) Prelude.. Lens.mapping Data._Sensitive

-- | Provides details regarding job progress. This field shows the completed
-- percentage of enrollment requests listed in the input file.
speakerEnrollmentJobSummary_jobProgress :: Lens.Lens' SpeakerEnrollmentJobSummary (Prelude.Maybe JobProgress)
speakerEnrollmentJobSummary_jobProgress = Lens.lens (\SpeakerEnrollmentJobSummary' {jobProgress} -> jobProgress) (\s@SpeakerEnrollmentJobSummary' {} a -> s {jobProgress = a} :: SpeakerEnrollmentJobSummary)

-- | The current status of the speaker enrollment job.
speakerEnrollmentJobSummary_jobStatus :: Lens.Lens' SpeakerEnrollmentJobSummary (Prelude.Maybe SpeakerEnrollmentJobStatus)
speakerEnrollmentJobSummary_jobStatus = Lens.lens (\SpeakerEnrollmentJobSummary' {jobStatus} -> jobStatus) (\s@SpeakerEnrollmentJobSummary' {} a -> s {jobStatus = a} :: SpeakerEnrollmentJobSummary)

instance Data.FromJSON SpeakerEnrollmentJobSummary where
  parseJSON =
    Data.withObject
      "SpeakerEnrollmentJobSummary"
      ( \x ->
          SpeakerEnrollmentJobSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "EndedAt")
            Prelude.<*> (x Data..:? "FailureDetails")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobProgress")
            Prelude.<*> (x Data..:? "JobStatus")
      )

instance Prelude.Hashable SpeakerEnrollmentJobSummary where
  hashWithSalt _salt SpeakerEnrollmentJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` endedAt
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobProgress
      `Prelude.hashWithSalt` jobStatus

instance Prelude.NFData SpeakerEnrollmentJobSummary where
  rnf SpeakerEnrollmentJobSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf endedAt
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobProgress
      `Prelude.seq` Prelude.rnf jobStatus
