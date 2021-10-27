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
-- Module      : Network.AWS.VoiceId.Types.FraudsterRegistrationJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.VoiceId.Types.FraudsterRegistrationJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.VoiceId.Types.FailureDetails
import Network.AWS.VoiceId.Types.FraudsterRegistrationJobStatus
import Network.AWS.VoiceId.Types.JobProgress

-- | Contains a summary of information about a fraudster registration job.
--
-- /See:/ 'newFraudsterRegistrationJobSummary' smart constructor.
data FraudsterRegistrationJobSummary = FraudsterRegistrationJobSummary'
  { -- | Contains details that are populated when an entire batch job fails. In
    -- cases of individual registration job failures, the batch job as a whole
    -- doesn\'t fail; it is completed with a @JobStatus@ of
    -- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
    -- individual registration requests that failed.
    failureDetails :: Prelude.Maybe FailureDetails,
    -- | The service-generated identifier for the fraudster registration job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp showing when the fraudster registration job is created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The client-provied name for the fraudster registration job.
    jobName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A timestamp showing when the fraudster registration job ended.
    endedAt :: Prelude.Maybe Core.POSIX,
    -- | Shows the completed percentage of registration requests listed in the
    -- input file.
    jobProgress :: Prelude.Maybe JobProgress,
    -- | The identifier of the domain containing the fraudster registration job.
    domainId :: Prelude.Maybe Prelude.Text,
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
-- 'failureDetails', 'fraudsterRegistrationJobSummary_failureDetails' - Contains details that are populated when an entire batch job fails. In
-- cases of individual registration job failures, the batch job as a whole
-- doesn\'t fail; it is completed with a @JobStatus@ of
-- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
-- individual registration requests that failed.
--
-- 'jobId', 'fraudsterRegistrationJobSummary_jobId' - The service-generated identifier for the fraudster registration job.
--
-- 'createdAt', 'fraudsterRegistrationJobSummary_createdAt' - A timestamp showing when the fraudster registration job is created.
--
-- 'jobName', 'fraudsterRegistrationJobSummary_jobName' - The client-provied name for the fraudster registration job.
--
-- 'endedAt', 'fraudsterRegistrationJobSummary_endedAt' - A timestamp showing when the fraudster registration job ended.
--
-- 'jobProgress', 'fraudsterRegistrationJobSummary_jobProgress' - Shows the completed percentage of registration requests listed in the
-- input file.
--
-- 'domainId', 'fraudsterRegistrationJobSummary_domainId' - The identifier of the domain containing the fraudster registration job.
--
-- 'jobStatus', 'fraudsterRegistrationJobSummary_jobStatus' - The current status of the fraudster registration job.
newFraudsterRegistrationJobSummary ::
  FraudsterRegistrationJobSummary
newFraudsterRegistrationJobSummary =
  FraudsterRegistrationJobSummary'
    { failureDetails =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      jobName = Prelude.Nothing,
      endedAt = Prelude.Nothing,
      jobProgress = Prelude.Nothing,
      domainId = Prelude.Nothing,
      jobStatus = Prelude.Nothing
    }

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

-- | A timestamp showing when the fraudster registration job is created.
fraudsterRegistrationJobSummary_createdAt :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe Prelude.UTCTime)
fraudsterRegistrationJobSummary_createdAt = Lens.lens (\FraudsterRegistrationJobSummary' {createdAt} -> createdAt) (\s@FraudsterRegistrationJobSummary' {} a -> s {createdAt = a} :: FraudsterRegistrationJobSummary) Prelude.. Lens.mapping Core._Time

-- | The client-provied name for the fraudster registration job.
fraudsterRegistrationJobSummary_jobName :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe Prelude.Text)
fraudsterRegistrationJobSummary_jobName = Lens.lens (\FraudsterRegistrationJobSummary' {jobName} -> jobName) (\s@FraudsterRegistrationJobSummary' {} a -> s {jobName = a} :: FraudsterRegistrationJobSummary) Prelude.. Lens.mapping Core._Sensitive

-- | A timestamp showing when the fraudster registration job ended.
fraudsterRegistrationJobSummary_endedAt :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe Prelude.UTCTime)
fraudsterRegistrationJobSummary_endedAt = Lens.lens (\FraudsterRegistrationJobSummary' {endedAt} -> endedAt) (\s@FraudsterRegistrationJobSummary' {} a -> s {endedAt = a} :: FraudsterRegistrationJobSummary) Prelude.. Lens.mapping Core._Time

-- | Shows the completed percentage of registration requests listed in the
-- input file.
fraudsterRegistrationJobSummary_jobProgress :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe JobProgress)
fraudsterRegistrationJobSummary_jobProgress = Lens.lens (\FraudsterRegistrationJobSummary' {jobProgress} -> jobProgress) (\s@FraudsterRegistrationJobSummary' {} a -> s {jobProgress = a} :: FraudsterRegistrationJobSummary)

-- | The identifier of the domain containing the fraudster registration job.
fraudsterRegistrationJobSummary_domainId :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe Prelude.Text)
fraudsterRegistrationJobSummary_domainId = Lens.lens (\FraudsterRegistrationJobSummary' {domainId} -> domainId) (\s@FraudsterRegistrationJobSummary' {} a -> s {domainId = a} :: FraudsterRegistrationJobSummary)

-- | The current status of the fraudster registration job.
fraudsterRegistrationJobSummary_jobStatus :: Lens.Lens' FraudsterRegistrationJobSummary (Prelude.Maybe FraudsterRegistrationJobStatus)
fraudsterRegistrationJobSummary_jobStatus = Lens.lens (\FraudsterRegistrationJobSummary' {jobStatus} -> jobStatus) (\s@FraudsterRegistrationJobSummary' {} a -> s {jobStatus = a} :: FraudsterRegistrationJobSummary)

instance
  Core.FromJSON
    FraudsterRegistrationJobSummary
  where
  parseJSON =
    Core.withObject
      "FraudsterRegistrationJobSummary"
      ( \x ->
          FraudsterRegistrationJobSummary'
            Prelude.<$> (x Core..:? "FailureDetails")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "EndedAt")
            Prelude.<*> (x Core..:? "JobProgress")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "JobStatus")
      )

instance
  Prelude.Hashable
    FraudsterRegistrationJobSummary

instance
  Prelude.NFData
    FraudsterRegistrationJobSummary
