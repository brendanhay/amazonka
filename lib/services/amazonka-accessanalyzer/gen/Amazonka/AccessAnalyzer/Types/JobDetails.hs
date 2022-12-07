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
-- Module      : Amazonka.AccessAnalyzer.Types.JobDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.JobDetails where

import Amazonka.AccessAnalyzer.Types.JobError
import Amazonka.AccessAnalyzer.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the policy generation request.
--
-- /See:/ 'newJobDetails' smart constructor.
data JobDetails = JobDetails'
  { -- | The job error for the policy generation request.
    jobError :: Prelude.Maybe JobError,
    -- | A timestamp of when the job was completed.
    completedOn :: Prelude.Maybe Data.POSIX,
    -- | The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
    -- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
    -- generated policies or used with @CancelPolicyGeneration@ to cancel the
    -- policy generation request.
    jobId :: Prelude.Text,
    -- | The status of the job request.
    status :: JobStatus,
    -- | A timestamp of when the job was started.
    startedOn :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobError', 'jobDetails_jobError' - The job error for the policy generation request.
--
-- 'completedOn', 'jobDetails_completedOn' - A timestamp of when the job was completed.
--
-- 'jobId', 'jobDetails_jobId' - The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
-- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
-- generated policies or used with @CancelPolicyGeneration@ to cancel the
-- policy generation request.
--
-- 'status', 'jobDetails_status' - The status of the job request.
--
-- 'startedOn', 'jobDetails_startedOn' - A timestamp of when the job was started.
newJobDetails ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'status'
  JobStatus ->
  -- | 'startedOn'
  Prelude.UTCTime ->
  JobDetails
newJobDetails pJobId_ pStatus_ pStartedOn_ =
  JobDetails'
    { jobError = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      jobId = pJobId_,
      status = pStatus_,
      startedOn = Data._Time Lens.# pStartedOn_
    }

-- | The job error for the policy generation request.
jobDetails_jobError :: Lens.Lens' JobDetails (Prelude.Maybe JobError)
jobDetails_jobError = Lens.lens (\JobDetails' {jobError} -> jobError) (\s@JobDetails' {} a -> s {jobError = a} :: JobDetails)

-- | A timestamp of when the job was completed.
jobDetails_completedOn :: Lens.Lens' JobDetails (Prelude.Maybe Prelude.UTCTime)
jobDetails_completedOn = Lens.lens (\JobDetails' {completedOn} -> completedOn) (\s@JobDetails' {} a -> s {completedOn = a} :: JobDetails) Prelude.. Lens.mapping Data._Time

-- | The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
-- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
-- generated policies or used with @CancelPolicyGeneration@ to cancel the
-- policy generation request.
jobDetails_jobId :: Lens.Lens' JobDetails Prelude.Text
jobDetails_jobId = Lens.lens (\JobDetails' {jobId} -> jobId) (\s@JobDetails' {} a -> s {jobId = a} :: JobDetails)

-- | The status of the job request.
jobDetails_status :: Lens.Lens' JobDetails JobStatus
jobDetails_status = Lens.lens (\JobDetails' {status} -> status) (\s@JobDetails' {} a -> s {status = a} :: JobDetails)

-- | A timestamp of when the job was started.
jobDetails_startedOn :: Lens.Lens' JobDetails Prelude.UTCTime
jobDetails_startedOn = Lens.lens (\JobDetails' {startedOn} -> startedOn) (\s@JobDetails' {} a -> s {startedOn = a} :: JobDetails) Prelude.. Data._Time

instance Data.FromJSON JobDetails where
  parseJSON =
    Data.withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            Prelude.<$> (x Data..:? "jobError")
            Prelude.<*> (x Data..:? "completedOn")
            Prelude.<*> (x Data..: "jobId")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "startedOn")
      )

instance Prelude.Hashable JobDetails where
  hashWithSalt _salt JobDetails' {..} =
    _salt `Prelude.hashWithSalt` jobError
      `Prelude.hashWithSalt` completedOn
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` startedOn

instance Prelude.NFData JobDetails where
  rnf JobDetails' {..} =
    Prelude.rnf jobError
      `Prelude.seq` Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startedOn
