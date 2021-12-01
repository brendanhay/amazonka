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
-- Module      : Amazonka.Snowball.Types.JobListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.JobListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.JobState
import Amazonka.Snowball.Types.JobType
import Amazonka.Snowball.Types.SnowballType

-- | Each @JobListEntry@ object contains a job\'s state, a job\'s ID, and a
-- value that indicates whether the job is a job part, in the case of an
-- export job.
--
-- /See:/ 'newJobListEntry' smart constructor.
data JobListEntry = JobListEntry'
  { -- | The type of job.
    jobType :: Prelude.Maybe JobType,
    -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The current state of this job.
    jobState :: Prelude.Maybe JobState,
    -- | The type of device used with this job.
    snowballType :: Prelude.Maybe SnowballType,
    -- | The creation date for this job.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The optional description of this specific job, for example
    -- @Important Photos 2016-08-11@.
    description :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates that this job is a main job. A main job
    -- represents a successful request to create an export job. Main jobs
    -- aren\'t associated with any Snowballs. Instead, each main job will have
    -- at least one job part, and each job part is associated with a Snowball.
    -- It might take some time before the job parts associated with a
    -- particular main job are listed, because they are created after the main
    -- job is created.
    isMaster :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobType', 'jobListEntry_jobType' - The type of job.
--
-- 'jobId', 'jobListEntry_jobId' - The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
--
-- 'jobState', 'jobListEntry_jobState' - The current state of this job.
--
-- 'snowballType', 'jobListEntry_snowballType' - The type of device used with this job.
--
-- 'creationDate', 'jobListEntry_creationDate' - The creation date for this job.
--
-- 'description', 'jobListEntry_description' - The optional description of this specific job, for example
-- @Important Photos 2016-08-11@.
--
-- 'isMaster', 'jobListEntry_isMaster' - A value that indicates that this job is a main job. A main job
-- represents a successful request to create an export job. Main jobs
-- aren\'t associated with any Snowballs. Instead, each main job will have
-- at least one job part, and each job part is associated with a Snowball.
-- It might take some time before the job parts associated with a
-- particular main job are listed, because they are created after the main
-- job is created.
newJobListEntry ::
  JobListEntry
newJobListEntry =
  JobListEntry'
    { jobType = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobState = Prelude.Nothing,
      snowballType = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      isMaster = Prelude.Nothing
    }

-- | The type of job.
jobListEntry_jobType :: Lens.Lens' JobListEntry (Prelude.Maybe JobType)
jobListEntry_jobType = Lens.lens (\JobListEntry' {jobType} -> jobType) (\s@JobListEntry' {} a -> s {jobType = a} :: JobListEntry)

-- | The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
jobListEntry_jobId :: Lens.Lens' JobListEntry (Prelude.Maybe Prelude.Text)
jobListEntry_jobId = Lens.lens (\JobListEntry' {jobId} -> jobId) (\s@JobListEntry' {} a -> s {jobId = a} :: JobListEntry)

-- | The current state of this job.
jobListEntry_jobState :: Lens.Lens' JobListEntry (Prelude.Maybe JobState)
jobListEntry_jobState = Lens.lens (\JobListEntry' {jobState} -> jobState) (\s@JobListEntry' {} a -> s {jobState = a} :: JobListEntry)

-- | The type of device used with this job.
jobListEntry_snowballType :: Lens.Lens' JobListEntry (Prelude.Maybe SnowballType)
jobListEntry_snowballType = Lens.lens (\JobListEntry' {snowballType} -> snowballType) (\s@JobListEntry' {} a -> s {snowballType = a} :: JobListEntry)

-- | The creation date for this job.
jobListEntry_creationDate :: Lens.Lens' JobListEntry (Prelude.Maybe Prelude.UTCTime)
jobListEntry_creationDate = Lens.lens (\JobListEntry' {creationDate} -> creationDate) (\s@JobListEntry' {} a -> s {creationDate = a} :: JobListEntry) Prelude.. Lens.mapping Core._Time

-- | The optional description of this specific job, for example
-- @Important Photos 2016-08-11@.
jobListEntry_description :: Lens.Lens' JobListEntry (Prelude.Maybe Prelude.Text)
jobListEntry_description = Lens.lens (\JobListEntry' {description} -> description) (\s@JobListEntry' {} a -> s {description = a} :: JobListEntry)

-- | A value that indicates that this job is a main job. A main job
-- represents a successful request to create an export job. Main jobs
-- aren\'t associated with any Snowballs. Instead, each main job will have
-- at least one job part, and each job part is associated with a Snowball.
-- It might take some time before the job parts associated with a
-- particular main job are listed, because they are created after the main
-- job is created.
jobListEntry_isMaster :: Lens.Lens' JobListEntry (Prelude.Maybe Prelude.Bool)
jobListEntry_isMaster = Lens.lens (\JobListEntry' {isMaster} -> isMaster) (\s@JobListEntry' {} a -> s {isMaster = a} :: JobListEntry)

instance Core.FromJSON JobListEntry where
  parseJSON =
    Core.withObject
      "JobListEntry"
      ( \x ->
          JobListEntry'
            Prelude.<$> (x Core..:? "JobType")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "JobState")
            Prelude.<*> (x Core..:? "SnowballType")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "IsMaster")
      )

instance Prelude.Hashable JobListEntry where
  hashWithSalt salt' JobListEntry' {..} =
    salt' `Prelude.hashWithSalt` isMaster
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` snowballType
      `Prelude.hashWithSalt` jobState
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobType

instance Prelude.NFData JobListEntry where
  rnf JobListEntry' {..} =
    Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf isMaster
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf snowballType
      `Prelude.seq` Prelude.rnf jobState
      `Prelude.seq` Prelude.rnf jobId
