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
-- Module      : Network.AWS.Snowball.Types.JobListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobListEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Snowball.Types.JobState
import Network.AWS.Snowball.Types.JobType
import Network.AWS.Snowball.Types.SnowballType

-- | Each @JobListEntry@ object contains a job\'s state, a job\'s ID, and a
-- value that indicates whether the job is a job part, in the case of an
-- export job.
--
-- /See:/ 'newJobListEntry' smart constructor.
data JobListEntry = JobListEntry'
  { -- | A value that indicates that this job is a main job. A main job
    -- represents a successful request to create an export job. Main jobs
    -- aren\'t associated with any Snowballs. Instead, each main job will have
    -- at least one job part, and each job part is associated with a Snowball.
    -- It might take some time before the job parts associated with a
    -- particular main job are listed, because they are created after the main
    -- job is created.
    isMaster :: Core.Maybe Core.Bool,
    -- | The current state of this job.
    jobState :: Core.Maybe JobState,
    -- | The creation date for this job.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The type of job.
    jobType :: Core.Maybe JobType,
    -- | The type of device used with this job.
    snowballType :: Core.Maybe SnowballType,
    -- | The optional description of this specific job, for example
    -- @Important Photos 2016-08-11@.
    description :: Core.Maybe Core.Text,
    -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isMaster', 'jobListEntry_isMaster' - A value that indicates that this job is a main job. A main job
-- represents a successful request to create an export job. Main jobs
-- aren\'t associated with any Snowballs. Instead, each main job will have
-- at least one job part, and each job part is associated with a Snowball.
-- It might take some time before the job parts associated with a
-- particular main job are listed, because they are created after the main
-- job is created.
--
-- 'jobState', 'jobListEntry_jobState' - The current state of this job.
--
-- 'creationDate', 'jobListEntry_creationDate' - The creation date for this job.
--
-- 'jobType', 'jobListEntry_jobType' - The type of job.
--
-- 'snowballType', 'jobListEntry_snowballType' - The type of device used with this job.
--
-- 'description', 'jobListEntry_description' - The optional description of this specific job, for example
-- @Important Photos 2016-08-11@.
--
-- 'jobId', 'jobListEntry_jobId' - The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
newJobListEntry ::
  JobListEntry
newJobListEntry =
  JobListEntry'
    { isMaster = Core.Nothing,
      jobState = Core.Nothing,
      creationDate = Core.Nothing,
      jobType = Core.Nothing,
      snowballType = Core.Nothing,
      description = Core.Nothing,
      jobId = Core.Nothing
    }

-- | A value that indicates that this job is a main job. A main job
-- represents a successful request to create an export job. Main jobs
-- aren\'t associated with any Snowballs. Instead, each main job will have
-- at least one job part, and each job part is associated with a Snowball.
-- It might take some time before the job parts associated with a
-- particular main job are listed, because they are created after the main
-- job is created.
jobListEntry_isMaster :: Lens.Lens' JobListEntry (Core.Maybe Core.Bool)
jobListEntry_isMaster = Lens.lens (\JobListEntry' {isMaster} -> isMaster) (\s@JobListEntry' {} a -> s {isMaster = a} :: JobListEntry)

-- | The current state of this job.
jobListEntry_jobState :: Lens.Lens' JobListEntry (Core.Maybe JobState)
jobListEntry_jobState = Lens.lens (\JobListEntry' {jobState} -> jobState) (\s@JobListEntry' {} a -> s {jobState = a} :: JobListEntry)

-- | The creation date for this job.
jobListEntry_creationDate :: Lens.Lens' JobListEntry (Core.Maybe Core.UTCTime)
jobListEntry_creationDate = Lens.lens (\JobListEntry' {creationDate} -> creationDate) (\s@JobListEntry' {} a -> s {creationDate = a} :: JobListEntry) Core.. Lens.mapping Core._Time

-- | The type of job.
jobListEntry_jobType :: Lens.Lens' JobListEntry (Core.Maybe JobType)
jobListEntry_jobType = Lens.lens (\JobListEntry' {jobType} -> jobType) (\s@JobListEntry' {} a -> s {jobType = a} :: JobListEntry)

-- | The type of device used with this job.
jobListEntry_snowballType :: Lens.Lens' JobListEntry (Core.Maybe SnowballType)
jobListEntry_snowballType = Lens.lens (\JobListEntry' {snowballType} -> snowballType) (\s@JobListEntry' {} a -> s {snowballType = a} :: JobListEntry)

-- | The optional description of this specific job, for example
-- @Important Photos 2016-08-11@.
jobListEntry_description :: Lens.Lens' JobListEntry (Core.Maybe Core.Text)
jobListEntry_description = Lens.lens (\JobListEntry' {description} -> description) (\s@JobListEntry' {} a -> s {description = a} :: JobListEntry)

-- | The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
jobListEntry_jobId :: Lens.Lens' JobListEntry (Core.Maybe Core.Text)
jobListEntry_jobId = Lens.lens (\JobListEntry' {jobId} -> jobId) (\s@JobListEntry' {} a -> s {jobId = a} :: JobListEntry)

instance Core.FromJSON JobListEntry where
  parseJSON =
    Core.withObject
      "JobListEntry"
      ( \x ->
          JobListEntry'
            Core.<$> (x Core..:? "IsMaster")
            Core.<*> (x Core..:? "JobState")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "JobType")
            Core.<*> (x Core..:? "SnowballType")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "JobId")
      )

instance Core.Hashable JobListEntry

instance Core.NFData JobListEntry
