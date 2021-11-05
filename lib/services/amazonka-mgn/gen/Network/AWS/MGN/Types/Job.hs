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
-- Module      : Network.AWS.MGN.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Types.Job where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MGN.Types.InitiatedBy
import Network.AWS.MGN.Types.JobStatus
import Network.AWS.MGN.Types.JobType
import Network.AWS.MGN.Types.ParticipatingServer
import qualified Network.AWS.Prelude as Prelude

-- | Job.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | Job initiated by field.
    initiatedBy :: Prelude.Maybe InitiatedBy,
    -- | Job status.
    status :: Prelude.Maybe JobStatus,
    -- | Servers participating in a specific Job.
    participatingServers :: Prelude.Maybe [ParticipatingServer],
    -- | the ARN of the specific Job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Job creation time.
    creationDateTime :: Prelude.Maybe Prelude.Text,
    -- | Job type.
    type' :: Prelude.Maybe JobType,
    -- | Job end time.
    endDateTime :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with spcific Job.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Job ID.
    jobID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initiatedBy', 'job_initiatedBy' - Job initiated by field.
--
-- 'status', 'job_status' - Job status.
--
-- 'participatingServers', 'job_participatingServers' - Servers participating in a specific Job.
--
-- 'arn', 'job_arn' - the ARN of the specific Job.
--
-- 'creationDateTime', 'job_creationDateTime' - Job creation time.
--
-- 'type'', 'job_type' - Job type.
--
-- 'endDateTime', 'job_endDateTime' - Job end time.
--
-- 'tags', 'job_tags' - Tags associated with spcific Job.
--
-- 'jobID', 'job_jobID' - Job ID.
newJob ::
  -- | 'jobID'
  Prelude.Text ->
  Job
newJob pJobID_ =
  Job'
    { initiatedBy = Prelude.Nothing,
      status = Prelude.Nothing,
      participatingServers = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      type' = Prelude.Nothing,
      endDateTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      jobID = pJobID_
    }

-- | Job initiated by field.
job_initiatedBy :: Lens.Lens' Job (Prelude.Maybe InitiatedBy)
job_initiatedBy = Lens.lens (\Job' {initiatedBy} -> initiatedBy) (\s@Job' {} a -> s {initiatedBy = a} :: Job)

-- | Job status.
job_status :: Lens.Lens' Job (Prelude.Maybe JobStatus)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | Servers participating in a specific Job.
job_participatingServers :: Lens.Lens' Job (Prelude.Maybe [ParticipatingServer])
job_participatingServers = Lens.lens (\Job' {participatingServers} -> participatingServers) (\s@Job' {} a -> s {participatingServers = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | the ARN of the specific Job.
job_arn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_arn = Lens.lens (\Job' {arn} -> arn) (\s@Job' {} a -> s {arn = a} :: Job)

-- | Job creation time.
job_creationDateTime :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_creationDateTime = Lens.lens (\Job' {creationDateTime} -> creationDateTime) (\s@Job' {} a -> s {creationDateTime = a} :: Job)

-- | Job type.
job_type :: Lens.Lens' Job (Prelude.Maybe JobType)
job_type = Lens.lens (\Job' {type'} -> type') (\s@Job' {} a -> s {type' = a} :: Job)

-- | Job end time.
job_endDateTime :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_endDateTime = Lens.lens (\Job' {endDateTime} -> endDateTime) (\s@Job' {} a -> s {endDateTime = a} :: Job)

-- | Tags associated with spcific Job.
job_tags :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_tags = Lens.lens (\Job' {tags} -> tags) (\s@Job' {} a -> s {tags = a} :: Job) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Job ID.
job_jobID :: Lens.Lens' Job Prelude.Text
job_jobID = Lens.lens (\Job' {jobID} -> jobID) (\s@Job' {} a -> s {jobID = a} :: Job)

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Core..:? "initiatedBy")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> ( x Core..:? "participatingServers"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "endDateTime")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "jobID")
      )

instance Prelude.Hashable Job

instance Prelude.NFData Job
