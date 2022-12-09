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
-- Module      : Amazonka.MGN.Types.Job
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.Job where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.InitiatedBy
import Amazonka.MGN.Types.JobStatus
import Amazonka.MGN.Types.JobType
import Amazonka.MGN.Types.ParticipatingServer
import qualified Amazonka.Prelude as Prelude

-- | Job.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | the ARN of the specific Job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Job creation time.
    creationDateTime :: Prelude.Maybe Prelude.Text,
    -- | Job end time.
    endDateTime :: Prelude.Maybe Prelude.Text,
    -- | Job initiated by field.
    initiatedBy :: Prelude.Maybe InitiatedBy,
    -- | Servers participating in a specific Job.
    participatingServers :: Prelude.Maybe [ParticipatingServer],
    -- | Job status.
    status :: Prelude.Maybe JobStatus,
    -- | Tags associated with specific Job.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Job type.
    type' :: Prelude.Maybe JobType,
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
-- 'arn', 'job_arn' - the ARN of the specific Job.
--
-- 'creationDateTime', 'job_creationDateTime' - Job creation time.
--
-- 'endDateTime', 'job_endDateTime' - Job end time.
--
-- 'initiatedBy', 'job_initiatedBy' - Job initiated by field.
--
-- 'participatingServers', 'job_participatingServers' - Servers participating in a specific Job.
--
-- 'status', 'job_status' - Job status.
--
-- 'tags', 'job_tags' - Tags associated with specific Job.
--
-- 'type'', 'job_type' - Job type.
--
-- 'jobID', 'job_jobID' - Job ID.
newJob ::
  -- | 'jobID'
  Prelude.Text ->
  Job
newJob pJobID_ =
  Job'
    { arn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      endDateTime = Prelude.Nothing,
      initiatedBy = Prelude.Nothing,
      participatingServers = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      jobID = pJobID_
    }

-- | the ARN of the specific Job.
job_arn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_arn = Lens.lens (\Job' {arn} -> arn) (\s@Job' {} a -> s {arn = a} :: Job)

-- | Job creation time.
job_creationDateTime :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_creationDateTime = Lens.lens (\Job' {creationDateTime} -> creationDateTime) (\s@Job' {} a -> s {creationDateTime = a} :: Job)

-- | Job end time.
job_endDateTime :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_endDateTime = Lens.lens (\Job' {endDateTime} -> endDateTime) (\s@Job' {} a -> s {endDateTime = a} :: Job)

-- | Job initiated by field.
job_initiatedBy :: Lens.Lens' Job (Prelude.Maybe InitiatedBy)
job_initiatedBy = Lens.lens (\Job' {initiatedBy} -> initiatedBy) (\s@Job' {} a -> s {initiatedBy = a} :: Job)

-- | Servers participating in a specific Job.
job_participatingServers :: Lens.Lens' Job (Prelude.Maybe [ParticipatingServer])
job_participatingServers = Lens.lens (\Job' {participatingServers} -> participatingServers) (\s@Job' {} a -> s {participatingServers = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | Job status.
job_status :: Lens.Lens' Job (Prelude.Maybe JobStatus)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | Tags associated with specific Job.
job_tags :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_tags = Lens.lens (\Job' {tags} -> tags) (\s@Job' {} a -> s {tags = a} :: Job) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Job type.
job_type :: Lens.Lens' Job (Prelude.Maybe JobType)
job_type = Lens.lens (\Job' {type'} -> type') (\s@Job' {} a -> s {type' = a} :: Job)

-- | Job ID.
job_jobID :: Lens.Lens' Job Prelude.Text
job_jobID = Lens.lens (\Job' {jobID} -> jobID) (\s@Job' {} a -> s {jobID = a} :: Job)

instance Data.FromJSON Job where
  parseJSON =
    Data.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "endDateTime")
            Prelude.<*> (x Data..:? "initiatedBy")
            Prelude.<*> ( x Data..:? "participatingServers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..: "jobID")
      )

instance Prelude.Hashable Job where
  hashWithSalt _salt Job' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` initiatedBy
      `Prelude.hashWithSalt` participatingServers
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` jobID

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf endDateTime
      `Prelude.seq` Prelude.rnf initiatedBy
      `Prelude.seq` Prelude.rnf participatingServers
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf jobID
