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
-- Module      : Amazonka.DrS.Types.Job
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.Job where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.InitiatedBy
import Amazonka.DrS.Types.JobStatus
import Amazonka.DrS.Types.JobType
import Amazonka.DrS.Types.ParticipatingServer
import qualified Amazonka.Prelude as Prelude

-- | A job is an asynchronous workflow.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | A list of tags associated with the Job.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A string representing who initiated the Job.
    initiatedBy :: Prelude.Maybe InitiatedBy,
    -- | The type of the Job.
    type' :: Prelude.Maybe JobType,
    -- | The date and time of when the Job was created.
    creationDateTime :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a Job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the Job.
    status :: Prelude.Maybe JobStatus,
    -- | A list of servers that the Job is acting upon.
    participatingServers :: Prelude.Maybe [ParticipatingServer],
    -- | The date and time of when the Job ended.
    endDateTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Job.
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
-- 'tags', 'job_tags' - A list of tags associated with the Job.
--
-- 'initiatedBy', 'job_initiatedBy' - A string representing who initiated the Job.
--
-- 'type'', 'job_type' - The type of the Job.
--
-- 'creationDateTime', 'job_creationDateTime' - The date and time of when the Job was created.
--
-- 'arn', 'job_arn' - The ARN of a Job.
--
-- 'status', 'job_status' - The status of the Job.
--
-- 'participatingServers', 'job_participatingServers' - A list of servers that the Job is acting upon.
--
-- 'endDateTime', 'job_endDateTime' - The date and time of when the Job ended.
--
-- 'jobID', 'job_jobID' - The ID of the Job.
newJob ::
  -- | 'jobID'
  Prelude.Text ->
  Job
newJob pJobID_ =
  Job'
    { tags = Prelude.Nothing,
      initiatedBy = Prelude.Nothing,
      type' = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      participatingServers = Prelude.Nothing,
      endDateTime = Prelude.Nothing,
      jobID = pJobID_
    }

-- | A list of tags associated with the Job.
job_tags :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_tags = Lens.lens (\Job' {tags} -> tags) (\s@Job' {} a -> s {tags = a} :: Job) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A string representing who initiated the Job.
job_initiatedBy :: Lens.Lens' Job (Prelude.Maybe InitiatedBy)
job_initiatedBy = Lens.lens (\Job' {initiatedBy} -> initiatedBy) (\s@Job' {} a -> s {initiatedBy = a} :: Job)

-- | The type of the Job.
job_type :: Lens.Lens' Job (Prelude.Maybe JobType)
job_type = Lens.lens (\Job' {type'} -> type') (\s@Job' {} a -> s {type' = a} :: Job)

-- | The date and time of when the Job was created.
job_creationDateTime :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_creationDateTime = Lens.lens (\Job' {creationDateTime} -> creationDateTime) (\s@Job' {} a -> s {creationDateTime = a} :: Job)

-- | The ARN of a Job.
job_arn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_arn = Lens.lens (\Job' {arn} -> arn) (\s@Job' {} a -> s {arn = a} :: Job)

-- | The status of the Job.
job_status :: Lens.Lens' Job (Prelude.Maybe JobStatus)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | A list of servers that the Job is acting upon.
job_participatingServers :: Lens.Lens' Job (Prelude.Maybe [ParticipatingServer])
job_participatingServers = Lens.lens (\Job' {participatingServers} -> participatingServers) (\s@Job' {} a -> s {participatingServers = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The date and time of when the Job ended.
job_endDateTime :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_endDateTime = Lens.lens (\Job' {endDateTime} -> endDateTime) (\s@Job' {} a -> s {endDateTime = a} :: Job)

-- | The ID of the Job.
job_jobID :: Lens.Lens' Job Prelude.Text
job_jobID = Lens.lens (\Job' {jobID} -> jobID) (\s@Job' {} a -> s {jobID = a} :: Job)

instance Data.FromJSON Job where
  parseJSON =
    Data.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "initiatedBy")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> ( x Data..:? "participatingServers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "endDateTime")
            Prelude.<*> (x Data..: "jobID")
      )

instance Prelude.Hashable Job where
  hashWithSalt _salt Job' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` initiatedBy
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` participatingServers
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` jobID

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf initiatedBy
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf participatingServers
      `Prelude.seq` Prelude.rnf endDateTime
      `Prelude.seq` Prelude.rnf jobID
