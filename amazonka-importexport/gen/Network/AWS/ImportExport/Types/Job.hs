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
-- Module      : Network.AWS.ImportExport.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImportExport.Types.Job where

import qualified Network.AWS.Core as Core
import Network.AWS.ImportExport.Types.JobType
import qualified Network.AWS.Lens as Lens

-- | Representation of a job returned by the ListJobs operation.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { jobType :: JobType,
    jobId :: Core.Text,
    isCanceled :: Core.Bool,
    creationDate :: Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobType', 'job_jobType' - Undocumented member.
--
-- 'jobId', 'job_jobId' - Undocumented member.
--
-- 'isCanceled', 'job_isCanceled' - Undocumented member.
--
-- 'creationDate', 'job_creationDate' - Undocumented member.
newJob ::
  -- | 'jobType'
  JobType ->
  -- | 'jobId'
  Core.Text ->
  -- | 'isCanceled'
  Core.Bool ->
  -- | 'creationDate'
  Core.UTCTime ->
  Job
newJob pJobType_ pJobId_ pIsCanceled_ pCreationDate_ =
  Job'
    { jobType = pJobType_,
      jobId = pJobId_,
      isCanceled = pIsCanceled_,
      creationDate = Core._Time Lens.# pCreationDate_
    }

-- | Undocumented member.
job_jobType :: Lens.Lens' Job JobType
job_jobType = Lens.lens (\Job' {jobType} -> jobType) (\s@Job' {} a -> s {jobType = a} :: Job)

-- | Undocumented member.
job_jobId :: Lens.Lens' Job Core.Text
job_jobId = Lens.lens (\Job' {jobId} -> jobId) (\s@Job' {} a -> s {jobId = a} :: Job)

-- | Undocumented member.
job_isCanceled :: Lens.Lens' Job Core.Bool
job_isCanceled = Lens.lens (\Job' {isCanceled} -> isCanceled) (\s@Job' {} a -> s {isCanceled = a} :: Job)

-- | Undocumented member.
job_creationDate :: Lens.Lens' Job Core.UTCTime
job_creationDate = Lens.lens (\Job' {creationDate} -> creationDate) (\s@Job' {} a -> s {creationDate = a} :: Job) Core.. Core._Time

instance Core.FromXML Job where
  parseXML x =
    Job'
      Core.<$> (x Core..@ "JobType")
      Core.<*> (x Core..@ "JobId")
      Core.<*> (x Core..@ "IsCanceled")
      Core.<*> (x Core..@ "CreationDate")

instance Core.Hashable Job

instance Core.NFData Job
