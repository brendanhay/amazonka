{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ImportExport.Types.JobType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Representation of a job returned by the ListJobs operation.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { jobType :: JobType,
    jobId :: Prelude.Text,
    isCanceled :: Prelude.Bool,
    creationDate :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'isCanceled'
  Prelude.Bool ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  Job
newJob pJobType_ pJobId_ pIsCanceled_ pCreationDate_ =
  Job'
    { jobType = pJobType_,
      jobId = pJobId_,
      isCanceled = pIsCanceled_,
      creationDate = Prelude._Time Lens.# pCreationDate_
    }

-- | Undocumented member.
job_jobType :: Lens.Lens' Job JobType
job_jobType = Lens.lens (\Job' {jobType} -> jobType) (\s@Job' {} a -> s {jobType = a} :: Job)

-- | Undocumented member.
job_jobId :: Lens.Lens' Job Prelude.Text
job_jobId = Lens.lens (\Job' {jobId} -> jobId) (\s@Job' {} a -> s {jobId = a} :: Job)

-- | Undocumented member.
job_isCanceled :: Lens.Lens' Job Prelude.Bool
job_isCanceled = Lens.lens (\Job' {isCanceled} -> isCanceled) (\s@Job' {} a -> s {isCanceled = a} :: Job)

-- | Undocumented member.
job_creationDate :: Lens.Lens' Job Prelude.UTCTime
job_creationDate = Lens.lens (\Job' {creationDate} -> creationDate) (\s@Job' {} a -> s {creationDate = a} :: Job) Prelude.. Prelude._Time

instance Prelude.FromXML Job where
  parseXML x =
    Job'
      Prelude.<$> (x Prelude..@ "JobType")
      Prelude.<*> (x Prelude..@ "JobId")
      Prelude.<*> (x Prelude..@ "IsCanceled")
      Prelude.<*> (x Prelude..@ "CreationDate")

instance Prelude.Hashable Job

instance Prelude.NFData Job
