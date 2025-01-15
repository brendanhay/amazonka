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
-- Module      : Amazonka.ImportExport.Types.Job
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImportExport.Types.Job where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImportExport.Types.JobType
import qualified Amazonka.Prelude as Prelude

-- | Representation of a job returned by the ListJobs operation.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { jobType :: JobType,
    jobId :: Prelude.Text,
    isCanceled :: Prelude.Bool,
    creationDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
      creationDate = Data._Time Lens.# pCreationDate_
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
job_creationDate = Lens.lens (\Job' {creationDate} -> creationDate) (\s@Job' {} a -> s {creationDate = a} :: Job) Prelude.. Data._Time

instance Data.FromXML Job where
  parseXML x =
    Job'
      Prelude.<$> (x Data..@ "JobType")
      Prelude.<*> (x Data..@ "JobId")
      Prelude.<*> (x Data..@ "IsCanceled")
      Prelude.<*> (x Data..@ "CreationDate")

instance Prelude.Hashable Job where
  hashWithSalt _salt Job' {..} =
    _salt
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` isCanceled
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf jobType `Prelude.seq`
      Prelude.rnf jobId `Prelude.seq`
        Prelude.rnf isCanceled `Prelude.seq`
          Prelude.rnf creationDate
