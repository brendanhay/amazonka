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
-- Module      : Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobFilter where

import Amazonka.ComprehendMedical.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information for filtering a list of detection jobs.
--
-- /See:/ 'newComprehendMedicalAsyncJobFilter' smart constructor.
data ComprehendMedicalAsyncJobFilter = ComprehendMedicalAsyncJobFilter'
  { -- | Filters on the name of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of jobs based on job status. Returns only jobs with the
    -- specified status.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Returns only jobs submitted after the specified time.
    -- Jobs are returned in descending order, newest to oldest.
    submitTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Returns only jobs submitted before the specified time.
    -- Jobs are returned in ascending order, oldest to newest.
    submitTimeBefore :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComprehendMedicalAsyncJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'comprehendMedicalAsyncJobFilter_jobName' - Filters on the name of the job.
--
-- 'jobStatus', 'comprehendMedicalAsyncJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeAfter', 'comprehendMedicalAsyncJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'submitTimeBefore', 'comprehendMedicalAsyncJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
newComprehendMedicalAsyncJobFilter ::
  ComprehendMedicalAsyncJobFilter
newComprehendMedicalAsyncJobFilter =
  ComprehendMedicalAsyncJobFilter'
    { jobName =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing
    }

-- | Filters on the name of the job.
comprehendMedicalAsyncJobFilter_jobName :: Lens.Lens' ComprehendMedicalAsyncJobFilter (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobFilter_jobName = Lens.lens (\ComprehendMedicalAsyncJobFilter' {jobName} -> jobName) (\s@ComprehendMedicalAsyncJobFilter' {} a -> s {jobName = a} :: ComprehendMedicalAsyncJobFilter)

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
comprehendMedicalAsyncJobFilter_jobStatus :: Lens.Lens' ComprehendMedicalAsyncJobFilter (Prelude.Maybe JobStatus)
comprehendMedicalAsyncJobFilter_jobStatus = Lens.lens (\ComprehendMedicalAsyncJobFilter' {jobStatus} -> jobStatus) (\s@ComprehendMedicalAsyncJobFilter' {} a -> s {jobStatus = a} :: ComprehendMedicalAsyncJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
comprehendMedicalAsyncJobFilter_submitTimeAfter :: Lens.Lens' ComprehendMedicalAsyncJobFilter (Prelude.Maybe Prelude.UTCTime)
comprehendMedicalAsyncJobFilter_submitTimeAfter = Lens.lens (\ComprehendMedicalAsyncJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@ComprehendMedicalAsyncJobFilter' {} a -> s {submitTimeAfter = a} :: ComprehendMedicalAsyncJobFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
comprehendMedicalAsyncJobFilter_submitTimeBefore :: Lens.Lens' ComprehendMedicalAsyncJobFilter (Prelude.Maybe Prelude.UTCTime)
comprehendMedicalAsyncJobFilter_submitTimeBefore = Lens.lens (\ComprehendMedicalAsyncJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@ComprehendMedicalAsyncJobFilter' {} a -> s {submitTimeBefore = a} :: ComprehendMedicalAsyncJobFilter) Prelude.. Lens.mapping Data._Time

instance
  Prelude.Hashable
    ComprehendMedicalAsyncJobFilter
  where
  hashWithSalt
    _salt
    ComprehendMedicalAsyncJobFilter' {..} =
      _salt `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` submitTimeAfter
        `Prelude.hashWithSalt` submitTimeBefore

instance
  Prelude.NFData
    ComprehendMedicalAsyncJobFilter
  where
  rnf ComprehendMedicalAsyncJobFilter' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf submitTimeAfter
      `Prelude.seq` Prelude.rnf submitTimeBefore

instance Data.ToJSON ComprehendMedicalAsyncJobFilter where
  toJSON ComprehendMedicalAsyncJobFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobName" Data..=) Prelude.<$> jobName,
            ("JobStatus" Data..=) Prelude.<$> jobStatus,
            ("SubmitTimeAfter" Data..=)
              Prelude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Data..=)
              Prelude.<$> submitTimeBefore
          ]
      )
