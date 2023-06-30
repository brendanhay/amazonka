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
-- Module      : Amazonka.Translate.Types.TextTranslationJobFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.TextTranslationJobFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.JobStatus

-- | Provides information for filtering a list of translation jobs. For more
-- information, see ListTextTranslationJobs.
--
-- /See:/ 'newTextTranslationJobFilter' smart constructor.
data TextTranslationJobFilter = TextTranslationJobFilter'
  { -- | Filters the list of jobs by name.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of jobs based by job status.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing and returns only the jobs submitted after the specified
    -- time. Jobs are returned in descending order, newest to oldest.
    submittedAfterTime :: Prelude.Maybe Data.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing and returns only the jobs submitted before the specified
    -- time. Jobs are returned in ascending order, oldest to newest.
    submittedBeforeTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextTranslationJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'textTranslationJobFilter_jobName' - Filters the list of jobs by name.
--
-- 'jobStatus', 'textTranslationJobFilter_jobStatus' - Filters the list of jobs based by job status.
--
-- 'submittedAfterTime', 'textTranslationJobFilter_submittedAfterTime' - Filters the list of jobs based on the time that the job was submitted
-- for processing and returns only the jobs submitted after the specified
-- time. Jobs are returned in descending order, newest to oldest.
--
-- 'submittedBeforeTime', 'textTranslationJobFilter_submittedBeforeTime' - Filters the list of jobs based on the time that the job was submitted
-- for processing and returns only the jobs submitted before the specified
-- time. Jobs are returned in ascending order, oldest to newest.
newTextTranslationJobFilter ::
  TextTranslationJobFilter
newTextTranslationJobFilter =
  TextTranslationJobFilter'
    { jobName =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      submittedAfterTime = Prelude.Nothing,
      submittedBeforeTime = Prelude.Nothing
    }

-- | Filters the list of jobs by name.
textTranslationJobFilter_jobName :: Lens.Lens' TextTranslationJobFilter (Prelude.Maybe Prelude.Text)
textTranslationJobFilter_jobName = Lens.lens (\TextTranslationJobFilter' {jobName} -> jobName) (\s@TextTranslationJobFilter' {} a -> s {jobName = a} :: TextTranslationJobFilter)

-- | Filters the list of jobs based by job status.
textTranslationJobFilter_jobStatus :: Lens.Lens' TextTranslationJobFilter (Prelude.Maybe JobStatus)
textTranslationJobFilter_jobStatus = Lens.lens (\TextTranslationJobFilter' {jobStatus} -> jobStatus) (\s@TextTranslationJobFilter' {} a -> s {jobStatus = a} :: TextTranslationJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing and returns only the jobs submitted after the specified
-- time. Jobs are returned in descending order, newest to oldest.
textTranslationJobFilter_submittedAfterTime :: Lens.Lens' TextTranslationJobFilter (Prelude.Maybe Prelude.UTCTime)
textTranslationJobFilter_submittedAfterTime = Lens.lens (\TextTranslationJobFilter' {submittedAfterTime} -> submittedAfterTime) (\s@TextTranslationJobFilter' {} a -> s {submittedAfterTime = a} :: TextTranslationJobFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing and returns only the jobs submitted before the specified
-- time. Jobs are returned in ascending order, oldest to newest.
textTranslationJobFilter_submittedBeforeTime :: Lens.Lens' TextTranslationJobFilter (Prelude.Maybe Prelude.UTCTime)
textTranslationJobFilter_submittedBeforeTime = Lens.lens (\TextTranslationJobFilter' {submittedBeforeTime} -> submittedBeforeTime) (\s@TextTranslationJobFilter' {} a -> s {submittedBeforeTime = a} :: TextTranslationJobFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable TextTranslationJobFilter where
  hashWithSalt _salt TextTranslationJobFilter' {..} =
    _salt
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` submittedAfterTime
      `Prelude.hashWithSalt` submittedBeforeTime

instance Prelude.NFData TextTranslationJobFilter where
  rnf TextTranslationJobFilter' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf submittedAfterTime
      `Prelude.seq` Prelude.rnf submittedBeforeTime

instance Data.ToJSON TextTranslationJobFilter where
  toJSON TextTranslationJobFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobName" Data..=) Prelude.<$> jobName,
            ("JobStatus" Data..=) Prelude.<$> jobStatus,
            ("SubmittedAfterTime" Data..=)
              Prelude.<$> submittedAfterTime,
            ("SubmittedBeforeTime" Data..=)
              Prelude.<$> submittedBeforeTime
          ]
      )
