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
-- Module      : Network.AWS.Translate.Types.TextTranslationJobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TextTranslationJobFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Translate.Types.JobStatus

-- | Provides information for filtering a list of translation jobs. For more
-- information, see ListTextTranslationJobs.
--
-- /See:/ 'newTextTranslationJobFilter' smart constructor.
data TextTranslationJobFilter = TextTranslationJobFilter'
  { -- | Filters the list of jobs based by job status.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing and returns only the jobs submitted after the specified
    -- time. Jobs are returned in descending order, newest to oldest.
    submittedAfterTime :: Prelude.Maybe Prelude.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing and returns only the jobs submitted before the specified
    -- time. Jobs are returned in ascending order, oldest to newest.
    submittedBeforeTime :: Prelude.Maybe Prelude.POSIX,
    -- | Filters the list of jobs by name.
    jobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TextTranslationJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'jobName', 'textTranslationJobFilter_jobName' - Filters the list of jobs by name.
newTextTranslationJobFilter ::
  TextTranslationJobFilter
newTextTranslationJobFilter =
  TextTranslationJobFilter'
    { jobStatus =
        Prelude.Nothing,
      submittedAfterTime = Prelude.Nothing,
      submittedBeforeTime = Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | Filters the list of jobs based by job status.
textTranslationJobFilter_jobStatus :: Lens.Lens' TextTranslationJobFilter (Prelude.Maybe JobStatus)
textTranslationJobFilter_jobStatus = Lens.lens (\TextTranslationJobFilter' {jobStatus} -> jobStatus) (\s@TextTranslationJobFilter' {} a -> s {jobStatus = a} :: TextTranslationJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing and returns only the jobs submitted after the specified
-- time. Jobs are returned in descending order, newest to oldest.
textTranslationJobFilter_submittedAfterTime :: Lens.Lens' TextTranslationJobFilter (Prelude.Maybe Prelude.UTCTime)
textTranslationJobFilter_submittedAfterTime = Lens.lens (\TextTranslationJobFilter' {submittedAfterTime} -> submittedAfterTime) (\s@TextTranslationJobFilter' {} a -> s {submittedAfterTime = a} :: TextTranslationJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing and returns only the jobs submitted before the specified
-- time. Jobs are returned in ascending order, oldest to newest.
textTranslationJobFilter_submittedBeforeTime :: Lens.Lens' TextTranslationJobFilter (Prelude.Maybe Prelude.UTCTime)
textTranslationJobFilter_submittedBeforeTime = Lens.lens (\TextTranslationJobFilter' {submittedBeforeTime} -> submittedBeforeTime) (\s@TextTranslationJobFilter' {} a -> s {submittedBeforeTime = a} :: TextTranslationJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters the list of jobs by name.
textTranslationJobFilter_jobName :: Lens.Lens' TextTranslationJobFilter (Prelude.Maybe Prelude.Text)
textTranslationJobFilter_jobName = Lens.lens (\TextTranslationJobFilter' {jobName} -> jobName) (\s@TextTranslationJobFilter' {} a -> s {jobName = a} :: TextTranslationJobFilter)

instance Prelude.Hashable TextTranslationJobFilter

instance Prelude.NFData TextTranslationJobFilter

instance Prelude.ToJSON TextTranslationJobFilter where
  toJSON TextTranslationJobFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("JobStatus" Prelude..=) Prelude.<$> jobStatus,
            ("SubmittedAfterTime" Prelude..=)
              Prelude.<$> submittedAfterTime,
            ("SubmittedBeforeTime" Prelude..=)
              Prelude.<$> submittedBeforeTime,
            ("JobName" Prelude..=) Prelude.<$> jobName
          ]
      )
