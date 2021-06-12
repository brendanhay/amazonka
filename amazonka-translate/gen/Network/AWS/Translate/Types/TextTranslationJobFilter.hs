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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Translate.Types.JobStatus

-- | Provides information for filtering a list of translation jobs. For more
-- information, see ListTextTranslationJobs.
--
-- /See:/ 'newTextTranslationJobFilter' smart constructor.
data TextTranslationJobFilter = TextTranslationJobFilter'
  { -- | Filters the list of jobs based by job status.
    jobStatus :: Core.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing and returns only the jobs submitted after the specified
    -- time. Jobs are returned in descending order, newest to oldest.
    submittedAfterTime :: Core.Maybe Core.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing and returns only the jobs submitted before the specified
    -- time. Jobs are returned in ascending order, oldest to newest.
    submittedBeforeTime :: Core.Maybe Core.POSIX,
    -- | Filters the list of jobs by name.
    jobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { jobStatus = Core.Nothing,
      submittedAfterTime = Core.Nothing,
      submittedBeforeTime = Core.Nothing,
      jobName = Core.Nothing
    }

-- | Filters the list of jobs based by job status.
textTranslationJobFilter_jobStatus :: Lens.Lens' TextTranslationJobFilter (Core.Maybe JobStatus)
textTranslationJobFilter_jobStatus = Lens.lens (\TextTranslationJobFilter' {jobStatus} -> jobStatus) (\s@TextTranslationJobFilter' {} a -> s {jobStatus = a} :: TextTranslationJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing and returns only the jobs submitted after the specified
-- time. Jobs are returned in descending order, newest to oldest.
textTranslationJobFilter_submittedAfterTime :: Lens.Lens' TextTranslationJobFilter (Core.Maybe Core.UTCTime)
textTranslationJobFilter_submittedAfterTime = Lens.lens (\TextTranslationJobFilter' {submittedAfterTime} -> submittedAfterTime) (\s@TextTranslationJobFilter' {} a -> s {submittedAfterTime = a} :: TextTranslationJobFilter) Core.. Lens.mapping Core._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing and returns only the jobs submitted before the specified
-- time. Jobs are returned in ascending order, oldest to newest.
textTranslationJobFilter_submittedBeforeTime :: Lens.Lens' TextTranslationJobFilter (Core.Maybe Core.UTCTime)
textTranslationJobFilter_submittedBeforeTime = Lens.lens (\TextTranslationJobFilter' {submittedBeforeTime} -> submittedBeforeTime) (\s@TextTranslationJobFilter' {} a -> s {submittedBeforeTime = a} :: TextTranslationJobFilter) Core.. Lens.mapping Core._Time

-- | Filters the list of jobs by name.
textTranslationJobFilter_jobName :: Lens.Lens' TextTranslationJobFilter (Core.Maybe Core.Text)
textTranslationJobFilter_jobName = Lens.lens (\TextTranslationJobFilter' {jobName} -> jobName) (\s@TextTranslationJobFilter' {} a -> s {jobName = a} :: TextTranslationJobFilter)

instance Core.Hashable TextTranslationJobFilter

instance Core.NFData TextTranslationJobFilter

instance Core.ToJSON TextTranslationJobFilter where
  toJSON TextTranslationJobFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobStatus" Core..=) Core.<$> jobStatus,
            ("SubmittedAfterTime" Core..=)
              Core.<$> submittedAfterTime,
            ("SubmittedBeforeTime" Core..=)
              Core.<$> submittedBeforeTime,
            ("JobName" Core..=) Core.<$> jobName
          ]
      )
