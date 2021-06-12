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
-- Module      : Network.AWS.Comprehend.Types.DocumentClassificationJobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassificationJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information for filtering a list of document classification
-- jobs. For more information, see the operation. You can provide only one
-- filter parameter in each request.
--
-- /See:/ 'newDocumentClassificationJobFilter' smart constructor.
data DocumentClassificationJobFilter = DocumentClassificationJobFilter'
  { -- | Filters the list based on job status. Returns only jobs with the
    -- specified status.
    jobStatus :: Core.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Returns only jobs submitted before the specified time.
    -- Jobs are returned in ascending order, oldest to newest.
    submitTimeBefore :: Core.Maybe Core.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Returns only jobs submitted after the specified time.
    -- Jobs are returned in descending order, newest to oldest.
    submitTimeAfter :: Core.Maybe Core.POSIX,
    -- | Filters on the name of the job.
    jobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentClassificationJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'documentClassificationJobFilter_jobStatus' - Filters the list based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeBefore', 'documentClassificationJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
--
-- 'submitTimeAfter', 'documentClassificationJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'jobName', 'documentClassificationJobFilter_jobName' - Filters on the name of the job.
newDocumentClassificationJobFilter ::
  DocumentClassificationJobFilter
newDocumentClassificationJobFilter =
  DocumentClassificationJobFilter'
    { jobStatus =
        Core.Nothing,
      submitTimeBefore = Core.Nothing,
      submitTimeAfter = Core.Nothing,
      jobName = Core.Nothing
    }

-- | Filters the list based on job status. Returns only jobs with the
-- specified status.
documentClassificationJobFilter_jobStatus :: Lens.Lens' DocumentClassificationJobFilter (Core.Maybe JobStatus)
documentClassificationJobFilter_jobStatus = Lens.lens (\DocumentClassificationJobFilter' {jobStatus} -> jobStatus) (\s@DocumentClassificationJobFilter' {} a -> s {jobStatus = a} :: DocumentClassificationJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
documentClassificationJobFilter_submitTimeBefore :: Lens.Lens' DocumentClassificationJobFilter (Core.Maybe Core.UTCTime)
documentClassificationJobFilter_submitTimeBefore = Lens.lens (\DocumentClassificationJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@DocumentClassificationJobFilter' {} a -> s {submitTimeBefore = a} :: DocumentClassificationJobFilter) Core.. Lens.mapping Core._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
documentClassificationJobFilter_submitTimeAfter :: Lens.Lens' DocumentClassificationJobFilter (Core.Maybe Core.UTCTime)
documentClassificationJobFilter_submitTimeAfter = Lens.lens (\DocumentClassificationJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@DocumentClassificationJobFilter' {} a -> s {submitTimeAfter = a} :: DocumentClassificationJobFilter) Core.. Lens.mapping Core._Time

-- | Filters on the name of the job.
documentClassificationJobFilter_jobName :: Lens.Lens' DocumentClassificationJobFilter (Core.Maybe Core.Text)
documentClassificationJobFilter_jobName = Lens.lens (\DocumentClassificationJobFilter' {jobName} -> jobName) (\s@DocumentClassificationJobFilter' {} a -> s {jobName = a} :: DocumentClassificationJobFilter)

instance
  Core.Hashable
    DocumentClassificationJobFilter

instance Core.NFData DocumentClassificationJobFilter

instance Core.ToJSON DocumentClassificationJobFilter where
  toJSON DocumentClassificationJobFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobStatus" Core..=) Core.<$> jobStatus,
            ("SubmitTimeBefore" Core..=)
              Core.<$> submitTimeBefore,
            ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
            ("JobName" Core..=) Core.<$> jobName
          ]
      )
