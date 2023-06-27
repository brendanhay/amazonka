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
-- Module      : Amazonka.Comprehend.Types.DocumentClassificationJobFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClassificationJobFilter where

import Amazonka.Comprehend.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information for filtering a list of document classification
-- jobs. For more information, see the operation. You can provide only one
-- filter parameter in each request.
--
-- /See:/ 'newDocumentClassificationJobFilter' smart constructor.
data DocumentClassificationJobFilter = DocumentClassificationJobFilter'
  { -- | Filters on the name of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Filters the list based on job status. Returns only jobs with the
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
-- Create a value of 'DocumentClassificationJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'documentClassificationJobFilter_jobName' - Filters on the name of the job.
--
-- 'jobStatus', 'documentClassificationJobFilter_jobStatus' - Filters the list based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeAfter', 'documentClassificationJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'submitTimeBefore', 'documentClassificationJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
newDocumentClassificationJobFilter ::
  DocumentClassificationJobFilter
newDocumentClassificationJobFilter =
  DocumentClassificationJobFilter'
    { jobName =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing
    }

-- | Filters on the name of the job.
documentClassificationJobFilter_jobName :: Lens.Lens' DocumentClassificationJobFilter (Prelude.Maybe Prelude.Text)
documentClassificationJobFilter_jobName = Lens.lens (\DocumentClassificationJobFilter' {jobName} -> jobName) (\s@DocumentClassificationJobFilter' {} a -> s {jobName = a} :: DocumentClassificationJobFilter)

-- | Filters the list based on job status. Returns only jobs with the
-- specified status.
documentClassificationJobFilter_jobStatus :: Lens.Lens' DocumentClassificationJobFilter (Prelude.Maybe JobStatus)
documentClassificationJobFilter_jobStatus = Lens.lens (\DocumentClassificationJobFilter' {jobStatus} -> jobStatus) (\s@DocumentClassificationJobFilter' {} a -> s {jobStatus = a} :: DocumentClassificationJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
documentClassificationJobFilter_submitTimeAfter :: Lens.Lens' DocumentClassificationJobFilter (Prelude.Maybe Prelude.UTCTime)
documentClassificationJobFilter_submitTimeAfter = Lens.lens (\DocumentClassificationJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@DocumentClassificationJobFilter' {} a -> s {submitTimeAfter = a} :: DocumentClassificationJobFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
documentClassificationJobFilter_submitTimeBefore :: Lens.Lens' DocumentClassificationJobFilter (Prelude.Maybe Prelude.UTCTime)
documentClassificationJobFilter_submitTimeBefore = Lens.lens (\DocumentClassificationJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@DocumentClassificationJobFilter' {} a -> s {submitTimeBefore = a} :: DocumentClassificationJobFilter) Prelude.. Lens.mapping Data._Time

instance
  Prelude.Hashable
    DocumentClassificationJobFilter
  where
  hashWithSalt
    _salt
    DocumentClassificationJobFilter' {..} =
      _salt
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` submitTimeAfter
        `Prelude.hashWithSalt` submitTimeBefore

instance
  Prelude.NFData
    DocumentClassificationJobFilter
  where
  rnf DocumentClassificationJobFilter' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf submitTimeAfter
      `Prelude.seq` Prelude.rnf submitTimeBefore

instance Data.ToJSON DocumentClassificationJobFilter where
  toJSON DocumentClassificationJobFilter' {..} =
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
