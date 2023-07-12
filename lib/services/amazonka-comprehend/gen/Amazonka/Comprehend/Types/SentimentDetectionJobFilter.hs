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
-- Module      : Amazonka.Comprehend.Types.SentimentDetectionJobFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.SentimentDetectionJobFilter where

import Amazonka.Comprehend.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information for filtering a list of dominant language detection
-- jobs. For more information, see the operation.
--
-- /See:/ 'newSentimentDetectionJobFilter' smart constructor.
data SentimentDetectionJobFilter = SentimentDetectionJobFilter'
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
-- Create a value of 'SentimentDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'sentimentDetectionJobFilter_jobName' - Filters on the name of the job.
--
-- 'jobStatus', 'sentimentDetectionJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeAfter', 'sentimentDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'submitTimeBefore', 'sentimentDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
newSentimentDetectionJobFilter ::
  SentimentDetectionJobFilter
newSentimentDetectionJobFilter =
  SentimentDetectionJobFilter'
    { jobName =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing
    }

-- | Filters on the name of the job.
sentimentDetectionJobFilter_jobName :: Lens.Lens' SentimentDetectionJobFilter (Prelude.Maybe Prelude.Text)
sentimentDetectionJobFilter_jobName = Lens.lens (\SentimentDetectionJobFilter' {jobName} -> jobName) (\s@SentimentDetectionJobFilter' {} a -> s {jobName = a} :: SentimentDetectionJobFilter)

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
sentimentDetectionJobFilter_jobStatus :: Lens.Lens' SentimentDetectionJobFilter (Prelude.Maybe JobStatus)
sentimentDetectionJobFilter_jobStatus = Lens.lens (\SentimentDetectionJobFilter' {jobStatus} -> jobStatus) (\s@SentimentDetectionJobFilter' {} a -> s {jobStatus = a} :: SentimentDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
sentimentDetectionJobFilter_submitTimeAfter :: Lens.Lens' SentimentDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
sentimentDetectionJobFilter_submitTimeAfter = Lens.lens (\SentimentDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@SentimentDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: SentimentDetectionJobFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
sentimentDetectionJobFilter_submitTimeBefore :: Lens.Lens' SentimentDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
sentimentDetectionJobFilter_submitTimeBefore = Lens.lens (\SentimentDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@SentimentDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: SentimentDetectionJobFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable SentimentDetectionJobFilter where
  hashWithSalt _salt SentimentDetectionJobFilter' {..} =
    _salt
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` submitTimeAfter
      `Prelude.hashWithSalt` submitTimeBefore

instance Prelude.NFData SentimentDetectionJobFilter where
  rnf SentimentDetectionJobFilter' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf submitTimeAfter
      `Prelude.seq` Prelude.rnf submitTimeBefore

instance Data.ToJSON SentimentDetectionJobFilter where
  toJSON SentimentDetectionJobFilter' {..} =
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
