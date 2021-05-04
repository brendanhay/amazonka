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
-- Module      : Network.AWS.Comprehend.Types.SentimentDetectionJobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information for filtering a list of dominant language detection
-- jobs. For more information, see the operation.
--
-- /See:/ 'newSentimentDetectionJobFilter' smart constructor.
data SentimentDetectionJobFilter = SentimentDetectionJobFilter'
  { -- | Filters the list of jobs based on job status. Returns only jobs with the
    -- specified status.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Returns only jobs submitted before the specified time.
    -- Jobs are returned in ascending order, oldest to newest.
    submitTimeBefore :: Prelude.Maybe Prelude.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Returns only jobs submitted after the specified time.
    -- Jobs are returned in descending order, newest to oldest.
    submitTimeAfter :: Prelude.Maybe Prelude.POSIX,
    -- | Filters on the name of the job.
    jobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SentimentDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'sentimentDetectionJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeBefore', 'sentimentDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
--
-- 'submitTimeAfter', 'sentimentDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'jobName', 'sentimentDetectionJobFilter_jobName' - Filters on the name of the job.
newSentimentDetectionJobFilter ::
  SentimentDetectionJobFilter
newSentimentDetectionJobFilter =
  SentimentDetectionJobFilter'
    { jobStatus =
        Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
sentimentDetectionJobFilter_jobStatus :: Lens.Lens' SentimentDetectionJobFilter (Prelude.Maybe JobStatus)
sentimentDetectionJobFilter_jobStatus = Lens.lens (\SentimentDetectionJobFilter' {jobStatus} -> jobStatus) (\s@SentimentDetectionJobFilter' {} a -> s {jobStatus = a} :: SentimentDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
sentimentDetectionJobFilter_submitTimeBefore :: Lens.Lens' SentimentDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
sentimentDetectionJobFilter_submitTimeBefore = Lens.lens (\SentimentDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@SentimentDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: SentimentDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
sentimentDetectionJobFilter_submitTimeAfter :: Lens.Lens' SentimentDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
sentimentDetectionJobFilter_submitTimeAfter = Lens.lens (\SentimentDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@SentimentDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: SentimentDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters on the name of the job.
sentimentDetectionJobFilter_jobName :: Lens.Lens' SentimentDetectionJobFilter (Prelude.Maybe Prelude.Text)
sentimentDetectionJobFilter_jobName = Lens.lens (\SentimentDetectionJobFilter' {jobName} -> jobName) (\s@SentimentDetectionJobFilter' {} a -> s {jobName = a} :: SentimentDetectionJobFilter)

instance Prelude.Hashable SentimentDetectionJobFilter

instance Prelude.NFData SentimentDetectionJobFilter

instance Prelude.ToJSON SentimentDetectionJobFilter where
  toJSON SentimentDetectionJobFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("JobStatus" Prelude..=) Prelude.<$> jobStatus,
            ("SubmitTimeBefore" Prelude..=)
              Prelude.<$> submitTimeBefore,
            ("SubmitTimeAfter" Prelude..=)
              Prelude.<$> submitTimeAfter,
            ("JobName" Prelude..=) Prelude.<$> jobName
          ]
      )
