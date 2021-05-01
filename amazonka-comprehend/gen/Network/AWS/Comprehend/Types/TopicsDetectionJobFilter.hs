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
-- Module      : Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.TopicsDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information for filtering topic detection jobs. For more
-- information, see .
--
-- /See:/ 'newTopicsDetectionJobFilter' smart constructor.
data TopicsDetectionJobFilter = TopicsDetectionJobFilter'
  { -- | Filters the list of topic detection jobs based on job status. Returns
    -- only jobs with the specified status.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Only returns jobs submitted before the specified time.
    -- Jobs are returned in descending order, newest to oldest.
    submitTimeBefore :: Prelude.Maybe Prelude.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Only returns jobs submitted after the specified time.
    -- Jobs are returned in ascending order, oldest to newest.
    submitTimeAfter :: Prelude.Maybe Prelude.POSIX,
    jobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TopicsDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'topicsDetectionJobFilter_jobStatus' - Filters the list of topic detection jobs based on job status. Returns
-- only jobs with the specified status.
--
-- 'submitTimeBefore', 'topicsDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted before the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'submitTimeAfter', 'topicsDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted after the specified time.
-- Jobs are returned in ascending order, oldest to newest.
--
-- 'jobName', 'topicsDetectionJobFilter_jobName' -
newTopicsDetectionJobFilter ::
  TopicsDetectionJobFilter
newTopicsDetectionJobFilter =
  TopicsDetectionJobFilter'
    { jobStatus =
        Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | Filters the list of topic detection jobs based on job status. Returns
-- only jobs with the specified status.
topicsDetectionJobFilter_jobStatus :: Lens.Lens' TopicsDetectionJobFilter (Prelude.Maybe JobStatus)
topicsDetectionJobFilter_jobStatus = Lens.lens (\TopicsDetectionJobFilter' {jobStatus} -> jobStatus) (\s@TopicsDetectionJobFilter' {} a -> s {jobStatus = a} :: TopicsDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted before the specified time.
-- Jobs are returned in descending order, newest to oldest.
topicsDetectionJobFilter_submitTimeBefore :: Lens.Lens' TopicsDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
topicsDetectionJobFilter_submitTimeBefore = Lens.lens (\TopicsDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@TopicsDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: TopicsDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted after the specified time.
-- Jobs are returned in ascending order, oldest to newest.
topicsDetectionJobFilter_submitTimeAfter :: Lens.Lens' TopicsDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
topicsDetectionJobFilter_submitTimeAfter = Lens.lens (\TopicsDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@TopicsDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: TopicsDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- |
topicsDetectionJobFilter_jobName :: Lens.Lens' TopicsDetectionJobFilter (Prelude.Maybe Prelude.Text)
topicsDetectionJobFilter_jobName = Lens.lens (\TopicsDetectionJobFilter' {jobName} -> jobName) (\s@TopicsDetectionJobFilter' {} a -> s {jobName = a} :: TopicsDetectionJobFilter)

instance Prelude.Hashable TopicsDetectionJobFilter

instance Prelude.NFData TopicsDetectionJobFilter

instance Prelude.ToJSON TopicsDetectionJobFilter where
  toJSON TopicsDetectionJobFilter' {..} =
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
