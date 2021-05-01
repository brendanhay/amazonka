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
-- Module      : Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information for filtering a list of dominant language detection
-- jobs. For more information, see the operation.
--
-- /See:/ 'newDominantLanguageDetectionJobFilter' smart constructor.
data DominantLanguageDetectionJobFilter = DominantLanguageDetectionJobFilter'
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
-- Create a value of 'DominantLanguageDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'dominantLanguageDetectionJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeBefore', 'dominantLanguageDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
--
-- 'submitTimeAfter', 'dominantLanguageDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'jobName', 'dominantLanguageDetectionJobFilter_jobName' - Filters on the name of the job.
newDominantLanguageDetectionJobFilter ::
  DominantLanguageDetectionJobFilter
newDominantLanguageDetectionJobFilter =
  DominantLanguageDetectionJobFilter'
    { jobStatus =
        Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
dominantLanguageDetectionJobFilter_jobStatus :: Lens.Lens' DominantLanguageDetectionJobFilter (Prelude.Maybe JobStatus)
dominantLanguageDetectionJobFilter_jobStatus = Lens.lens (\DominantLanguageDetectionJobFilter' {jobStatus} -> jobStatus) (\s@DominantLanguageDetectionJobFilter' {} a -> s {jobStatus = a} :: DominantLanguageDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
dominantLanguageDetectionJobFilter_submitTimeBefore :: Lens.Lens' DominantLanguageDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
dominantLanguageDetectionJobFilter_submitTimeBefore = Lens.lens (\DominantLanguageDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@DominantLanguageDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: DominantLanguageDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
dominantLanguageDetectionJobFilter_submitTimeAfter :: Lens.Lens' DominantLanguageDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
dominantLanguageDetectionJobFilter_submitTimeAfter = Lens.lens (\DominantLanguageDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@DominantLanguageDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: DominantLanguageDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters on the name of the job.
dominantLanguageDetectionJobFilter_jobName :: Lens.Lens' DominantLanguageDetectionJobFilter (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobFilter_jobName = Lens.lens (\DominantLanguageDetectionJobFilter' {jobName} -> jobName) (\s@DominantLanguageDetectionJobFilter' {} a -> s {jobName = a} :: DominantLanguageDetectionJobFilter)

instance
  Prelude.Hashable
    DominantLanguageDetectionJobFilter

instance
  Prelude.NFData
    DominantLanguageDetectionJobFilter

instance
  Prelude.ToJSON
    DominantLanguageDetectionJobFilter
  where
  toJSON DominantLanguageDetectionJobFilter' {..} =
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
