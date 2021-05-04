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
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information for filtering a list of PII entity detection jobs.
--
-- /See:/ 'newPiiEntitiesDetectionJobFilter' smart constructor.
data PiiEntitiesDetectionJobFilter = PiiEntitiesDetectionJobFilter'
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
-- Create a value of 'PiiEntitiesDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'piiEntitiesDetectionJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeBefore', 'piiEntitiesDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
--
-- 'submitTimeAfter', 'piiEntitiesDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'jobName', 'piiEntitiesDetectionJobFilter_jobName' - Filters on the name of the job.
newPiiEntitiesDetectionJobFilter ::
  PiiEntitiesDetectionJobFilter
newPiiEntitiesDetectionJobFilter =
  PiiEntitiesDetectionJobFilter'
    { jobStatus =
        Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
piiEntitiesDetectionJobFilter_jobStatus :: Lens.Lens' PiiEntitiesDetectionJobFilter (Prelude.Maybe JobStatus)
piiEntitiesDetectionJobFilter_jobStatus = Lens.lens (\PiiEntitiesDetectionJobFilter' {jobStatus} -> jobStatus) (\s@PiiEntitiesDetectionJobFilter' {} a -> s {jobStatus = a} :: PiiEntitiesDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
piiEntitiesDetectionJobFilter_submitTimeBefore :: Lens.Lens' PiiEntitiesDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
piiEntitiesDetectionJobFilter_submitTimeBefore = Lens.lens (\PiiEntitiesDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@PiiEntitiesDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: PiiEntitiesDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
piiEntitiesDetectionJobFilter_submitTimeAfter :: Lens.Lens' PiiEntitiesDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
piiEntitiesDetectionJobFilter_submitTimeAfter = Lens.lens (\PiiEntitiesDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@PiiEntitiesDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: PiiEntitiesDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters on the name of the job.
piiEntitiesDetectionJobFilter_jobName :: Lens.Lens' PiiEntitiesDetectionJobFilter (Prelude.Maybe Prelude.Text)
piiEntitiesDetectionJobFilter_jobName = Lens.lens (\PiiEntitiesDetectionJobFilter' {jobName} -> jobName) (\s@PiiEntitiesDetectionJobFilter' {} a -> s {jobName = a} :: PiiEntitiesDetectionJobFilter)

instance
  Prelude.Hashable
    PiiEntitiesDetectionJobFilter

instance Prelude.NFData PiiEntitiesDetectionJobFilter

instance Prelude.ToJSON PiiEntitiesDetectionJobFilter where
  toJSON PiiEntitiesDetectionJobFilter' {..} =
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
