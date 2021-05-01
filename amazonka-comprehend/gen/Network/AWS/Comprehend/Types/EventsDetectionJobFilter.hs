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
-- Module      : Network.AWS.Comprehend.Types.EventsDetectionJobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EventsDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information for filtering a list of event detection jobs.
--
-- /See:/ 'newEventsDetectionJobFilter' smart constructor.
data EventsDetectionJobFilter = EventsDetectionJobFilter'
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
    -- | Filters on the name of the events detection job.
    jobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventsDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'eventsDetectionJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeBefore', 'eventsDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
--
-- 'submitTimeAfter', 'eventsDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'jobName', 'eventsDetectionJobFilter_jobName' - Filters on the name of the events detection job.
newEventsDetectionJobFilter ::
  EventsDetectionJobFilter
newEventsDetectionJobFilter =
  EventsDetectionJobFilter'
    { jobStatus =
        Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
eventsDetectionJobFilter_jobStatus :: Lens.Lens' EventsDetectionJobFilter (Prelude.Maybe JobStatus)
eventsDetectionJobFilter_jobStatus = Lens.lens (\EventsDetectionJobFilter' {jobStatus} -> jobStatus) (\s@EventsDetectionJobFilter' {} a -> s {jobStatus = a} :: EventsDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
eventsDetectionJobFilter_submitTimeBefore :: Lens.Lens' EventsDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
eventsDetectionJobFilter_submitTimeBefore = Lens.lens (\EventsDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@EventsDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: EventsDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
eventsDetectionJobFilter_submitTimeAfter :: Lens.Lens' EventsDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
eventsDetectionJobFilter_submitTimeAfter = Lens.lens (\EventsDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@EventsDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: EventsDetectionJobFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters on the name of the events detection job.
eventsDetectionJobFilter_jobName :: Lens.Lens' EventsDetectionJobFilter (Prelude.Maybe Prelude.Text)
eventsDetectionJobFilter_jobName = Lens.lens (\EventsDetectionJobFilter' {jobName} -> jobName) (\s@EventsDetectionJobFilter' {} a -> s {jobName = a} :: EventsDetectionJobFilter)

instance Prelude.Hashable EventsDetectionJobFilter

instance Prelude.NFData EventsDetectionJobFilter

instance Prelude.ToJSON EventsDetectionJobFilter where
  toJSON EventsDetectionJobFilter' {..} =
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
