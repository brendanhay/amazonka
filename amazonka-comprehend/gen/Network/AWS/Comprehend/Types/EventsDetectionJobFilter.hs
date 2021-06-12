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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information for filtering a list of event detection jobs.
--
-- /See:/ 'newEventsDetectionJobFilter' smart constructor.
data EventsDetectionJobFilter = EventsDetectionJobFilter'
  { -- | Filters the list of jobs based on job status. Returns only jobs with the
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
    -- | Filters on the name of the events detection job.
    jobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { jobStatus = Core.Nothing,
      submitTimeBefore = Core.Nothing,
      submitTimeAfter = Core.Nothing,
      jobName = Core.Nothing
    }

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
eventsDetectionJobFilter_jobStatus :: Lens.Lens' EventsDetectionJobFilter (Core.Maybe JobStatus)
eventsDetectionJobFilter_jobStatus = Lens.lens (\EventsDetectionJobFilter' {jobStatus} -> jobStatus) (\s@EventsDetectionJobFilter' {} a -> s {jobStatus = a} :: EventsDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
eventsDetectionJobFilter_submitTimeBefore :: Lens.Lens' EventsDetectionJobFilter (Core.Maybe Core.UTCTime)
eventsDetectionJobFilter_submitTimeBefore = Lens.lens (\EventsDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@EventsDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: EventsDetectionJobFilter) Core.. Lens.mapping Core._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
eventsDetectionJobFilter_submitTimeAfter :: Lens.Lens' EventsDetectionJobFilter (Core.Maybe Core.UTCTime)
eventsDetectionJobFilter_submitTimeAfter = Lens.lens (\EventsDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@EventsDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: EventsDetectionJobFilter) Core.. Lens.mapping Core._Time

-- | Filters on the name of the events detection job.
eventsDetectionJobFilter_jobName :: Lens.Lens' EventsDetectionJobFilter (Core.Maybe Core.Text)
eventsDetectionJobFilter_jobName = Lens.lens (\EventsDetectionJobFilter' {jobName} -> jobName) (\s@EventsDetectionJobFilter' {} a -> s {jobName = a} :: EventsDetectionJobFilter)

instance Core.Hashable EventsDetectionJobFilter

instance Core.NFData EventsDetectionJobFilter

instance Core.ToJSON EventsDetectionJobFilter where
  toJSON EventsDetectionJobFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobStatus" Core..=) Core.<$> jobStatus,
            ("SubmitTimeBefore" Core..=)
              Core.<$> submitTimeBefore,
            ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
            ("JobName" Core..=) Core.<$> jobName
          ]
      )
