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
-- Module      : Amazonka.Comprehend.Types.EventsDetectionJobFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EventsDetectionJobFilter where

import Amazonka.Comprehend.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information for filtering a list of event detection jobs.
--
-- /See:/ 'newEventsDetectionJobFilter' smart constructor.
data EventsDetectionJobFilter = EventsDetectionJobFilter'
  { -- | Filters on the name of the events detection job.
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
-- Create a value of 'EventsDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'eventsDetectionJobFilter_jobName' - Filters on the name of the events detection job.
--
-- 'jobStatus', 'eventsDetectionJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeAfter', 'eventsDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'submitTimeBefore', 'eventsDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
newEventsDetectionJobFilter ::
  EventsDetectionJobFilter
newEventsDetectionJobFilter =
  EventsDetectionJobFilter'
    { jobName =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing
    }

-- | Filters on the name of the events detection job.
eventsDetectionJobFilter_jobName :: Lens.Lens' EventsDetectionJobFilter (Prelude.Maybe Prelude.Text)
eventsDetectionJobFilter_jobName = Lens.lens (\EventsDetectionJobFilter' {jobName} -> jobName) (\s@EventsDetectionJobFilter' {} a -> s {jobName = a} :: EventsDetectionJobFilter)

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
eventsDetectionJobFilter_jobStatus :: Lens.Lens' EventsDetectionJobFilter (Prelude.Maybe JobStatus)
eventsDetectionJobFilter_jobStatus = Lens.lens (\EventsDetectionJobFilter' {jobStatus} -> jobStatus) (\s@EventsDetectionJobFilter' {} a -> s {jobStatus = a} :: EventsDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
eventsDetectionJobFilter_submitTimeAfter :: Lens.Lens' EventsDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
eventsDetectionJobFilter_submitTimeAfter = Lens.lens (\EventsDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@EventsDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: EventsDetectionJobFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
eventsDetectionJobFilter_submitTimeBefore :: Lens.Lens' EventsDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
eventsDetectionJobFilter_submitTimeBefore = Lens.lens (\EventsDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@EventsDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: EventsDetectionJobFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable EventsDetectionJobFilter where
  hashWithSalt _salt EventsDetectionJobFilter' {..} =
    _salt
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` submitTimeAfter
      `Prelude.hashWithSalt` submitTimeBefore

instance Prelude.NFData EventsDetectionJobFilter where
  rnf EventsDetectionJobFilter' {..} =
    Prelude.rnf jobName `Prelude.seq`
      Prelude.rnf jobStatus `Prelude.seq`
        Prelude.rnf submitTimeAfter `Prelude.seq`
          Prelude.rnf submitTimeBefore

instance Data.ToJSON EventsDetectionJobFilter where
  toJSON EventsDetectionJobFilter' {..} =
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
