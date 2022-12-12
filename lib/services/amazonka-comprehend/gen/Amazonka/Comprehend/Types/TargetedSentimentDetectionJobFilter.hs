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
-- Module      : Amazonka.Comprehend.Types.TargetedSentimentDetectionJobFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.TargetedSentimentDetectionJobFilter where

import Amazonka.Comprehend.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information for filtering a list of dominant language detection
-- jobs. For more information, see the operation.
--
-- /See:/ 'newTargetedSentimentDetectionJobFilter' smart constructor.
data TargetedSentimentDetectionJobFilter = TargetedSentimentDetectionJobFilter'
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
-- Create a value of 'TargetedSentimentDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'targetedSentimentDetectionJobFilter_jobName' - Filters on the name of the job.
--
-- 'jobStatus', 'targetedSentimentDetectionJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeAfter', 'targetedSentimentDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'submitTimeBefore', 'targetedSentimentDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
newTargetedSentimentDetectionJobFilter ::
  TargetedSentimentDetectionJobFilter
newTargetedSentimentDetectionJobFilter =
  TargetedSentimentDetectionJobFilter'
    { jobName =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing
    }

-- | Filters on the name of the job.
targetedSentimentDetectionJobFilter_jobName :: Lens.Lens' TargetedSentimentDetectionJobFilter (Prelude.Maybe Prelude.Text)
targetedSentimentDetectionJobFilter_jobName = Lens.lens (\TargetedSentimentDetectionJobFilter' {jobName} -> jobName) (\s@TargetedSentimentDetectionJobFilter' {} a -> s {jobName = a} :: TargetedSentimentDetectionJobFilter)

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
targetedSentimentDetectionJobFilter_jobStatus :: Lens.Lens' TargetedSentimentDetectionJobFilter (Prelude.Maybe JobStatus)
targetedSentimentDetectionJobFilter_jobStatus = Lens.lens (\TargetedSentimentDetectionJobFilter' {jobStatus} -> jobStatus) (\s@TargetedSentimentDetectionJobFilter' {} a -> s {jobStatus = a} :: TargetedSentimentDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
targetedSentimentDetectionJobFilter_submitTimeAfter :: Lens.Lens' TargetedSentimentDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
targetedSentimentDetectionJobFilter_submitTimeAfter = Lens.lens (\TargetedSentimentDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@TargetedSentimentDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: TargetedSentimentDetectionJobFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
targetedSentimentDetectionJobFilter_submitTimeBefore :: Lens.Lens' TargetedSentimentDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
targetedSentimentDetectionJobFilter_submitTimeBefore = Lens.lens (\TargetedSentimentDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@TargetedSentimentDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: TargetedSentimentDetectionJobFilter) Prelude.. Lens.mapping Data._Time

instance
  Prelude.Hashable
    TargetedSentimentDetectionJobFilter
  where
  hashWithSalt
    _salt
    TargetedSentimentDetectionJobFilter' {..} =
      _salt `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` submitTimeAfter
        `Prelude.hashWithSalt` submitTimeBefore

instance
  Prelude.NFData
    TargetedSentimentDetectionJobFilter
  where
  rnf TargetedSentimentDetectionJobFilter' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf submitTimeAfter
      `Prelude.seq` Prelude.rnf submitTimeBefore

instance
  Data.ToJSON
    TargetedSentimentDetectionJobFilter
  where
  toJSON TargetedSentimentDetectionJobFilter' {..} =
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
