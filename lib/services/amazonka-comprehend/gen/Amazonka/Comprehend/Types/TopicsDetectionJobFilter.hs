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
-- Module      : Amazonka.Comprehend.Types.TopicsDetectionJobFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.TopicsDetectionJobFilter where

import Amazonka.Comprehend.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information for filtering topic detection jobs. For more
-- information, see .
--
-- /See:/ 'newTopicsDetectionJobFilter' smart constructor.
data TopicsDetectionJobFilter = TopicsDetectionJobFilter'
  { jobName :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of topic detection jobs based on job status. Returns
    -- only jobs with the specified status.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Only returns jobs submitted after the specified time.
    -- Jobs are returned in ascending order, oldest to newest.
    submitTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Only returns jobs submitted before the specified time.
    -- Jobs are returned in descending order, newest to oldest.
    submitTimeBefore :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicsDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'topicsDetectionJobFilter_jobName' -
--
-- 'jobStatus', 'topicsDetectionJobFilter_jobStatus' - Filters the list of topic detection jobs based on job status. Returns
-- only jobs with the specified status.
--
-- 'submitTimeAfter', 'topicsDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted after the specified time.
-- Jobs are returned in ascending order, oldest to newest.
--
-- 'submitTimeBefore', 'topicsDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted before the specified time.
-- Jobs are returned in descending order, newest to oldest.
newTopicsDetectionJobFilter ::
  TopicsDetectionJobFilter
newTopicsDetectionJobFilter =
  TopicsDetectionJobFilter'
    { jobName =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing
    }

-- |
topicsDetectionJobFilter_jobName :: Lens.Lens' TopicsDetectionJobFilter (Prelude.Maybe Prelude.Text)
topicsDetectionJobFilter_jobName = Lens.lens (\TopicsDetectionJobFilter' {jobName} -> jobName) (\s@TopicsDetectionJobFilter' {} a -> s {jobName = a} :: TopicsDetectionJobFilter)

-- | Filters the list of topic detection jobs based on job status. Returns
-- only jobs with the specified status.
topicsDetectionJobFilter_jobStatus :: Lens.Lens' TopicsDetectionJobFilter (Prelude.Maybe JobStatus)
topicsDetectionJobFilter_jobStatus = Lens.lens (\TopicsDetectionJobFilter' {jobStatus} -> jobStatus) (\s@TopicsDetectionJobFilter' {} a -> s {jobStatus = a} :: TopicsDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted after the specified time.
-- Jobs are returned in ascending order, oldest to newest.
topicsDetectionJobFilter_submitTimeAfter :: Lens.Lens' TopicsDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
topicsDetectionJobFilter_submitTimeAfter = Lens.lens (\TopicsDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@TopicsDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: TopicsDetectionJobFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted before the specified time.
-- Jobs are returned in descending order, newest to oldest.
topicsDetectionJobFilter_submitTimeBefore :: Lens.Lens' TopicsDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
topicsDetectionJobFilter_submitTimeBefore = Lens.lens (\TopicsDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@TopicsDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: TopicsDetectionJobFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable TopicsDetectionJobFilter where
  hashWithSalt _salt TopicsDetectionJobFilter' {..} =
    _salt `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` submitTimeAfter
      `Prelude.hashWithSalt` submitTimeBefore

instance Prelude.NFData TopicsDetectionJobFilter where
  rnf TopicsDetectionJobFilter' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf submitTimeAfter
      `Prelude.seq` Prelude.rnf submitTimeBefore

instance Data.ToJSON TopicsDetectionJobFilter where
  toJSON TopicsDetectionJobFilter' {..} =
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
