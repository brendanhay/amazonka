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
-- Module      : Amazonka.Comprehend.Types.EntitiesDetectionJobFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntitiesDetectionJobFilter where

import Amazonka.Comprehend.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information for filtering a list of dominant language detection
-- jobs. For more information, see the operation.
--
-- /See:/ 'newEntitiesDetectionJobFilter' smart constructor.
data EntitiesDetectionJobFilter = EntitiesDetectionJobFilter'
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
-- Create a value of 'EntitiesDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'entitiesDetectionJobFilter_jobName' - Filters on the name of the job.
--
-- 'jobStatus', 'entitiesDetectionJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeAfter', 'entitiesDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'submitTimeBefore', 'entitiesDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
newEntitiesDetectionJobFilter ::
  EntitiesDetectionJobFilter
newEntitiesDetectionJobFilter =
  EntitiesDetectionJobFilter'
    { jobName =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing
    }

-- | Filters on the name of the job.
entitiesDetectionJobFilter_jobName :: Lens.Lens' EntitiesDetectionJobFilter (Prelude.Maybe Prelude.Text)
entitiesDetectionJobFilter_jobName = Lens.lens (\EntitiesDetectionJobFilter' {jobName} -> jobName) (\s@EntitiesDetectionJobFilter' {} a -> s {jobName = a} :: EntitiesDetectionJobFilter)

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
entitiesDetectionJobFilter_jobStatus :: Lens.Lens' EntitiesDetectionJobFilter (Prelude.Maybe JobStatus)
entitiesDetectionJobFilter_jobStatus = Lens.lens (\EntitiesDetectionJobFilter' {jobStatus} -> jobStatus) (\s@EntitiesDetectionJobFilter' {} a -> s {jobStatus = a} :: EntitiesDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
entitiesDetectionJobFilter_submitTimeAfter :: Lens.Lens' EntitiesDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
entitiesDetectionJobFilter_submitTimeAfter = Lens.lens (\EntitiesDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@EntitiesDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: EntitiesDetectionJobFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
entitiesDetectionJobFilter_submitTimeBefore :: Lens.Lens' EntitiesDetectionJobFilter (Prelude.Maybe Prelude.UTCTime)
entitiesDetectionJobFilter_submitTimeBefore = Lens.lens (\EntitiesDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@EntitiesDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: EntitiesDetectionJobFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable EntitiesDetectionJobFilter where
  hashWithSalt _salt EntitiesDetectionJobFilter' {..} =
    _salt
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` submitTimeAfter
      `Prelude.hashWithSalt` submitTimeBefore

instance Prelude.NFData EntitiesDetectionJobFilter where
  rnf EntitiesDetectionJobFilter' {..} =
    Prelude.rnf jobName `Prelude.seq`
      Prelude.rnf jobStatus `Prelude.seq`
        Prelude.rnf submitTimeAfter `Prelude.seq`
          Prelude.rnf submitTimeBefore

instance Data.ToJSON EntitiesDetectionJobFilter where
  toJSON EntitiesDetectionJobFilter' {..} =
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
