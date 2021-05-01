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
-- Module      : Network.AWS.IoTJobsData.Types.JobExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types.JobExecutionSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains a subset of information about a job execution.
--
-- /See:/ 'newJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { -- | The time, in milliseconds since the epoch, when the job execution
    -- started.
    startedAt :: Prelude.Maybe Prelude.Integer,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- enqueued.
    queuedAt :: Prelude.Maybe Prelude.Integer,
    -- | The version of the job execution. Job execution versions are incremented
    -- each time AWS IoT Jobs receives an update from a device.
    versionNumber :: Prelude.Maybe Prelude.Integer,
    -- | A number that identifies a particular job execution on a particular
    -- device.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- last updated.
    lastUpdatedAt :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startedAt', 'jobExecutionSummary_startedAt' - The time, in milliseconds since the epoch, when the job execution
-- started.
--
-- 'queuedAt', 'jobExecutionSummary_queuedAt' - The time, in milliseconds since the epoch, when the job execution was
-- enqueued.
--
-- 'versionNumber', 'jobExecutionSummary_versionNumber' - The version of the job execution. Job execution versions are incremented
-- each time AWS IoT Jobs receives an update from a device.
--
-- 'executionNumber', 'jobExecutionSummary_executionNumber' - A number that identifies a particular job execution on a particular
-- device.
--
-- 'lastUpdatedAt', 'jobExecutionSummary_lastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was
-- last updated.
--
-- 'jobId', 'jobExecutionSummary_jobId' - The unique identifier you assigned to this job when it was created.
newJobExecutionSummary ::
  JobExecutionSummary
newJobExecutionSummary =
  JobExecutionSummary'
    { startedAt = Prelude.Nothing,
      queuedAt = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      executionNumber = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The time, in milliseconds since the epoch, when the job execution
-- started.
jobExecutionSummary_startedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_startedAt = Lens.lens (\JobExecutionSummary' {startedAt} -> startedAt) (\s@JobExecutionSummary' {} a -> s {startedAt = a} :: JobExecutionSummary)

-- | The time, in milliseconds since the epoch, when the job execution was
-- enqueued.
jobExecutionSummary_queuedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_queuedAt = Lens.lens (\JobExecutionSummary' {queuedAt} -> queuedAt) (\s@JobExecutionSummary' {} a -> s {queuedAt = a} :: JobExecutionSummary)

-- | The version of the job execution. Job execution versions are incremented
-- each time AWS IoT Jobs receives an update from a device.
jobExecutionSummary_versionNumber :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_versionNumber = Lens.lens (\JobExecutionSummary' {versionNumber} -> versionNumber) (\s@JobExecutionSummary' {} a -> s {versionNumber = a} :: JobExecutionSummary)

-- | A number that identifies a particular job execution on a particular
-- device.
jobExecutionSummary_executionNumber :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_executionNumber = Lens.lens (\JobExecutionSummary' {executionNumber} -> executionNumber) (\s@JobExecutionSummary' {} a -> s {executionNumber = a} :: JobExecutionSummary)

-- | The time, in milliseconds since the epoch, when the job execution was
-- last updated.
jobExecutionSummary_lastUpdatedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_lastUpdatedAt = Lens.lens (\JobExecutionSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecutionSummary' {} a -> s {lastUpdatedAt = a} :: JobExecutionSummary)

-- | The unique identifier you assigned to this job when it was created.
jobExecutionSummary_jobId :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Text)
jobExecutionSummary_jobId = Lens.lens (\JobExecutionSummary' {jobId} -> jobId) (\s@JobExecutionSummary' {} a -> s {jobId = a} :: JobExecutionSummary)

instance Prelude.FromJSON JobExecutionSummary where
  parseJSON =
    Prelude.withObject
      "JobExecutionSummary"
      ( \x ->
          JobExecutionSummary'
            Prelude.<$> (x Prelude..:? "startedAt")
            Prelude.<*> (x Prelude..:? "queuedAt")
            Prelude.<*> (x Prelude..:? "versionNumber")
            Prelude.<*> (x Prelude..:? "executionNumber")
            Prelude.<*> (x Prelude..:? "lastUpdatedAt")
            Prelude.<*> (x Prelude..:? "jobId")
      )

instance Prelude.Hashable JobExecutionSummary

instance Prelude.NFData JobExecutionSummary
