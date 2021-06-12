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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains a subset of information about a job execution.
--
-- /See:/ 'newJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { -- | The time, in milliseconds since the epoch, when the job execution
    -- started.
    startedAt :: Core.Maybe Core.Integer,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- enqueued.
    queuedAt :: Core.Maybe Core.Integer,
    -- | The version of the job execution. Job execution versions are incremented
    -- each time AWS IoT Jobs receives an update from a device.
    versionNumber :: Core.Maybe Core.Integer,
    -- | A number that identifies a particular job execution on a particular
    -- device.
    executionNumber :: Core.Maybe Core.Integer,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- last updated.
    lastUpdatedAt :: Core.Maybe Core.Integer,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { startedAt = Core.Nothing,
      queuedAt = Core.Nothing,
      versionNumber = Core.Nothing,
      executionNumber = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      jobId = Core.Nothing
    }

-- | The time, in milliseconds since the epoch, when the job execution
-- started.
jobExecutionSummary_startedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jobExecutionSummary_startedAt = Lens.lens (\JobExecutionSummary' {startedAt} -> startedAt) (\s@JobExecutionSummary' {} a -> s {startedAt = a} :: JobExecutionSummary)

-- | The time, in milliseconds since the epoch, when the job execution was
-- enqueued.
jobExecutionSummary_queuedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jobExecutionSummary_queuedAt = Lens.lens (\JobExecutionSummary' {queuedAt} -> queuedAt) (\s@JobExecutionSummary' {} a -> s {queuedAt = a} :: JobExecutionSummary)

-- | The version of the job execution. Job execution versions are incremented
-- each time AWS IoT Jobs receives an update from a device.
jobExecutionSummary_versionNumber :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jobExecutionSummary_versionNumber = Lens.lens (\JobExecutionSummary' {versionNumber} -> versionNumber) (\s@JobExecutionSummary' {} a -> s {versionNumber = a} :: JobExecutionSummary)

-- | A number that identifies a particular job execution on a particular
-- device.
jobExecutionSummary_executionNumber :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jobExecutionSummary_executionNumber = Lens.lens (\JobExecutionSummary' {executionNumber} -> executionNumber) (\s@JobExecutionSummary' {} a -> s {executionNumber = a} :: JobExecutionSummary)

-- | The time, in milliseconds since the epoch, when the job execution was
-- last updated.
jobExecutionSummary_lastUpdatedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jobExecutionSummary_lastUpdatedAt = Lens.lens (\JobExecutionSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecutionSummary' {} a -> s {lastUpdatedAt = a} :: JobExecutionSummary)

-- | The unique identifier you assigned to this job when it was created.
jobExecutionSummary_jobId :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Text)
jobExecutionSummary_jobId = Lens.lens (\JobExecutionSummary' {jobId} -> jobId) (\s@JobExecutionSummary' {} a -> s {jobId = a} :: JobExecutionSummary)

instance Core.FromJSON JobExecutionSummary where
  parseJSON =
    Core.withObject
      "JobExecutionSummary"
      ( \x ->
          JobExecutionSummary'
            Core.<$> (x Core..:? "startedAt")
            Core.<*> (x Core..:? "queuedAt")
            Core.<*> (x Core..:? "versionNumber")
            Core.<*> (x Core..:? "executionNumber")
            Core.<*> (x Core..:? "lastUpdatedAt")
            Core.<*> (x Core..:? "jobId")
      )

instance Core.Hashable JobExecutionSummary

instance Core.NFData JobExecutionSummary
