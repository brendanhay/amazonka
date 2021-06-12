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
-- Module      : Network.AWS.IoT.Types.JobExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.JobExecutionStatus
import qualified Network.AWS.Lens as Lens

-- | The job execution summary.
--
-- /See:/ 'newJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { -- | The time, in seconds since the epoch, when the job execution started.
    startedAt :: Core.Maybe Core.POSIX,
    -- | The status of the job execution.
    status :: Core.Maybe JobExecutionStatus,
    -- | The time, in seconds since the epoch, when the job execution was queued.
    queuedAt :: Core.Maybe Core.POSIX,
    -- | A string (consisting of the digits \"0\" through \"9\") which identifies
    -- this particular job execution on this particular device. It can be used
    -- later in commands which return or update job execution information.
    executionNumber :: Core.Maybe Core.Integer,
    -- | The time, in seconds since the epoch, when the job execution was last
    -- updated.
    lastUpdatedAt :: Core.Maybe Core.POSIX
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
-- 'startedAt', 'jobExecutionSummary_startedAt' - The time, in seconds since the epoch, when the job execution started.
--
-- 'status', 'jobExecutionSummary_status' - The status of the job execution.
--
-- 'queuedAt', 'jobExecutionSummary_queuedAt' - The time, in seconds since the epoch, when the job execution was queued.
--
-- 'executionNumber', 'jobExecutionSummary_executionNumber' - A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- later in commands which return or update job execution information.
--
-- 'lastUpdatedAt', 'jobExecutionSummary_lastUpdatedAt' - The time, in seconds since the epoch, when the job execution was last
-- updated.
newJobExecutionSummary ::
  JobExecutionSummary
newJobExecutionSummary =
  JobExecutionSummary'
    { startedAt = Core.Nothing,
      status = Core.Nothing,
      queuedAt = Core.Nothing,
      executionNumber = Core.Nothing,
      lastUpdatedAt = Core.Nothing
    }

-- | The time, in seconds since the epoch, when the job execution started.
jobExecutionSummary_startedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.UTCTime)
jobExecutionSummary_startedAt = Lens.lens (\JobExecutionSummary' {startedAt} -> startedAt) (\s@JobExecutionSummary' {} a -> s {startedAt = a} :: JobExecutionSummary) Core.. Lens.mapping Core._Time

-- | The status of the job execution.
jobExecutionSummary_status :: Lens.Lens' JobExecutionSummary (Core.Maybe JobExecutionStatus)
jobExecutionSummary_status = Lens.lens (\JobExecutionSummary' {status} -> status) (\s@JobExecutionSummary' {} a -> s {status = a} :: JobExecutionSummary)

-- | The time, in seconds since the epoch, when the job execution was queued.
jobExecutionSummary_queuedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.UTCTime)
jobExecutionSummary_queuedAt = Lens.lens (\JobExecutionSummary' {queuedAt} -> queuedAt) (\s@JobExecutionSummary' {} a -> s {queuedAt = a} :: JobExecutionSummary) Core.. Lens.mapping Core._Time

-- | A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- later in commands which return or update job execution information.
jobExecutionSummary_executionNumber :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jobExecutionSummary_executionNumber = Lens.lens (\JobExecutionSummary' {executionNumber} -> executionNumber) (\s@JobExecutionSummary' {} a -> s {executionNumber = a} :: JobExecutionSummary)

-- | The time, in seconds since the epoch, when the job execution was last
-- updated.
jobExecutionSummary_lastUpdatedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.UTCTime)
jobExecutionSummary_lastUpdatedAt = Lens.lens (\JobExecutionSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecutionSummary' {} a -> s {lastUpdatedAt = a} :: JobExecutionSummary) Core.. Lens.mapping Core._Time

instance Core.FromJSON JobExecutionSummary where
  parseJSON =
    Core.withObject
      "JobExecutionSummary"
      ( \x ->
          JobExecutionSummary'
            Core.<$> (x Core..:? "startedAt")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "queuedAt")
            Core.<*> (x Core..:? "executionNumber")
            Core.<*> (x Core..:? "lastUpdatedAt")
      )

instance Core.Hashable JobExecutionSummary

instance Core.NFData JobExecutionSummary
