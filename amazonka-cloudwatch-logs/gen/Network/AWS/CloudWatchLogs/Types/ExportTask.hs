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
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTask where

import Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
import Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an export task.
--
-- /See:/ 'newExportTask' smart constructor.
data ExportTask = ExportTask'
  { -- | The status of the export task.
    status :: Prelude.Maybe ExportTaskStatus,
    -- | Execution information about the export task.
    executionInfo :: Prelude.Maybe ExportTaskExecutionInfo,
    -- | The end time, expressed as the number of milliseconds after Jan 1, 1970
    -- 00:00:00 UTC. Events with a timestamp later than this time are not
    -- exported.
    to :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the export task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The name of the export task.
    taskName :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group from which logs data was exported.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket to which the log data was exported.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The prefix that was used as the start of Amazon S3 key for every object
    -- exported.
    destinationPrefix :: Prelude.Maybe Prelude.Text,
    -- | The start time, expressed as the number of milliseconds after Jan 1,
    -- 1970 00:00:00 UTC. Events with a timestamp before this time are not
    -- exported.
    from :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'exportTask_status' - The status of the export task.
--
-- 'executionInfo', 'exportTask_executionInfo' - Execution information about the export task.
--
-- 'to', 'exportTask_to' - The end time, expressed as the number of milliseconds after Jan 1, 1970
-- 00:00:00 UTC. Events with a timestamp later than this time are not
-- exported.
--
-- 'taskId', 'exportTask_taskId' - The ID of the export task.
--
-- 'taskName', 'exportTask_taskName' - The name of the export task.
--
-- 'logGroupName', 'exportTask_logGroupName' - The name of the log group from which logs data was exported.
--
-- 'destination', 'exportTask_destination' - The name of the S3 bucket to which the log data was exported.
--
-- 'destinationPrefix', 'exportTask_destinationPrefix' - The prefix that was used as the start of Amazon S3 key for every object
-- exported.
--
-- 'from', 'exportTask_from' - The start time, expressed as the number of milliseconds after Jan 1,
-- 1970 00:00:00 UTC. Events with a timestamp before this time are not
-- exported.
newExportTask ::
  ExportTask
newExportTask =
  ExportTask'
    { status = Prelude.Nothing,
      executionInfo = Prelude.Nothing,
      to = Prelude.Nothing,
      taskId = Prelude.Nothing,
      taskName = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      destination = Prelude.Nothing,
      destinationPrefix = Prelude.Nothing,
      from = Prelude.Nothing
    }

-- | The status of the export task.
exportTask_status :: Lens.Lens' ExportTask (Prelude.Maybe ExportTaskStatus)
exportTask_status = Lens.lens (\ExportTask' {status} -> status) (\s@ExportTask' {} a -> s {status = a} :: ExportTask)

-- | Execution information about the export task.
exportTask_executionInfo :: Lens.Lens' ExportTask (Prelude.Maybe ExportTaskExecutionInfo)
exportTask_executionInfo = Lens.lens (\ExportTask' {executionInfo} -> executionInfo) (\s@ExportTask' {} a -> s {executionInfo = a} :: ExportTask)

-- | The end time, expressed as the number of milliseconds after Jan 1, 1970
-- 00:00:00 UTC. Events with a timestamp later than this time are not
-- exported.
exportTask_to :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Natural)
exportTask_to = Lens.lens (\ExportTask' {to} -> to) (\s@ExportTask' {} a -> s {to = a} :: ExportTask)

-- | The ID of the export task.
exportTask_taskId :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_taskId = Lens.lens (\ExportTask' {taskId} -> taskId) (\s@ExportTask' {} a -> s {taskId = a} :: ExportTask)

-- | The name of the export task.
exportTask_taskName :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_taskName = Lens.lens (\ExportTask' {taskName} -> taskName) (\s@ExportTask' {} a -> s {taskName = a} :: ExportTask)

-- | The name of the log group from which logs data was exported.
exportTask_logGroupName :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_logGroupName = Lens.lens (\ExportTask' {logGroupName} -> logGroupName) (\s@ExportTask' {} a -> s {logGroupName = a} :: ExportTask)

-- | The name of the S3 bucket to which the log data was exported.
exportTask_destination :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_destination = Lens.lens (\ExportTask' {destination} -> destination) (\s@ExportTask' {} a -> s {destination = a} :: ExportTask)

-- | The prefix that was used as the start of Amazon S3 key for every object
-- exported.
exportTask_destinationPrefix :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Text)
exportTask_destinationPrefix = Lens.lens (\ExportTask' {destinationPrefix} -> destinationPrefix) (\s@ExportTask' {} a -> s {destinationPrefix = a} :: ExportTask)

-- | The start time, expressed as the number of milliseconds after Jan 1,
-- 1970 00:00:00 UTC. Events with a timestamp before this time are not
-- exported.
exportTask_from :: Lens.Lens' ExportTask (Prelude.Maybe Prelude.Natural)
exportTask_from = Lens.lens (\ExportTask' {from} -> from) (\s@ExportTask' {} a -> s {from = a} :: ExportTask)

instance Core.FromJSON ExportTask where
  parseJSON =
    Core.withObject
      "ExportTask"
      ( \x ->
          ExportTask'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "executionInfo")
            Prelude.<*> (x Core..:? "to")
            Prelude.<*> (x Core..:? "taskId")
            Prelude.<*> (x Core..:? "taskName")
            Prelude.<*> (x Core..:? "logGroupName")
            Prelude.<*> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "destinationPrefix")
            Prelude.<*> (x Core..:? "from")
      )

instance Prelude.Hashable ExportTask

instance Prelude.NFData ExportTask
