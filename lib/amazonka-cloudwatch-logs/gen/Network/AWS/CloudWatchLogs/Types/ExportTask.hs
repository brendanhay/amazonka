{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTask
  ( ExportTask (..),

    -- * Smart constructor
    mkExportTask,

    -- * Lenses
    etDestination,
    etDestinationPrefix,
    etExecutionInfo,
    etFrom,
    etLogGroupName,
    etStatus,
    etTaskId,
    etTaskName,
    etTo,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types.ExportDestinationBucket as Types
import qualified Network.AWS.CloudWatchLogs.Types.ExportDestinationPrefix as Types
import qualified Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo as Types
import qualified Network.AWS.CloudWatchLogs.Types.ExportTaskId as Types
import qualified Network.AWS.CloudWatchLogs.Types.ExportTaskName as Types
import qualified Network.AWS.CloudWatchLogs.Types.ExportTaskStatus as Types
import qualified Network.AWS.CloudWatchLogs.Types.LogGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an export task.
--
-- /See:/ 'mkExportTask' smart constructor.
data ExportTask = ExportTask'
  { -- | The name of the S3 bucket to which the log data was exported.
    destination :: Core.Maybe Types.ExportDestinationBucket,
    -- | The prefix that was used as the start of Amazon S3 key for every object exported.
    destinationPrefix :: Core.Maybe Types.ExportDestinationPrefix,
    -- | Execution information about the export task.
    executionInfo :: Core.Maybe Types.ExportTaskExecutionInfo,
    -- | The start time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time are not exported.
    from :: Core.Maybe Core.Natural,
    -- | The name of the log group from which logs data was exported.
    logGroupName :: Core.Maybe Types.LogGroupName,
    -- | The status of the export task.
    status :: Core.Maybe Types.ExportTaskStatus,
    -- | The ID of the export task.
    taskId :: Core.Maybe Types.ExportTaskId,
    -- | The name of the export task.
    taskName :: Core.Maybe Types.ExportTaskName,
    -- | The end time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
    to :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTask' value with any optional fields omitted.
mkExportTask ::
  ExportTask
mkExportTask =
  ExportTask'
    { destination = Core.Nothing,
      destinationPrefix = Core.Nothing,
      executionInfo = Core.Nothing,
      from = Core.Nothing,
      logGroupName = Core.Nothing,
      status = Core.Nothing,
      taskId = Core.Nothing,
      taskName = Core.Nothing,
      to = Core.Nothing
    }

-- | The name of the S3 bucket to which the log data was exported.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDestination :: Lens.Lens' ExportTask (Core.Maybe Types.ExportDestinationBucket)
etDestination = Lens.field @"destination"
{-# DEPRECATED etDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The prefix that was used as the start of Amazon S3 key for every object exported.
--
-- /Note:/ Consider using 'destinationPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDestinationPrefix :: Lens.Lens' ExportTask (Core.Maybe Types.ExportDestinationPrefix)
etDestinationPrefix = Lens.field @"destinationPrefix"
{-# DEPRECATED etDestinationPrefix "Use generic-lens or generic-optics with 'destinationPrefix' instead." #-}

-- | Execution information about the export task.
--
-- /Note:/ Consider using 'executionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExecutionInfo :: Lens.Lens' ExportTask (Core.Maybe Types.ExportTaskExecutionInfo)
etExecutionInfo = Lens.field @"executionInfo"
{-# DEPRECATED etExecutionInfo "Use generic-lens or generic-optics with 'executionInfo' instead." #-}

-- | The start time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time are not exported.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etFrom :: Lens.Lens' ExportTask (Core.Maybe Core.Natural)
etFrom = Lens.field @"from"
{-# DEPRECATED etFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The name of the log group from which logs data was exported.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etLogGroupName :: Lens.Lens' ExportTask (Core.Maybe Types.LogGroupName)
etLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED etLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The status of the export task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatus :: Lens.Lens' ExportTask (Core.Maybe Types.ExportTaskStatus)
etStatus = Lens.field @"status"
{-# DEPRECATED etStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the export task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskId :: Lens.Lens' ExportTask (Core.Maybe Types.ExportTaskId)
etTaskId = Lens.field @"taskId"
{-# DEPRECATED etTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The name of the export task.
--
-- /Note:/ Consider using 'taskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskName :: Lens.Lens' ExportTask (Core.Maybe Types.ExportTaskName)
etTaskName = Lens.field @"taskName"
{-# DEPRECATED etTaskName "Use generic-lens or generic-optics with 'taskName' instead." #-}

-- | The end time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTo :: Lens.Lens' ExportTask (Core.Maybe Core.Natural)
etTo = Lens.field @"to"
{-# DEPRECATED etTo "Use generic-lens or generic-optics with 'to' instead." #-}

instance Core.FromJSON ExportTask where
  parseJSON =
    Core.withObject "ExportTask" Core.$
      \x ->
        ExportTask'
          Core.<$> (x Core..:? "destination")
          Core.<*> (x Core..:? "destinationPrefix")
          Core.<*> (x Core..:? "executionInfo")
          Core.<*> (x Core..:? "from")
          Core.<*> (x Core..:? "logGroupName")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "taskId")
          Core.<*> (x Core..:? "taskName")
          Core.<*> (x Core..:? "to")
