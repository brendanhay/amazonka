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
    etDestinationPrefix,
    etDestination,
    etStatus,
    etTaskName,
    etTaskId,
    etTo,
    etFrom,
    etLogGroupName,
    etExecutionInfo,
  )
where

import Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
import Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an export task.
--
-- /See:/ 'mkExportTask' smart constructor.
data ExportTask = ExportTask'
  { destinationPrefix ::
      Lude.Maybe Lude.Text,
    destination :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe ExportTaskStatus,
    taskName :: Lude.Maybe Lude.Text,
    taskId :: Lude.Maybe Lude.Text,
    to :: Lude.Maybe Lude.Natural,
    from :: Lude.Maybe Lude.Natural,
    logGroupName :: Lude.Maybe Lude.Text,
    executionInfo :: Lude.Maybe ExportTaskExecutionInfo
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTask' with the minimum fields required to make a request.
--
-- * 'destination' - The name of the S3 bucket to which the log data was exported.
-- * 'destinationPrefix' - The prefix that was used as the start of Amazon S3 key for every object exported.
-- * 'executionInfo' - Execution information about the export task.
-- * 'from' - The start time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time are not exported.
-- * 'logGroupName' - The name of the log group from which logs data was exported.
-- * 'status' - The status of the export task.
-- * 'taskId' - The ID of the export task.
-- * 'taskName' - The name of the export task.
-- * 'to' - The end time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
mkExportTask ::
  ExportTask
mkExportTask =
  ExportTask'
    { destinationPrefix = Lude.Nothing,
      destination = Lude.Nothing,
      status = Lude.Nothing,
      taskName = Lude.Nothing,
      taskId = Lude.Nothing,
      to = Lude.Nothing,
      from = Lude.Nothing,
      logGroupName = Lude.Nothing,
      executionInfo = Lude.Nothing
    }

-- | The prefix that was used as the start of Amazon S3 key for every object exported.
--
-- /Note:/ Consider using 'destinationPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDestinationPrefix :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etDestinationPrefix = Lens.lens (destinationPrefix :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {destinationPrefix = a} :: ExportTask)
{-# DEPRECATED etDestinationPrefix "Use generic-lens or generic-optics with 'destinationPrefix' instead." #-}

-- | The name of the S3 bucket to which the log data was exported.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDestination :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etDestination = Lens.lens (destination :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {destination = a} :: ExportTask)
{-# DEPRECATED etDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The status of the export task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatus :: Lens.Lens' ExportTask (Lude.Maybe ExportTaskStatus)
etStatus = Lens.lens (status :: ExportTask -> Lude.Maybe ExportTaskStatus) (\s a -> s {status = a} :: ExportTask)
{-# DEPRECATED etStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the export task.
--
-- /Note:/ Consider using 'taskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskName :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etTaskName = Lens.lens (taskName :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {taskName = a} :: ExportTask)
{-# DEPRECATED etTaskName "Use generic-lens or generic-optics with 'taskName' instead." #-}

-- | The ID of the export task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTaskId :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etTaskId = Lens.lens (taskId :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: ExportTask)
{-# DEPRECATED etTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The end time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTo :: Lens.Lens' ExportTask (Lude.Maybe Lude.Natural)
etTo = Lens.lens (to :: ExportTask -> Lude.Maybe Lude.Natural) (\s a -> s {to = a} :: ExportTask)
{-# DEPRECATED etTo "Use generic-lens or generic-optics with 'to' instead." #-}

-- | The start time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time are not exported.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etFrom :: Lens.Lens' ExportTask (Lude.Maybe Lude.Natural)
etFrom = Lens.lens (from :: ExportTask -> Lude.Maybe Lude.Natural) (\s a -> s {from = a} :: ExportTask)
{-# DEPRECATED etFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The name of the log group from which logs data was exported.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etLogGroupName :: Lens.Lens' ExportTask (Lude.Maybe Lude.Text)
etLogGroupName = Lens.lens (logGroupName :: ExportTask -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: ExportTask)
{-# DEPRECATED etLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Execution information about the export task.
--
-- /Note:/ Consider using 'executionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExecutionInfo :: Lens.Lens' ExportTask (Lude.Maybe ExportTaskExecutionInfo)
etExecutionInfo = Lens.lens (executionInfo :: ExportTask -> Lude.Maybe ExportTaskExecutionInfo) (\s a -> s {executionInfo = a} :: ExportTask)
{-# DEPRECATED etExecutionInfo "Use generic-lens or generic-optics with 'executionInfo' instead." #-}

instance Lude.FromJSON ExportTask where
  parseJSON =
    Lude.withObject
      "ExportTask"
      ( \x ->
          ExportTask'
            Lude.<$> (x Lude..:? "destinationPrefix")
            Lude.<*> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "taskName")
            Lude.<*> (x Lude..:? "taskId")
            Lude.<*> (x Lude..:? "to")
            Lude.<*> (x Lude..:? "from")
            Lude.<*> (x Lude..:? "logGroupName")
            Lude.<*> (x Lude..:? "executionInfo")
      )
