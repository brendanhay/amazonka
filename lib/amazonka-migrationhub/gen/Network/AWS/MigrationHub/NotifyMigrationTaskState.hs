{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.NotifyMigrationTaskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies Migration Hub of the current status, progress, or other detail regarding a migration task. This API has the following traits:
--
--
--     * Migration tools will call the @NotifyMigrationTaskState@ API to share the latest progress and status.
--
--
--     * @MigrationTaskName@ is used for addressing updates to the correct target.
--
--
--     * @ProgressUpdateStream@ is used for access control and to provide a namespace for each migration tool.
module Network.AWS.MigrationHub.NotifyMigrationTaskState
  ( -- * Creating a request
    NotifyMigrationTaskState (..),
    mkNotifyMigrationTaskState,

    -- ** Request lenses
    nmtsProgressUpdateStream,
    nmtsMigrationTaskName,
    nmtsTask,
    nmtsUpdateDateTime,
    nmtsNextUpdateSeconds,
    nmtsDryRun,

    -- * Destructuring the response
    NotifyMigrationTaskStateResponse (..),
    mkNotifyMigrationTaskStateResponse,

    -- ** Response lenses
    nmtsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkNotifyMigrationTaskState' smart constructor.
data NotifyMigrationTaskState = NotifyMigrationTaskState'
  { -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Types.ProgressUpdateStream,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Types.MigrationTaskName,
    -- | Information about the task's progress and status.
    task :: Types.Task,
    -- | The timestamp when the task was gathered.
    updateDateTime :: Core.NominalDiffTime,
    -- | Number of seconds after the UpdateDateTime within which the Migration Hub can expect an update. If Migration Hub does not receive an update within the specified interval, then the migration task will be considered stale.
    nextUpdateSeconds :: Core.Natural,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NotifyMigrationTaskState' value with any optional fields omitted.
mkNotifyMigrationTaskState ::
  -- | 'progressUpdateStream'
  Types.ProgressUpdateStream ->
  -- | 'migrationTaskName'
  Types.MigrationTaskName ->
  -- | 'task'
  Types.Task ->
  -- | 'updateDateTime'
  Core.NominalDiffTime ->
  -- | 'nextUpdateSeconds'
  Core.Natural ->
  NotifyMigrationTaskState
mkNotifyMigrationTaskState
  progressUpdateStream
  migrationTaskName
  task
  updateDateTime
  nextUpdateSeconds =
    NotifyMigrationTaskState'
      { progressUpdateStream,
        migrationTaskName,
        task,
        updateDateTime,
        nextUpdateSeconds,
        dryRun = Core.Nothing
      }

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsProgressUpdateStream :: Lens.Lens' NotifyMigrationTaskState Types.ProgressUpdateStream
nmtsProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# DEPRECATED nmtsProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsMigrationTaskName :: Lens.Lens' NotifyMigrationTaskState Types.MigrationTaskName
nmtsMigrationTaskName = Lens.field @"migrationTaskName"
{-# DEPRECATED nmtsMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Information about the task's progress and status.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsTask :: Lens.Lens' NotifyMigrationTaskState Types.Task
nmtsTask = Lens.field @"task"
{-# DEPRECATED nmtsTask "Use generic-lens or generic-optics with 'task' instead." #-}

-- | The timestamp when the task was gathered.
--
-- /Note:/ Consider using 'updateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsUpdateDateTime :: Lens.Lens' NotifyMigrationTaskState Core.NominalDiffTime
nmtsUpdateDateTime = Lens.field @"updateDateTime"
{-# DEPRECATED nmtsUpdateDateTime "Use generic-lens or generic-optics with 'updateDateTime' instead." #-}

-- | Number of seconds after the UpdateDateTime within which the Migration Hub can expect an update. If Migration Hub does not receive an update within the specified interval, then the migration task will be considered stale.
--
-- /Note:/ Consider using 'nextUpdateSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsNextUpdateSeconds :: Lens.Lens' NotifyMigrationTaskState Core.Natural
nmtsNextUpdateSeconds = Lens.field @"nextUpdateSeconds"
{-# DEPRECATED nmtsNextUpdateSeconds "Use generic-lens or generic-optics with 'nextUpdateSeconds' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsDryRun :: Lens.Lens' NotifyMigrationTaskState (Core.Maybe Core.Bool)
nmtsDryRun = Lens.field @"dryRun"
{-# DEPRECATED nmtsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON NotifyMigrationTaskState where
  toJSON NotifyMigrationTaskState {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
            Core.Just ("MigrationTaskName" Core..= migrationTaskName),
            Core.Just ("Task" Core..= task),
            Core.Just ("UpdateDateTime" Core..= updateDateTime),
            Core.Just ("NextUpdateSeconds" Core..= nextUpdateSeconds),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest NotifyMigrationTaskState where
  type Rs NotifyMigrationTaskState = NotifyMigrationTaskStateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSMigrationHub.NotifyMigrationTaskState")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyMigrationTaskStateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkNotifyMigrationTaskStateResponse' smart constructor.
newtype NotifyMigrationTaskStateResponse = NotifyMigrationTaskStateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyMigrationTaskStateResponse' value with any optional fields omitted.
mkNotifyMigrationTaskStateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  NotifyMigrationTaskStateResponse
mkNotifyMigrationTaskStateResponse responseStatus =
  NotifyMigrationTaskStateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsrrsResponseStatus :: Lens.Lens' NotifyMigrationTaskStateResponse Core.Int
nmtsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED nmtsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
