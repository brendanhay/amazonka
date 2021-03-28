{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.MigrationHub.NotifyMigrationTaskState
    (
    -- * Creating a request
      NotifyMigrationTaskState (..)
    , mkNotifyMigrationTaskState
    -- ** Request lenses
    , nmtsProgressUpdateStream
    , nmtsMigrationTaskName
    , nmtsTask
    , nmtsUpdateDateTime
    , nmtsNextUpdateSeconds
    , nmtsDryRun

    -- * Destructuring the response
    , NotifyMigrationTaskStateResponse (..)
    , mkNotifyMigrationTaskStateResponse
    -- ** Response lenses
    , nmtsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkNotifyMigrationTaskState' smart constructor.
data NotifyMigrationTaskState = NotifyMigrationTaskState'
  { progressUpdateStream :: Types.ProgressUpdateStream
    -- ^ The name of the ProgressUpdateStream. 
  , migrationTaskName :: Types.MigrationTaskName
    -- ^ Unique identifier that references the migration task. /Do not store personal data in this field./ 
  , task :: Types.Task
    -- ^ Information about the task's progress and status.
  , updateDateTime :: Core.NominalDiffTime
    -- ^ The timestamp when the task was gathered.
  , nextUpdateSeconds :: Core.Natural
    -- ^ Number of seconds after the UpdateDateTime within which the Migration Hub can expect an update. If Migration Hub does not receive an update within the specified interval, then the migration task will be considered stale.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'NotifyMigrationTaskState' value with any optional fields omitted.
mkNotifyMigrationTaskState
    :: Types.ProgressUpdateStream -- ^ 'progressUpdateStream'
    -> Types.MigrationTaskName -- ^ 'migrationTaskName'
    -> Types.Task -- ^ 'task'
    -> Core.NominalDiffTime -- ^ 'updateDateTime'
    -> Core.Natural -- ^ 'nextUpdateSeconds'
    -> NotifyMigrationTaskState
mkNotifyMigrationTaskState progressUpdateStream migrationTaskName
  task updateDateTime nextUpdateSeconds
  = NotifyMigrationTaskState'{progressUpdateStream,
                              migrationTaskName, task, updateDateTime, nextUpdateSeconds,
                              dryRun = Core.Nothing}

-- | The name of the ProgressUpdateStream. 
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsProgressUpdateStream :: Lens.Lens' NotifyMigrationTaskState Types.ProgressUpdateStream
nmtsProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# INLINEABLE nmtsProgressUpdateStream #-}
{-# DEPRECATED progressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead"  #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./ 
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsMigrationTaskName :: Lens.Lens' NotifyMigrationTaskState Types.MigrationTaskName
nmtsMigrationTaskName = Lens.field @"migrationTaskName"
{-# INLINEABLE nmtsMigrationTaskName #-}
{-# DEPRECATED migrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead"  #-}

-- | Information about the task's progress and status.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsTask :: Lens.Lens' NotifyMigrationTaskState Types.Task
nmtsTask = Lens.field @"task"
{-# INLINEABLE nmtsTask #-}
{-# DEPRECATED task "Use generic-lens or generic-optics with 'task' instead"  #-}

-- | The timestamp when the task was gathered.
--
-- /Note:/ Consider using 'updateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsUpdateDateTime :: Lens.Lens' NotifyMigrationTaskState Core.NominalDiffTime
nmtsUpdateDateTime = Lens.field @"updateDateTime"
{-# INLINEABLE nmtsUpdateDateTime #-}
{-# DEPRECATED updateDateTime "Use generic-lens or generic-optics with 'updateDateTime' instead"  #-}

-- | Number of seconds after the UpdateDateTime within which the Migration Hub can expect an update. If Migration Hub does not receive an update within the specified interval, then the migration task will be considered stale.
--
-- /Note:/ Consider using 'nextUpdateSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsNextUpdateSeconds :: Lens.Lens' NotifyMigrationTaskState Core.Natural
nmtsNextUpdateSeconds = Lens.field @"nextUpdateSeconds"
{-# INLINEABLE nmtsNextUpdateSeconds #-}
{-# DEPRECATED nextUpdateSeconds "Use generic-lens or generic-optics with 'nextUpdateSeconds' instead"  #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsDryRun :: Lens.Lens' NotifyMigrationTaskState (Core.Maybe Core.Bool)
nmtsDryRun = Lens.field @"dryRun"
{-# INLINEABLE nmtsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery NotifyMigrationTaskState where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders NotifyMigrationTaskState where
        toHeaders NotifyMigrationTaskState{..}
          = Core.pure
              ("X-Amz-Target", "AWSMigrationHub.NotifyMigrationTaskState")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON NotifyMigrationTaskState where
        toJSON NotifyMigrationTaskState{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
                  Core.Just ("MigrationTaskName" Core..= migrationTaskName),
                  Core.Just ("Task" Core..= task),
                  Core.Just ("UpdateDateTime" Core..= updateDateTime),
                  Core.Just ("NextUpdateSeconds" Core..= nextUpdateSeconds),
                  ("DryRun" Core..=) Core.<$> dryRun])

instance Core.AWSRequest NotifyMigrationTaskState where
        type Rs NotifyMigrationTaskState = NotifyMigrationTaskStateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 NotifyMigrationTaskStateResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkNotifyMigrationTaskStateResponse' smart constructor.
newtype NotifyMigrationTaskStateResponse = NotifyMigrationTaskStateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyMigrationTaskStateResponse' value with any optional fields omitted.
mkNotifyMigrationTaskStateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> NotifyMigrationTaskStateResponse
mkNotifyMigrationTaskStateResponse responseStatus
  = NotifyMigrationTaskStateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsrrsResponseStatus :: Lens.Lens' NotifyMigrationTaskStateResponse Core.Int
nmtsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE nmtsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
