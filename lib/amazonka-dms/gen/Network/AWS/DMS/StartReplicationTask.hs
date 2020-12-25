{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the replication task.
--
-- For more information about AWS DMS tasks, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks > in the /AWS Database Migration Service User Guide./
module Network.AWS.DMS.StartReplicationTask
  ( -- * Creating a request
    StartReplicationTask (..),
    mkStartReplicationTask,

    -- ** Request lenses
    srtReplicationTaskArn,
    srtStartReplicationTaskType,
    srtCdcStartPosition,
    srtCdcStartTime,
    srtCdcStopPosition,

    -- * Destructuring the response
    StartReplicationTaskResponse (..),
    mkStartReplicationTaskResponse,

    -- ** Response lenses
    srtrrsReplicationTask,
    srtrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkStartReplicationTask' smart constructor.
data StartReplicationTask = StartReplicationTask'
  { -- | The Amazon Resource Name (ARN) of the replication task to be started.
    replicationTaskArn :: Types.ReplicationTaskArn,
    -- | A type of replication task.
    startReplicationTaskType :: Types.StartReplicationTaskTypeValue,
    -- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
    --
    -- The value can be in date, checkpoint, or LSN/SCN format.
    -- Date Example: --cdc-start-position “2018-03-08T12:12:12”
    -- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
    -- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
    cdcStartPosition :: Core.Maybe Types.CdcStartPosition,
    -- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
    --
    -- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
    cdcStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
    --
    -- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
    -- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
    cdcStopPosition :: Core.Maybe Types.CdcStopPosition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartReplicationTask' value with any optional fields omitted.
mkStartReplicationTask ::
  -- | 'replicationTaskArn'
  Types.ReplicationTaskArn ->
  -- | 'startReplicationTaskType'
  Types.StartReplicationTaskTypeValue ->
  StartReplicationTask
mkStartReplicationTask replicationTaskArn startReplicationTaskType =
  StartReplicationTask'
    { replicationTaskArn,
      startReplicationTaskType,
      cdcStartPosition = Core.Nothing,
      cdcStartTime = Core.Nothing,
      cdcStopPosition = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replication task to be started.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtReplicationTaskArn :: Lens.Lens' StartReplicationTask Types.ReplicationTaskArn
srtReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# DEPRECATED srtReplicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead." #-}

-- | A type of replication task.
--
-- /Note:/ Consider using 'startReplicationTaskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtStartReplicationTaskType :: Lens.Lens' StartReplicationTask Types.StartReplicationTaskTypeValue
srtStartReplicationTaskType = Lens.field @"startReplicationTaskType"
{-# DEPRECATED srtStartReplicationTaskType "Use generic-lens or generic-optics with 'startReplicationTaskType' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtCdcStartPosition :: Lens.Lens' StartReplicationTask (Core.Maybe Types.CdcStartPosition)
srtCdcStartPosition = Lens.field @"cdcStartPosition"
{-# DEPRECATED srtCdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead." #-}

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- /Note:/ Consider using 'cdcStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtCdcStartTime :: Lens.Lens' StartReplicationTask (Core.Maybe Core.NominalDiffTime)
srtCdcStartTime = Lens.field @"cdcStartTime"
{-# DEPRECATED srtCdcStartTime "Use generic-lens or generic-optics with 'cdcStartTime' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtCdcStopPosition :: Lens.Lens' StartReplicationTask (Core.Maybe Types.CdcStopPosition)
srtCdcStopPosition = Lens.field @"cdcStopPosition"
{-# DEPRECATED srtCdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead." #-}

instance Core.FromJSON StartReplicationTask where
  toJSON StartReplicationTask {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn),
            Core.Just
              ("StartReplicationTaskType" Core..= startReplicationTaskType),
            ("CdcStartPosition" Core..=) Core.<$> cdcStartPosition,
            ("CdcStartTime" Core..=) Core.<$> cdcStartTime,
            ("CdcStopPosition" Core..=) Core.<$> cdcStopPosition
          ]
      )

instance Core.AWSRequest StartReplicationTask where
  type Rs StartReplicationTask = StartReplicationTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.StartReplicationTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplicationTaskResponse'
            Core.<$> (x Core..:? "ReplicationTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkStartReplicationTaskResponse' smart constructor.
data StartReplicationTaskResponse = StartReplicationTaskResponse'
  { -- | The replication task started.
    replicationTask :: Core.Maybe Types.ReplicationTask,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartReplicationTaskResponse' value with any optional fields omitted.
mkStartReplicationTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartReplicationTaskResponse
mkStartReplicationTaskResponse responseStatus =
  StartReplicationTaskResponse'
    { replicationTask = Core.Nothing,
      responseStatus
    }

-- | The replication task started.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtrrsReplicationTask :: Lens.Lens' StartReplicationTaskResponse (Core.Maybe Types.ReplicationTask)
srtrrsReplicationTask = Lens.field @"replicationTask"
{-# DEPRECATED srtrrsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtrrsResponseStatus :: Lens.Lens' StartReplicationTaskResponse Core.Int
srtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
