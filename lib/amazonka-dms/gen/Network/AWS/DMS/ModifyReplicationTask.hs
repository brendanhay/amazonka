{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ModifyReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified replication task.
--
-- You can't modify the task endpoints. The task must be stopped before you can modify it.
-- For more information about AWS DMS tasks, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks> in the /AWS Database Migration Service User Guide/ .
module Network.AWS.DMS.ModifyReplicationTask
  ( -- * Creating a request
    ModifyReplicationTask (..),
    mkModifyReplicationTask,

    -- ** Request lenses
    mReplicationTaskArn,
    mCdcStartPosition,
    mCdcStartTime,
    mCdcStopPosition,
    mMigrationType,
    mReplicationTaskIdentifier,
    mReplicationTaskSettings,
    mTableMappings,
    mTaskData,

    -- * Destructuring the response
    ModifyReplicationTaskResponse (..),
    mkModifyReplicationTaskResponse,

    -- ** Response lenses
    mrsReplicationTask,
    mrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyReplicationTask' smart constructor.
data ModifyReplicationTask = ModifyReplicationTask'
  { -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Types.String,
    -- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
    --
    -- The value can be in date, checkpoint, or LSN/SCN format.
    -- Date Example: --cdc-start-position “2018-03-08T12:12:12”
    -- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
    -- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
    cdcStartPosition :: Core.Maybe Types.String,
    -- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
    --
    -- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
    cdcStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
    --
    -- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
    -- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
    cdcStopPosition :: Core.Maybe Types.String,
    -- | The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@
    migrationType :: Core.Maybe Types.MigrationTypeValue,
    -- | The replication task identifier.
    --
    -- Constraints:
    --
    --     * Must contain 1-255 alphanumeric characters or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens.
    replicationTaskIdentifier :: Core.Maybe Types.String,
    -- | JSON file that contains settings for the task, such as task metadata settings.
    replicationTaskSettings :: Core.Maybe Types.String,
    -- | When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with @file://@ . When working with the DMS API, provide the JSON as the parameter value, for example: @--table-mappings file://mappingfile.json@
    tableMappings :: Core.Maybe Types.String,
    -- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
    taskData :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyReplicationTask' value with any optional fields omitted.
mkModifyReplicationTask ::
  -- | 'replicationTaskArn'
  Types.String ->
  ModifyReplicationTask
mkModifyReplicationTask replicationTaskArn =
  ModifyReplicationTask'
    { replicationTaskArn,
      cdcStartPosition = Core.Nothing,
      cdcStartTime = Core.Nothing,
      cdcStopPosition = Core.Nothing,
      migrationType = Core.Nothing,
      replicationTaskIdentifier = Core.Nothing,
      replicationTaskSettings = Core.Nothing,
      tableMappings = Core.Nothing,
      taskData = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mReplicationTaskArn :: Lens.Lens' ModifyReplicationTask Types.String
mReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# DEPRECATED mReplicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCdcStartPosition :: Lens.Lens' ModifyReplicationTask (Core.Maybe Types.String)
mCdcStartPosition = Lens.field @"cdcStartPosition"
{-# DEPRECATED mCdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead." #-}

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- /Note:/ Consider using 'cdcStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCdcStartTime :: Lens.Lens' ModifyReplicationTask (Core.Maybe Core.NominalDiffTime)
mCdcStartTime = Lens.field @"cdcStartTime"
{-# DEPRECATED mCdcStartTime "Use generic-lens or generic-optics with 'cdcStartTime' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCdcStopPosition :: Lens.Lens' ModifyReplicationTask (Core.Maybe Types.String)
mCdcStopPosition = Lens.field @"cdcStopPosition"
{-# DEPRECATED mCdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead." #-}

-- | The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMigrationType :: Lens.Lens' ModifyReplicationTask (Core.Maybe Types.MigrationTypeValue)
mMigrationType = Lens.field @"migrationType"
{-# DEPRECATED mMigrationType "Use generic-lens or generic-optics with 'migrationType' instead." #-}

-- | The replication task identifier.
--
-- Constraints:
--
--     * Must contain 1-255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'replicationTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mReplicationTaskIdentifier :: Lens.Lens' ModifyReplicationTask (Core.Maybe Types.String)
mReplicationTaskIdentifier = Lens.field @"replicationTaskIdentifier"
{-# DEPRECATED mReplicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead." #-}

-- | JSON file that contains settings for the task, such as task metadata settings.
--
-- /Note:/ Consider using 'replicationTaskSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mReplicationTaskSettings :: Lens.Lens' ModifyReplicationTask (Core.Maybe Types.String)
mReplicationTaskSettings = Lens.field @"replicationTaskSettings"
{-# DEPRECATED mReplicationTaskSettings "Use generic-lens or generic-optics with 'replicationTaskSettings' instead." #-}

-- | When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with @file://@ . When working with the DMS API, provide the JSON as the parameter value, for example: @--table-mappings file://mappingfile.json@
--
-- /Note:/ Consider using 'tableMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTableMappings :: Lens.Lens' ModifyReplicationTask (Core.Maybe Types.String)
mTableMappings = Lens.field @"tableMappings"
{-# DEPRECATED mTableMappings "Use generic-lens or generic-optics with 'tableMappings' instead." #-}

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'taskData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTaskData :: Lens.Lens' ModifyReplicationTask (Core.Maybe Types.String)
mTaskData = Lens.field @"taskData"
{-# DEPRECATED mTaskData "Use generic-lens or generic-optics with 'taskData' instead." #-}

instance Core.FromJSON ModifyReplicationTask where
  toJSON ModifyReplicationTask {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn),
            ("CdcStartPosition" Core..=) Core.<$> cdcStartPosition,
            ("CdcStartTime" Core..=) Core.<$> cdcStartTime,
            ("CdcStopPosition" Core..=) Core.<$> cdcStopPosition,
            ("MigrationType" Core..=) Core.<$> migrationType,
            ("ReplicationTaskIdentifier" Core..=)
              Core.<$> replicationTaskIdentifier,
            ("ReplicationTaskSettings" Core..=)
              Core.<$> replicationTaskSettings,
            ("TableMappings" Core..=) Core.<$> tableMappings,
            ("TaskData" Core..=) Core.<$> taskData
          ]
      )

instance Core.AWSRequest ModifyReplicationTask where
  type Rs ModifyReplicationTask = ModifyReplicationTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.ModifyReplicationTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyReplicationTaskResponse'
            Core.<$> (x Core..:? "ReplicationTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkModifyReplicationTaskResponse' smart constructor.
data ModifyReplicationTaskResponse = ModifyReplicationTaskResponse'
  { -- | The replication task that was modified.
    replicationTask :: Core.Maybe Types.ReplicationTask,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyReplicationTaskResponse' value with any optional fields omitted.
mkModifyReplicationTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyReplicationTaskResponse
mkModifyReplicationTaskResponse responseStatus =
  ModifyReplicationTaskResponse'
    { replicationTask = Core.Nothing,
      responseStatus
    }

-- | The replication task that was modified.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsReplicationTask :: Lens.Lens' ModifyReplicationTaskResponse (Core.Maybe Types.ReplicationTask)
mrsReplicationTask = Lens.field @"replicationTask"
{-# DEPRECATED mrsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsResponseStatus :: Lens.Lens' ModifyReplicationTaskResponse Core.Int
mrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
