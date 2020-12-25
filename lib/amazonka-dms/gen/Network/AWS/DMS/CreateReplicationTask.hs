{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.CreateReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication task using the specified parameters.
module Network.AWS.DMS.CreateReplicationTask
  ( -- * Creating a request
    CreateReplicationTask (..),
    mkCreateReplicationTask,

    -- ** Request lenses
    crtReplicationTaskIdentifier,
    crtSourceEndpointArn,
    crtTargetEndpointArn,
    crtReplicationInstanceArn,
    crtMigrationType,
    crtTableMappings,
    crtCdcStartPosition,
    crtCdcStartTime,
    crtCdcStopPosition,
    crtReplicationTaskSettings,
    crtResourceIdentifier,
    crtTags,
    crtTaskData,

    -- * Destructuring the response
    CreateReplicationTaskResponse (..),
    mkCreateReplicationTaskResponse,

    -- ** Response lenses
    crtrrsReplicationTask,
    crtrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateReplicationTask' smart constructor.
data CreateReplicationTask = CreateReplicationTask'
  { -- | An identifier for the replication task.
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
    replicationTaskIdentifier :: Types.String,
    -- | An Amazon Resource Name (ARN) that uniquely identifies the source endpoint.
    sourceEndpointArn :: Types.String,
    -- | An Amazon Resource Name (ARN) that uniquely identifies the target endpoint.
    targetEndpointArn :: Types.String,
    -- | The Amazon Resource Name (ARN) of a replication instance.
    replicationInstanceArn :: Types.String,
    -- | The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@
    migrationType :: Types.MigrationTypeValue,
    -- | The table mappings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TableMapping.html Using Table Mapping to Specify Task Settings> in the /AWS Database Migration Service User Guide./
    tableMappings :: Types.String,
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
    -- | Overall settings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Specifying Task Settings for AWS Database Migration Service Tasks> in the /AWS Database Migration User Guide./
    replicationTaskSettings :: Core.Maybe Types.String,
    -- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
    resourceIdentifier :: Core.Maybe Types.String,
    -- | One or more tags to be assigned to the replication task.
    tags :: Core.Maybe [Types.Tag],
    -- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
    taskData :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateReplicationTask' value with any optional fields omitted.
mkCreateReplicationTask ::
  -- | 'replicationTaskIdentifier'
  Types.String ->
  -- | 'sourceEndpointArn'
  Types.String ->
  -- | 'targetEndpointArn'
  Types.String ->
  -- | 'replicationInstanceArn'
  Types.String ->
  -- | 'migrationType'
  Types.MigrationTypeValue ->
  -- | 'tableMappings'
  Types.String ->
  CreateReplicationTask
mkCreateReplicationTask
  replicationTaskIdentifier
  sourceEndpointArn
  targetEndpointArn
  replicationInstanceArn
  migrationType
  tableMappings =
    CreateReplicationTask'
      { replicationTaskIdentifier,
        sourceEndpointArn,
        targetEndpointArn,
        replicationInstanceArn,
        migrationType,
        tableMappings,
        cdcStartPosition = Core.Nothing,
        cdcStartTime = Core.Nothing,
        cdcStopPosition = Core.Nothing,
        replicationTaskSettings = Core.Nothing,
        resourceIdentifier = Core.Nothing,
        tags = Core.Nothing,
        taskData = Core.Nothing
      }

-- | An identifier for the replication task.
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
crtReplicationTaskIdentifier :: Lens.Lens' CreateReplicationTask Types.String
crtReplicationTaskIdentifier = Lens.field @"replicationTaskIdentifier"
{-# DEPRECATED crtReplicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead." #-}

-- | An Amazon Resource Name (ARN) that uniquely identifies the source endpoint.
--
-- /Note:/ Consider using 'sourceEndpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtSourceEndpointArn :: Lens.Lens' CreateReplicationTask Types.String
crtSourceEndpointArn = Lens.field @"sourceEndpointArn"
{-# DEPRECATED crtSourceEndpointArn "Use generic-lens or generic-optics with 'sourceEndpointArn' instead." #-}

-- | An Amazon Resource Name (ARN) that uniquely identifies the target endpoint.
--
-- /Note:/ Consider using 'targetEndpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTargetEndpointArn :: Lens.Lens' CreateReplicationTask Types.String
crtTargetEndpointArn = Lens.field @"targetEndpointArn"
{-# DEPRECATED crtTargetEndpointArn "Use generic-lens or generic-optics with 'targetEndpointArn' instead." #-}

-- | The Amazon Resource Name (ARN) of a replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtReplicationInstanceArn :: Lens.Lens' CreateReplicationTask Types.String
crtReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# DEPRECATED crtReplicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead." #-}

-- | The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtMigrationType :: Lens.Lens' CreateReplicationTask Types.MigrationTypeValue
crtMigrationType = Lens.field @"migrationType"
{-# DEPRECATED crtMigrationType "Use generic-lens or generic-optics with 'migrationType' instead." #-}

-- | The table mappings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TableMapping.html Using Table Mapping to Specify Task Settings> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'tableMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTableMappings :: Lens.Lens' CreateReplicationTask Types.String
crtTableMappings = Lens.field @"tableMappings"
{-# DEPRECATED crtTableMappings "Use generic-lens or generic-optics with 'tableMappings' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCdcStartPosition :: Lens.Lens' CreateReplicationTask (Core.Maybe Types.String)
crtCdcStartPosition = Lens.field @"cdcStartPosition"
{-# DEPRECATED crtCdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead." #-}

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- /Note:/ Consider using 'cdcStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCdcStartTime :: Lens.Lens' CreateReplicationTask (Core.Maybe Core.NominalDiffTime)
crtCdcStartTime = Lens.field @"cdcStartTime"
{-# DEPRECATED crtCdcStartTime "Use generic-lens or generic-optics with 'cdcStartTime' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCdcStopPosition :: Lens.Lens' CreateReplicationTask (Core.Maybe Types.String)
crtCdcStopPosition = Lens.field @"cdcStopPosition"
{-# DEPRECATED crtCdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead." #-}

-- | Overall settings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Specifying Task Settings for AWS Database Migration Service Tasks> in the /AWS Database Migration User Guide./
--
-- /Note:/ Consider using 'replicationTaskSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtReplicationTaskSettings :: Lens.Lens' CreateReplicationTask (Core.Maybe Types.String)
crtReplicationTaskSettings = Lens.field @"replicationTaskSettings"
{-# DEPRECATED crtReplicationTaskSettings "Use generic-lens or generic-optics with 'replicationTaskSettings' instead." #-}

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtResourceIdentifier :: Lens.Lens' CreateReplicationTask (Core.Maybe Types.String)
crtResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED crtResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | One or more tags to be assigned to the replication task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTags :: Lens.Lens' CreateReplicationTask (Core.Maybe [Types.Tag])
crtTags = Lens.field @"tags"
{-# DEPRECATED crtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'taskData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTaskData :: Lens.Lens' CreateReplicationTask (Core.Maybe Types.String)
crtTaskData = Lens.field @"taskData"
{-# DEPRECATED crtTaskData "Use generic-lens or generic-optics with 'taskData' instead." #-}

instance Core.FromJSON CreateReplicationTask where
  toJSON CreateReplicationTask {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ReplicationTaskIdentifier" Core..= replicationTaskIdentifier),
            Core.Just ("SourceEndpointArn" Core..= sourceEndpointArn),
            Core.Just ("TargetEndpointArn" Core..= targetEndpointArn),
            Core.Just
              ("ReplicationInstanceArn" Core..= replicationInstanceArn),
            Core.Just ("MigrationType" Core..= migrationType),
            Core.Just ("TableMappings" Core..= tableMappings),
            ("CdcStartPosition" Core..=) Core.<$> cdcStartPosition,
            ("CdcStartTime" Core..=) Core.<$> cdcStartTime,
            ("CdcStopPosition" Core..=) Core.<$> cdcStopPosition,
            ("ReplicationTaskSettings" Core..=)
              Core.<$> replicationTaskSettings,
            ("ResourceIdentifier" Core..=) Core.<$> resourceIdentifier,
            ("Tags" Core..=) Core.<$> tags,
            ("TaskData" Core..=) Core.<$> taskData
          ]
      )

instance Core.AWSRequest CreateReplicationTask where
  type Rs CreateReplicationTask = CreateReplicationTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.CreateReplicationTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationTaskResponse'
            Core.<$> (x Core..:? "ReplicationTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkCreateReplicationTaskResponse' smart constructor.
data CreateReplicationTaskResponse = CreateReplicationTaskResponse'
  { -- | The replication task that was created.
    replicationTask :: Core.Maybe Types.ReplicationTask,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateReplicationTaskResponse' value with any optional fields omitted.
mkCreateReplicationTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateReplicationTaskResponse
mkCreateReplicationTaskResponse responseStatus =
  CreateReplicationTaskResponse'
    { replicationTask = Core.Nothing,
      responseStatus
    }

-- | The replication task that was created.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrrsReplicationTask :: Lens.Lens' CreateReplicationTaskResponse (Core.Maybe Types.ReplicationTask)
crtrrsReplicationTask = Lens.field @"replicationTask"
{-# DEPRECATED crtrrsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrrsResponseStatus :: Lens.Lens' CreateReplicationTaskResponse Core.Int
crtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
