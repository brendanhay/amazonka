{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateReplicationTask (..)
    , mkCreateReplicationTask
    -- ** Request lenses
    , crtReplicationTaskIdentifier
    , crtSourceEndpointArn
    , crtTargetEndpointArn
    , crtReplicationInstanceArn
    , crtMigrationType
    , crtTableMappings
    , crtCdcStartPosition
    , crtCdcStartTime
    , crtCdcStopPosition
    , crtReplicationTaskSettings
    , crtResourceIdentifier
    , crtTags
    , crtTaskData

    -- * Destructuring the response
    , CreateReplicationTaskResponse (..)
    , mkCreateReplicationTaskResponse
    -- ** Response lenses
    , crtrrsReplicationTask
    , crtrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateReplicationTask' smart constructor.
data CreateReplicationTask = CreateReplicationTask'
  { replicationTaskIdentifier :: Core.Text
    -- ^ An identifier for the replication task.
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
  , sourceEndpointArn :: Core.Text
    -- ^ An Amazon Resource Name (ARN) that uniquely identifies the source endpoint.
  , targetEndpointArn :: Core.Text
    -- ^ An Amazon Resource Name (ARN) that uniquely identifies the target endpoint.
  , replicationInstanceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of a replication instance.
  , migrationType :: Types.MigrationTypeValue
    -- ^ The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@ 
  , tableMappings :: Core.Text
    -- ^ The table mappings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TableMapping.html Using Table Mapping to Specify Task Settings> in the /AWS Database Migration Service User Guide./ 
  , cdcStartPosition :: Core.Maybe Core.Text
    -- ^ Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
  , cdcStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
  , cdcStopPosition :: Core.Maybe Core.Text
    -- ^ Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
  , replicationTaskSettings :: Core.Maybe Core.Text
    -- ^ Overall settings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Specifying Task Settings for AWS Database Migration Service Tasks> in the /AWS Database Migration User Guide./ 
  , resourceIdentifier :: Core.Maybe Core.Text
    -- ^ A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ One or more tags to be assigned to the replication task.
  , taskData :: Core.Maybe Core.Text
    -- ^ Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateReplicationTask' value with any optional fields omitted.
mkCreateReplicationTask
    :: Core.Text -- ^ 'replicationTaskIdentifier'
    -> Core.Text -- ^ 'sourceEndpointArn'
    -> Core.Text -- ^ 'targetEndpointArn'
    -> Core.Text -- ^ 'replicationInstanceArn'
    -> Types.MigrationTypeValue -- ^ 'migrationType'
    -> Core.Text -- ^ 'tableMappings'
    -> CreateReplicationTask
mkCreateReplicationTask replicationTaskIdentifier sourceEndpointArn
  targetEndpointArn replicationInstanceArn migrationType
  tableMappings
  = CreateReplicationTask'{replicationTaskIdentifier,
                           sourceEndpointArn, targetEndpointArn, replicationInstanceArn,
                           migrationType, tableMappings, cdcStartPosition = Core.Nothing,
                           cdcStartTime = Core.Nothing, cdcStopPosition = Core.Nothing,
                           replicationTaskSettings = Core.Nothing,
                           resourceIdentifier = Core.Nothing, tags = Core.Nothing,
                           taskData = Core.Nothing}

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
crtReplicationTaskIdentifier :: Lens.Lens' CreateReplicationTask Core.Text
crtReplicationTaskIdentifier = Lens.field @"replicationTaskIdentifier"
{-# INLINEABLE crtReplicationTaskIdentifier #-}
{-# DEPRECATED replicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead"  #-}

-- | An Amazon Resource Name (ARN) that uniquely identifies the source endpoint.
--
-- /Note:/ Consider using 'sourceEndpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtSourceEndpointArn :: Lens.Lens' CreateReplicationTask Core.Text
crtSourceEndpointArn = Lens.field @"sourceEndpointArn"
{-# INLINEABLE crtSourceEndpointArn #-}
{-# DEPRECATED sourceEndpointArn "Use generic-lens or generic-optics with 'sourceEndpointArn' instead"  #-}

-- | An Amazon Resource Name (ARN) that uniquely identifies the target endpoint.
--
-- /Note:/ Consider using 'targetEndpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTargetEndpointArn :: Lens.Lens' CreateReplicationTask Core.Text
crtTargetEndpointArn = Lens.field @"targetEndpointArn"
{-# INLINEABLE crtTargetEndpointArn #-}
{-# DEPRECATED targetEndpointArn "Use generic-lens or generic-optics with 'targetEndpointArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of a replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtReplicationInstanceArn :: Lens.Lens' CreateReplicationTask Core.Text
crtReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE crtReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

-- | The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@ 
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtMigrationType :: Lens.Lens' CreateReplicationTask Types.MigrationTypeValue
crtMigrationType = Lens.field @"migrationType"
{-# INLINEABLE crtMigrationType #-}
{-# DEPRECATED migrationType "Use generic-lens or generic-optics with 'migrationType' instead"  #-}

-- | The table mappings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TableMapping.html Using Table Mapping to Specify Task Settings> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'tableMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTableMappings :: Lens.Lens' CreateReplicationTask Core.Text
crtTableMappings = Lens.field @"tableMappings"
{-# INLINEABLE crtTableMappings #-}
{-# DEPRECATED tableMappings "Use generic-lens or generic-optics with 'tableMappings' instead"  #-}

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCdcStartPosition :: Lens.Lens' CreateReplicationTask (Core.Maybe Core.Text)
crtCdcStartPosition = Lens.field @"cdcStartPosition"
{-# INLINEABLE crtCdcStartPosition #-}
{-# DEPRECATED cdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead"  #-}

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- /Note:/ Consider using 'cdcStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCdcStartTime :: Lens.Lens' CreateReplicationTask (Core.Maybe Core.NominalDiffTime)
crtCdcStartTime = Lens.field @"cdcStartTime"
{-# INLINEABLE crtCdcStartTime #-}
{-# DEPRECATED cdcStartTime "Use generic-lens or generic-optics with 'cdcStartTime' instead"  #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCdcStopPosition :: Lens.Lens' CreateReplicationTask (Core.Maybe Core.Text)
crtCdcStopPosition = Lens.field @"cdcStopPosition"
{-# INLINEABLE crtCdcStopPosition #-}
{-# DEPRECATED cdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead"  #-}

-- | Overall settings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Specifying Task Settings for AWS Database Migration Service Tasks> in the /AWS Database Migration User Guide./ 
--
-- /Note:/ Consider using 'replicationTaskSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtReplicationTaskSettings :: Lens.Lens' CreateReplicationTask (Core.Maybe Core.Text)
crtReplicationTaskSettings = Lens.field @"replicationTaskSettings"
{-# INLINEABLE crtReplicationTaskSettings #-}
{-# DEPRECATED replicationTaskSettings "Use generic-lens or generic-optics with 'replicationTaskSettings' instead"  #-}

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtResourceIdentifier :: Lens.Lens' CreateReplicationTask (Core.Maybe Core.Text)
crtResourceIdentifier = Lens.field @"resourceIdentifier"
{-# INLINEABLE crtResourceIdentifier #-}
{-# DEPRECATED resourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead"  #-}

-- | One or more tags to be assigned to the replication task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTags :: Lens.Lens' CreateReplicationTask (Core.Maybe [Types.Tag])
crtTags = Lens.field @"tags"
{-# INLINEABLE crtTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'taskData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTaskData :: Lens.Lens' CreateReplicationTask (Core.Maybe Core.Text)
crtTaskData = Lens.field @"taskData"
{-# INLINEABLE crtTaskData #-}
{-# DEPRECATED taskData "Use generic-lens or generic-optics with 'taskData' instead"  #-}

instance Core.ToQuery CreateReplicationTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateReplicationTask where
        toHeaders CreateReplicationTask{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.CreateReplicationTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateReplicationTask where
        toJSON CreateReplicationTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
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
                  ("ReplicationTaskSettings" Core..=) Core.<$>
                    replicationTaskSettings,
                  ("ResourceIdentifier" Core..=) Core.<$> resourceIdentifier,
                  ("Tags" Core..=) Core.<$> tags,
                  ("TaskData" Core..=) Core.<$> taskData])

instance Core.AWSRequest CreateReplicationTask where
        type Rs CreateReplicationTask = CreateReplicationTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateReplicationTaskResponse' Core.<$>
                   (x Core..:? "ReplicationTask") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkCreateReplicationTaskResponse' smart constructor.
data CreateReplicationTaskResponse = CreateReplicationTaskResponse'
  { replicationTask :: Core.Maybe Types.ReplicationTask
    -- ^ The replication task that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateReplicationTaskResponse' value with any optional fields omitted.
mkCreateReplicationTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateReplicationTaskResponse
mkCreateReplicationTaskResponse responseStatus
  = CreateReplicationTaskResponse'{replicationTask = Core.Nothing,
                                   responseStatus}

-- | The replication task that was created.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrrsReplicationTask :: Lens.Lens' CreateReplicationTaskResponse (Core.Maybe Types.ReplicationTask)
crtrrsReplicationTask = Lens.field @"replicationTask"
{-# INLINEABLE crtrrsReplicationTask #-}
{-# DEPRECATED replicationTask "Use generic-lens or generic-optics with 'replicationTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrrsResponseStatus :: Lens.Lens' CreateReplicationTaskResponse Core.Int
crtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
