{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReplicationTask
  ( ReplicationTask (..)
  -- * Smart constructor
  , mkReplicationTask
  -- * Lenses
  , rtCdcStartPosition
  , rtCdcStopPosition
  , rtLastFailureMessage
  , rtMigrationType
  , rtRecoveryCheckpoint
  , rtReplicationInstanceArn
  , rtReplicationTaskArn
  , rtReplicationTaskCreationDate
  , rtReplicationTaskIdentifier
  , rtReplicationTaskSettings
  , rtReplicationTaskStartDate
  , rtReplicationTaskStats
  , rtSourceEndpointArn
  , rtStatus
  , rtStopReason
  , rtTableMappings
  , rtTargetEndpointArn
  , rtTargetReplicationInstanceArn
  , rtTaskData
  ) where

import qualified Network.AWS.DMS.Types.MigrationTypeValue as Types
import qualified Network.AWS.DMS.Types.ReplicationTaskStats as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that describes a replication task created by the @CreateReplicationTask@ operation.
--
-- /See:/ 'mkReplicationTask' smart constructor.
data ReplicationTask = ReplicationTask'
  { cdcStartPosition :: Core.Maybe Core.Text
    -- ^ Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
  , cdcStopPosition :: Core.Maybe Core.Text
    -- ^ Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
  , lastFailureMessage :: Core.Maybe Core.Text
    -- ^ The last error (failure) message generated for the replication task.
  , migrationType :: Core.Maybe Types.MigrationTypeValue
    -- ^ The type of migration.
  , recoveryCheckpoint :: Core.Maybe Core.Text
    -- ^ Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
  , replicationInstanceArn :: Core.Maybe Core.Text
    -- ^ The ARN of the replication instance.
  , replicationTaskArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication task.
  , replicationTaskCreationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the replication task was created.
  , replicationTaskIdentifier :: Core.Maybe Core.Text
    -- ^ The user-assigned replication task identifier or name.
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
  , replicationTaskSettings :: Core.Maybe Core.Text
    -- ^ The settings for the replication task.
  , replicationTaskStartDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the replication task is scheduled to start.
  , replicationTaskStats :: Core.Maybe Types.ReplicationTaskStats
    -- ^ The statistics for the task, including elapsed time, tables loaded, and table errors.
  , sourceEndpointArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the replication task. This response parameter can return one of the following values:
--
--
--     * @"moving"@ – The task is being moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation.
--
--
--     * @"creating"@ – The task is being created in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html @CreateReplicationTask@ > operation.
--
--
--     * @"deleting"@ – The task is being deleted in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html @DeleteReplicationTask@ > operation.
--
--
--     * @"failed"@ – The task failed to successfully complete the database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.
--
--
--     * @"failed-move"@ – The task failed to move in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation.
--
--
--     * @"modifying"@ – The task definition is being modified in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html @ModifyReplicationTask@ > operation.
--
--
--     * @"ready"@ – The task is in a @ready@ state where it can respond to other task operations, such as <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > or <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html @DeleteReplicationTask@ > . 
--
--
--     * @"running"@ – The task is performing a database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.
--
--
--     * @"starting"@ – The task is preparing to perform a database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.
--
--
--     * @"stopped"@ – The task has stopped in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html @StopReplicationTask@ > operation.
--
--
--     * @"stopping"@ – The task is preparing to stop in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html @StopReplicationTask@ > operation.
--
--
--     * @"testing"@ – The database migration specified for this task is being tested in response to running either the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html @StartReplicationTaskAssessmentRun@ > or the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html @StartReplicationTaskAssessment@ > operation.
--
--
  , stopReason :: Core.Maybe Core.Text
    -- ^ The reason the replication task was stopped. This response parameter can return one of the following values:
--
--
--     * @"STOP_REASON_FULL_LOAD_COMPLETED"@ – Full-load migration completed.
--
--
--     * @"STOP_REASON_CACHED_CHANGES_APPLIED"@ – Change data capture (CDC) load completed.
--
--
--     * @"STOP_REASON_CACHED_CHANGES_NOT_APPLIED"@ – In a full-load and CDC migration, the full load stopped as specified before starting the CDC migration.
--
--
--     * @"STOP_REASON_SERVER_TIME"@ – The migration stopped at the specified server time.
--
--
  , tableMappings :: Core.Maybe Core.Text
    -- ^ Table mappings specified in the task.
  , targetEndpointArn :: Core.Maybe Core.Text
    -- ^ The ARN that uniquely identifies the endpoint.
  , targetReplicationInstanceArn :: Core.Maybe Core.Text
    -- ^ The ARN of the replication instance to which this task is moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation. Otherwise, this response parameter isn't a member of the @ReplicationTask@ object.
  , taskData :: Core.Maybe Core.Text
    -- ^ Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReplicationTask' value with any optional fields omitted.
mkReplicationTask
    :: ReplicationTask
mkReplicationTask
  = ReplicationTask'{cdcStartPosition = Core.Nothing,
                     cdcStopPosition = Core.Nothing, lastFailureMessage = Core.Nothing,
                     migrationType = Core.Nothing, recoveryCheckpoint = Core.Nothing,
                     replicationInstanceArn = Core.Nothing,
                     replicationTaskArn = Core.Nothing,
                     replicationTaskCreationDate = Core.Nothing,
                     replicationTaskIdentifier = Core.Nothing,
                     replicationTaskSettings = Core.Nothing,
                     replicationTaskStartDate = Core.Nothing,
                     replicationTaskStats = Core.Nothing,
                     sourceEndpointArn = Core.Nothing, status = Core.Nothing,
                     stopReason = Core.Nothing, tableMappings = Core.Nothing,
                     targetEndpointArn = Core.Nothing,
                     targetReplicationInstanceArn = Core.Nothing,
                     taskData = Core.Nothing}

-- | Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCdcStartPosition :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtCdcStartPosition = Lens.field @"cdcStartPosition"
{-# INLINEABLE rtCdcStartPosition #-}
{-# DEPRECATED cdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead"  #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCdcStopPosition :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtCdcStopPosition = Lens.field @"cdcStopPosition"
{-# INLINEABLE rtCdcStopPosition #-}
{-# DEPRECATED cdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead"  #-}

-- | The last error (failure) message generated for the replication task.
--
-- /Note:/ Consider using 'lastFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtLastFailureMessage :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtLastFailureMessage = Lens.field @"lastFailureMessage"
{-# INLINEABLE rtLastFailureMessage #-}
{-# DEPRECATED lastFailureMessage "Use generic-lens or generic-optics with 'lastFailureMessage' instead"  #-}

-- | The type of migration.
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtMigrationType :: Lens.Lens' ReplicationTask (Core.Maybe Types.MigrationTypeValue)
rtMigrationType = Lens.field @"migrationType"
{-# INLINEABLE rtMigrationType #-}
{-# DEPRECATED migrationType "Use generic-lens or generic-optics with 'migrationType' instead"  #-}

-- | Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
--
-- /Note:/ Consider using 'recoveryCheckpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtRecoveryCheckpoint :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtRecoveryCheckpoint = Lens.field @"recoveryCheckpoint"
{-# INLINEABLE rtRecoveryCheckpoint #-}
{-# DEPRECATED recoveryCheckpoint "Use generic-lens or generic-optics with 'recoveryCheckpoint' instead"  #-}

-- | The ARN of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationInstanceArn :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE rtReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationTaskArn :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE rtReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

-- | The date the replication task was created.
--
-- /Note:/ Consider using 'replicationTaskCreationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationTaskCreationDate :: Lens.Lens' ReplicationTask (Core.Maybe Core.NominalDiffTime)
rtReplicationTaskCreationDate = Lens.field @"replicationTaskCreationDate"
{-# INLINEABLE rtReplicationTaskCreationDate #-}
{-# DEPRECATED replicationTaskCreationDate "Use generic-lens or generic-optics with 'replicationTaskCreationDate' instead"  #-}

-- | The user-assigned replication task identifier or name.
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
rtReplicationTaskIdentifier :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtReplicationTaskIdentifier = Lens.field @"replicationTaskIdentifier"
{-# INLINEABLE rtReplicationTaskIdentifier #-}
{-# DEPRECATED replicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead"  #-}

-- | The settings for the replication task.
--
-- /Note:/ Consider using 'replicationTaskSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationTaskSettings :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtReplicationTaskSettings = Lens.field @"replicationTaskSettings"
{-# INLINEABLE rtReplicationTaskSettings #-}
{-# DEPRECATED replicationTaskSettings "Use generic-lens or generic-optics with 'replicationTaskSettings' instead"  #-}

-- | The date the replication task is scheduled to start.
--
-- /Note:/ Consider using 'replicationTaskStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationTaskStartDate :: Lens.Lens' ReplicationTask (Core.Maybe Core.NominalDiffTime)
rtReplicationTaskStartDate = Lens.field @"replicationTaskStartDate"
{-# INLINEABLE rtReplicationTaskStartDate #-}
{-# DEPRECATED replicationTaskStartDate "Use generic-lens or generic-optics with 'replicationTaskStartDate' instead"  #-}

-- | The statistics for the task, including elapsed time, tables loaded, and table errors.
--
-- /Note:/ Consider using 'replicationTaskStats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationTaskStats :: Lens.Lens' ReplicationTask (Core.Maybe Types.ReplicationTaskStats)
rtReplicationTaskStats = Lens.field @"replicationTaskStats"
{-# INLINEABLE rtReplicationTaskStats #-}
{-# DEPRECATED replicationTaskStats "Use generic-lens or generic-optics with 'replicationTaskStats' instead"  #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'sourceEndpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtSourceEndpointArn :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtSourceEndpointArn = Lens.field @"sourceEndpointArn"
{-# INLINEABLE rtSourceEndpointArn #-}
{-# DEPRECATED sourceEndpointArn "Use generic-lens or generic-optics with 'sourceEndpointArn' instead"  #-}

-- | The status of the replication task. This response parameter can return one of the following values:
--
--
--     * @"moving"@ – The task is being moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation.
--
--
--     * @"creating"@ – The task is being created in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html @CreateReplicationTask@ > operation.
--
--
--     * @"deleting"@ – The task is being deleted in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html @DeleteReplicationTask@ > operation.
--
--
--     * @"failed"@ – The task failed to successfully complete the database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.
--
--
--     * @"failed-move"@ – The task failed to move in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation.
--
--
--     * @"modifying"@ – The task definition is being modified in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html @ModifyReplicationTask@ > operation.
--
--
--     * @"ready"@ – The task is in a @ready@ state where it can respond to other task operations, such as <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > or <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html @DeleteReplicationTask@ > . 
--
--
--     * @"running"@ – The task is performing a database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.
--
--
--     * @"starting"@ – The task is preparing to perform a database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.
--
--
--     * @"stopped"@ – The task has stopped in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html @StopReplicationTask@ > operation.
--
--
--     * @"stopping"@ – The task is preparing to stop in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html @StopReplicationTask@ > operation.
--
--
--     * @"testing"@ – The database migration specified for this task is being tested in response to running either the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html @StartReplicationTaskAssessmentRun@ > or the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html @StartReplicationTaskAssessment@ > operation.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtStatus :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtStatus = Lens.field @"status"
{-# INLINEABLE rtStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The reason the replication task was stopped. This response parameter can return one of the following values:
--
--
--     * @"STOP_REASON_FULL_LOAD_COMPLETED"@ – Full-load migration completed.
--
--
--     * @"STOP_REASON_CACHED_CHANGES_APPLIED"@ – Change data capture (CDC) load completed.
--
--
--     * @"STOP_REASON_CACHED_CHANGES_NOT_APPLIED"@ – In a full-load and CDC migration, the full load stopped as specified before starting the CDC migration.
--
--
--     * @"STOP_REASON_SERVER_TIME"@ – The migration stopped at the specified server time.
--
--
--
-- /Note:/ Consider using 'stopReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtStopReason :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtStopReason = Lens.field @"stopReason"
{-# INLINEABLE rtStopReason #-}
{-# DEPRECATED stopReason "Use generic-lens or generic-optics with 'stopReason' instead"  #-}

-- | Table mappings specified in the task.
--
-- /Note:/ Consider using 'tableMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTableMappings :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtTableMappings = Lens.field @"tableMappings"
{-# INLINEABLE rtTableMappings #-}
{-# DEPRECATED tableMappings "Use generic-lens or generic-optics with 'tableMappings' instead"  #-}

-- | The ARN that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'targetEndpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargetEndpointArn :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtTargetEndpointArn = Lens.field @"targetEndpointArn"
{-# INLINEABLE rtTargetEndpointArn #-}
{-# DEPRECATED targetEndpointArn "Use generic-lens or generic-optics with 'targetEndpointArn' instead"  #-}

-- | The ARN of the replication instance to which this task is moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation. Otherwise, this response parameter isn't a member of the @ReplicationTask@ object.
--
-- /Note:/ Consider using 'targetReplicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargetReplicationInstanceArn :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtTargetReplicationInstanceArn = Lens.field @"targetReplicationInstanceArn"
{-# INLINEABLE rtTargetReplicationInstanceArn #-}
{-# DEPRECATED targetReplicationInstanceArn "Use generic-lens or generic-optics with 'targetReplicationInstanceArn' instead"  #-}

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'taskData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTaskData :: Lens.Lens' ReplicationTask (Core.Maybe Core.Text)
rtTaskData = Lens.field @"taskData"
{-# INLINEABLE rtTaskData #-}
{-# DEPRECATED taskData "Use generic-lens or generic-optics with 'taskData' instead"  #-}

instance Core.FromJSON ReplicationTask where
        parseJSON
          = Core.withObject "ReplicationTask" Core.$
              \ x ->
                ReplicationTask' Core.<$>
                  (x Core..:? "CdcStartPosition") Core.<*>
                    x Core..:? "CdcStopPosition"
                    Core.<*> x Core..:? "LastFailureMessage"
                    Core.<*> x Core..:? "MigrationType"
                    Core.<*> x Core..:? "RecoveryCheckpoint"
                    Core.<*> x Core..:? "ReplicationInstanceArn"
                    Core.<*> x Core..:? "ReplicationTaskArn"
                    Core.<*> x Core..:? "ReplicationTaskCreationDate"
                    Core.<*> x Core..:? "ReplicationTaskIdentifier"
                    Core.<*> x Core..:? "ReplicationTaskSettings"
                    Core.<*> x Core..:? "ReplicationTaskStartDate"
                    Core.<*> x Core..:? "ReplicationTaskStats"
                    Core.<*> x Core..:? "SourceEndpointArn"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StopReason"
                    Core.<*> x Core..:? "TableMappings"
                    Core.<*> x Core..:? "TargetEndpointArn"
                    Core.<*> x Core..:? "TargetReplicationInstanceArn"
                    Core.<*> x Core..:? "TaskData"
