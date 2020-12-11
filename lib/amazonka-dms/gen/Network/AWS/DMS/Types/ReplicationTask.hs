-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTask
  ( ReplicationTask (..),

    -- * Smart constructor
    mkReplicationTask,

    -- * Lenses
    repReplicationTaskSettings,
    repStatus,
    repStopReason,
    repTargetEndpointARN,
    repReplicationTaskIdentifier,
    repCdcStartPosition,
    repReplicationTaskStartDate,
    repSourceEndpointARN,
    repRecoveryCheckpoint,
    repTableMappings,
    repTargetReplicationInstanceARN,
    repReplicationTaskCreationDate,
    repMigrationType,
    repReplicationTaskARN,
    repTaskData,
    repCdcStopPosition,
    repReplicationTaskStats,
    repReplicationInstanceARN,
    repLastFailureMessage,
  )
where

import Network.AWS.DMS.Types.MigrationTypeValue
import Network.AWS.DMS.Types.ReplicationTaskStats
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that describes a replication task created by the @CreateReplicationTask@ operation.
--
-- /See:/ 'mkReplicationTask' smart constructor.
data ReplicationTask = ReplicationTask'
  { replicationTaskSettings ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    stopReason :: Lude.Maybe Lude.Text,
    targetEndpointARN :: Lude.Maybe Lude.Text,
    replicationTaskIdentifier :: Lude.Maybe Lude.Text,
    cdcStartPosition :: Lude.Maybe Lude.Text,
    replicationTaskStartDate :: Lude.Maybe Lude.Timestamp,
    sourceEndpointARN :: Lude.Maybe Lude.Text,
    recoveryCheckpoint :: Lude.Maybe Lude.Text,
    tableMappings :: Lude.Maybe Lude.Text,
    targetReplicationInstanceARN :: Lude.Maybe Lude.Text,
    replicationTaskCreationDate :: Lude.Maybe Lude.Timestamp,
    migrationType :: Lude.Maybe MigrationTypeValue,
    replicationTaskARN :: Lude.Maybe Lude.Text,
    taskData :: Lude.Maybe Lude.Text,
    cdcStopPosition :: Lude.Maybe Lude.Text,
    replicationTaskStats :: Lude.Maybe ReplicationTaskStats,
    replicationInstanceARN :: Lude.Maybe Lude.Text,
    lastFailureMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationTask' with the minimum fields required to make a request.
--
-- * 'cdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
-- * 'cdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
-- * 'lastFailureMessage' - The last error (failure) message generated for the replication task.
-- * 'migrationType' - The type of migration.
-- * 'recoveryCheckpoint' - Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
-- * 'replicationInstanceARN' - The ARN of the replication instance.
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
-- * 'replicationTaskCreationDate' - The date the replication task was created.
-- * 'replicationTaskIdentifier' - The user-assigned replication task identifier or name.
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
-- * 'replicationTaskSettings' - The settings for the replication task.
-- * 'replicationTaskStartDate' - The date the replication task is scheduled to start.
-- * 'replicationTaskStats' - The statistics for the task, including elapsed time, tables loaded, and table errors.
-- * 'sourceEndpointARN' - The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
-- * 'status' - The status of the replication task. This response parameter can return one of the following values:
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
-- * 'stopReason' - The reason the replication task was stopped. This response parameter can return one of the following values:
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
-- * 'tableMappings' - Table mappings specified in the task.
-- * 'targetEndpointARN' - The ARN that uniquely identifies the endpoint.
-- * 'targetReplicationInstanceARN' - The ARN of the replication instance to which this task is moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation. Otherwise, this response parameter isn't a member of the @ReplicationTask@ object.
-- * 'taskData' - Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
mkReplicationTask ::
  ReplicationTask
mkReplicationTask =
  ReplicationTask'
    { replicationTaskSettings = Lude.Nothing,
      status = Lude.Nothing,
      stopReason = Lude.Nothing,
      targetEndpointARN = Lude.Nothing,
      replicationTaskIdentifier = Lude.Nothing,
      cdcStartPosition = Lude.Nothing,
      replicationTaskStartDate = Lude.Nothing,
      sourceEndpointARN = Lude.Nothing,
      recoveryCheckpoint = Lude.Nothing,
      tableMappings = Lude.Nothing,
      targetReplicationInstanceARN = Lude.Nothing,
      replicationTaskCreationDate = Lude.Nothing,
      migrationType = Lude.Nothing,
      replicationTaskARN = Lude.Nothing,
      taskData = Lude.Nothing,
      cdcStopPosition = Lude.Nothing,
      replicationTaskStats = Lude.Nothing,
      replicationInstanceARN = Lude.Nothing,
      lastFailureMessage = Lude.Nothing
    }

-- | The settings for the replication task.
--
-- /Note:/ Consider using 'replicationTaskSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repReplicationTaskSettings :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repReplicationTaskSettings = Lens.lens (replicationTaskSettings :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskSettings = a} :: ReplicationTask)
{-# DEPRECATED repReplicationTaskSettings "Use generic-lens or generic-optics with 'replicationTaskSettings' instead." #-}

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
repStatus :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repStatus = Lens.lens (status :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ReplicationTask)
{-# DEPRECATED repStatus "Use generic-lens or generic-optics with 'status' instead." #-}

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
repStopReason :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repStopReason = Lens.lens (stopReason :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {stopReason = a} :: ReplicationTask)
{-# DEPRECATED repStopReason "Use generic-lens or generic-optics with 'stopReason' instead." #-}

-- | The ARN that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'targetEndpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repTargetEndpointARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repTargetEndpointARN = Lens.lens (targetEndpointARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {targetEndpointARN = a} :: ReplicationTask)
{-# DEPRECATED repTargetEndpointARN "Use generic-lens or generic-optics with 'targetEndpointARN' instead." #-}

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
repReplicationTaskIdentifier :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repReplicationTaskIdentifier = Lens.lens (replicationTaskIdentifier :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskIdentifier = a} :: ReplicationTask)
{-# DEPRECATED repReplicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repCdcStartPosition :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repCdcStartPosition = Lens.lens (cdcStartPosition :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStartPosition = a} :: ReplicationTask)
{-# DEPRECATED repCdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead." #-}

-- | The date the replication task is scheduled to start.
--
-- /Note:/ Consider using 'replicationTaskStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repReplicationTaskStartDate :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Timestamp)
repReplicationTaskStartDate = Lens.lens (replicationTaskStartDate :: ReplicationTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {replicationTaskStartDate = a} :: ReplicationTask)
{-# DEPRECATED repReplicationTaskStartDate "Use generic-lens or generic-optics with 'replicationTaskStartDate' instead." #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'sourceEndpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repSourceEndpointARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repSourceEndpointARN = Lens.lens (sourceEndpointARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {sourceEndpointARN = a} :: ReplicationTask)
{-# DEPRECATED repSourceEndpointARN "Use generic-lens or generic-optics with 'sourceEndpointARN' instead." #-}

-- | Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
--
-- /Note:/ Consider using 'recoveryCheckpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repRecoveryCheckpoint :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repRecoveryCheckpoint = Lens.lens (recoveryCheckpoint :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {recoveryCheckpoint = a} :: ReplicationTask)
{-# DEPRECATED repRecoveryCheckpoint "Use generic-lens or generic-optics with 'recoveryCheckpoint' instead." #-}

-- | Table mappings specified in the task.
--
-- /Note:/ Consider using 'tableMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repTableMappings :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repTableMappings = Lens.lens (tableMappings :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {tableMappings = a} :: ReplicationTask)
{-# DEPRECATED repTableMappings "Use generic-lens or generic-optics with 'tableMappings' instead." #-}

-- | The ARN of the replication instance to which this task is moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation. Otherwise, this response parameter isn't a member of the @ReplicationTask@ object.
--
-- /Note:/ Consider using 'targetReplicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repTargetReplicationInstanceARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repTargetReplicationInstanceARN = Lens.lens (targetReplicationInstanceARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {targetReplicationInstanceARN = a} :: ReplicationTask)
{-# DEPRECATED repTargetReplicationInstanceARN "Use generic-lens or generic-optics with 'targetReplicationInstanceARN' instead." #-}

-- | The date the replication task was created.
--
-- /Note:/ Consider using 'replicationTaskCreationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repReplicationTaskCreationDate :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Timestamp)
repReplicationTaskCreationDate = Lens.lens (replicationTaskCreationDate :: ReplicationTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {replicationTaskCreationDate = a} :: ReplicationTask)
{-# DEPRECATED repReplicationTaskCreationDate "Use generic-lens or generic-optics with 'replicationTaskCreationDate' instead." #-}

-- | The type of migration.
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repMigrationType :: Lens.Lens' ReplicationTask (Lude.Maybe MigrationTypeValue)
repMigrationType = Lens.lens (migrationType :: ReplicationTask -> Lude.Maybe MigrationTypeValue) (\s a -> s {migrationType = a} :: ReplicationTask)
{-# DEPRECATED repMigrationType "Use generic-lens or generic-optics with 'migrationType' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repReplicationTaskARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repReplicationTaskARN = Lens.lens (replicationTaskARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskARN = a} :: ReplicationTask)
{-# DEPRECATED repReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'taskData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repTaskData :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repTaskData = Lens.lens (taskData :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {taskData = a} :: ReplicationTask)
{-# DEPRECATED repTaskData "Use generic-lens or generic-optics with 'taskData' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repCdcStopPosition :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repCdcStopPosition = Lens.lens (cdcStopPosition :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStopPosition = a} :: ReplicationTask)
{-# DEPRECATED repCdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead." #-}

-- | The statistics for the task, including elapsed time, tables loaded, and table errors.
--
-- /Note:/ Consider using 'replicationTaskStats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repReplicationTaskStats :: Lens.Lens' ReplicationTask (Lude.Maybe ReplicationTaskStats)
repReplicationTaskStats = Lens.lens (replicationTaskStats :: ReplicationTask -> Lude.Maybe ReplicationTaskStats) (\s a -> s {replicationTaskStats = a} :: ReplicationTask)
{-# DEPRECATED repReplicationTaskStats "Use generic-lens or generic-optics with 'replicationTaskStats' instead." #-}

-- | The ARN of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repReplicationInstanceARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repReplicationInstanceARN = Lens.lens (replicationInstanceARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceARN = a} :: ReplicationTask)
{-# DEPRECATED repReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

-- | The last error (failure) message generated for the replication task.
--
-- /Note:/ Consider using 'lastFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
repLastFailureMessage :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
repLastFailureMessage = Lens.lens (lastFailureMessage :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {lastFailureMessage = a} :: ReplicationTask)
{-# DEPRECATED repLastFailureMessage "Use generic-lens or generic-optics with 'lastFailureMessage' instead." #-}

instance Lude.FromJSON ReplicationTask where
  parseJSON =
    Lude.withObject
      "ReplicationTask"
      ( \x ->
          ReplicationTask'
            Lude.<$> (x Lude..:? "ReplicationTaskSettings")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "StopReason")
            Lude.<*> (x Lude..:? "TargetEndpointArn")
            Lude.<*> (x Lude..:? "ReplicationTaskIdentifier")
            Lude.<*> (x Lude..:? "CdcStartPosition")
            Lude.<*> (x Lude..:? "ReplicationTaskStartDate")
            Lude.<*> (x Lude..:? "SourceEndpointArn")
            Lude.<*> (x Lude..:? "RecoveryCheckpoint")
            Lude.<*> (x Lude..:? "TableMappings")
            Lude.<*> (x Lude..:? "TargetReplicationInstanceArn")
            Lude.<*> (x Lude..:? "ReplicationTaskCreationDate")
            Lude.<*> (x Lude..:? "MigrationType")
            Lude.<*> (x Lude..:? "ReplicationTaskArn")
            Lude.<*> (x Lude..:? "TaskData")
            Lude.<*> (x Lude..:? "CdcStopPosition")
            Lude.<*> (x Lude..:? "ReplicationTaskStats")
            Lude.<*> (x Lude..:? "ReplicationInstanceArn")
            Lude.<*> (x Lude..:? "LastFailureMessage")
      )
