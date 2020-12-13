{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    rtReplicationTaskSettings,
    rtStatus,
    rtStopReason,
    rtTargetEndpointARN,
    rtReplicationTaskIdentifier,
    rtCdcStartPosition,
    rtReplicationTaskStartDate,
    rtSourceEndpointARN,
    rtRecoveryCheckpoint,
    rtTableMappings,
    rtTargetReplicationInstanceARN,
    rtReplicationTaskCreationDate,
    rtMigrationType,
    rtReplicationTaskARN,
    rtTaskData,
    rtCdcStopPosition,
    rtReplicationTaskStats,
    rtReplicationInstanceARN,
    rtLastFailureMessage,
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
  { -- | The settings for the replication task.
    replicationTaskSettings :: Lude.Maybe Lude.Text,
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
    status :: Lude.Maybe Lude.Text,
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
    stopReason :: Lude.Maybe Lude.Text,
    -- | The ARN that uniquely identifies the endpoint.
    targetEndpointARN :: Lude.Maybe Lude.Text,
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
    replicationTaskIdentifier :: Lude.Maybe Lude.Text,
    -- | Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error.
    --
    -- The value can be in date, checkpoint, or LSN/SCN format.
    -- Date Example: --cdc-start-position “2018-03-08T12:12:12”
    -- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
    -- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
    cdcStartPosition :: Lude.Maybe Lude.Text,
    -- | The date the replication task is scheduled to start.
    replicationTaskStartDate :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
    sourceEndpointARN :: Lude.Maybe Lude.Text,
    -- | Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
    recoveryCheckpoint :: Lude.Maybe Lude.Text,
    -- | Table mappings specified in the task.
    tableMappings :: Lude.Maybe Lude.Text,
    -- | The ARN of the replication instance to which this task is moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation. Otherwise, this response parameter isn't a member of the @ReplicationTask@ object.
    targetReplicationInstanceARN :: Lude.Maybe Lude.Text,
    -- | The date the replication task was created.
    replicationTaskCreationDate :: Lude.Maybe Lude.Timestamp,
    -- | The type of migration.
    migrationType :: Lude.Maybe MigrationTypeValue,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskARN :: Lude.Maybe Lude.Text,
    -- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
    taskData :: Lude.Maybe Lude.Text,
    -- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
    --
    -- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
    -- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
    cdcStopPosition :: Lude.Maybe Lude.Text,
    -- | The statistics for the task, including elapsed time, tables loaded, and table errors.
    replicationTaskStats :: Lude.Maybe ReplicationTaskStats,
    -- | The ARN of the replication instance.
    replicationInstanceARN :: Lude.Maybe Lude.Text,
    -- | The last error (failure) message generated for the replication task.
    lastFailureMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationTask' with the minimum fields required to make a request.
--
-- * 'replicationTaskSettings' - The settings for the replication task.
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
-- * 'targetEndpointARN' - The ARN that uniquely identifies the endpoint.
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
-- * 'cdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
-- * 'replicationTaskStartDate' - The date the replication task is scheduled to start.
-- * 'sourceEndpointARN' - The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
-- * 'recoveryCheckpoint' - Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
-- * 'tableMappings' - Table mappings specified in the task.
-- * 'targetReplicationInstanceARN' - The ARN of the replication instance to which this task is moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation. Otherwise, this response parameter isn't a member of the @ReplicationTask@ object.
-- * 'replicationTaskCreationDate' - The date the replication task was created.
-- * 'migrationType' - The type of migration.
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
-- * 'taskData' - Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
-- * 'cdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
-- * 'replicationTaskStats' - The statistics for the task, including elapsed time, tables loaded, and table errors.
-- * 'replicationInstanceARN' - The ARN of the replication instance.
-- * 'lastFailureMessage' - The last error (failure) message generated for the replication task.
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
rtReplicationTaskSettings :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtReplicationTaskSettings = Lens.lens (replicationTaskSettings :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskSettings = a} :: ReplicationTask)
{-# DEPRECATED rtReplicationTaskSettings "Use generic-lens or generic-optics with 'replicationTaskSettings' instead." #-}

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
rtStatus :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtStatus = Lens.lens (status :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ReplicationTask)
{-# DEPRECATED rtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

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
rtStopReason :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtStopReason = Lens.lens (stopReason :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {stopReason = a} :: ReplicationTask)
{-# DEPRECATED rtStopReason "Use generic-lens or generic-optics with 'stopReason' instead." #-}

-- | The ARN that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'targetEndpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargetEndpointARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtTargetEndpointARN = Lens.lens (targetEndpointARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {targetEndpointARN = a} :: ReplicationTask)
{-# DEPRECATED rtTargetEndpointARN "Use generic-lens or generic-optics with 'targetEndpointARN' instead." #-}

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
rtReplicationTaskIdentifier :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtReplicationTaskIdentifier = Lens.lens (replicationTaskIdentifier :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskIdentifier = a} :: ReplicationTask)
{-# DEPRECATED rtReplicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCdcStartPosition :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtCdcStartPosition = Lens.lens (cdcStartPosition :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStartPosition = a} :: ReplicationTask)
{-# DEPRECATED rtCdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead." #-}

-- | The date the replication task is scheduled to start.
--
-- /Note:/ Consider using 'replicationTaskStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationTaskStartDate :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Timestamp)
rtReplicationTaskStartDate = Lens.lens (replicationTaskStartDate :: ReplicationTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {replicationTaskStartDate = a} :: ReplicationTask)
{-# DEPRECATED rtReplicationTaskStartDate "Use generic-lens or generic-optics with 'replicationTaskStartDate' instead." #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'sourceEndpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtSourceEndpointARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtSourceEndpointARN = Lens.lens (sourceEndpointARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {sourceEndpointARN = a} :: ReplicationTask)
{-# DEPRECATED rtSourceEndpointARN "Use generic-lens or generic-optics with 'sourceEndpointARN' instead." #-}

-- | Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
--
-- /Note:/ Consider using 'recoveryCheckpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtRecoveryCheckpoint :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtRecoveryCheckpoint = Lens.lens (recoveryCheckpoint :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {recoveryCheckpoint = a} :: ReplicationTask)
{-# DEPRECATED rtRecoveryCheckpoint "Use generic-lens or generic-optics with 'recoveryCheckpoint' instead." #-}

-- | Table mappings specified in the task.
--
-- /Note:/ Consider using 'tableMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTableMappings :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtTableMappings = Lens.lens (tableMappings :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {tableMappings = a} :: ReplicationTask)
{-# DEPRECATED rtTableMappings "Use generic-lens or generic-optics with 'tableMappings' instead." #-}

-- | The ARN of the replication instance to which this task is moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation. Otherwise, this response parameter isn't a member of the @ReplicationTask@ object.
--
-- /Note:/ Consider using 'targetReplicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargetReplicationInstanceARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtTargetReplicationInstanceARN = Lens.lens (targetReplicationInstanceARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {targetReplicationInstanceARN = a} :: ReplicationTask)
{-# DEPRECATED rtTargetReplicationInstanceARN "Use generic-lens or generic-optics with 'targetReplicationInstanceARN' instead." #-}

-- | The date the replication task was created.
--
-- /Note:/ Consider using 'replicationTaskCreationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationTaskCreationDate :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Timestamp)
rtReplicationTaskCreationDate = Lens.lens (replicationTaskCreationDate :: ReplicationTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {replicationTaskCreationDate = a} :: ReplicationTask)
{-# DEPRECATED rtReplicationTaskCreationDate "Use generic-lens or generic-optics with 'replicationTaskCreationDate' instead." #-}

-- | The type of migration.
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtMigrationType :: Lens.Lens' ReplicationTask (Lude.Maybe MigrationTypeValue)
rtMigrationType = Lens.lens (migrationType :: ReplicationTask -> Lude.Maybe MigrationTypeValue) (\s a -> s {migrationType = a} :: ReplicationTask)
{-# DEPRECATED rtMigrationType "Use generic-lens or generic-optics with 'migrationType' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationTaskARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtReplicationTaskARN = Lens.lens (replicationTaskARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskARN = a} :: ReplicationTask)
{-# DEPRECATED rtReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'taskData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTaskData :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtTaskData = Lens.lens (taskData :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {taskData = a} :: ReplicationTask)
{-# DEPRECATED rtTaskData "Use generic-lens or generic-optics with 'taskData' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCdcStopPosition :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtCdcStopPosition = Lens.lens (cdcStopPosition :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStopPosition = a} :: ReplicationTask)
{-# DEPRECATED rtCdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead." #-}

-- | The statistics for the task, including elapsed time, tables loaded, and table errors.
--
-- /Note:/ Consider using 'replicationTaskStats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationTaskStats :: Lens.Lens' ReplicationTask (Lude.Maybe ReplicationTaskStats)
rtReplicationTaskStats = Lens.lens (replicationTaskStats :: ReplicationTask -> Lude.Maybe ReplicationTaskStats) (\s a -> s {replicationTaskStats = a} :: ReplicationTask)
{-# DEPRECATED rtReplicationTaskStats "Use generic-lens or generic-optics with 'replicationTaskStats' instead." #-}

-- | The ARN of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReplicationInstanceARN :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtReplicationInstanceARN = Lens.lens (replicationInstanceARN :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceARN = a} :: ReplicationTask)
{-# DEPRECATED rtReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

-- | The last error (failure) message generated for the replication task.
--
-- /Note:/ Consider using 'lastFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtLastFailureMessage :: Lens.Lens' ReplicationTask (Lude.Maybe Lude.Text)
rtLastFailureMessage = Lens.lens (lastFailureMessage :: ReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {lastFailureMessage = a} :: ReplicationTask)
{-# DEPRECATED rtLastFailureMessage "Use generic-lens or generic-optics with 'lastFailureMessage' instead." #-}

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
