{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTask where

import Network.AWS.DMS.Types.MigrationTypeValue
import Network.AWS.DMS.Types.ReplicationTaskStats
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that describes a replication task created by the
-- @CreateReplicationTask@ operation.
--
-- /See:/ 'newReplicationTask' smart constructor.
data ReplicationTask = ReplicationTask'
  { -- | The status of the replication task. This response parameter can return
    -- one of the following values:
    --
    -- -   @\"moving\"@ – The task is being moved in response to running the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html MoveReplicationTask>
    --     operation.
    --
    -- -   @\"creating\"@ – The task is being created in response to running
    --     the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html CreateReplicationTask>
    --     operation.
    --
    -- -   @\"deleting\"@ – The task is being deleted in response to running
    --     the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html DeleteReplicationTask>
    --     operation.
    --
    -- -   @\"failed\"@ – The task failed to successfully complete the database
    --     migration in response to running the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
    --     operation.
    --
    -- -   @\"failed-move\"@ – The task failed to move in response to running
    --     the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html MoveReplicationTask>
    --     operation.
    --
    -- -   @\"modifying\"@ – The task definition is being modified in response
    --     to running the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html ModifyReplicationTask>
    --     operation.
    --
    -- -   @\"ready\"@ – The task is in a @ready@ state where it can respond to
    --     other task operations, such as
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
    --     or
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html DeleteReplicationTask>
    --     .
    --
    -- -   @\"running\"@ – The task is performing a database migration in
    --     response to running the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
    --     operation.
    --
    -- -   @\"starting\"@ – The task is preparing to perform a database
    --     migration in response to running the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
    --     operation.
    --
    -- -   @\"stopped\"@ – The task has stopped in response to running the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html StopReplicationTask>
    --     operation.
    --
    -- -   @\"stopping\"@ – The task is preparing to stop in response to
    --     running the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html StopReplicationTask>
    --     operation.
    --
    -- -   @\"testing\"@ – The database migration specified for this task is
    --     being tested in response to running either the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html StartReplicationTaskAssessmentRun>
    --     or the
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html StartReplicationTaskAssessment>
    --     operation.
    --
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html StartReplicationTaskAssessmentRun>
    --     is an improved premigration task assessment operation. The
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html StartReplicationTaskAssessment>
    --     operation assesses data type compatibility only between the source
    --     and target database of a given migration task. In contrast,
    --     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html StartReplicationTaskAssessmentRun>
    --     enables you to specify a variety of premigration task assessments in
    --     addition to data type compatibility. These assessments include ones
    --     for the validity of primary key definitions and likely issues with
    --     database migration performance, among others.
    status :: Prelude.Maybe Prelude.Text,
    -- | The type of migration.
    migrationType :: Prelude.Maybe MigrationTypeValue,
    -- | The date the replication task was created.
    replicationTaskCreationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The reason the replication task was stopped. This response parameter can
    -- return one of the following values:
    --
    -- -   @\"STOP_REASON_FULL_LOAD_COMPLETED\"@ – Full-load migration
    --     completed.
    --
    -- -   @\"STOP_REASON_CACHED_CHANGES_APPLIED\"@ – Change data capture (CDC)
    --     load completed.
    --
    -- -   @\"STOP_REASON_CACHED_CHANGES_NOT_APPLIED\"@ – In a full-load and
    --     CDC migration, the full load stopped as specified before starting
    --     the CDC migration.
    --
    -- -   @\"STOP_REASON_SERVER_TIME\"@ – The migration stopped at the
    --     specified server time.
    stopReason :: Prelude.Maybe Prelude.Text,
    -- | Indicates the last checkpoint that occurred during a change data capture
    -- (CDC) operation. You can provide this value to the @CdcStartPosition@
    -- parameter to start a CDC operation that begins at that checkpoint.
    recoveryCheckpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the replication instance to which this task is moved in
    -- response to running the
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html MoveReplicationTask>
    -- operation. Otherwise, this response parameter isn\'t a member of the
    -- @ReplicationTask@ object.
    targetReplicationInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | Supplemental information that the task requires to migrate the data for
    -- certain source and target endpoints. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
    -- in the /AWS Database Migration Service User Guide./
    taskData :: Prelude.Maybe Prelude.Text,
    -- | The ARN that uniquely identifies the endpoint.
    targetEndpointArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Maybe Prelude.Text,
    -- | The settings for the replication task.
    replicationTaskSettings :: Prelude.Maybe Prelude.Text,
    -- | The last error (failure) message generated for the replication task.
    lastFailureMessage :: Prelude.Maybe Prelude.Text,
    -- | Table mappings specified in the task.
    tableMappings :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
    sourceEndpointArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the replication instance.
    replicationInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The statistics for the task, including elapsed time, tables loaded, and
    -- table errors.
    replicationTaskStats :: Prelude.Maybe ReplicationTaskStats,
    -- | The date the replication task is scheduled to start.
    replicationTaskStartDate :: Prelude.Maybe Prelude.POSIX,
    -- | Indicates when you want a change data capture (CDC) operation to stop.
    -- The value can be either server time or commit time.
    --
    -- Server time example: --cdc-stop-position
    -- “server_time:2018-02-09T12:12:12”
    --
    -- Commit time example: --cdc-stop-position “commit_time:
    -- 2018-02-09T12:12:12 “
    cdcStopPosition :: Prelude.Maybe Prelude.Text,
    -- | Indicates when you want a change data capture (CDC) operation to start.
    -- Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want
    -- the CDC operation to start. Specifying both values results in an error.
    --
    -- The value can be in date, checkpoint, or LSN\/SCN format.
    --
    -- Date Example: --cdc-start-position “2018-03-08T12:12:12”
    --
    -- Checkpoint Example: --cdc-start-position
    -- \"checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93\"
    --
    -- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
    cdcStartPosition :: Prelude.Maybe Prelude.Text,
    -- | The user-assigned replication task identifier or name.
    --
    -- Constraints:
    --
    -- -   Must contain 1-255 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    replicationTaskIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplicationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'replicationTask_status' - The status of the replication task. This response parameter can return
-- one of the following values:
--
-- -   @\"moving\"@ – The task is being moved in response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html MoveReplicationTask>
--     operation.
--
-- -   @\"creating\"@ – The task is being created in response to running
--     the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html CreateReplicationTask>
--     operation.
--
-- -   @\"deleting\"@ – The task is being deleted in response to running
--     the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html DeleteReplicationTask>
--     operation.
--
-- -   @\"failed\"@ – The task failed to successfully complete the database
--     migration in response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
--     operation.
--
-- -   @\"failed-move\"@ – The task failed to move in response to running
--     the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html MoveReplicationTask>
--     operation.
--
-- -   @\"modifying\"@ – The task definition is being modified in response
--     to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html ModifyReplicationTask>
--     operation.
--
-- -   @\"ready\"@ – The task is in a @ready@ state where it can respond to
--     other task operations, such as
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
--     or
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html DeleteReplicationTask>
--     .
--
-- -   @\"running\"@ – The task is performing a database migration in
--     response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
--     operation.
--
-- -   @\"starting\"@ – The task is preparing to perform a database
--     migration in response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
--     operation.
--
-- -   @\"stopped\"@ – The task has stopped in response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html StopReplicationTask>
--     operation.
--
-- -   @\"stopping\"@ – The task is preparing to stop in response to
--     running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html StopReplicationTask>
--     operation.
--
-- -   @\"testing\"@ – The database migration specified for this task is
--     being tested in response to running either the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html StartReplicationTaskAssessmentRun>
--     or the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html StartReplicationTaskAssessment>
--     operation.
--
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html StartReplicationTaskAssessmentRun>
--     is an improved premigration task assessment operation. The
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html StartReplicationTaskAssessment>
--     operation assesses data type compatibility only between the source
--     and target database of a given migration task. In contrast,
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html StartReplicationTaskAssessmentRun>
--     enables you to specify a variety of premigration task assessments in
--     addition to data type compatibility. These assessments include ones
--     for the validity of primary key definitions and likely issues with
--     database migration performance, among others.
--
-- 'migrationType', 'replicationTask_migrationType' - The type of migration.
--
-- 'replicationTaskCreationDate', 'replicationTask_replicationTaskCreationDate' - The date the replication task was created.
--
-- 'stopReason', 'replicationTask_stopReason' - The reason the replication task was stopped. This response parameter can
-- return one of the following values:
--
-- -   @\"STOP_REASON_FULL_LOAD_COMPLETED\"@ – Full-load migration
--     completed.
--
-- -   @\"STOP_REASON_CACHED_CHANGES_APPLIED\"@ – Change data capture (CDC)
--     load completed.
--
-- -   @\"STOP_REASON_CACHED_CHANGES_NOT_APPLIED\"@ – In a full-load and
--     CDC migration, the full load stopped as specified before starting
--     the CDC migration.
--
-- -   @\"STOP_REASON_SERVER_TIME\"@ – The migration stopped at the
--     specified server time.
--
-- 'recoveryCheckpoint', 'replicationTask_recoveryCheckpoint' - Indicates the last checkpoint that occurred during a change data capture
-- (CDC) operation. You can provide this value to the @CdcStartPosition@
-- parameter to start a CDC operation that begins at that checkpoint.
--
-- 'targetReplicationInstanceArn', 'replicationTask_targetReplicationInstanceArn' - The ARN of the replication instance to which this task is moved in
-- response to running the
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html MoveReplicationTask>
-- operation. Otherwise, this response parameter isn\'t a member of the
-- @ReplicationTask@ object.
--
-- 'taskData', 'replicationTask_taskData' - Supplemental information that the task requires to migrate the data for
-- certain source and target endpoints. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
-- in the /AWS Database Migration Service User Guide./
--
-- 'targetEndpointArn', 'replicationTask_targetEndpointArn' - The ARN that uniquely identifies the endpoint.
--
-- 'replicationTaskArn', 'replicationTask_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
--
-- 'replicationTaskSettings', 'replicationTask_replicationTaskSettings' - The settings for the replication task.
--
-- 'lastFailureMessage', 'replicationTask_lastFailureMessage' - The last error (failure) message generated for the replication task.
--
-- 'tableMappings', 'replicationTask_tableMappings' - Table mappings specified in the task.
--
-- 'sourceEndpointArn', 'replicationTask_sourceEndpointArn' - The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
--
-- 'replicationInstanceArn', 'replicationTask_replicationInstanceArn' - The ARN of the replication instance.
--
-- 'replicationTaskStats', 'replicationTask_replicationTaskStats' - The statistics for the task, including elapsed time, tables loaded, and
-- table errors.
--
-- 'replicationTaskStartDate', 'replicationTask_replicationTaskStartDate' - The date the replication task is scheduled to start.
--
-- 'cdcStopPosition', 'replicationTask_cdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop.
-- The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position
-- “server_time:2018-02-09T12:12:12”
--
-- Commit time example: --cdc-stop-position “commit_time:
-- 2018-02-09T12:12:12 “
--
-- 'cdcStartPosition', 'replicationTask_cdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start.
-- Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want
-- the CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN\/SCN format.
--
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
--
-- Checkpoint Example: --cdc-start-position
-- \"checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93\"
--
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- 'replicationTaskIdentifier', 'replicationTask_replicationTaskIdentifier' - The user-assigned replication task identifier or name.
--
-- Constraints:
--
-- -   Must contain 1-255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
newReplicationTask ::
  ReplicationTask
newReplicationTask =
  ReplicationTask'
    { status = Prelude.Nothing,
      migrationType = Prelude.Nothing,
      replicationTaskCreationDate = Prelude.Nothing,
      stopReason = Prelude.Nothing,
      recoveryCheckpoint = Prelude.Nothing,
      targetReplicationInstanceArn = Prelude.Nothing,
      taskData = Prelude.Nothing,
      targetEndpointArn = Prelude.Nothing,
      replicationTaskArn = Prelude.Nothing,
      replicationTaskSettings = Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing,
      tableMappings = Prelude.Nothing,
      sourceEndpointArn = Prelude.Nothing,
      replicationInstanceArn = Prelude.Nothing,
      replicationTaskStats = Prelude.Nothing,
      replicationTaskStartDate = Prelude.Nothing,
      cdcStopPosition = Prelude.Nothing,
      cdcStartPosition = Prelude.Nothing,
      replicationTaskIdentifier = Prelude.Nothing
    }

-- | The status of the replication task. This response parameter can return
-- one of the following values:
--
-- -   @\"moving\"@ – The task is being moved in response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html MoveReplicationTask>
--     operation.
--
-- -   @\"creating\"@ – The task is being created in response to running
--     the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html CreateReplicationTask>
--     operation.
--
-- -   @\"deleting\"@ – The task is being deleted in response to running
--     the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html DeleteReplicationTask>
--     operation.
--
-- -   @\"failed\"@ – The task failed to successfully complete the database
--     migration in response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
--     operation.
--
-- -   @\"failed-move\"@ – The task failed to move in response to running
--     the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html MoveReplicationTask>
--     operation.
--
-- -   @\"modifying\"@ – The task definition is being modified in response
--     to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html ModifyReplicationTask>
--     operation.
--
-- -   @\"ready\"@ – The task is in a @ready@ state where it can respond to
--     other task operations, such as
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
--     or
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html DeleteReplicationTask>
--     .
--
-- -   @\"running\"@ – The task is performing a database migration in
--     response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
--     operation.
--
-- -   @\"starting\"@ – The task is preparing to perform a database
--     migration in response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>
--     operation.
--
-- -   @\"stopped\"@ – The task has stopped in response to running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html StopReplicationTask>
--     operation.
--
-- -   @\"stopping\"@ – The task is preparing to stop in response to
--     running the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html StopReplicationTask>
--     operation.
--
-- -   @\"testing\"@ – The database migration specified for this task is
--     being tested in response to running either the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html StartReplicationTaskAssessmentRun>
--     or the
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html StartReplicationTaskAssessment>
--     operation.
--
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html StartReplicationTaskAssessmentRun>
--     is an improved premigration task assessment operation. The
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html StartReplicationTaskAssessment>
--     operation assesses data type compatibility only between the source
--     and target database of a given migration task. In contrast,
--     <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html StartReplicationTaskAssessmentRun>
--     enables you to specify a variety of premigration task assessments in
--     addition to data type compatibility. These assessments include ones
--     for the validity of primary key definitions and likely issues with
--     database migration performance, among others.
replicationTask_status :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_status = Lens.lens (\ReplicationTask' {status} -> status) (\s@ReplicationTask' {} a -> s {status = a} :: ReplicationTask)

-- | The type of migration.
replicationTask_migrationType :: Lens.Lens' ReplicationTask (Prelude.Maybe MigrationTypeValue)
replicationTask_migrationType = Lens.lens (\ReplicationTask' {migrationType} -> migrationType) (\s@ReplicationTask' {} a -> s {migrationType = a} :: ReplicationTask)

-- | The date the replication task was created.
replicationTask_replicationTaskCreationDate :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.UTCTime)
replicationTask_replicationTaskCreationDate = Lens.lens (\ReplicationTask' {replicationTaskCreationDate} -> replicationTaskCreationDate) (\s@ReplicationTask' {} a -> s {replicationTaskCreationDate = a} :: ReplicationTask) Prelude.. Lens.mapping Prelude._Time

-- | The reason the replication task was stopped. This response parameter can
-- return one of the following values:
--
-- -   @\"STOP_REASON_FULL_LOAD_COMPLETED\"@ – Full-load migration
--     completed.
--
-- -   @\"STOP_REASON_CACHED_CHANGES_APPLIED\"@ – Change data capture (CDC)
--     load completed.
--
-- -   @\"STOP_REASON_CACHED_CHANGES_NOT_APPLIED\"@ – In a full-load and
--     CDC migration, the full load stopped as specified before starting
--     the CDC migration.
--
-- -   @\"STOP_REASON_SERVER_TIME\"@ – The migration stopped at the
--     specified server time.
replicationTask_stopReason :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_stopReason = Lens.lens (\ReplicationTask' {stopReason} -> stopReason) (\s@ReplicationTask' {} a -> s {stopReason = a} :: ReplicationTask)

-- | Indicates the last checkpoint that occurred during a change data capture
-- (CDC) operation. You can provide this value to the @CdcStartPosition@
-- parameter to start a CDC operation that begins at that checkpoint.
replicationTask_recoveryCheckpoint :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_recoveryCheckpoint = Lens.lens (\ReplicationTask' {recoveryCheckpoint} -> recoveryCheckpoint) (\s@ReplicationTask' {} a -> s {recoveryCheckpoint = a} :: ReplicationTask)

-- | The ARN of the replication instance to which this task is moved in
-- response to running the
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html MoveReplicationTask>
-- operation. Otherwise, this response parameter isn\'t a member of the
-- @ReplicationTask@ object.
replicationTask_targetReplicationInstanceArn :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_targetReplicationInstanceArn = Lens.lens (\ReplicationTask' {targetReplicationInstanceArn} -> targetReplicationInstanceArn) (\s@ReplicationTask' {} a -> s {targetReplicationInstanceArn = a} :: ReplicationTask)

-- | Supplemental information that the task requires to migrate the data for
-- certain source and target endpoints. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
-- in the /AWS Database Migration Service User Guide./
replicationTask_taskData :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_taskData = Lens.lens (\ReplicationTask' {taskData} -> taskData) (\s@ReplicationTask' {} a -> s {taskData = a} :: ReplicationTask)

-- | The ARN that uniquely identifies the endpoint.
replicationTask_targetEndpointArn :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_targetEndpointArn = Lens.lens (\ReplicationTask' {targetEndpointArn} -> targetEndpointArn) (\s@ReplicationTask' {} a -> s {targetEndpointArn = a} :: ReplicationTask)

-- | The Amazon Resource Name (ARN) of the replication task.
replicationTask_replicationTaskArn :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_replicationTaskArn = Lens.lens (\ReplicationTask' {replicationTaskArn} -> replicationTaskArn) (\s@ReplicationTask' {} a -> s {replicationTaskArn = a} :: ReplicationTask)

-- | The settings for the replication task.
replicationTask_replicationTaskSettings :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_replicationTaskSettings = Lens.lens (\ReplicationTask' {replicationTaskSettings} -> replicationTaskSettings) (\s@ReplicationTask' {} a -> s {replicationTaskSettings = a} :: ReplicationTask)

-- | The last error (failure) message generated for the replication task.
replicationTask_lastFailureMessage :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_lastFailureMessage = Lens.lens (\ReplicationTask' {lastFailureMessage} -> lastFailureMessage) (\s@ReplicationTask' {} a -> s {lastFailureMessage = a} :: ReplicationTask)

-- | Table mappings specified in the task.
replicationTask_tableMappings :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_tableMappings = Lens.lens (\ReplicationTask' {tableMappings} -> tableMappings) (\s@ReplicationTask' {} a -> s {tableMappings = a} :: ReplicationTask)

-- | The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
replicationTask_sourceEndpointArn :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_sourceEndpointArn = Lens.lens (\ReplicationTask' {sourceEndpointArn} -> sourceEndpointArn) (\s@ReplicationTask' {} a -> s {sourceEndpointArn = a} :: ReplicationTask)

-- | The ARN of the replication instance.
replicationTask_replicationInstanceArn :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_replicationInstanceArn = Lens.lens (\ReplicationTask' {replicationInstanceArn} -> replicationInstanceArn) (\s@ReplicationTask' {} a -> s {replicationInstanceArn = a} :: ReplicationTask)

-- | The statistics for the task, including elapsed time, tables loaded, and
-- table errors.
replicationTask_replicationTaskStats :: Lens.Lens' ReplicationTask (Prelude.Maybe ReplicationTaskStats)
replicationTask_replicationTaskStats = Lens.lens (\ReplicationTask' {replicationTaskStats} -> replicationTaskStats) (\s@ReplicationTask' {} a -> s {replicationTaskStats = a} :: ReplicationTask)

-- | The date the replication task is scheduled to start.
replicationTask_replicationTaskStartDate :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.UTCTime)
replicationTask_replicationTaskStartDate = Lens.lens (\ReplicationTask' {replicationTaskStartDate} -> replicationTaskStartDate) (\s@ReplicationTask' {} a -> s {replicationTaskStartDate = a} :: ReplicationTask) Prelude.. Lens.mapping Prelude._Time

-- | Indicates when you want a change data capture (CDC) operation to stop.
-- The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position
-- “server_time:2018-02-09T12:12:12”
--
-- Commit time example: --cdc-stop-position “commit_time:
-- 2018-02-09T12:12:12 “
replicationTask_cdcStopPosition :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_cdcStopPosition = Lens.lens (\ReplicationTask' {cdcStopPosition} -> cdcStopPosition) (\s@ReplicationTask' {} a -> s {cdcStopPosition = a} :: ReplicationTask)

-- | Indicates when you want a change data capture (CDC) operation to start.
-- Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want
-- the CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN\/SCN format.
--
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
--
-- Checkpoint Example: --cdc-start-position
-- \"checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93\"
--
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
replicationTask_cdcStartPosition :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_cdcStartPosition = Lens.lens (\ReplicationTask' {cdcStartPosition} -> cdcStartPosition) (\s@ReplicationTask' {} a -> s {cdcStartPosition = a} :: ReplicationTask)

-- | The user-assigned replication task identifier or name.
--
-- Constraints:
--
-- -   Must contain 1-255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
replicationTask_replicationTaskIdentifier :: Lens.Lens' ReplicationTask (Prelude.Maybe Prelude.Text)
replicationTask_replicationTaskIdentifier = Lens.lens (\ReplicationTask' {replicationTaskIdentifier} -> replicationTaskIdentifier) (\s@ReplicationTask' {} a -> s {replicationTaskIdentifier = a} :: ReplicationTask)

instance Prelude.FromJSON ReplicationTask where
  parseJSON =
    Prelude.withObject
      "ReplicationTask"
      ( \x ->
          ReplicationTask'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "MigrationType")
            Prelude.<*> (x Prelude..:? "ReplicationTaskCreationDate")
            Prelude.<*> (x Prelude..:? "StopReason")
            Prelude.<*> (x Prelude..:? "RecoveryCheckpoint")
            Prelude.<*> (x Prelude..:? "TargetReplicationInstanceArn")
            Prelude.<*> (x Prelude..:? "TaskData")
            Prelude.<*> (x Prelude..:? "TargetEndpointArn")
            Prelude.<*> (x Prelude..:? "ReplicationTaskArn")
            Prelude.<*> (x Prelude..:? "ReplicationTaskSettings")
            Prelude.<*> (x Prelude..:? "LastFailureMessage")
            Prelude.<*> (x Prelude..:? "TableMappings")
            Prelude.<*> (x Prelude..:? "SourceEndpointArn")
            Prelude.<*> (x Prelude..:? "ReplicationInstanceArn")
            Prelude.<*> (x Prelude..:? "ReplicationTaskStats")
            Prelude.<*> (x Prelude..:? "ReplicationTaskStartDate")
            Prelude.<*> (x Prelude..:? "CdcStopPosition")
            Prelude.<*> (x Prelude..:? "CdcStartPosition")
            Prelude.<*> (x Prelude..:? "ReplicationTaskIdentifier")
      )

instance Prelude.Hashable ReplicationTask

instance Prelude.NFData ReplicationTask
