{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTask where

import Network.AWS.DMS.Types.MigrationTypeValue
import Network.AWS.DMS.Types.ReplicationTaskStats
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that describes a replication task created by the @CreateReplicationTask@ operation.
--
--
--
-- /See:/ 'replicationTask' smart constructor.
data ReplicationTask = ReplicationTask'
  { _repReplicationTaskSettings ::
      !(Maybe Text),
    _repStatus :: !(Maybe Text),
    _repStopReason :: !(Maybe Text),
    _repTargetEndpointARN :: !(Maybe Text),
    _repReplicationTaskIdentifier :: !(Maybe Text),
    _repCdcStartPosition :: !(Maybe Text),
    _repReplicationTaskStartDate :: !(Maybe POSIX),
    _repSourceEndpointARN :: !(Maybe Text),
    _repRecoveryCheckpoint :: !(Maybe Text),
    _repTableMappings :: !(Maybe Text),
    _repTargetReplicationInstanceARN :: !(Maybe Text),
    _repReplicationTaskCreationDate :: !(Maybe POSIX),
    _repMigrationType :: !(Maybe MigrationTypeValue),
    _repReplicationTaskARN :: !(Maybe Text),
    _repTaskData :: !(Maybe Text),
    _repCdcStopPosition :: !(Maybe Text),
    _repReplicationTaskStats :: !(Maybe ReplicationTaskStats),
    _repReplicationInstanceARN :: !(Maybe Text),
    _repLastFailureMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'repReplicationTaskSettings' - The settings for the replication task.
--
-- * 'repStatus' - The status of the replication task. This response parameter can return one of the following values:     * @"moving"@ – The task is being moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation.     * @"creating"@ – The task is being created in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html @CreateReplicationTask@ > operation.     * @"deleting"@ – The task is being deleted in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html @DeleteReplicationTask@ > operation.     * @"failed"@ – The task failed to successfully complete the database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.     * @"failed-move"@ – The task failed to move in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation.     * @"modifying"@ – The task definition is being modified in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html @ModifyReplicationTask@ > operation.     * @"ready"@ – The task is in a @ready@ state where it can respond to other task operations, such as <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > or <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html @DeleteReplicationTask@ > .      * @"running"@ – The task is performing a database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.     * @"starting"@ – The task is preparing to perform a database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.     * @"stopped"@ – The task has stopped in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html @StopReplicationTask@ > operation.     * @"stopping"@ – The task is preparing to stop in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html @StopReplicationTask@ > operation.     * @"testing"@ – The database migration specified for this task is being tested in response to running either the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html @StartReplicationTaskAssessmentRun@ > or the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html @StartReplicationTaskAssessment@ > operation.
--
-- * 'repStopReason' - The reason the replication task was stopped. This response parameter can return one of the following values:     * @"STOP_REASON_FULL_LOAD_COMPLETED"@ – Full-load migration completed.     * @"STOP_REASON_CACHED_CHANGES_APPLIED"@ – Change data capture (CDC) load completed.     * @"STOP_REASON_CACHED_CHANGES_NOT_APPLIED"@ – In a full-load and CDC migration, the full load stopped as specified before starting the CDC migration.     * @"STOP_REASON_SERVER_TIME"@ – The migration stopped at the specified server time.
--
-- * 'repTargetEndpointARN' - The ARN that uniquely identifies the endpoint.
--
-- * 'repReplicationTaskIdentifier' - The user-assigned replication task identifier or name. Constraints:     * Must contain 1-255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'repCdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- * 'repReplicationTaskStartDate' - The date the replication task is scheduled to start.
--
-- * 'repSourceEndpointARN' - The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
--
-- * 'repRecoveryCheckpoint' - Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
--
-- * 'repTableMappings' - Table mappings specified in the task.
--
-- * 'repTargetReplicationInstanceARN' - The ARN of the replication instance to which this task is moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation. Otherwise, this response parameter isn't a member of the @ReplicationTask@ object.
--
-- * 'repReplicationTaskCreationDate' - The date the replication task was created.
--
-- * 'repMigrationType' - The type of migration.
--
-- * 'repReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
--
-- * 'repTaskData' - Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
--
-- * 'repCdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- * 'repReplicationTaskStats' - The statistics for the task, including elapsed time, tables loaded, and table errors.
--
-- * 'repReplicationInstanceARN' - The ARN of the replication instance.
--
-- * 'repLastFailureMessage' - The last error (failure) message generated for the replication task.
replicationTask ::
  ReplicationTask
replicationTask =
  ReplicationTask'
    { _repReplicationTaskSettings = Nothing,
      _repStatus = Nothing,
      _repStopReason = Nothing,
      _repTargetEndpointARN = Nothing,
      _repReplicationTaskIdentifier = Nothing,
      _repCdcStartPosition = Nothing,
      _repReplicationTaskStartDate = Nothing,
      _repSourceEndpointARN = Nothing,
      _repRecoveryCheckpoint = Nothing,
      _repTableMappings = Nothing,
      _repTargetReplicationInstanceARN = Nothing,
      _repReplicationTaskCreationDate = Nothing,
      _repMigrationType = Nothing,
      _repReplicationTaskARN = Nothing,
      _repTaskData = Nothing,
      _repCdcStopPosition = Nothing,
      _repReplicationTaskStats = Nothing,
      _repReplicationInstanceARN = Nothing,
      _repLastFailureMessage = Nothing
    }

-- | The settings for the replication task.
repReplicationTaskSettings :: Lens' ReplicationTask (Maybe Text)
repReplicationTaskSettings = lens _repReplicationTaskSettings (\s a -> s {_repReplicationTaskSettings = a})

-- | The status of the replication task. This response parameter can return one of the following values:     * @"moving"@ – The task is being moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation.     * @"creating"@ – The task is being created in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html @CreateReplicationTask@ > operation.     * @"deleting"@ – The task is being deleted in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html @DeleteReplicationTask@ > operation.     * @"failed"@ – The task failed to successfully complete the database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.     * @"failed-move"@ – The task failed to move in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation.     * @"modifying"@ – The task definition is being modified in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html @ModifyReplicationTask@ > operation.     * @"ready"@ – The task is in a @ready@ state where it can respond to other task operations, such as <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > or <https://docs.aws.amazon.com/dms/latest/APIReference/API_DeleteReplicationTask.html @DeleteReplicationTask@ > .      * @"running"@ – The task is performing a database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.     * @"starting"@ – The task is preparing to perform a database migration in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html @StartReplicationTask@ > operation.     * @"stopped"@ – The task has stopped in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html @StopReplicationTask@ > operation.     * @"stopping"@ – The task is preparing to stop in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StopReplicationTask.html @StopReplicationTask@ > operation.     * @"testing"@ – The database migration specified for this task is being tested in response to running either the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessmentRun.html @StartReplicationTaskAssessmentRun@ > or the <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTaskAssessment.html @StartReplicationTaskAssessment@ > operation.
repStatus :: Lens' ReplicationTask (Maybe Text)
repStatus = lens _repStatus (\s a -> s {_repStatus = a})

-- | The reason the replication task was stopped. This response parameter can return one of the following values:     * @"STOP_REASON_FULL_LOAD_COMPLETED"@ – Full-load migration completed.     * @"STOP_REASON_CACHED_CHANGES_APPLIED"@ – Change data capture (CDC) load completed.     * @"STOP_REASON_CACHED_CHANGES_NOT_APPLIED"@ – In a full-load and CDC migration, the full load stopped as specified before starting the CDC migration.     * @"STOP_REASON_SERVER_TIME"@ – The migration stopped at the specified server time.
repStopReason :: Lens' ReplicationTask (Maybe Text)
repStopReason = lens _repStopReason (\s a -> s {_repStopReason = a})

-- | The ARN that uniquely identifies the endpoint.
repTargetEndpointARN :: Lens' ReplicationTask (Maybe Text)
repTargetEndpointARN = lens _repTargetEndpointARN (\s a -> s {_repTargetEndpointARN = a})

-- | The user-assigned replication task identifier or name. Constraints:     * Must contain 1-255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
repReplicationTaskIdentifier :: Lens' ReplicationTask (Maybe Text)
repReplicationTaskIdentifier = lens _repReplicationTaskIdentifier (\s a -> s {_repReplicationTaskIdentifier = a})

-- | Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
repCdcStartPosition :: Lens' ReplicationTask (Maybe Text)
repCdcStartPosition = lens _repCdcStartPosition (\s a -> s {_repCdcStartPosition = a})

-- | The date the replication task is scheduled to start.
repReplicationTaskStartDate :: Lens' ReplicationTask (Maybe UTCTime)
repReplicationTaskStartDate = lens _repReplicationTaskStartDate (\s a -> s {_repReplicationTaskStartDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) that uniquely identifies the endpoint.
repSourceEndpointARN :: Lens' ReplicationTask (Maybe Text)
repSourceEndpointARN = lens _repSourceEndpointARN (\s a -> s {_repSourceEndpointARN = a})

-- | Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
repRecoveryCheckpoint :: Lens' ReplicationTask (Maybe Text)
repRecoveryCheckpoint = lens _repRecoveryCheckpoint (\s a -> s {_repRecoveryCheckpoint = a})

-- | Table mappings specified in the task.
repTableMappings :: Lens' ReplicationTask (Maybe Text)
repTableMappings = lens _repTableMappings (\s a -> s {_repTableMappings = a})

-- | The ARN of the replication instance to which this task is moved in response to running the <https://docs.aws.amazon.com/dms/latest/APIReference/API_MoveReplicationTask.html @MoveReplicationTask@ > operation. Otherwise, this response parameter isn't a member of the @ReplicationTask@ object.
repTargetReplicationInstanceARN :: Lens' ReplicationTask (Maybe Text)
repTargetReplicationInstanceARN = lens _repTargetReplicationInstanceARN (\s a -> s {_repTargetReplicationInstanceARN = a})

-- | The date the replication task was created.
repReplicationTaskCreationDate :: Lens' ReplicationTask (Maybe UTCTime)
repReplicationTaskCreationDate = lens _repReplicationTaskCreationDate (\s a -> s {_repReplicationTaskCreationDate = a}) . mapping _Time

-- | The type of migration.
repMigrationType :: Lens' ReplicationTask (Maybe MigrationTypeValue)
repMigrationType = lens _repMigrationType (\s a -> s {_repMigrationType = a})

-- | The Amazon Resource Name (ARN) of the replication task.
repReplicationTaskARN :: Lens' ReplicationTask (Maybe Text)
repReplicationTaskARN = lens _repReplicationTaskARN (\s a -> s {_repReplicationTaskARN = a})

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
repTaskData :: Lens' ReplicationTask (Maybe Text)
repTaskData = lens _repTaskData (\s a -> s {_repTaskData = a})

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
repCdcStopPosition :: Lens' ReplicationTask (Maybe Text)
repCdcStopPosition = lens _repCdcStopPosition (\s a -> s {_repCdcStopPosition = a})

-- | The statistics for the task, including elapsed time, tables loaded, and table errors.
repReplicationTaskStats :: Lens' ReplicationTask (Maybe ReplicationTaskStats)
repReplicationTaskStats = lens _repReplicationTaskStats (\s a -> s {_repReplicationTaskStats = a})

-- | The ARN of the replication instance.
repReplicationInstanceARN :: Lens' ReplicationTask (Maybe Text)
repReplicationInstanceARN = lens _repReplicationInstanceARN (\s a -> s {_repReplicationInstanceARN = a})

-- | The last error (failure) message generated for the replication task.
repLastFailureMessage :: Lens' ReplicationTask (Maybe Text)
repLastFailureMessage = lens _repLastFailureMessage (\s a -> s {_repLastFailureMessage = a})

instance FromJSON ReplicationTask where
  parseJSON =
    withObject
      "ReplicationTask"
      ( \x ->
          ReplicationTask'
            <$> (x .:? "ReplicationTaskSettings")
            <*> (x .:? "Status")
            <*> (x .:? "StopReason")
            <*> (x .:? "TargetEndpointArn")
            <*> (x .:? "ReplicationTaskIdentifier")
            <*> (x .:? "CdcStartPosition")
            <*> (x .:? "ReplicationTaskStartDate")
            <*> (x .:? "SourceEndpointArn")
            <*> (x .:? "RecoveryCheckpoint")
            <*> (x .:? "TableMappings")
            <*> (x .:? "TargetReplicationInstanceArn")
            <*> (x .:? "ReplicationTaskCreationDate")
            <*> (x .:? "MigrationType")
            <*> (x .:? "ReplicationTaskArn")
            <*> (x .:? "TaskData")
            <*> (x .:? "CdcStopPosition")
            <*> (x .:? "ReplicationTaskStats")
            <*> (x .:? "ReplicationInstanceArn")
            <*> (x .:? "LastFailureMessage")
      )

instance Hashable ReplicationTask

instance NFData ReplicationTask
