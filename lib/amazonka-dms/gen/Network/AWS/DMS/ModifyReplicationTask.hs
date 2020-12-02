{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- You can't modify the task endpoints. The task must be stopped before you can modify it.
--
-- For more information about AWS DMS tasks, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks> in the /AWS Database Migration Service User Guide/ .
module Network.AWS.DMS.ModifyReplicationTask
  ( -- * Creating a Request
    modifyReplicationTask,
    ModifyReplicationTask,

    -- * Request Lenses
    mReplicationTaskSettings,
    mReplicationTaskIdentifier,
    mCdcStartPosition,
    mTableMappings,
    mMigrationType,
    mTaskData,
    mCdcStopPosition,
    mCdcStartTime,
    mReplicationTaskARN,

    -- * Destructuring the Response
    modifyReplicationTaskResponse,
    ModifyReplicationTaskResponse,

    -- * Response Lenses
    mrsReplicationTask,
    mrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifyReplicationTask' smart constructor.
data ModifyReplicationTask = ModifyReplicationTask'
  { _mReplicationTaskSettings ::
      !(Maybe Text),
    _mReplicationTaskIdentifier :: !(Maybe Text),
    _mCdcStartPosition :: !(Maybe Text),
    _mTableMappings :: !(Maybe Text),
    _mMigrationType :: !(Maybe MigrationTypeValue),
    _mTaskData :: !(Maybe Text),
    _mCdcStopPosition :: !(Maybe Text),
    _mCdcStartTime :: !(Maybe POSIX),
    _mReplicationTaskARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mReplicationTaskSettings' - JSON file that contains settings for the task, such as task metadata settings.
--
-- * 'mReplicationTaskIdentifier' - The replication task identifier. Constraints:     * Must contain 1-255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'mCdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- * 'mTableMappings' - When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with @file://@ . When working with the DMS API, provide the JSON as the parameter value, for example: @--table-mappings file://mappingfile.json@
--
-- * 'mMigrationType' - The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@
--
-- * 'mTaskData' - Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
--
-- * 'mCdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- * 'mCdcStartTime' - Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error. Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- * 'mReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
modifyReplicationTask ::
  -- | 'mReplicationTaskARN'
  Text ->
  ModifyReplicationTask
modifyReplicationTask pReplicationTaskARN_ =
  ModifyReplicationTask'
    { _mReplicationTaskSettings = Nothing,
      _mReplicationTaskIdentifier = Nothing,
      _mCdcStartPosition = Nothing,
      _mTableMappings = Nothing,
      _mMigrationType = Nothing,
      _mTaskData = Nothing,
      _mCdcStopPosition = Nothing,
      _mCdcStartTime = Nothing,
      _mReplicationTaskARN = pReplicationTaskARN_
    }

-- | JSON file that contains settings for the task, such as task metadata settings.
mReplicationTaskSettings :: Lens' ModifyReplicationTask (Maybe Text)
mReplicationTaskSettings = lens _mReplicationTaskSettings (\s a -> s {_mReplicationTaskSettings = a})

-- | The replication task identifier. Constraints:     * Must contain 1-255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
mReplicationTaskIdentifier :: Lens' ModifyReplicationTask (Maybe Text)
mReplicationTaskIdentifier = lens _mReplicationTaskIdentifier (\s a -> s {_mReplicationTaskIdentifier = a})

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
mCdcStartPosition :: Lens' ModifyReplicationTask (Maybe Text)
mCdcStartPosition = lens _mCdcStartPosition (\s a -> s {_mCdcStartPosition = a})

-- | When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with @file://@ . When working with the DMS API, provide the JSON as the parameter value, for example: @--table-mappings file://mappingfile.json@
mTableMappings :: Lens' ModifyReplicationTask (Maybe Text)
mTableMappings = lens _mTableMappings (\s a -> s {_mTableMappings = a})

-- | The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@
mMigrationType :: Lens' ModifyReplicationTask (Maybe MigrationTypeValue)
mMigrationType = lens _mMigrationType (\s a -> s {_mMigrationType = a})

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
mTaskData :: Lens' ModifyReplicationTask (Maybe Text)
mTaskData = lens _mTaskData (\s a -> s {_mTaskData = a})

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
mCdcStopPosition :: Lens' ModifyReplicationTask (Maybe Text)
mCdcStopPosition = lens _mCdcStopPosition (\s a -> s {_mCdcStopPosition = a})

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error. Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
mCdcStartTime :: Lens' ModifyReplicationTask (Maybe UTCTime)
mCdcStartTime = lens _mCdcStartTime (\s a -> s {_mCdcStartTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the replication task.
mReplicationTaskARN :: Lens' ModifyReplicationTask Text
mReplicationTaskARN = lens _mReplicationTaskARN (\s a -> s {_mReplicationTaskARN = a})

instance AWSRequest ModifyReplicationTask where
  type Rs ModifyReplicationTask = ModifyReplicationTaskResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          ModifyReplicationTaskResponse'
            <$> (x .?> "ReplicationTask") <*> (pure (fromEnum s))
      )

instance Hashable ModifyReplicationTask

instance NFData ModifyReplicationTask

instance ToHeaders ModifyReplicationTask where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonDMSv20160101.ModifyReplicationTask" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ModifyReplicationTask where
  toJSON ModifyReplicationTask' {..} =
    object
      ( catMaybes
          [ ("ReplicationTaskSettings" .=) <$> _mReplicationTaskSettings,
            ("ReplicationTaskIdentifier" .=) <$> _mReplicationTaskIdentifier,
            ("CdcStartPosition" .=) <$> _mCdcStartPosition,
            ("TableMappings" .=) <$> _mTableMappings,
            ("MigrationType" .=) <$> _mMigrationType,
            ("TaskData" .=) <$> _mTaskData,
            ("CdcStopPosition" .=) <$> _mCdcStopPosition,
            ("CdcStartTime" .=) <$> _mCdcStartTime,
            Just ("ReplicationTaskArn" .= _mReplicationTaskARN)
          ]
      )

instance ToPath ModifyReplicationTask where
  toPath = const "/"

instance ToQuery ModifyReplicationTask where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'modifyReplicationTaskResponse' smart constructor.
data ModifyReplicationTaskResponse = ModifyReplicationTaskResponse'
  { _mrsReplicationTask ::
      !(Maybe ReplicationTask),
    _mrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyReplicationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrsReplicationTask' - The replication task that was modified.
--
-- * 'mrsResponseStatus' - -- | The response status code.
modifyReplicationTaskResponse ::
  -- | 'mrsResponseStatus'
  Int ->
  ModifyReplicationTaskResponse
modifyReplicationTaskResponse pResponseStatus_ =
  ModifyReplicationTaskResponse'
    { _mrsReplicationTask = Nothing,
      _mrsResponseStatus = pResponseStatus_
    }

-- | The replication task that was modified.
mrsReplicationTask :: Lens' ModifyReplicationTaskResponse (Maybe ReplicationTask)
mrsReplicationTask = lens _mrsReplicationTask (\s a -> s {_mrsReplicationTask = a})

-- | -- | The response status code.
mrsResponseStatus :: Lens' ModifyReplicationTaskResponse Int
mrsResponseStatus = lens _mrsResponseStatus (\s a -> s {_mrsResponseStatus = a})

instance NFData ModifyReplicationTaskResponse
