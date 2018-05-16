{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ModifyReplicationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
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
-- For more information about AWS DMS tasks, see the AWS DMS user guide at <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks >
--
module Network.AWS.DMS.ModifyReplicationTask
    (
    -- * Creating a Request
      modifyReplicationTask
    , ModifyReplicationTask
    -- * Request Lenses
    , mrtReplicationTaskSettings
    , mrtReplicationTaskIdentifier
    , mrtCdcStartPosition
    , mrtTableMappings
    , mrtMigrationType
    , mrtCdcStopPosition
    , mrtCdcStartTime
    , mrtReplicationTaskARN

    -- * Destructuring the Response
    , modifyReplicationTaskResponse
    , ModifyReplicationTaskResponse
    -- * Response Lenses
    , mrtrsReplicationTask
    , mrtrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
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
  { _mrtReplicationTaskSettings   :: !(Maybe Text)
  , _mrtReplicationTaskIdentifier :: !(Maybe Text)
  , _mrtCdcStartPosition          :: !(Maybe Text)
  , _mrtTableMappings             :: !(Maybe Text)
  , _mrtMigrationType             :: !(Maybe MigrationTypeValue)
  , _mrtCdcStopPosition           :: !(Maybe Text)
  , _mrtCdcStartTime              :: !(Maybe POSIX)
  , _mrtReplicationTaskARN        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrtReplicationTaskSettings' - JSON file that contains settings for the task, such as target metadata settings.
--
-- * 'mrtReplicationTaskIdentifier' - The replication task identifier. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'mrtCdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- * 'mrtTableMappings' - When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with "file://". When working with the DMS API, provide the JSON as the parameter value. For example, --table-mappings file://mappingfile.json
--
-- * 'mrtMigrationType' - The migration type. Valid values: full-load | cdc | full-load-and-cdc
--
-- * 'mrtCdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:3018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 3018-02-09T12:12:12 “
--
-- * 'mrtCdcStartTime' - Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- * 'mrtReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
modifyReplicationTask
    :: Text -- ^ 'mrtReplicationTaskARN'
    -> ModifyReplicationTask
modifyReplicationTask pReplicationTaskARN_ =
  ModifyReplicationTask'
    { _mrtReplicationTaskSettings = Nothing
    , _mrtReplicationTaskIdentifier = Nothing
    , _mrtCdcStartPosition = Nothing
    , _mrtTableMappings = Nothing
    , _mrtMigrationType = Nothing
    , _mrtCdcStopPosition = Nothing
    , _mrtCdcStartTime = Nothing
    , _mrtReplicationTaskARN = pReplicationTaskARN_
    }


-- | JSON file that contains settings for the task, such as target metadata settings.
mrtReplicationTaskSettings :: Lens' ModifyReplicationTask (Maybe Text)
mrtReplicationTaskSettings = lens _mrtReplicationTaskSettings (\ s a -> s{_mrtReplicationTaskSettings = a})

-- | The replication task identifier. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
mrtReplicationTaskIdentifier :: Lens' ModifyReplicationTask (Maybe Text)
mrtReplicationTaskIdentifier = lens _mrtReplicationTaskIdentifier (\ s a -> s{_mrtReplicationTaskIdentifier = a})

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
mrtCdcStartPosition :: Lens' ModifyReplicationTask (Maybe Text)
mrtCdcStartPosition = lens _mrtCdcStartPosition (\ s a -> s{_mrtCdcStartPosition = a})

-- | When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with "file://". When working with the DMS API, provide the JSON as the parameter value. For example, --table-mappings file://mappingfile.json
mrtTableMappings :: Lens' ModifyReplicationTask (Maybe Text)
mrtTableMappings = lens _mrtTableMappings (\ s a -> s{_mrtTableMappings = a})

-- | The migration type. Valid values: full-load | cdc | full-load-and-cdc
mrtMigrationType :: Lens' ModifyReplicationTask (Maybe MigrationTypeValue)
mrtMigrationType = lens _mrtMigrationType (\ s a -> s{_mrtMigrationType = a})

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:3018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 3018-02-09T12:12:12 “
mrtCdcStopPosition :: Lens' ModifyReplicationTask (Maybe Text)
mrtCdcStopPosition = lens _mrtCdcStopPosition (\ s a -> s{_mrtCdcStopPosition = a})

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
mrtCdcStartTime :: Lens' ModifyReplicationTask (Maybe UTCTime)
mrtCdcStartTime = lens _mrtCdcStartTime (\ s a -> s{_mrtCdcStartTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the replication task.
mrtReplicationTaskARN :: Lens' ModifyReplicationTask Text
mrtReplicationTaskARN = lens _mrtReplicationTaskARN (\ s a -> s{_mrtReplicationTaskARN = a})

instance AWSRequest ModifyReplicationTask where
        type Rs ModifyReplicationTask =
             ModifyReplicationTaskResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 ModifyReplicationTaskResponse' <$>
                   (x .?> "ReplicationTask") <*> (pure (fromEnum s)))

instance Hashable ModifyReplicationTask where

instance NFData ModifyReplicationTask where

instance ToHeaders ModifyReplicationTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.ModifyReplicationTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyReplicationTask where
        toJSON ModifyReplicationTask'{..}
          = object
              (catMaybes
                 [("ReplicationTaskSettings" .=) <$>
                    _mrtReplicationTaskSettings,
                  ("ReplicationTaskIdentifier" .=) <$>
                    _mrtReplicationTaskIdentifier,
                  ("CdcStartPosition" .=) <$> _mrtCdcStartPosition,
                  ("TableMappings" .=) <$> _mrtTableMappings,
                  ("MigrationType" .=) <$> _mrtMigrationType,
                  ("CdcStopPosition" .=) <$> _mrtCdcStopPosition,
                  ("CdcStartTime" .=) <$> _mrtCdcStartTime,
                  Just
                    ("ReplicationTaskArn" .= _mrtReplicationTaskARN)])

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
  { _mrtrsReplicationTask :: !(Maybe ReplicationTask)
  , _mrtrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrtrsReplicationTask' - The replication task that was modified.
--
-- * 'mrtrsResponseStatus' - -- | The response status code.
modifyReplicationTaskResponse
    :: Int -- ^ 'mrtrsResponseStatus'
    -> ModifyReplicationTaskResponse
modifyReplicationTaskResponse pResponseStatus_ =
  ModifyReplicationTaskResponse'
    {_mrtrsReplicationTask = Nothing, _mrtrsResponseStatus = pResponseStatus_}


-- | The replication task that was modified.
mrtrsReplicationTask :: Lens' ModifyReplicationTaskResponse (Maybe ReplicationTask)
mrtrsReplicationTask = lens _mrtrsReplicationTask (\ s a -> s{_mrtrsReplicationTask = a})

-- | -- | The response status code.
mrtrsResponseStatus :: Lens' ModifyReplicationTaskResponse Int
mrtrsResponseStatus = lens _mrtrsResponseStatus (\ s a -> s{_mrtrsResponseStatus = a})

instance NFData ModifyReplicationTaskResponse where
