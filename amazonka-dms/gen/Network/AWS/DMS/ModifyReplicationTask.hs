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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , mrtTableMappings
    , mrtMigrationType
    , mrtCdcStartTime
    , mrtReplicationTaskARN

    -- * Destructuring the Response
    , modifyReplicationTaskResponse
    , ModifyReplicationTaskResponse
    -- * Response Lenses
    , mrtrsReplicationTask
    , mrtrsResponseStatus
    ) where

import           Network.AWS.DMS.Types
import           Network.AWS.DMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifyReplicationTask' smart constructor.
data ModifyReplicationTask = ModifyReplicationTask'
    { _mrtReplicationTaskSettings   :: !(Maybe Text)
    , _mrtReplicationTaskIdentifier :: !(Maybe Text)
    , _mrtTableMappings             :: !(Maybe Text)
    , _mrtMigrationType             :: !(Maybe MigrationTypeValue)
    , _mrtCdcStartTime              :: !(Maybe POSIX)
    , _mrtReplicationTaskARN        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrtReplicationTaskSettings' - JSON file that contains settings for the task, such as target metadata settings.
--
-- * 'mrtReplicationTaskIdentifier' - The replication task identifier. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'mrtTableMappings' - When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with "file://". When working with the DMS API, provide the JSON as the parameter value. For example, --table-mappings file://mappingfile.json
--
-- * 'mrtMigrationType' - The migration type. Valid values: full-load | cdc | full-load-and-cdc
--
-- * 'mrtCdcStartTime' - The start time for the Change Data Capture (CDC) operation.
--
-- * 'mrtReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
modifyReplicationTask
    :: Text -- ^ 'mrtReplicationTaskARN'
    -> ModifyReplicationTask
modifyReplicationTask pReplicationTaskARN_ =
    ModifyReplicationTask'
    { _mrtReplicationTaskSettings = Nothing
    , _mrtReplicationTaskIdentifier = Nothing
    , _mrtTableMappings = Nothing
    , _mrtMigrationType = Nothing
    , _mrtCdcStartTime = Nothing
    , _mrtReplicationTaskARN = pReplicationTaskARN_
    }

-- | JSON file that contains settings for the task, such as target metadata settings.
mrtReplicationTaskSettings :: Lens' ModifyReplicationTask (Maybe Text)
mrtReplicationTaskSettings = lens _mrtReplicationTaskSettings (\ s a -> s{_mrtReplicationTaskSettings = a});

-- | The replication task identifier. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
mrtReplicationTaskIdentifier :: Lens' ModifyReplicationTask (Maybe Text)
mrtReplicationTaskIdentifier = lens _mrtReplicationTaskIdentifier (\ s a -> s{_mrtReplicationTaskIdentifier = a});

-- | When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with "file://". When working with the DMS API, provide the JSON as the parameter value. For example, --table-mappings file://mappingfile.json
mrtTableMappings :: Lens' ModifyReplicationTask (Maybe Text)
mrtTableMappings = lens _mrtTableMappings (\ s a -> s{_mrtTableMappings = a});

-- | The migration type. Valid values: full-load | cdc | full-load-and-cdc
mrtMigrationType :: Lens' ModifyReplicationTask (Maybe MigrationTypeValue)
mrtMigrationType = lens _mrtMigrationType (\ s a -> s{_mrtMigrationType = a});

-- | The start time for the Change Data Capture (CDC) operation.
mrtCdcStartTime :: Lens' ModifyReplicationTask (Maybe UTCTime)
mrtCdcStartTime = lens _mrtCdcStartTime (\ s a -> s{_mrtCdcStartTime = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the replication task.
mrtReplicationTaskARN :: Lens' ModifyReplicationTask Text
mrtReplicationTaskARN = lens _mrtReplicationTaskARN (\ s a -> s{_mrtReplicationTaskARN = a});

instance AWSRequest ModifyReplicationTask where
        type Rs ModifyReplicationTask =
             ModifyReplicationTaskResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 ModifyReplicationTaskResponse' <$>
                   (x .?> "ReplicationTask") <*> (pure (fromEnum s)))

instance Hashable ModifyReplicationTask

instance NFData ModifyReplicationTask

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
                  ("TableMappings" .=) <$> _mrtTableMappings,
                  ("MigrationType" .=) <$> _mrtMigrationType,
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _mrtrsReplicationTask = Nothing
    , _mrtrsResponseStatus = pResponseStatus_
    }

-- | The replication task that was modified.
mrtrsReplicationTask :: Lens' ModifyReplicationTaskResponse (Maybe ReplicationTask)
mrtrsReplicationTask = lens _mrtrsReplicationTask (\ s a -> s{_mrtrsReplicationTask = a});

-- | -- | The response status code.
mrtrsResponseStatus :: Lens' ModifyReplicationTaskResponse Int
mrtrsResponseStatus = lens _mrtrsResponseStatus (\ s a -> s{_mrtrsResponseStatus = a});

instance NFData ModifyReplicationTaskResponse
