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
-- Module      : Network.AWS.DMS.CreateReplicationTask
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication task using the specified parameters.
--
--
module Network.AWS.DMS.CreateReplicationTask
    (
    -- * Creating a Request
      createReplicationTask
    , CreateReplicationTask
    -- * Request Lenses
    , crtReplicationTaskSettings
    , crtTags
    , crtCdcStartTime
    , crtReplicationTaskIdentifier
    , crtSourceEndpointARN
    , crtTargetEndpointARN
    , crtReplicationInstanceARN
    , crtMigrationType
    , crtTableMappings

    -- * Destructuring the Response
    , createReplicationTaskResponse
    , CreateReplicationTaskResponse
    -- * Response Lenses
    , crtrsReplicationTask
    , crtrsResponseStatus
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
-- /See:/ 'createReplicationTask' smart constructor.
data CreateReplicationTask = CreateReplicationTask'
  { _crtReplicationTaskSettings   :: !(Maybe Text)
  , _crtTags                      :: !(Maybe [Tag])
  , _crtCdcStartTime              :: !(Maybe POSIX)
  , _crtReplicationTaskIdentifier :: !Text
  , _crtSourceEndpointARN         :: !Text
  , _crtTargetEndpointARN         :: !Text
  , _crtReplicationInstanceARN    :: !Text
  , _crtMigrationType             :: !MigrationTypeValue
  , _crtTableMappings             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtReplicationTaskSettings' - Settings for the task, such as target metadata settings. For a complete list of task settings, see <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Task Settings for AWS Database Migration Service Tasks> .
--
-- * 'crtTags' - Tags to be added to the replication instance.
--
-- * 'crtCdcStartTime' - The start time for the Change Data Capture (CDC) operation.
--
-- * 'crtReplicationTaskIdentifier' - The replication task identifier. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'crtSourceEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'crtTargetEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'crtReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'crtMigrationType' - The migration type.
--
-- * 'crtTableMappings' - When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with "file://". When working with the DMS API, provide the JSON as the parameter value. For example, --table-mappings file://mappingfile.json
createReplicationTask
    :: Text -- ^ 'crtReplicationTaskIdentifier'
    -> Text -- ^ 'crtSourceEndpointARN'
    -> Text -- ^ 'crtTargetEndpointARN'
    -> Text -- ^ 'crtReplicationInstanceARN'
    -> MigrationTypeValue -- ^ 'crtMigrationType'
    -> Text -- ^ 'crtTableMappings'
    -> CreateReplicationTask
createReplicationTask pReplicationTaskIdentifier_ pSourceEndpointARN_ pTargetEndpointARN_ pReplicationInstanceARN_ pMigrationType_ pTableMappings_ =
  CreateReplicationTask'
  { _crtReplicationTaskSettings = Nothing
  , _crtTags = Nothing
  , _crtCdcStartTime = Nothing
  , _crtReplicationTaskIdentifier = pReplicationTaskIdentifier_
  , _crtSourceEndpointARN = pSourceEndpointARN_
  , _crtTargetEndpointARN = pTargetEndpointARN_
  , _crtReplicationInstanceARN = pReplicationInstanceARN_
  , _crtMigrationType = pMigrationType_
  , _crtTableMappings = pTableMappings_
  }


-- | Settings for the task, such as target metadata settings. For a complete list of task settings, see <http://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Task Settings for AWS Database Migration Service Tasks> .
crtReplicationTaskSettings :: Lens' CreateReplicationTask (Maybe Text)
crtReplicationTaskSettings = lens _crtReplicationTaskSettings (\ s a -> s{_crtReplicationTaskSettings = a});

-- | Tags to be added to the replication instance.
crtTags :: Lens' CreateReplicationTask [Tag]
crtTags = lens _crtTags (\ s a -> s{_crtTags = a}) . _Default . _Coerce;

-- | The start time for the Change Data Capture (CDC) operation.
crtCdcStartTime :: Lens' CreateReplicationTask (Maybe UTCTime)
crtCdcStartTime = lens _crtCdcStartTime (\ s a -> s{_crtCdcStartTime = a}) . mapping _Time;

-- | The replication task identifier. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
crtReplicationTaskIdentifier :: Lens' CreateReplicationTask Text
crtReplicationTaskIdentifier = lens _crtReplicationTaskIdentifier (\ s a -> s{_crtReplicationTaskIdentifier = a});

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
crtSourceEndpointARN :: Lens' CreateReplicationTask Text
crtSourceEndpointARN = lens _crtSourceEndpointARN (\ s a -> s{_crtSourceEndpointARN = a});

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
crtTargetEndpointARN :: Lens' CreateReplicationTask Text
crtTargetEndpointARN = lens _crtTargetEndpointARN (\ s a -> s{_crtTargetEndpointARN = a});

-- | The Amazon Resource Name (ARN) of the replication instance.
crtReplicationInstanceARN :: Lens' CreateReplicationTask Text
crtReplicationInstanceARN = lens _crtReplicationInstanceARN (\ s a -> s{_crtReplicationInstanceARN = a});

-- | The migration type.
crtMigrationType :: Lens' CreateReplicationTask MigrationTypeValue
crtMigrationType = lens _crtMigrationType (\ s a -> s{_crtMigrationType = a});

-- | When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with "file://". When working with the DMS API, provide the JSON as the parameter value. For example, --table-mappings file://mappingfile.json
crtTableMappings :: Lens' CreateReplicationTask Text
crtTableMappings = lens _crtTableMappings (\ s a -> s{_crtTableMappings = a});

instance AWSRequest CreateReplicationTask where
        type Rs CreateReplicationTask =
             CreateReplicationTaskResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 CreateReplicationTaskResponse' <$>
                   (x .?> "ReplicationTask") <*> (pure (fromEnum s)))

instance Hashable CreateReplicationTask where

instance NFData CreateReplicationTask where

instance ToHeaders CreateReplicationTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.CreateReplicationTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateReplicationTask where
        toJSON CreateReplicationTask'{..}
          = object
              (catMaybes
                 [("ReplicationTaskSettings" .=) <$>
                    _crtReplicationTaskSettings,
                  ("Tags" .=) <$> _crtTags,
                  ("CdcStartTime" .=) <$> _crtCdcStartTime,
                  Just
                    ("ReplicationTaskIdentifier" .=
                       _crtReplicationTaskIdentifier),
                  Just ("SourceEndpointArn" .= _crtSourceEndpointARN),
                  Just ("TargetEndpointArn" .= _crtTargetEndpointARN),
                  Just
                    ("ReplicationInstanceArn" .=
                       _crtReplicationInstanceARN),
                  Just ("MigrationType" .= _crtMigrationType),
                  Just ("TableMappings" .= _crtTableMappings)])

instance ToPath CreateReplicationTask where
        toPath = const "/"

instance ToQuery CreateReplicationTask where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'createReplicationTaskResponse' smart constructor.
data CreateReplicationTaskResponse = CreateReplicationTaskResponse'
  { _crtrsReplicationTask :: !(Maybe ReplicationTask)
  , _crtrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtrsReplicationTask' - The replication task that was created.
--
-- * 'crtrsResponseStatus' - -- | The response status code.
createReplicationTaskResponse
    :: Int -- ^ 'crtrsResponseStatus'
    -> CreateReplicationTaskResponse
createReplicationTaskResponse pResponseStatus_ =
  CreateReplicationTaskResponse'
  {_crtrsReplicationTask = Nothing, _crtrsResponseStatus = pResponseStatus_}


-- | The replication task that was created.
crtrsReplicationTask :: Lens' CreateReplicationTaskResponse (Maybe ReplicationTask)
crtrsReplicationTask = lens _crtrsReplicationTask (\ s a -> s{_crtrsReplicationTask = a});

-- | -- | The response status code.
crtrsResponseStatus :: Lens' CreateReplicationTaskResponse Int
crtrsResponseStatus = lens _crtrsResponseStatus (\ s a -> s{_crtrsResponseStatus = a});

instance NFData CreateReplicationTaskResponse where
