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
-- Module      : Network.AWS.IoT.CreateOTAUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS IoT OTAUpdate on a target group of things or groups.
module Network.AWS.IoT.CreateOTAUpdate
  ( -- * Creating a Request
    createOTAUpdate,
    CreateOTAUpdate,

    -- * Request Lenses
    cotauAwsJobAbortConfig,
    cotauAwsJobExecutionsRolloutConfig,
    cotauProtocols,
    cotauAwsJobPresignedURLConfig,
    cotauAdditionalParameters,
    cotauAwsJobTimeoutConfig,
    cotauDescription,
    cotauTargetSelection,
    cotauTags,
    cotauOtaUpdateId,
    cotauTargets,
    cotauFiles,
    cotauRoleARN,

    -- * Destructuring the Response
    createOTAUpdateResponse,
    CreateOTAUpdateResponse,

    -- * Response Lenses
    cotaursAwsIotJobId,
    cotaursOtaUpdateStatus,
    cotaursAwsIotJobARN,
    cotaursOtaUpdateId,
    cotaursOtaUpdateARN,
    cotaursResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createOTAUpdate' smart constructor.
data CreateOTAUpdate = CreateOTAUpdate'
  { _cotauAwsJobAbortConfig ::
      !(Maybe AWSJobAbortConfig),
    _cotauAwsJobExecutionsRolloutConfig ::
      !(Maybe AWSJobExecutionsRolloutConfig),
    _cotauProtocols :: !(Maybe (List1 Protocol)),
    _cotauAwsJobPresignedURLConfig ::
      !(Maybe AWSJobPresignedURLConfig),
    _cotauAdditionalParameters :: !(Maybe (Map Text (Text))),
    _cotauAwsJobTimeoutConfig :: !(Maybe AWSJobTimeoutConfig),
    _cotauDescription :: !(Maybe Text),
    _cotauTargetSelection :: !(Maybe TargetSelection),
    _cotauTags :: !(Maybe [Tag]),
    _cotauOtaUpdateId :: !Text,
    _cotauTargets :: !(List1 Text),
    _cotauFiles :: !(List1 OTAUpdateFile),
    _cotauRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateOTAUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cotauAwsJobAbortConfig' - The criteria that determine when and how a job abort takes place.
--
-- * 'cotauAwsJobExecutionsRolloutConfig' - Configuration for the rollout of OTA updates.
--
-- * 'cotauProtocols' - The protocol used to transfer the OTA update image. Valid values are [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the target device can choose the protocol.
--
-- * 'cotauAwsJobPresignedURLConfig' - Configuration information for pre-signed URLs.
--
-- * 'cotauAdditionalParameters' - A list of additional OTA update parameters which are name-value pairs.
--
-- * 'cotauAwsJobTimeoutConfig' - Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
--
-- * 'cotauDescription' - The description of the OTA update.
--
-- * 'cotauTargetSelection' - Specifies whether the update will continue to run (CONTINUOUS), or will be complete after all the things specified as targets have completed the update (SNAPSHOT). If continuous, the update may also be run on a thing when a change is detected in a target. For example, an update will run on a thing when the thing is added to a target group, even after the update was completed by all things originally in the group. Valid values: CONTINUOUS | SNAPSHOT.
--
-- * 'cotauTags' - Metadata which can be used to manage updates.
--
-- * 'cotauOtaUpdateId' - The ID of the OTA update to be created.
--
-- * 'cotauTargets' - The devices targeted to receive OTA updates.
--
-- * 'cotauFiles' - The files to be streamed by the OTA update.
--
-- * 'cotauRoleARN' - The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs and AWS Code Signing resources to create an OTA update job.
createOTAUpdate ::
  -- | 'cotauOtaUpdateId'
  Text ->
  -- | 'cotauTargets'
  NonEmpty Text ->
  -- | 'cotauFiles'
  NonEmpty OTAUpdateFile ->
  -- | 'cotauRoleARN'
  Text ->
  CreateOTAUpdate
createOTAUpdate pOtaUpdateId_ pTargets_ pFiles_ pRoleARN_ =
  CreateOTAUpdate'
    { _cotauAwsJobAbortConfig = Nothing,
      _cotauAwsJobExecutionsRolloutConfig = Nothing,
      _cotauProtocols = Nothing,
      _cotauAwsJobPresignedURLConfig = Nothing,
      _cotauAdditionalParameters = Nothing,
      _cotauAwsJobTimeoutConfig = Nothing,
      _cotauDescription = Nothing,
      _cotauTargetSelection = Nothing,
      _cotauTags = Nothing,
      _cotauOtaUpdateId = pOtaUpdateId_,
      _cotauTargets = _List1 # pTargets_,
      _cotauFiles = _List1 # pFiles_,
      _cotauRoleARN = pRoleARN_
    }

-- | The criteria that determine when and how a job abort takes place.
cotauAwsJobAbortConfig :: Lens' CreateOTAUpdate (Maybe AWSJobAbortConfig)
cotauAwsJobAbortConfig = lens _cotauAwsJobAbortConfig (\s a -> s {_cotauAwsJobAbortConfig = a})

-- | Configuration for the rollout of OTA updates.
cotauAwsJobExecutionsRolloutConfig :: Lens' CreateOTAUpdate (Maybe AWSJobExecutionsRolloutConfig)
cotauAwsJobExecutionsRolloutConfig = lens _cotauAwsJobExecutionsRolloutConfig (\s a -> s {_cotauAwsJobExecutionsRolloutConfig = a})

-- | The protocol used to transfer the OTA update image. Valid values are [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the target device can choose the protocol.
cotauProtocols :: Lens' CreateOTAUpdate (Maybe (NonEmpty Protocol))
cotauProtocols = lens _cotauProtocols (\s a -> s {_cotauProtocols = a}) . mapping _List1

-- | Configuration information for pre-signed URLs.
cotauAwsJobPresignedURLConfig :: Lens' CreateOTAUpdate (Maybe AWSJobPresignedURLConfig)
cotauAwsJobPresignedURLConfig = lens _cotauAwsJobPresignedURLConfig (\s a -> s {_cotauAwsJobPresignedURLConfig = a})

-- | A list of additional OTA update parameters which are name-value pairs.
cotauAdditionalParameters :: Lens' CreateOTAUpdate (HashMap Text (Text))
cotauAdditionalParameters = lens _cotauAdditionalParameters (\s a -> s {_cotauAdditionalParameters = a}) . _Default . _Map

-- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
cotauAwsJobTimeoutConfig :: Lens' CreateOTAUpdate (Maybe AWSJobTimeoutConfig)
cotauAwsJobTimeoutConfig = lens _cotauAwsJobTimeoutConfig (\s a -> s {_cotauAwsJobTimeoutConfig = a})

-- | The description of the OTA update.
cotauDescription :: Lens' CreateOTAUpdate (Maybe Text)
cotauDescription = lens _cotauDescription (\s a -> s {_cotauDescription = a})

-- | Specifies whether the update will continue to run (CONTINUOUS), or will be complete after all the things specified as targets have completed the update (SNAPSHOT). If continuous, the update may also be run on a thing when a change is detected in a target. For example, an update will run on a thing when the thing is added to a target group, even after the update was completed by all things originally in the group. Valid values: CONTINUOUS | SNAPSHOT.
cotauTargetSelection :: Lens' CreateOTAUpdate (Maybe TargetSelection)
cotauTargetSelection = lens _cotauTargetSelection (\s a -> s {_cotauTargetSelection = a})

-- | Metadata which can be used to manage updates.
cotauTags :: Lens' CreateOTAUpdate [Tag]
cotauTags = lens _cotauTags (\s a -> s {_cotauTags = a}) . _Default . _Coerce

-- | The ID of the OTA update to be created.
cotauOtaUpdateId :: Lens' CreateOTAUpdate Text
cotauOtaUpdateId = lens _cotauOtaUpdateId (\s a -> s {_cotauOtaUpdateId = a})

-- | The devices targeted to receive OTA updates.
cotauTargets :: Lens' CreateOTAUpdate (NonEmpty Text)
cotauTargets = lens _cotauTargets (\s a -> s {_cotauTargets = a}) . _List1

-- | The files to be streamed by the OTA update.
cotauFiles :: Lens' CreateOTAUpdate (NonEmpty OTAUpdateFile)
cotauFiles = lens _cotauFiles (\s a -> s {_cotauFiles = a}) . _List1

-- | The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs and AWS Code Signing resources to create an OTA update job.
cotauRoleARN :: Lens' CreateOTAUpdate Text
cotauRoleARN = lens _cotauRoleARN (\s a -> s {_cotauRoleARN = a})

instance AWSRequest CreateOTAUpdate where
  type Rs CreateOTAUpdate = CreateOTAUpdateResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateOTAUpdateResponse'
            <$> (x .?> "awsIotJobId")
            <*> (x .?> "otaUpdateStatus")
            <*> (x .?> "awsIotJobArn")
            <*> (x .?> "otaUpdateId")
            <*> (x .?> "otaUpdateArn")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateOTAUpdate

instance NFData CreateOTAUpdate

instance ToHeaders CreateOTAUpdate where
  toHeaders = const mempty

instance ToJSON CreateOTAUpdate where
  toJSON CreateOTAUpdate' {..} =
    object
      ( catMaybes
          [ ("awsJobAbortConfig" .=) <$> _cotauAwsJobAbortConfig,
            ("awsJobExecutionsRolloutConfig" .=)
              <$> _cotauAwsJobExecutionsRolloutConfig,
            ("protocols" .=) <$> _cotauProtocols,
            ("awsJobPresignedUrlConfig" .=) <$> _cotauAwsJobPresignedURLConfig,
            ("additionalParameters" .=) <$> _cotauAdditionalParameters,
            ("awsJobTimeoutConfig" .=) <$> _cotauAwsJobTimeoutConfig,
            ("description" .=) <$> _cotauDescription,
            ("targetSelection" .=) <$> _cotauTargetSelection,
            ("tags" .=) <$> _cotauTags,
            Just ("targets" .= _cotauTargets),
            Just ("files" .= _cotauFiles),
            Just ("roleArn" .= _cotauRoleARN)
          ]
      )

instance ToPath CreateOTAUpdate where
  toPath CreateOTAUpdate' {..} =
    mconcat ["/otaUpdates/", toBS _cotauOtaUpdateId]

instance ToQuery CreateOTAUpdate where
  toQuery = const mempty

-- | /See:/ 'createOTAUpdateResponse' smart constructor.
data CreateOTAUpdateResponse = CreateOTAUpdateResponse'
  { _cotaursAwsIotJobId ::
      !(Maybe Text),
    _cotaursOtaUpdateStatus ::
      !(Maybe OTAUpdateStatus),
    _cotaursAwsIotJobARN :: !(Maybe Text),
    _cotaursOtaUpdateId :: !(Maybe Text),
    _cotaursOtaUpdateARN :: !(Maybe Text),
    _cotaursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateOTAUpdateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cotaursAwsIotJobId' - The AWS IoT job ID associated with the OTA update.
--
-- * 'cotaursOtaUpdateStatus' - The OTA update status.
--
-- * 'cotaursAwsIotJobARN' - The AWS IoT job ARN associated with the OTA update.
--
-- * 'cotaursOtaUpdateId' - The OTA update ID.
--
-- * 'cotaursOtaUpdateARN' - The OTA update ARN.
--
-- * 'cotaursResponseStatus' - -- | The response status code.
createOTAUpdateResponse ::
  -- | 'cotaursResponseStatus'
  Int ->
  CreateOTAUpdateResponse
createOTAUpdateResponse pResponseStatus_ =
  CreateOTAUpdateResponse'
    { _cotaursAwsIotJobId = Nothing,
      _cotaursOtaUpdateStatus = Nothing,
      _cotaursAwsIotJobARN = Nothing,
      _cotaursOtaUpdateId = Nothing,
      _cotaursOtaUpdateARN = Nothing,
      _cotaursResponseStatus = pResponseStatus_
    }

-- | The AWS IoT job ID associated with the OTA update.
cotaursAwsIotJobId :: Lens' CreateOTAUpdateResponse (Maybe Text)
cotaursAwsIotJobId = lens _cotaursAwsIotJobId (\s a -> s {_cotaursAwsIotJobId = a})

-- | The OTA update status.
cotaursOtaUpdateStatus :: Lens' CreateOTAUpdateResponse (Maybe OTAUpdateStatus)
cotaursOtaUpdateStatus = lens _cotaursOtaUpdateStatus (\s a -> s {_cotaursOtaUpdateStatus = a})

-- | The AWS IoT job ARN associated with the OTA update.
cotaursAwsIotJobARN :: Lens' CreateOTAUpdateResponse (Maybe Text)
cotaursAwsIotJobARN = lens _cotaursAwsIotJobARN (\s a -> s {_cotaursAwsIotJobARN = a})

-- | The OTA update ID.
cotaursOtaUpdateId :: Lens' CreateOTAUpdateResponse (Maybe Text)
cotaursOtaUpdateId = lens _cotaursOtaUpdateId (\s a -> s {_cotaursOtaUpdateId = a})

-- | The OTA update ARN.
cotaursOtaUpdateARN :: Lens' CreateOTAUpdateResponse (Maybe Text)
cotaursOtaUpdateARN = lens _cotaursOtaUpdateARN (\s a -> s {_cotaursOtaUpdateARN = a})

-- | -- | The response status code.
cotaursResponseStatus :: Lens' CreateOTAUpdateResponse Int
cotaursResponseStatus = lens _cotaursResponseStatus (\s a -> s {_cotaursResponseStatus = a})

instance NFData CreateOTAUpdateResponse
