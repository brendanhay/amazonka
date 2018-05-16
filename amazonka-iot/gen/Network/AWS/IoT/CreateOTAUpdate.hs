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
-- Module      : Network.AWS.IoT.CreateOTAUpdate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS IoT OTAUpdate on a target group of things or groups.
--
--
module Network.AWS.IoT.CreateOTAUpdate
    (
    -- * Creating a Request
      createOTAUpdate
    , CreateOTAUpdate
    -- * Request Lenses
    , cotauAdditionalParameters
    , cotauDescription
    , cotauTargetSelection
    , cotauOtaUpdateId
    , cotauTargets
    , cotauFiles
    , cotauRoleARN

    -- * Destructuring the Response
    , createOTAUpdateResponse
    , CreateOTAUpdateResponse
    -- * Response Lenses
    , cotaursAwsIotJobId
    , cotaursOtaUpdateStatus
    , cotaursAwsIotJobARN
    , cotaursOtaUpdateId
    , cotaursOtaUpdateARN
    , cotaursResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createOTAUpdate' smart constructor.
data CreateOTAUpdate = CreateOTAUpdate'
  { _cotauAdditionalParameters :: !(Maybe (Map Text Text))
  , _cotauDescription          :: !(Maybe Text)
  , _cotauTargetSelection      :: !(Maybe TargetSelection)
  , _cotauOtaUpdateId          :: !Text
  , _cotauTargets              :: !(List1 Text)
  , _cotauFiles                :: !(List1 OTAUpdateFile)
  , _cotauRoleARN              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateOTAUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cotauAdditionalParameters' - A list of additional OTA update parameters which are name-value pairs.
--
-- * 'cotauDescription' - The description of the OTA update.
--
-- * 'cotauTargetSelection' - Specifies whether the update will continue to run (CONTINUOUS), or will be complete after all the things specified as targets have completed the update (SNAPSHOT). If continuous, the update may also be run on a thing when a change is detected in a target. For example, an update will run on a thing when the thing is added to a target group, even after the update was completed by all things originally in the group. Valid values: CONTINUOUS | SNAPSHOT.
--
-- * 'cotauOtaUpdateId' - The ID of the OTA update to be created.
--
-- * 'cotauTargets' - The targeted devices to receive OTA updates.
--
-- * 'cotauFiles' - The files to be streamed by the OTA update.
--
-- * 'cotauRoleARN' - The IAM role that allows access to the AWS IoT Jobs service.
createOTAUpdate
    :: Text -- ^ 'cotauOtaUpdateId'
    -> NonEmpty Text -- ^ 'cotauTargets'
    -> NonEmpty OTAUpdateFile -- ^ 'cotauFiles'
    -> Text -- ^ 'cotauRoleARN'
    -> CreateOTAUpdate
createOTAUpdate pOtaUpdateId_ pTargets_ pFiles_ pRoleARN_ =
  CreateOTAUpdate'
    { _cotauAdditionalParameters = Nothing
    , _cotauDescription = Nothing
    , _cotauTargetSelection = Nothing
    , _cotauOtaUpdateId = pOtaUpdateId_
    , _cotauTargets = _List1 # pTargets_
    , _cotauFiles = _List1 # pFiles_
    , _cotauRoleARN = pRoleARN_
    }


-- | A list of additional OTA update parameters which are name-value pairs.
cotauAdditionalParameters :: Lens' CreateOTAUpdate (HashMap Text Text)
cotauAdditionalParameters = lens _cotauAdditionalParameters (\ s a -> s{_cotauAdditionalParameters = a}) . _Default . _Map

-- | The description of the OTA update.
cotauDescription :: Lens' CreateOTAUpdate (Maybe Text)
cotauDescription = lens _cotauDescription (\ s a -> s{_cotauDescription = a})

-- | Specifies whether the update will continue to run (CONTINUOUS), or will be complete after all the things specified as targets have completed the update (SNAPSHOT). If continuous, the update may also be run on a thing when a change is detected in a target. For example, an update will run on a thing when the thing is added to a target group, even after the update was completed by all things originally in the group. Valid values: CONTINUOUS | SNAPSHOT.
cotauTargetSelection :: Lens' CreateOTAUpdate (Maybe TargetSelection)
cotauTargetSelection = lens _cotauTargetSelection (\ s a -> s{_cotauTargetSelection = a})

-- | The ID of the OTA update to be created.
cotauOtaUpdateId :: Lens' CreateOTAUpdate Text
cotauOtaUpdateId = lens _cotauOtaUpdateId (\ s a -> s{_cotauOtaUpdateId = a})

-- | The targeted devices to receive OTA updates.
cotauTargets :: Lens' CreateOTAUpdate (NonEmpty Text)
cotauTargets = lens _cotauTargets (\ s a -> s{_cotauTargets = a}) . _List1

-- | The files to be streamed by the OTA update.
cotauFiles :: Lens' CreateOTAUpdate (NonEmpty OTAUpdateFile)
cotauFiles = lens _cotauFiles (\ s a -> s{_cotauFiles = a}) . _List1

-- | The IAM role that allows access to the AWS IoT Jobs service.
cotauRoleARN :: Lens' CreateOTAUpdate Text
cotauRoleARN = lens _cotauRoleARN (\ s a -> s{_cotauRoleARN = a})

instance AWSRequest CreateOTAUpdate where
        type Rs CreateOTAUpdate = CreateOTAUpdateResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateOTAUpdateResponse' <$>
                   (x .?> "awsIotJobId") <*> (x .?> "otaUpdateStatus")
                     <*> (x .?> "awsIotJobArn")
                     <*> (x .?> "otaUpdateId")
                     <*> (x .?> "otaUpdateArn")
                     <*> (pure (fromEnum s)))

instance Hashable CreateOTAUpdate where

instance NFData CreateOTAUpdate where

instance ToHeaders CreateOTAUpdate where
        toHeaders = const mempty

instance ToJSON CreateOTAUpdate where
        toJSON CreateOTAUpdate'{..}
          = object
              (catMaybes
                 [("additionalParameters" .=) <$>
                    _cotauAdditionalParameters,
                  ("description" .=) <$> _cotauDescription,
                  ("targetSelection" .=) <$> _cotauTargetSelection,
                  Just ("targets" .= _cotauTargets),
                  Just ("files" .= _cotauFiles),
                  Just ("roleArn" .= _cotauRoleARN)])

instance ToPath CreateOTAUpdate where
        toPath CreateOTAUpdate'{..}
          = mconcat ["/otaUpdates/", toBS _cotauOtaUpdateId]

instance ToQuery CreateOTAUpdate where
        toQuery = const mempty

-- | /See:/ 'createOTAUpdateResponse' smart constructor.
data CreateOTAUpdateResponse = CreateOTAUpdateResponse'
  { _cotaursAwsIotJobId     :: !(Maybe Text)
  , _cotaursOtaUpdateStatus :: !(Maybe OTAUpdateStatus)
  , _cotaursAwsIotJobARN    :: !(Maybe Text)
  , _cotaursOtaUpdateId     :: !(Maybe Text)
  , _cotaursOtaUpdateARN    :: !(Maybe Text)
  , _cotaursResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
createOTAUpdateResponse
    :: Int -- ^ 'cotaursResponseStatus'
    -> CreateOTAUpdateResponse
createOTAUpdateResponse pResponseStatus_ =
  CreateOTAUpdateResponse'
    { _cotaursAwsIotJobId = Nothing
    , _cotaursOtaUpdateStatus = Nothing
    , _cotaursAwsIotJobARN = Nothing
    , _cotaursOtaUpdateId = Nothing
    , _cotaursOtaUpdateARN = Nothing
    , _cotaursResponseStatus = pResponseStatus_
    }


-- | The AWS IoT job ID associated with the OTA update.
cotaursAwsIotJobId :: Lens' CreateOTAUpdateResponse (Maybe Text)
cotaursAwsIotJobId = lens _cotaursAwsIotJobId (\ s a -> s{_cotaursAwsIotJobId = a})

-- | The OTA update status.
cotaursOtaUpdateStatus :: Lens' CreateOTAUpdateResponse (Maybe OTAUpdateStatus)
cotaursOtaUpdateStatus = lens _cotaursOtaUpdateStatus (\ s a -> s{_cotaursOtaUpdateStatus = a})

-- | The AWS IoT job ARN associated with the OTA update.
cotaursAwsIotJobARN :: Lens' CreateOTAUpdateResponse (Maybe Text)
cotaursAwsIotJobARN = lens _cotaursAwsIotJobARN (\ s a -> s{_cotaursAwsIotJobARN = a})

-- | The OTA update ID.
cotaursOtaUpdateId :: Lens' CreateOTAUpdateResponse (Maybe Text)
cotaursOtaUpdateId = lens _cotaursOtaUpdateId (\ s a -> s{_cotaursOtaUpdateId = a})

-- | The OTA update ARN.
cotaursOtaUpdateARN :: Lens' CreateOTAUpdateResponse (Maybe Text)
cotaursOtaUpdateARN = lens _cotaursOtaUpdateARN (\ s a -> s{_cotaursOtaUpdateARN = a})

-- | -- | The response status code.
cotaursResponseStatus :: Lens' CreateOTAUpdateResponse Int
cotaursResponseStatus = lens _cotaursResponseStatus (\ s a -> s{_cotaursResponseStatus = a})

instance NFData CreateOTAUpdateResponse where
