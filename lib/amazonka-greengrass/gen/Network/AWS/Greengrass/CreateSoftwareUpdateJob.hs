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
-- Module      : Network.AWS.Greengrass.CreateSoftwareUpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a software update for a core or group of cores (specified as an IoT thing group.) Use this to update the OTA Agent as well as the Greengrass core software. It makes use of the IoT Jobs feature which provides additional commands to manage a Greengrass core software update job.
module Network.AWS.Greengrass.CreateSoftwareUpdateJob
  ( -- * Creating a Request
    createSoftwareUpdateJob,
    CreateSoftwareUpdateJob,

    -- * Request Lenses
    csujUpdateAgentLogLevel,
    csujAmznClientToken,
    csujS3URLSignerRole,
    csujUpdateTargetsArchitecture,
    csujSoftwareToUpdate,
    csujUpdateTargets,
    csujUpdateTargetsOperatingSystem,

    -- * Destructuring the Response
    createSoftwareUpdateJobResponse,
    CreateSoftwareUpdateJobResponse,

    -- * Response Lenses
    csujrsPlatformSoftwareVersion,
    csujrsIotJobARN,
    csujrsIotJobId,
    csujrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSoftwareUpdateJob' smart constructor.
data CreateSoftwareUpdateJob = CreateSoftwareUpdateJob'
  { _csujUpdateAgentLogLevel ::
      !(Maybe UpdateAgentLogLevel),
    _csujAmznClientToken :: !(Maybe Text),
    _csujS3URLSignerRole :: !Text,
    _csujUpdateTargetsArchitecture ::
      !UpdateTargetsArchitecture,
    _csujSoftwareToUpdate :: !SoftwareToUpdate,
    _csujUpdateTargets :: ![Text],
    _csujUpdateTargetsOperatingSystem ::
      !UpdateTargetsOperatingSystem
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSoftwareUpdateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csujUpdateAgentLogLevel' - Undocumented member.
--
-- * 'csujAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'csujS3URLSignerRole' - Undocumented member.
--
-- * 'csujUpdateTargetsArchitecture' - Undocumented member.
--
-- * 'csujSoftwareToUpdate' - Undocumented member.
--
-- * 'csujUpdateTargets' - Undocumented member.
--
-- * 'csujUpdateTargetsOperatingSystem' - Undocumented member.
createSoftwareUpdateJob ::
  -- | 'csujS3URLSignerRole'
  Text ->
  -- | 'csujUpdateTargetsArchitecture'
  UpdateTargetsArchitecture ->
  -- | 'csujSoftwareToUpdate'
  SoftwareToUpdate ->
  -- | 'csujUpdateTargetsOperatingSystem'
  UpdateTargetsOperatingSystem ->
  CreateSoftwareUpdateJob
createSoftwareUpdateJob
  pS3URLSignerRole_
  pUpdateTargetsArchitecture_
  pSoftwareToUpdate_
  pUpdateTargetsOperatingSystem_ =
    CreateSoftwareUpdateJob'
      { _csujUpdateAgentLogLevel = Nothing,
        _csujAmznClientToken = Nothing,
        _csujS3URLSignerRole = pS3URLSignerRole_,
        _csujUpdateTargetsArchitecture = pUpdateTargetsArchitecture_,
        _csujSoftwareToUpdate = pSoftwareToUpdate_,
        _csujUpdateTargets = mempty,
        _csujUpdateTargetsOperatingSystem = pUpdateTargetsOperatingSystem_
      }

-- | Undocumented member.
csujUpdateAgentLogLevel :: Lens' CreateSoftwareUpdateJob (Maybe UpdateAgentLogLevel)
csujUpdateAgentLogLevel = lens _csujUpdateAgentLogLevel (\s a -> s {_csujUpdateAgentLogLevel = a})

-- | A client token used to correlate requests and responses.
csujAmznClientToken :: Lens' CreateSoftwareUpdateJob (Maybe Text)
csujAmznClientToken = lens _csujAmznClientToken (\s a -> s {_csujAmznClientToken = a})

-- | Undocumented member.
csujS3URLSignerRole :: Lens' CreateSoftwareUpdateJob Text
csujS3URLSignerRole = lens _csujS3URLSignerRole (\s a -> s {_csujS3URLSignerRole = a})

-- | Undocumented member.
csujUpdateTargetsArchitecture :: Lens' CreateSoftwareUpdateJob UpdateTargetsArchitecture
csujUpdateTargetsArchitecture = lens _csujUpdateTargetsArchitecture (\s a -> s {_csujUpdateTargetsArchitecture = a})

-- | Undocumented member.
csujSoftwareToUpdate :: Lens' CreateSoftwareUpdateJob SoftwareToUpdate
csujSoftwareToUpdate = lens _csujSoftwareToUpdate (\s a -> s {_csujSoftwareToUpdate = a})

-- | Undocumented member.
csujUpdateTargets :: Lens' CreateSoftwareUpdateJob [Text]
csujUpdateTargets = lens _csujUpdateTargets (\s a -> s {_csujUpdateTargets = a}) . _Coerce

-- | Undocumented member.
csujUpdateTargetsOperatingSystem :: Lens' CreateSoftwareUpdateJob UpdateTargetsOperatingSystem
csujUpdateTargetsOperatingSystem = lens _csujUpdateTargetsOperatingSystem (\s a -> s {_csujUpdateTargetsOperatingSystem = a})

instance AWSRequest CreateSoftwareUpdateJob where
  type Rs CreateSoftwareUpdateJob = CreateSoftwareUpdateJobResponse
  request = postJSON greengrass
  response =
    receiveJSON
      ( \s h x ->
          CreateSoftwareUpdateJobResponse'
            <$> (x .?> "PlatformSoftwareVersion")
            <*> (x .?> "IotJobArn")
            <*> (x .?> "IotJobId")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateSoftwareUpdateJob

instance NFData CreateSoftwareUpdateJob

instance ToHeaders CreateSoftwareUpdateJob where
  toHeaders CreateSoftwareUpdateJob' {..} =
    mconcat
      [ "X-Amzn-Client-Token" =# _csujAmznClientToken,
        "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
      ]

instance ToJSON CreateSoftwareUpdateJob where
  toJSON CreateSoftwareUpdateJob' {..} =
    object
      ( catMaybes
          [ ("UpdateAgentLogLevel" .=) <$> _csujUpdateAgentLogLevel,
            Just ("S3UrlSignerRole" .= _csujS3URLSignerRole),
            Just
              ("UpdateTargetsArchitecture" .= _csujUpdateTargetsArchitecture),
            Just ("SoftwareToUpdate" .= _csujSoftwareToUpdate),
            Just ("UpdateTargets" .= _csujUpdateTargets),
            Just
              ( "UpdateTargetsOperatingSystem"
                  .= _csujUpdateTargetsOperatingSystem
              )
          ]
      )

instance ToPath CreateSoftwareUpdateJob where
  toPath = const "/greengrass/updates"

instance ToQuery CreateSoftwareUpdateJob where
  toQuery = const mempty

-- | /See:/ 'createSoftwareUpdateJobResponse' smart constructor.
data CreateSoftwareUpdateJobResponse = CreateSoftwareUpdateJobResponse'
  { _csujrsPlatformSoftwareVersion ::
      !(Maybe Text),
    _csujrsIotJobARN ::
      !(Maybe Text),
    _csujrsIotJobId ::
      !(Maybe Text),
    _csujrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSoftwareUpdateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csujrsPlatformSoftwareVersion' - The software version installed on the device or devices after the update.
--
-- * 'csujrsIotJobARN' - The IoT Job ARN corresponding to this update.
--
-- * 'csujrsIotJobId' - The IoT Job Id corresponding to this update.
--
-- * 'csujrsResponseStatus' - -- | The response status code.
createSoftwareUpdateJobResponse ::
  -- | 'csujrsResponseStatus'
  Int ->
  CreateSoftwareUpdateJobResponse
createSoftwareUpdateJobResponse pResponseStatus_ =
  CreateSoftwareUpdateJobResponse'
    { _csujrsPlatformSoftwareVersion =
        Nothing,
      _csujrsIotJobARN = Nothing,
      _csujrsIotJobId = Nothing,
      _csujrsResponseStatus = pResponseStatus_
    }

-- | The software version installed on the device or devices after the update.
csujrsPlatformSoftwareVersion :: Lens' CreateSoftwareUpdateJobResponse (Maybe Text)
csujrsPlatformSoftwareVersion = lens _csujrsPlatformSoftwareVersion (\s a -> s {_csujrsPlatformSoftwareVersion = a})

-- | The IoT Job ARN corresponding to this update.
csujrsIotJobARN :: Lens' CreateSoftwareUpdateJobResponse (Maybe Text)
csujrsIotJobARN = lens _csujrsIotJobARN (\s a -> s {_csujrsIotJobARN = a})

-- | The IoT Job Id corresponding to this update.
csujrsIotJobId :: Lens' CreateSoftwareUpdateJobResponse (Maybe Text)
csujrsIotJobId = lens _csujrsIotJobId (\s a -> s {_csujrsIotJobId = a})

-- | -- | The response status code.
csujrsResponseStatus :: Lens' CreateSoftwareUpdateJobResponse Int
csujrsResponseStatus = lens _csujrsResponseStatus (\s a -> s {_csujrsResponseStatus = a})

instance NFData CreateSoftwareUpdateJobResponse
