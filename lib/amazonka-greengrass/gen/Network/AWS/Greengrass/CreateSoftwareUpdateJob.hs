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
-- Module      : Network.AWS.Greengrass.CreateSoftwareUpdateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a software update for a core or group of cores (specified as an IoT thing group.) Use this to update the OTA Agent as well as the Greengrass core software. It makes use of the IoT Jobs feature which provides additional commands to manage a Greengrass core software update job.
module Network.AWS.Greengrass.CreateSoftwareUpdateJob
    (
    -- * Creating a Request
      createSoftwareUpdateJob
    , CreateSoftwareUpdateJob
    -- * Request Lenses
    , csujUpdateAgentLogLevel
    , csujAmznClientToken
    , csujSoftwareToUpdate
    , csujUpdateTargetsOperatingSystem
    , csujS3URLSignerRole
    , csujUpdateTargets
    , csujUpdateTargetsArchitecture

    -- * Destructuring the Response
    , createSoftwareUpdateJobResponse
    , CreateSoftwareUpdateJobResponse
    -- * Response Lenses
    , csujrsIotJobARN
    , csujrsIotJobId
    , csujrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSoftwareUpdateJob' smart constructor.
data CreateSoftwareUpdateJob = CreateSoftwareUpdateJob'
  { _csujUpdateAgentLogLevel          :: !(Maybe UpdateAgentLogLevel)
  , _csujAmznClientToken              :: !(Maybe Text)
  , _csujSoftwareToUpdate             :: !(Maybe SoftwareToUpdate)
  , _csujUpdateTargetsOperatingSystem :: !(Maybe UpdateTargetsOperatingSystem)
  , _csujS3URLSignerRole              :: !(Maybe Text)
  , _csujUpdateTargets                :: !(Maybe [Text])
  , _csujUpdateTargetsArchitecture    :: !(Maybe UpdateTargetsArchitecture)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSoftwareUpdateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csujUpdateAgentLogLevel' - Undocumented member.
--
-- * 'csujAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'csujSoftwareToUpdate' - Undocumented member.
--
-- * 'csujUpdateTargetsOperatingSystem' - Undocumented member.
--
-- * 'csujS3URLSignerRole' - Undocumented member.
--
-- * 'csujUpdateTargets' - Undocumented member.
--
-- * 'csujUpdateTargetsArchitecture' - Undocumented member.
createSoftwareUpdateJob
    :: CreateSoftwareUpdateJob
createSoftwareUpdateJob =
  CreateSoftwareUpdateJob'
    { _csujUpdateAgentLogLevel = Nothing
    , _csujAmznClientToken = Nothing
    , _csujSoftwareToUpdate = Nothing
    , _csujUpdateTargetsOperatingSystem = Nothing
    , _csujS3URLSignerRole = Nothing
    , _csujUpdateTargets = Nothing
    , _csujUpdateTargetsArchitecture = Nothing
    }


-- | Undocumented member.
csujUpdateAgentLogLevel :: Lens' CreateSoftwareUpdateJob (Maybe UpdateAgentLogLevel)
csujUpdateAgentLogLevel = lens _csujUpdateAgentLogLevel (\ s a -> s{_csujUpdateAgentLogLevel = a})

-- | A client token used to correlate requests and responses.
csujAmznClientToken :: Lens' CreateSoftwareUpdateJob (Maybe Text)
csujAmznClientToken = lens _csujAmznClientToken (\ s a -> s{_csujAmznClientToken = a})

-- | Undocumented member.
csujSoftwareToUpdate :: Lens' CreateSoftwareUpdateJob (Maybe SoftwareToUpdate)
csujSoftwareToUpdate = lens _csujSoftwareToUpdate (\ s a -> s{_csujSoftwareToUpdate = a})

-- | Undocumented member.
csujUpdateTargetsOperatingSystem :: Lens' CreateSoftwareUpdateJob (Maybe UpdateTargetsOperatingSystem)
csujUpdateTargetsOperatingSystem = lens _csujUpdateTargetsOperatingSystem (\ s a -> s{_csujUpdateTargetsOperatingSystem = a})

-- | Undocumented member.
csujS3URLSignerRole :: Lens' CreateSoftwareUpdateJob (Maybe Text)
csujS3URLSignerRole = lens _csujS3URLSignerRole (\ s a -> s{_csujS3URLSignerRole = a})

-- | Undocumented member.
csujUpdateTargets :: Lens' CreateSoftwareUpdateJob [Text]
csujUpdateTargets = lens _csujUpdateTargets (\ s a -> s{_csujUpdateTargets = a}) . _Default . _Coerce

-- | Undocumented member.
csujUpdateTargetsArchitecture :: Lens' CreateSoftwareUpdateJob (Maybe UpdateTargetsArchitecture)
csujUpdateTargetsArchitecture = lens _csujUpdateTargetsArchitecture (\ s a -> s{_csujUpdateTargetsArchitecture = a})

instance AWSRequest CreateSoftwareUpdateJob where
        type Rs CreateSoftwareUpdateJob =
             CreateSoftwareUpdateJobResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateSoftwareUpdateJobResponse' <$>
                   (x .?> "IotJobArn") <*> (x .?> "IotJobId") <*>
                     (pure (fromEnum s)))

instance Hashable CreateSoftwareUpdateJob where

instance NFData CreateSoftwareUpdateJob where

instance ToHeaders CreateSoftwareUpdateJob where
        toHeaders CreateSoftwareUpdateJob'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _csujAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateSoftwareUpdateJob where
        toJSON CreateSoftwareUpdateJob'{..}
          = object
              (catMaybes
                 [("UpdateAgentLogLevel" .=) <$>
                    _csujUpdateAgentLogLevel,
                  ("SoftwareToUpdate" .=) <$> _csujSoftwareToUpdate,
                  ("UpdateTargetsOperatingSystem" .=) <$>
                    _csujUpdateTargetsOperatingSystem,
                  ("S3UrlSignerRole" .=) <$> _csujS3URLSignerRole,
                  ("UpdateTargets" .=) <$> _csujUpdateTargets,
                  ("UpdateTargetsArchitecture" .=) <$>
                    _csujUpdateTargetsArchitecture])

instance ToPath CreateSoftwareUpdateJob where
        toPath = const "/greengrass/updates"

instance ToQuery CreateSoftwareUpdateJob where
        toQuery = const mempty

-- | /See:/ 'createSoftwareUpdateJobResponse' smart constructor.
data CreateSoftwareUpdateJobResponse = CreateSoftwareUpdateJobResponse'
  { _csujrsIotJobARN      :: !(Maybe Text)
  , _csujrsIotJobId       :: !(Maybe Text)
  , _csujrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSoftwareUpdateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csujrsIotJobARN' - The IoT Job ARN corresponding to this update.
--
-- * 'csujrsIotJobId' - The IoT Job Id corresponding to this update.
--
-- * 'csujrsResponseStatus' - -- | The response status code.
createSoftwareUpdateJobResponse
    :: Int -- ^ 'csujrsResponseStatus'
    -> CreateSoftwareUpdateJobResponse
createSoftwareUpdateJobResponse pResponseStatus_ =
  CreateSoftwareUpdateJobResponse'
    { _csujrsIotJobARN = Nothing
    , _csujrsIotJobId = Nothing
    , _csujrsResponseStatus = pResponseStatus_
    }


-- | The IoT Job ARN corresponding to this update.
csujrsIotJobARN :: Lens' CreateSoftwareUpdateJobResponse (Maybe Text)
csujrsIotJobARN = lens _csujrsIotJobARN (\ s a -> s{_csujrsIotJobARN = a})

-- | The IoT Job Id corresponding to this update.
csujrsIotJobId :: Lens' CreateSoftwareUpdateJobResponse (Maybe Text)
csujrsIotJobId = lens _csujrsIotJobId (\ s a -> s{_csujrsIotJobId = a})

-- | -- | The response status code.
csujrsResponseStatus :: Lens' CreateSoftwareUpdateJobResponse Int
csujrsResponseStatus = lens _csujrsResponseStatus (\ s a -> s{_csujrsResponseStatus = a})

instance NFData CreateSoftwareUpdateJobResponse where
