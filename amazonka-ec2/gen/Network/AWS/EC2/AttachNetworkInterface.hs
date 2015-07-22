{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachNetworkInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Attaches a network interface to an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachNetworkInterface.html>
module Network.AWS.EC2.AttachNetworkInterface
    (
    -- * Request
      AttachNetworkInterface
    -- ** Request constructor
    , attachNetworkInterface
    -- ** Request lenses
    , anirqDryRun
    , anirqNetworkInterfaceId
    , anirqInstanceId
    , anirqDeviceIndex

    -- * Response
    , AttachNetworkInterfaceResponse
    -- ** Response constructor
    , attachNetworkInterfaceResponse
    -- ** Response lenses
    , anirsAttachmentId
    , anirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachNetworkInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'anirqDryRun'
--
-- * 'anirqNetworkInterfaceId'
--
-- * 'anirqInstanceId'
--
-- * 'anirqDeviceIndex'
data AttachNetworkInterface = AttachNetworkInterface'
    { _anirqDryRun             :: !(Maybe Bool)
    , _anirqNetworkInterfaceId :: !Text
    , _anirqInstanceId         :: !Text
    , _anirqDeviceIndex        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachNetworkInterface' smart constructor.
attachNetworkInterface :: Text -> Text -> Int -> AttachNetworkInterface
attachNetworkInterface pNetworkInterfaceId pInstanceId pDeviceIndex =
    AttachNetworkInterface'
    { _anirqDryRun = Nothing
    , _anirqNetworkInterfaceId = pNetworkInterfaceId
    , _anirqInstanceId = pInstanceId
    , _anirqDeviceIndex = pDeviceIndex
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
anirqDryRun :: Lens' AttachNetworkInterface (Maybe Bool)
anirqDryRun = lens _anirqDryRun (\ s a -> s{_anirqDryRun = a});

-- | The ID of the network interface.
anirqNetworkInterfaceId :: Lens' AttachNetworkInterface Text
anirqNetworkInterfaceId = lens _anirqNetworkInterfaceId (\ s a -> s{_anirqNetworkInterfaceId = a});

-- | The ID of the instance.
anirqInstanceId :: Lens' AttachNetworkInterface Text
anirqInstanceId = lens _anirqInstanceId (\ s a -> s{_anirqInstanceId = a});

-- | The index of the device for the network interface attachment.
anirqDeviceIndex :: Lens' AttachNetworkInterface Int
anirqDeviceIndex = lens _anirqDeviceIndex (\ s a -> s{_anirqDeviceIndex = a});

instance AWSRequest AttachNetworkInterface where
        type Sv AttachNetworkInterface = EC2
        type Rs AttachNetworkInterface =
             AttachNetworkInterfaceResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 AttachNetworkInterfaceResponse' <$>
                   (x .@? "attachmentId") <*> (pure (fromEnum s)))

instance ToHeaders AttachNetworkInterface where
        toHeaders = const mempty

instance ToPath AttachNetworkInterface where
        toPath = const "/"

instance ToQuery AttachNetworkInterface where
        toQuery AttachNetworkInterface'{..}
          = mconcat
              ["Action" =:
                 ("AttachNetworkInterface" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _anirqDryRun,
               "NetworkInterfaceId" =: _anirqNetworkInterfaceId,
               "InstanceId" =: _anirqInstanceId,
               "DeviceIndex" =: _anirqDeviceIndex]

-- | /See:/ 'attachNetworkInterfaceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'anirsAttachmentId'
--
-- * 'anirsStatus'
data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse'
    { _anirsAttachmentId :: !(Maybe Text)
    , _anirsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachNetworkInterfaceResponse' smart constructor.
attachNetworkInterfaceResponse :: Int -> AttachNetworkInterfaceResponse
attachNetworkInterfaceResponse pStatus =
    AttachNetworkInterfaceResponse'
    { _anirsAttachmentId = Nothing
    , _anirsStatus = pStatus
    }

-- | The ID of the network interface attachment.
anirsAttachmentId :: Lens' AttachNetworkInterfaceResponse (Maybe Text)
anirsAttachmentId = lens _anirsAttachmentId (\ s a -> s{_anirsAttachmentId = a});

-- | FIXME: Undocumented member.
anirsStatus :: Lens' AttachNetworkInterfaceResponse Int
anirsStatus = lens _anirsStatus (\ s a -> s{_anirsStatus = a});
