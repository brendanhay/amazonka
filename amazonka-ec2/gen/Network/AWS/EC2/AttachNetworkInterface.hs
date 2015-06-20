{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.AttachNetworkInterface
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Attaches a network interface to an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachNetworkInterface.html>
module Network.AWS.EC2.AttachNetworkInterface
    (
    -- * Request
      AttachNetworkInterface
    -- ** Request constructor
    , attachNetworkInterface
    -- ** Request lenses
    , aniDryRun
    , aniNetworkInterfaceId
    , aniInstanceId
    , aniDeviceIndex

    -- * Response
    , AttachNetworkInterfaceResponse
    -- ** Response constructor
    , attachNetworkInterfaceResponse
    -- ** Response lenses
    , anirAttachmentId
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachNetworkInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aniDryRun'
--
-- * 'aniNetworkInterfaceId'
--
-- * 'aniInstanceId'
--
-- * 'aniDeviceIndex'
data AttachNetworkInterface = AttachNetworkInterface'{_aniDryRun :: Maybe Bool, _aniNetworkInterfaceId :: Text, _aniInstanceId :: Text, _aniDeviceIndex :: Int} deriving (Eq, Read, Show)

-- | 'AttachNetworkInterface' smart constructor.
attachNetworkInterface :: Text -> Text -> Int -> AttachNetworkInterface
attachNetworkInterface pNetworkInterfaceId pInstanceId pDeviceIndex = AttachNetworkInterface'{_aniDryRun = Nothing, _aniNetworkInterfaceId = pNetworkInterfaceId, _aniInstanceId = pInstanceId, _aniDeviceIndex = pDeviceIndex};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
aniDryRun :: Lens' AttachNetworkInterface (Maybe Bool)
aniDryRun = lens _aniDryRun (\ s a -> s{_aniDryRun = a});

-- | The ID of the network interface.
aniNetworkInterfaceId :: Lens' AttachNetworkInterface Text
aniNetworkInterfaceId = lens _aniNetworkInterfaceId (\ s a -> s{_aniNetworkInterfaceId = a});

-- | The ID of the instance.
aniInstanceId :: Lens' AttachNetworkInterface Text
aniInstanceId = lens _aniInstanceId (\ s a -> s{_aniInstanceId = a});

-- | The index of the device for the network interface attachment.
aniDeviceIndex :: Lens' AttachNetworkInterface Int
aniDeviceIndex = lens _aniDeviceIndex (\ s a -> s{_aniDeviceIndex = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest AttachNetworkInterface where
        type Sv AttachNetworkInterface = EC2
        type Rs AttachNetworkInterface =
             AttachNetworkInterfaceResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 AttachNetworkInterfaceResponse' <$>
                   (x .@? "attachmentId"))

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
               "DryRun" =: _aniDryRun,
               "NetworkInterfaceId" =: _aniNetworkInterfaceId,
               "InstanceId" =: _aniInstanceId,
               "DeviceIndex" =: _aniDeviceIndex]

-- | /See:/ 'attachNetworkInterfaceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'anirAttachmentId'
newtype AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse'{_anirAttachmentId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AttachNetworkInterfaceResponse' smart constructor.
attachNetworkInterfaceResponse :: AttachNetworkInterfaceResponse
attachNetworkInterfaceResponse = AttachNetworkInterfaceResponse'{_anirAttachmentId = Nothing};

-- | The ID of the network interface attachment.
anirAttachmentId :: Lens' AttachNetworkInterfaceResponse (Maybe Text)
anirAttachmentId = lens _anirAttachmentId (\ s a -> s{_anirAttachmentId = a});
