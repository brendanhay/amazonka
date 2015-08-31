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
-- Module      : Network.AWS.EC2.AttachNetworkInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a network interface to an instance.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachNetworkInterface.html AWS API Reference> for AttachNetworkInterface.
module Network.AWS.EC2.AttachNetworkInterface
    (
    -- * Creating a Request
      attachNetworkInterface
    , AttachNetworkInterface
    -- * Request Lenses
    , aniDryRun
    , aniNetworkInterfaceId
    , aniInstanceId
    , aniDeviceIndex

    -- * Destructuring the Response
    , attachNetworkInterfaceResponse
    , AttachNetworkInterfaceResponse
    -- * Response Lenses
    , anirsAttachmentId
    , anirsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachNetworkInterface' smart constructor.
data AttachNetworkInterface = AttachNetworkInterface'
    { _aniDryRun             :: !(Maybe Bool)
    , _aniNetworkInterfaceId :: !Text
    , _aniInstanceId         :: !Text
    , _aniDeviceIndex        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachNetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aniDryRun'
--
-- * 'aniNetworkInterfaceId'
--
-- * 'aniInstanceId'
--
-- * 'aniDeviceIndex'
attachNetworkInterface
    :: Text -- ^ 'aniNetworkInterfaceId'
    -> Text -- ^ 'aniInstanceId'
    -> Int -- ^ 'aniDeviceIndex'
    -> AttachNetworkInterface
attachNetworkInterface pNetworkInterfaceId_ pInstanceId_ pDeviceIndex_ =
    AttachNetworkInterface'
    { _aniDryRun = Nothing
    , _aniNetworkInterfaceId = pNetworkInterfaceId_
    , _aniInstanceId = pInstanceId_
    , _aniDeviceIndex = pDeviceIndex_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
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

instance AWSRequest AttachNetworkInterface where
        type Rs AttachNetworkInterface =
             AttachNetworkInterfaceResponse
        request = postQuery eC2
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
               "DryRun" =: _aniDryRun,
               "NetworkInterfaceId" =: _aniNetworkInterfaceId,
               "InstanceId" =: _aniInstanceId,
               "DeviceIndex" =: _aniDeviceIndex]

-- | /See:/ 'attachNetworkInterfaceResponse' smart constructor.
data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse'
    { _anirsAttachmentId   :: !(Maybe Text)
    , _anirsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachNetworkInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'anirsAttachmentId'
--
-- * 'anirsResponseStatus'
attachNetworkInterfaceResponse
    :: Int -- ^ 'anirsResponseStatus'
    -> AttachNetworkInterfaceResponse
attachNetworkInterfaceResponse pResponseStatus_ =
    AttachNetworkInterfaceResponse'
    { _anirsAttachmentId = Nothing
    , _anirsResponseStatus = pResponseStatus_
    }

-- | The ID of the network interface attachment.
anirsAttachmentId :: Lens' AttachNetworkInterfaceResponse (Maybe Text)
anirsAttachmentId = lens _anirsAttachmentId (\ s a -> s{_anirsAttachmentId = a});

-- | The response status code.
anirsResponseStatus :: Lens' AttachNetworkInterfaceResponse Int
anirsResponseStatus = lens _anirsResponseStatus (\ s a -> s{_anirsResponseStatus = a});
